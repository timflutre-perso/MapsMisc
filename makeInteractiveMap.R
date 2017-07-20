## Copyright (C) 2017 Timothée Flutre
## License AGPL-3 https://www.gnu.org/licenses/agpl.html

##' Make an interactive map
##'
##' Make an interactive map.
##' @param photos.glob glob to find all photos to add to the map
##' @param cadastre.file path to the file containing the cadastre in GeoJSON format (can be NULL)
##' @param out.dir path to the output directory (if NULL, will be the current directory)
##' @param center.lng longitude of the map center
##' @param center.lat latitude of the map center
##' @param zoom zoom of the map
##' @param thumbnails.width width of the thumbnail photos in the popups
##' @param thumbnails.height height of the thumbnail photos in the popups
##' @param verbose verbosity level (0/1)
##' @return invisible leaflet map
##' @author Timothee Flutre
##' @examples
##' \dontrun{## retrieve the file containing the code of the function
##' download.file("https://github.com/timflutre-perso/MapsMisc/raw/master/makeInteractiveMap.R",
##'               "makeInteractiveMap.R")
##'
##' ## make the function available in the current R session
##' source("makeInteractiveMap.R")
##'
##' ## set the paths to files (replace <...> by what works on your computer)
##' photos.glob <- "<...>/*.JPG"
##' cadastre.file <- "<...>/<...>.geojson"
##' out.dir <- "<...>"
##'
##' ## execute the function
##' makeInteractiveMap(photos.glob, cadastre.file, out.dir)
##'
##' ## open the html file in your web browser
##' }
##' @export
makeInteractiveMap <- function(photos.glob,
                               cadastre.file=NULL,
                               out.dir=NULL,
                               center.lng=106.669554, center.lat=10.790601,
                               zoom=16,
                               thumbnails.width=300, thumbnails.height=300,
                               verbose=1){
  requireNamespace("exifr")
  requireNamespace("leaflet")
  requireNamespace("mapview")
  requireNamespace("htmlwidgets")
  requireNamespace("geojsonio")
  if(! is.null(cadastre.file))
    stopifnot(file.exists(cadastre.file))

  if(is.null(out.dir))
    out.dir <- paste0(getwd(), "/interactive-map")
  if(verbose > 0){
    msg <- paste0("create the output directory '", out.dir, "'...")
    write(msg, stdout())
  }
  if(dir.exists(out.dir)){
    msg <- paste0("output directory '", out.dir, "' already exists")
    stop(msg)
  }
  dir.create(out.dir)

  if(verbose > 0){
    msg <- "copy photos and load GPS coords..."
    write(msg, stdout())
  }
  photos.dir <- paste0(out.dir, "/photos")
  dir.create(photos.dir, showWarnings=FALSE)
  photos <- Sys.glob(photos.glob)
  if(length(photos) == 0){
    msg <- "no photos were found, check 'photos.glob'"
    stop(msg)
  }
  file.copy(photos, photos.dir)
  ## use pkg jpeg or imager to copy files in smaller resolution?
  tmp <- list.files(photos.dir, full.names=TRUE)
  dat <- exifr::exifr(tmp, recursive=TRUE,
                      quiet=ifelse(verbose <= 0, TRUE, FALSE))
  stopifnot(all(c("SourceFile", "DateTimeOriginal", "GPSLongitude",
                  "GPSLatitude") %in% colnames(dat)))
  dat <- dat[, c("SourceFile", "DateTimeOriginal",
                 "GPSLongitude", "GPSLatitude")]
  has.coords <- ! is.na(dat$GPSLongitude) & ! is.na(dat$GPSLatitude)
  if(! any(has.coords)){
    msg <- "no photo has GPS coordinate"
    stop(msg)
  }
  dat <- dat[which(has.coords),]

  cad <- NULL
  if(! is.null(cadastre.file)){
    if(verbose > 0){
      msg <- "load the cadastre..."
      write(msg, stdout())
    }
    cad <- geojsonio::geojson_read(cadastre.file, what="sp")
  }

  if(verbose > 0){
    msg <- "make the map..."
    write(msg, stdout())
  }
  m <- leaflet::leaflet()
  m <- leaflet::setView(m, lng=center.lng, lat=center.lat, zoom=zoom)
  m <- leaflet::addProviderTiles(m, leaflet::providers$OpenStreetMap)
  m <- leaflet::addTiles(m, group="OSM (default)")
  m <- leaflet::addProviderTiles(m, leaflet::providers$CartoDB.Positron,
                                 group="CartoDB")

  ovl.groups <- c("Photos")

  for(i in 1:nrow(dat))
    m <- leaflet::addMarkers(
                      m, lng=dat$GPSLongitude[i], lat=dat$GPSLatitude[i],
                      group="Photos",
                      popup=mapview::popupImage(
                                         paste0(basename(photos.dir), "/",
                                                basename(dat$SourceFile[i])),
                                         src="local",
                                         height=thumbnails.height,
                                         width=thumbnails.width))

  if(! is.null(cad)){
    ovl.groups <- c(ovl.groups, "Cadastre")
    m <- leaflet::addPolygons(m, data=cad, stroke=TRUE, group="Cadastre",
                              fill=FALSE, weight=1, color="#444444")
  }

  m <- leaflet::addLayersControl(
                    m, baseGroups=c("OSM (default)", "CartoDB"),
                    overlayGroups=ovl.groups,
                    options=leaflet::layersControlOptions(collapsed=FALSE))

  if(verbose > 0){
    msg <- "save the map..."
    write(msg, stdout())
  }
  htmlwidgets::saveWidget(m, file=paste0(out.dir, "/map.html"))

  msg <- paste0("open the following file in your web browser:\n",
                paste0(out.dir, "/map.html"))
  write(msg, stdout())

  invisible(m)
}
