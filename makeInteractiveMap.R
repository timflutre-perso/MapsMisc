## Copyright (C) 2017-2018 Timothée Flutre
## License AGPL-3 https://www.gnu.org/licenses/agpl.html

##' Make an interactive map
##'
##' Make an interactive map.
##' @param center.lng longitude of the map center
##' @param center.lat latitude of the map center
##' @param zoom zoom of the map
##' @param provinces.file optional path to the file containing the provinces in GeoJSON format
##' @param photos.glob optional glob to find all photos to add to the map, for instance \code{"photos/*.JPG"}
##' @param img.max.height maximum height in pixels for the output images (if NULL, no proportional resizing will be performed)
##' @param cadastre.file optional path to the file containing the cadastre in GeoJSON format
##' @param out.dir path to the output directory (if NULL, will be the current directory)
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
##' prov.f <- "<...>/<...>.geojson"
##'
##' ## execute the function
##' makeInteractiveMap(provinces.file=prov.f)
##'
##' ## open the html file in your web browser
##' }
##' @export
makeInteractiveMap <- function(center.lng=108.194733,
                               center.lat=16.049377,
                               zoom=5,
                               provinces.file=NULL,
                               photos.glob=NULL,
                               img.max.height=1200,
                               cadastre.file=NULL,
                               out.dir=NULL,
                               thumbnails.width=400, thumbnails.height=300,
                               verbose=1){
  if(is.null(out.dir))
    out.dir <- paste0(getwd(), "/interactive-map")
  if(dir.exists(out.dir)){
    msg <- paste0("output directory '", out.dir, "' already exists")
    stop(msg)
  }
  requireNamespace("leaflet")
  requireNamespace("htmlwidgets")
  if(! is.null(photos.glob) & ! is.null(img.max.height))
    requireNamespace("magick")
  if(! is.null(provinces.file)){
    if(! file.exists(provinces.file)){
      msg <- paste0("provinces.file '", provinces.file, "' doesn't exist")
      warning(msg, immediate.=TRUE)
      provinces.file <- NULL
    }
  }
  if(! is.null(cadastre.file)){
    if(! file.exists(cadastre.file)){
      msg <- paste0("cadastre.file '", cadastre.file, "' doesn't exist")
      warning(msg, immediate.=TRUE)
      cadastre.file <- NULL
    }
  }
  if(! is.null(provinces.file) | ! is.null(cadastre.file))
    requireNamespace("geojsonio")
  if(! is.null(photos.glob)){
    requireNamespace("exifr")
    requireNamespace("mapview")
  }

  if(verbose > 0){
    msg <- paste0("create the output directory '", out.dir, "'...")
    write(msg, stdout())
  }
  dir.create(out.dir)

  prov <- NULL
  if(! is.null(provinces.file)){
    if(verbose > 0){
      msg <- "load the provinces..."
      write(msg, stdout())
    }
    prov <- geojsonio::geojson_read(provinces.file, what="sp")
  }

  dat <- NULL
  if(! is.null(photos.glob)){
    if(verbose > 0){
      msg <- "copy photos and load GPS coords..."
      write(msg, stdout())
    }
    photos.dir <- paste0(out.dir, "/graphs") # name made compulsory by mapview::popupLocalImage()
    dir.create(photos.dir, showWarnings=FALSE)
    photos <- Sys.glob(photos.glob)
    if(length(photos) == 0){
      msg <- "no photos were found, check 'photos.glob'"
      warning(msg)
    } else{
      if(is.null(img.max.height)){
        file.copy(photos, photos.dir)
      } else{
        isImgToTall <- function(img, img.max.height){
          magick::image_info(img)$height > img.max.height
        }
        for(i in seq_along(photos)){
          img <- magick::image_read(photos[i])
          if(isImgToTall(img, img.max.height))
            img <- magick::image_scale(img, paste0("x", img.max.height))
          p2f.new <- paste0(photos.dir, "/", basename(photos[i]))
          stopifnot(! file.exists(p2f.new))
          magick::image_write(img, path=p2f.new, format="jpeg")
        }
      }
      tmp <- list.files(photos.dir, full.names=TRUE)
      dat <- exifr::read_exif(tmp, recursive=FALSE,
                              quiet=ifelse(verbose <= 0, TRUE, FALSE))
      stopifnot(all(c("SourceFile", "DateTimeOriginal", "GPSLongitude",
                      "GPSLatitude") %in% colnames(dat)))
      dat <- dat[, c("SourceFile", "DateTimeOriginal",
                     "GPSLongitude", "GPSLatitude")]
      has.coords <- ! is.na(dat$GPSLongitude) & ! is.na(dat$GPSLatitude)
      if(! any(has.coords)){
        msg <- "no photo has GPS coordinate"
        dat <- NULL
        warning(msg)
      } else
        dat <- dat[which(has.coords),]
    }
  }

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

  ovl.groups <- c()

  if(! is.null(prov)){
    ovl.groups <- c(ovl.groups, "Provinces")
    m <- leaflet::addPolygons(m, data=prov, stroke=TRUE, group="Provinces",
                              fill=FALSE, weight=2)
  }

  if(! is.null(dat)){
    ovl.groups <- c(ovl.groups, "Photos")
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
  }

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
