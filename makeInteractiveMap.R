## Copyright (C) 2017-2018 Timothée Flutre
## License AGPL-3 https://www.gnu.org/licenses/agpl.html

##' Make an interactive map
##'
##' Make an interactive map.
##' @param center.lng longitude of the map center
##' @param center.lat latitude of the map center
##' @param zoom zoom of the map
##' @param geojsons to add layers with polygons, list whose components are lists with compulsory attributes 'name' (for the legend) and 'file' (in geoJSON format), and optional attributes 'color' (default="#03F"), 'weight' (default=1), 'opacity' (default=0.5), 'fill' (default=FALSE), 'fillColor' (default="#808080"), 'fillOpacity' (default=0.2) and 'highlightOptions' (default=NULL)
##' @param jpegs to add layers of markers with georeferenced photos, list whose components are lists with compulsory attributes 'name' (for the legend) and 'glob', and optional attribute 'icon' (requires \code{marker.icons})
##' @param img.max.height maximum height in pixels for the output images (if NULL, no proportional resizing will be performed)
##' @param out.dir path to the output directory (if NULL, will be the current directory)
##' @param thumbnails.width width of the thumbnail photos in the popups
##' @param thumbnails.height height of the thumbnail photos in the popups
##' @param marker.icons output from [leaflet::iconList()] (optional, used for custom icons)
##' @param author author of the map
##' @param lang language (fr/en)
##' @param author.url URL associated with the author
##' @param verbose verbosity level (0/1/2)
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
##' geojsons <- list(list(name="Provinces",
##'                       file="<...>/<...>.geojson"))
##'
##' ## execute the function
##' makeInteractiveMap(geojsons=geojsons)
##'
##' ## open the html file in your web browser
##' }
##' @export
##' @md
makeInteractiveMap <- function(center.lng=108.194733,
                               center.lat=16.049377,
                               zoom=5,
                               geojsons=NULL,
                               jpegs=NULL,
                               img.max.height=1200,
                               out.dir=NULL,
                               thumbnails.width=400,
                               thumbnails.height=300,
                               marker.icons=NULL,
                               author="<author>",
                               lang="fr",
                               author.url=NULL,
                               verbose=1){
  ## check inputs
  if(is.null(out.dir))
    out.dir <- paste0(getwd(), "/interactive-map")
  if(dir.exists(out.dir)){
    msg <- paste0("output directory '", out.dir, "' already exists")
    stop(msg)
  }
  stopifnot(requireNamespace("leaflet"))
  stopifnot(requireNamespace("htmlwidgets"))
  stopifnot(requireNamespace("htmltools"))
  if(! is.null(geojsons))
    stopifnot(is.list(geojsons),
              all(sapply(geojsons, class) == "list"),
              all(sapply(geojsons, function(x){
                ! is.null(names(x))
              })),
              all(sapply(geojsons, function(x){
                all(c("name","file") %in% names(x))
              })),
              all(sapply(geojsons, function(x){
                is.character(x[["name"]])
              })),
              all(sapply(geojsons, function(x){
                is.character(x[["file"]])
              })),
              ## requireNamespace("geojsonio"))
              requireNamespace("sf"))
  if(! is.null(jpegs))
    stopifnot(is.list(jpegs),
              all(sapply(jpegs, class) == "list"),
              all(sapply(jpegs, function(x){
                ! is.null(names(x))
              })),
              all(sapply(jpegs, function(x){
                all(c("name","glob") %in% names(x))
              })),
              all(sapply(jpegs, function(x){
                is.character(x[["name"]])
              })),
              all(sapply(jpegs, function(x){
                is.character(x[["glob"]])
              })),
              requireNamespace("exifr"),
              requireNamespace("mapview"))
  if(! is.null(jpegs) & ! is.null(img.max.height))
    stopifnot(requireNamespace("magick"))
  if(! is.null(marker.icons))
    stopifnot(is.list(marker.icons),
              class(marker.icons) == "leaflet_icon_set",
              all(sapply(sapply(marker.icons, `[`, "iconUrl"), file.exists)))
  stopifnot(is.character(author),
            length(author) == 1,
            is.character(lang),
            lang %in% c("fr","en"))
  if(! is.null(author.url))
    stopifnot(is.character(author.url),
              length(author.url) == 1)

  ## create out dir
  if(verbose > 0){
    msg <- paste0("create the output directory '", out.dir, "'...")
    write(msg, stdout())
  }
  dir.create(out.dir)

  ## make the map
  if(verbose > 0){
    msg <- "make the map..."
    write(msg, stdout())
  }
  m <- leaflet::leaflet()
  m <- leaflet::setView(m,
                        lng=center.lng, lat=center.lat, zoom=zoom)
  m <- leaflet::addProviderTiles(m,
                                 leaflet::providers$OpenStreetMap)
  m <- leaflet::addTiles(m,
                         group="OSM (default)")
  m <- leaflet::addProviderTiles(m,
                                 leaflet::providers$CartoDB.Positron,
                                 group="CartoDB")

  ## add author info
  tmp <- paste0("<p>",
                ifelse(lang == "en", "Made by:", "Réalisé par :"),
                "<br />")
  if(is.null(author.url)){
    tmp <- paste0(tmp, author)
  } else
    tmp <- paste0(tmp,
                  '<a href="', author.url, '">', author, '</a>')
  tmp <- paste0(tmp,
                ", ", format.Date(Sys.Date(), format="%Y"),
                ".</p>")
  title <- htmltools::tags$div(htmltools::HTML(tmp))
  m <- leaflet::addControl(m,
                           html=title,
                           position="bottomright")

  ## add mini map
  m <- leaflet::addMiniMap(m,
                           position="bottomleft",
                           zoomLevelOffset=-6,
                           toggleDisplay=TRUE)

  ovl.groups <- c()

  ## add layers of polygons
  if(! is.null(geojsons)){
    for(i in seq_along(geojsons)){
      if(verbose > 0){
        msg <- paste0("handle geojson ", i, "/", length(geojsons),
                      " '", geojsons[[i]]$name, "'...")
        write(msg, stdout())
      }
      if(file.exists(geojsons[[i]]$file)){
        ## tmp.sp <- geojsonio::geojson_read(geojsons[[i]]$file, what="sp")
        tmp <- sf::st_read(geojsons[[i]]$file,
                           quiet=ifelse(verbose <= 1, TRUE, FALSE))
        tmp <- sf::st_zm(tmp)
        tmp.sp <- methods::as(tmp, "Spatial")
        polygon.color <-
          ifelse("color" %in% names(geojsons[[i]]),
                 geojsons[[i]]$color,
                 "#03F")
        polygon.weight <-
          ifelse("weight" %in% names(geojsons[[i]]),
                 geojsons[[i]]$weight,
                 1)
        polygon.opacity <-
          ifelse("fill" %in% names(geojsons[[i]]),
                 geojsons[[i]]$opacity,
                 0.5)
        polygon.fill <-
          ifelse("fill" %in% names(geojsons[[i]]),
                 geojsons[[i]]$fill,
                 FALSE)
        polygon.fillColor <-
          ifelse("fill" %in% names(geojsons[[i]]),
                 geojsons[[i]]$fillColor,
                 "#808080")
        polygon.fillOpacity <-
          ifelse("fill" %in% names(geojsons[[i]]),
                 geojsons[[i]]$fillOpacity,
                 0.2)
        polygon.highlightOptions <- NULL
        if("highlightOptions" %in% names(geojsons[[i]]))
          polygon.highlightOptions <- geojsons[[i]]$highlightOptions
        m <- leaflet::addPolygons(
                          m,
                          data=tmp.sp,
                          group=geojsons[[i]]$name,
                          stroke=TRUE,
                          color=polygon.color,
                          weight=polygon.weight,
                          opacity=polygon.opacity,
                          fill=polygon.fill,
                          fillColor=polygon.fillColor,
                          fillOpacity=polygon.fillOpacity,
                          highlightOptions=polygon.highlightOptions)
        ovl.groups <- c(ovl.groups, geojsons[[i]]$name)
      } else{
        msg <- paste0("skip it because file '", geojsons[[i]]$file,
                      "' doesn't exist")
        warning(msg, call.=FALSE, immediate.=TRUE)
      }
    }
  }

  ## add layers of markers for photos
  if(! is.null(jpegs)){
    for(i in seq_along(jpegs)){
      if(verbose > 0){
        msg <- paste0("handle jpegs ", i, "/", length(jpegs),
                      " '", jpegs[[i]]$name, "'...")
        write(msg, stdout())
      }

      ## copy photos (and eventually resize)
      photos.dir <- paste0(out.dir, "/img", i, "/graphs") # name made compulsory by mapview::popupLocalImage()
      dir.create(photos.dir, showWarnings=FALSE, recursive=TRUE)
      photos <- Sys.glob(jpegs[[i]]$glob)
      if(length(photos) == 0){
        msg <- "skip it because no photos were found"
        warning(msg, call.=FALSE, immediate.=TRUE)
        unlink(paste0(out.dir, "/img", i), recursive=TRUE)
        next
      }
      if(verbose > 0){
        msg <- paste0(length(photos), " photo",
                      ifelse(length(photos) == 1, "", "s"), " found")
        write(msg, stdout())
      }
      if(is.null(img.max.height)){
        file.copy(photos, photos.dir)
      } else{
        isImgToTall <- function(img, img.max.height){
          magick::image_info(img)$height > img.max.height
        }
        for(j in seq_along(photos)){
          img <- magick::image_read(photos[j])
          if(isImgToTall(img, img.max.height))
            img <- magick::image_scale(img, paste0("x", img.max.height))
          p2f.new <- paste0(photos.dir, "/", basename(photos[j]))
          stopifnot(! file.exists(p2f.new))
          magick::image_write(img, path=p2f.new, format="jpeg")
        }
      }

      ## extract GPS coords
      tmp <- list.files(photos.dir, full.names=TRUE)
      dat <- exifr::read_exif(tmp, recursive=FALSE,
                              quiet=ifelse(verbose <= 1, TRUE, FALSE))
      stopifnot(all(c("SourceFile", "DateTimeOriginal", "GPSLongitude",
                      "GPSLatitude") %in% colnames(dat)))
      dat <- dat[, c("SourceFile", "DateTimeOriginal",
                     "GPSLongitude", "GPSLatitude")]
      has.coords <- ! is.na(dat$GPSLongitude) & ! is.na(dat$GPSLatitude)
      if(! any(has.coords)){
        msg <- "skip it because no photo has GPS coordinates"
        dat <- NULL
        warning(msg, call.=FALSE, immediate.=TRUE)
        unlink(photos.dir, recursive=TRUE)
        next
      }
      if(! all(has.coords)){
        msg <- paste0("only ", sum(has.coords), " have GPS coordinates")
        warning(msg, call.=FALSE, immediate.=TRUE)
        dat <- dat[which(has.coords),]
      }

      ## add marker(s)
      marker.icon <- NULL
      if(! is.null(marker.icons) &
         "icon" %in% names(jpegs[[i]])){
        if(jpegs[[i]]$icon %in% names(marker.icons))
          marker.icon <- marker.icons[jpegs[[i]]$icon]
        else{
          msg <- paste0("icon '", jpegs[[i]]$icon,
                        "' not found in marker.icons")
          warning(msg, call.=FALSE, immediate.=TRUE)
        }
      }
      for(j in 1:nrow(dat)){
        p2f <- paste0("img", i, "/graphs/",
                      basename(dat$SourceFile[j]))
        m <- leaflet::addMarkers(
                          m,
                          group=jpegs[[i]]$name,
                          lng=dat$GPSLongitude[j],
                          lat=dat$GPSLatitude[j],
                          popup=mapview::popupImage(
                                             p2f,
                                             src="local",
                                             height=thumbnails.height,
                                             width=thumbnails.width),
                          icon=marker.icon)
      }
      ovl.groups <- c(ovl.groups, jpegs[[i]]$name)

    } # end of "for i in jpegs"
  } # end of "if jpegs not NULL"

  ## add layers controls
  m <- leaflet::addLayersControl(
                    m,
                    baseGroups=c("OSM (default)", "CartoDB"),
                    overlayGroups=ovl.groups,
                    options=leaflet::layersControlOptions(collapsed=FALSE))

  ## save the map
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
