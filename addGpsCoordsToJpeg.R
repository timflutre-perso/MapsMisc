## Copyright (C) 2018 Timothée Flutre
## License AGPL-3 https://www.gnu.org/licenses/agpl.html

##' Add GPS coordinates to a JPEG photo
##'
##' Add GPS coordinates interactively to a JPEG photo.
##' @return nothing
##' @author Timothee Flutre
##' @export
addGpsCoordsToJpeg <- function(){
  stopifnot(requireNamespace("tcltk"),
            requireNamespace("exifr"))

  ## get original photo file
  p2f.jpeg <- tcltk::tk_choose.files(caption="Choose JPEG file:")
  if(length(p2f.jpeg) == 0)
    stop("no input file, exit!", call.=FALSE)
  p2f.jpeg.ext <- tools::file_ext(p2f.jpeg)
  if(! p2f.jpeg.ext %in% c("jpg","jpeg","JPG","JPEG")){
    msg <- paste0("the file extension is '", p2f.jpeg.ext,
                  "' instead of jpg/jpeg/JPG/JPEG;\n",
                  "are you sure it is a JPEG file?")
    stop(msg, call.=FALSE)
  }
  msg <- paste0("The processed file is '", p2f.jpeg, "'...")
  message(msg)

  ## check if it has GPS coords
  dat <- exifr::read_exif(p2f.jpeg, recursive=FALSE, quiet=TRUE)
  has.coords <- all(c("GPSLongitude","GPSLatitude") %in% colnames(dat))
  if(has.coords){
    has.coords <- ! is.na(dat$GPSLongitude) & ! is.na(dat$GPSLatitude)
    if(has.coords){
      msg <- paste0("this file already contains GPS coordinates:",
                    "\nlongitude = ", dat$GPSLongitude,
                    "\nlatitude = ", dat$GPSLatitude)
      stop(msg, call.=FALSE)
    }
  }

  ## choose new file name
  p2f.jpeg.new <- paste0(tools::file_path_sans_ext(p2f.jpeg),
                         "_with-GPS.", p2f.jpeg.ext)
  msg <- paste0("The new file will be '", p2f.jpeg.new, "'")
  message(msg)
  if(file.exists(p2f.jpeg.new)){
    rmv.file <- readline(prompt="Do you want to remove it? (yes/no): ")
    if(rmv.file == "yes"){
      file.remove(p2f.jpeg.new)
      message("Done!")
    } else{
      stop("ok, exit then!", call.=FALSE)
    }
  }

  ## get GPS coords
  lat <- tryCatch({
    suppressWarnings(as.numeric(sub(",", ".",
                                    readline(prompt="Enter GPS latitude: "),
                                    fixed=TRUE)))
  },
  error=function(cond){
    return(NA)
  },
  warning=function(cond){
    return(NA)
  })
  if(is.na(lat)){
    msg <- "it doesn't seem to be a number"
    stop(msg, call.=FALSE)
  }
  long <- tryCatch({
    suppressWarnings(as.numeric(sub(",", ".",
                                    readline(prompt="Enter GPS longitude: "),
                                    fixed=TRUE)))
  },
  error=function(cond){
    return(NA)
  },
  warning=function(cond){
    return(NA)
  })
  if(is.na(long)){
    msg <- "it doesn't seem to be a number"
    stop(msg, call.=FALSE)
  }

  ## write GPS coord to new file
  msg <- "Write these GPS coordinates to the new file..."
  message(msg)
  file.copy(from=p2f.jpeg, to=p2f.jpeg.new)
  ret.val <- exifr::exiftool_call(args=c(paste0("-GPSLatitude=", lat),
                                         paste0("-GPSLongitude=", long)),
                                  fnames=p2f.jpeg.new,
                                  quiet=FALSE)
  orig.file <- paste0(p2f.jpeg.ext, "_original")
  if(file.exists(orig.file))
    file.remove(orig.file)
  if(ret.val != 0){
    msg <- "exiftool failed!"
    stop(msg, call.=FALSE)
  } else
    message("Done!")
}
