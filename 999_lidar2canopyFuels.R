if(!require("lasR")) install.packages('lasR', repos = 'https://r-lidar.r-universe.dev')

## GLOBAL INFO -----
### input directory with all las files -----
indir <- "/archivio/shared/geodati/las/fvg"
odir <- file.path(indir, "outdir")
laspattern <- "(?i)\\.la(s|z)$"
### max num of LAS files to process ----
limit <- 10 ##limit=Inf
### list of LAS files ----
f <- NULL
## Helper functions ----
plotShade <- function(dtm){
  hillshade <- shade(terrain(dtm, "slope", unit="radians"),
                     terrain(dtm, "aspect", unit="radians"),
                     angle = 45, direction = 315)
  dtm_cols <- terrain.colors(100)
  dtm_cols_alpha <- adjustcolor(dtm_cols, alpha.f = 0.5)  # alpha.f from 0 (transparent) to 1 (opaque)

  plot(hillshade, col = gray.colors(100), main = "DTM with Hillshade", legend=F)
  plot(dtm, col = dtm_cols_alpha, add = TRUE )
}
`%+%` <- function(a, b) {
  paste0(a, b)
}
### checks if ground points classified already ----
hasGround <- function(f){
  pipe <- reader(filter=keep_class(2)) + write_las( ofile = "tmp.las")
  ans = exec(pipe, on = f[[1]],  progress = TRUE)
  suppressWarnings(file.remove("tmp.las"))
}
prepLog <- function(remove=F){
  logdir <- "logs"
  logfile <-  logdir%+%"/"%+%Sys.getpid()%+%"_Message.log"
  warnfile <- logdir%+%"/"%+%Sys.getpid()%+%"_Warning.log"
  if(!dir.exists(logdir)){
    dir.create(logdir)
  }
  if(remove&&file.exists(logfile)) file.remove(logfile)
  if(remove&&file.exists(warnfile))file.remove(warnfile)

  warning_log <<- function(...,   call. = TRUE, immediate. = FALSE) {
    # Write to logfile
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", paste(..., concatenate=" "), "\n", file = warnfile, append = TRUE)
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", paste(..., concatenate=" "), "\n", file = logfile, append = TRUE)

    warning(..., call. = call., immediate. = immediate.)
  }
  message_log <<- function(..., call. = TRUE, immediate. = FALSE) {
    # Write to logfile
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", paste(..., concatenate=" "), "\n", file = logfile, append = TRUE)
    # Emit warning
    message(...)
  }
}

## END helper functions

## step 0 prepare  -----
#' step00prepare
#' @description
#' Checks directory and return a list
#' of las file for processing.
#'
#'
#' @param verbose
#'
#' @returns A list of laz files
#' @export
#'
#' @examples
step00prepare <- function(verbose=F){
  prepLog()
  if(verbose) message_log("Starting")

  lasR::set_parallel_strategy(lasR::nested(ncores = 8L, ncores2 = 4L))
  ## input directory with all las files -----

  if(!dir.exists(indir)){
    if(verbose) warning_log("No '",indir,"' directory found! Stopping")
    return(NULL)
  }
  f  <<- list.files(indir,
                        pattern=laspattern,
                        recursive = T,
                        ignore.case = T,
                        full.names = T)

  if(length(f)==0){
    warning_log("No LAS/LAZ files found in ",indir," directory! Stopping")
    f  <<- NULL
    return(NULL)
  }

  ## create out directory -----
  if(verbose) message_log("Checking ",odir," directory.")
  if(dir.exists(odir)){
    if(verbose) message_log(odir," Esists.")
    answer <- readline(prompt = "Do you want to overwrite contents of ? (Y/N): ")
    if (tolower(answer) == "Y") {
      if(verbose) message_log("Removing " , odir)
      file.remove(odir)
      suppressWarnings(dir.create(odir))
    }
  } else {
    if(verbose) message_log("Creating " , odir)
    suppressWarnings(dir.create(odir))
  }
  if(limit != Inf){
    message("Limito a ", limit, " LAS/Z files...")
    f<<-f[1:limit]
  }
  f
}



# step 1 - classify ground points -----------
step01createGround<-function(verbose=F){
  if(!exists("f")){
    warning_log("Please run previous functions, no files found")
    stop("No files")
  }
  ### create ground points ---------
  fsz <- file.size(f)/1000000 ## file size in MB
  mm <- which(fsz > 1 )
  if(length(mm)==0){
    mm <- which.max(fsz)
  } else {
    mm <- mm[[1]]
  }
  if(verbose) message_log("Checking if ground class already present, by choosing 1 file ")
  if(!hasGround(f[[mm]])){
    if(verbose) message_log("Ground points are NOT  available, will create using CSF algorithm.")

    pipeline = classify_with_csf(TRUE, 1 ,1, time_step = 1) + write_las(file.path(odir,"/ground_*.laz"))
    fg = exec(pipeline, on = f, progress = TRUE)
    f <<- fg
  } else {
    if(verbose) message_log("Ground points are already available.")
  }
  ### FINISH check  ground class has points


}

# step 2 - classify ground points -----------
step02createDigitalModels<-function(verbose=T, force=F, kriging=F, plot=F){

  outDTM <- list.files(odir,"DTM_.*\\.tif$")
  if(T){
    del = triangulate(filter = keep_ground())
    dtm = rasterize(1, del, ofile = file.path(odir,"/DTM_*.tif") )
    pipeline = del + dtm
    outDTM = exec(pipeline, on = f)
  }
  vrt <- terra::vrt(ans)
  if( isTRUE(plot)){
    if( dir.exists( dirname(plot) )){
      plotShade(vrt)
    } else {
      plotShade(vrt)
    }
  }
  vrt
}

# PROCESS -----
o <- step00prepare(verbose = T)
if(!is.null(o)){
  step01createGround(T)
  step02createDigitalModels(T)
  step03normalise()
}

