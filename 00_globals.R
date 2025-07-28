logfile <- "logMessage.log"
warnfile <- "logWarning.log"

filename <- function(fullname){
  basename(tools::file_path_sans_ext(fullname))
}

sameFileNoExt <- function(fullname, fullname2){
  filename(fullname) == filename(fullname2)
}
`%+%` <- function(a, b) paste0(a, b)

if(file.exists(logfile)) file.remove(logfile)
if(file.exists(warnfile))file.remove(warnfile)
warning_log <- function(...,   call. = TRUE, immediate. = FALSE) {
  # Write to logfile
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", paste(..., concatenate=" "), "\n", file = warnfile, append = TRUE)
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", paste(..., concatenate=" "), "\n", file = logfile, append = TRUE)

  warning(..., call. = call., immediate. = immediate.)
}
message_log <- function(..., call. = TRUE, immediate. = FALSE) {
  # Write to logfile
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", paste(..., concatenate=" "), "\n", file = logfile, append = TRUE)
  # Emit warning
  message(...)
}
