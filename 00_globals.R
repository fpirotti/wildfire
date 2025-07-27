filename <- function(fullname){
  basename(tools::file_path_sans_ext(fullname))
}

sameFileNoExt <- function(fullname, fullname2){
  filename(fullname) == filename(fullname2)
}
`%+%` <- function(a, b) paste0(a, b)

if(file.exists("message.log")) file.remove("message.log")
if(file.exists("warnings.log"))file.remove("warnings.log")
warning_log <- function(..., logfile = "warnings.log", call. = TRUE, immediate. = FALSE) {
  # Write to logfile
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", paste(..., concatenate=" "), "\n", file = logfile, append = TRUE)
  # Emit warning
  warning(..., call. = call., immediate. = immediate.)
}
message_log <- function(..., logfile = "message.log", call. = TRUE, immediate. = FALSE) {
  # Write to logfile
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", paste(..., concatenate=" "), "\n", file = logfile, append = TRUE)
  # Emit warning
  message(...)
}
