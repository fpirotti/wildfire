filename <- function(fullname){
  basename(tools::file_path_sans_ext(fullname))
}

sameFileNoExt <- function(fullname, fullname2){
  filename(fullname) == filename(fullname2)
}
`%+%` <- function(a, b) paste0(a, b)
