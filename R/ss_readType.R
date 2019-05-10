#' Given a pattern, returns a read-in of all data files matching that pattern
#'
#' @param location The folder location where data files are
#' @param pattern The pattern to match for reading in files
#' @name ss_readType

ss_readType <- function(location,pattern){
  file.names <- as.list(dir(location,pattern=pattern))
  data <- lapply(file.names, ss_read)
  file.names <- trimws(sub(pattern="(.*?)\\.dat(.*)000.*$",replacement = "\\1 \\2",x=file.names))
  names(data) <- file.names
  data <- tibble::set_tidy_names(data)
}
