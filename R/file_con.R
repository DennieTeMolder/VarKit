##' @export
file_con <- function(description, open = "", ..., compression = NULL) {
  stopifnot(is.character(description), length(description) == 1L)
  stopifnot(is.character(open), length(open) == 1L)

  type <- substr(open, 1L, 1L)
  ext <- match(tools::file_ext(description), c("gz", "gzip", "bz2", "xz"), 0L)

  if (type != "w") {
    # Additional checks before passing to file()
    if (!file.exists(description))
      stop("File does not exist: ", description)
    if (type == "a" && ext != 0L)
      stop("Compressed files do not support appending!")
  } else {
    # Handle recognised extensions using the correct function
    if (ext == 1L || ext == 2L) {
      if (is.null(compression)) compression <- 6L
      return(gzfile(description = description, open = open, compression = compression, ...))
    } else if (ext == 3L) {
      if (is.null(compression)) compression <- 9L
      return(bzfile(description = description, open = open, compression = compression, ...))
    } else if (ext == 4L) {
      if (is.null(compression)) compression <- 6L
      return(xzfile(description = description, open = open, compression = compression, ...))
    }
  }

  file(description = description, open = open, ...)
}
