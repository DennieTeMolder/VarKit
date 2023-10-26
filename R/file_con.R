##' @export
file_con <- function(description, open = "", ..., compression = NULL) {
  stopifnot(is.character(description), length(description) == 1)
  stopifnot(is.character(open), length(open) == 1)

  type <- substr(open, 1, 1)
  ext <- match(tools::file_ext(description), c("gz", "gzip", "bz2", "xz"), 0L)

  if (type == "a" && ext != 0L && file.exists(description))
    stop("Compressed files do not support appending!")

  if (type != "w") {
    # Skip & let file() handle this case
  } else if (ext == 1L || ext == 2L) {
    if (is.null(compression)) compression <- 6
    return(gzfile(description = description, open = open, compression = compression, ...))
  } else if (ext == 3L) {
    if (is.null(compression)) compression <- 9
    return(bzfile(description = description, open = open, compression = compression, ...))
  } else if (ext == 4L) {
    if (is.null(compression)) compression <- 6
    return(xzfile(description = description, open = open, compression = compression, ...))
  }

  file(description = description, open = open, ...)
}
