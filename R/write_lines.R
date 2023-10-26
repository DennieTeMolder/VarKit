##' @export
write_lines2 <- function(text, file, sep = "\n", na = "NA", append = FALSE, ...) {
  if (inherits(file, "connection")) {
    if (!is_open.connection(file))
      stop("The provided 'file' connection is invalid!")
  } else {
    file <- file_con(file, if (append) "a" else "w")
    on.exit(close(file))
  }
  stopifnot(is.character(na), length(na) == 1)

  if (anyNA(text))
    text[is.na(text)] <- na

  writeLines(text = text, con = file, sep = sep, ...)
}
