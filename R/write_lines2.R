##' @export
write_lines2 <- function(text, file, sep = "\n", na = "NA", append = FALSE, compression = NULL, ...) {
  stopifnot(is.character(na), length(na) == 1)

  if (!.is_connection(file)) {
    file <- file_con(file, open = if (append) "a" else "w", compression = compression)
    .defer(close(file))
  }

  if (anyNA(text))
    text[is.na(text)] <- na

  writeLines(text = text, con = file, sep = sep, ...)
}
