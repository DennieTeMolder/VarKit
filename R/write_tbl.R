##' @export
write_tbl <- function(x,
                      file,
                      sep = "\t",
                      append = FALSE,
                      col_names = !append,
                      row_names = FALSE,
                      quote = FALSE,
                      ...) {
  if (inherits(file, "connection")) {
    if (!is_open.connection(file))
      stop("The provided 'file' connection is invalid!")
  } else {
    file <- file_con(file, if (append) "a" else "w")
    on.exit(close(file))
  }

  utils::write.table(
    x = x, file = file, sep = sep, append = FALSE, col.names = col_names,
    row.names = row_names, quote = quote, ...
  )
}
