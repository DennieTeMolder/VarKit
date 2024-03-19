##' @export
write_tbl <- function(x,
                      file,
                      sep = "\t",
                      append = FALSE,
                      col_names = !append,
                      row_names = FALSE,
                      quote = sep == ",",
                      compression = NULL,
                      ...) {
  if (!.is_connection(file)) {
    file <- file_con(file, open = if (append) "a" else "w", compression = compression)
    on.exit(close(file))
  }

  utils::write.table(
    x = x, file = file, sep = sep, append = FALSE, col.names = col_names,
    row.names = row_names, quote = quote, ...
  )
}
