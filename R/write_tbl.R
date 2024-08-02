##' @export
write_tbl <- function(x,
                      file,
                      sep = "\t",
                      append = FALSE,
                      col_names = !append,
                      row_names = FALSE,
                      quote = sep == ",",
                      digits = NULL,
                      scipen = 9,
                      compression = NULL,
                      ...) {
  if (!.is_connection(file)) {
    file <- file_con(file, open = if (append) "a" else "w", compression = compression)
    on.exit(close(file))
  }

  # The 'scipen' option affects write.table() because it uses format() internally
  rlang::local_options(scipen = scipen)

  utils::write.table(
    x = .round(x, digits), file = file, sep = sep, append = FALSE,
    col.names = col_names, row.names = row_names, quote = quote, ...
  )
}
