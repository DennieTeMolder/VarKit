# NOTE in contrast to the R options DIGITS controls the actual number of digits
# (using round()) and NSIGNIF controls the number of significant numbers.
##' @export
write_tbl <- function(x,
                      file,
                      sep = "\t",
                      append = FALSE,
                      col_names = !append,
                      row_names = FALSE,
                      quote = sep == ",",
                      digits = NULL,
                      nsignif = getOption("digits", 7L),
                      scipen = 6,
                      compression = NULL,
                      ...) {
  if (!.is_connection(file)) {
    file <- file_con(file, open = if (append) "a" else "w", compression = compression)
    .defer(close(file))
  }

  # The 'ditigts' and 'scipen' options affect write.table() because it uses format() internally
  rlang::local_options(digits = nsignif, scipen = scipen)

  # Rounding on DIGITS is not possible during format() so we round() first
  utils::write.table(
    x = .round(x, digits), file = file, sep = sep, append = FALSE,
    col.names = col_names, row.names = row_names, quote = quote, ...
  )
}
