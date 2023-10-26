##' @export
write_tbl <- function(x,
                      file,
                      sep = "\t",
                      append = FALSE,
                      col_names = !append,
                      row_names = !is.null(rownames(x)),
                      quote = FALSE,
                      ...) {
  utils::write.table(
    x = x, file = file, sep = sep, append = append, col.names = col_names,
    row.names = row_names, quote = quote, ...
  )
}
