##' @export
write_link_list <- function(x, file) {
  UseMethod("write_link_list", x)
}

##' @export
write_link_list.link_list <- function(x, file) {
  df <- data.frame(links = sapply(x, .collapse, delim = ";"))
  trans <- attr(x, "translation")
  if (!is.null(trans))
    df <- cbind(translation = trans, df)
  readr::write_tsv(df, file = file)
}
