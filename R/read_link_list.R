##' @export
read_link_list <- function(file) {
  types <- readr::cols(
    translation = readr::col_guess(),
    links = readr::col_character()
  )
  suppressWarnings(df <- readr::read_tsv(file, col_types = types))

  llist <- stringr::str_split(df$links, ";")
  llist <- lapply(llist, as.numeric)

  if ("translation" %in% colnames(df))
    attr(llist, "translation") <- df$translation

  class(llist) <- "link_list"
  llist
}
