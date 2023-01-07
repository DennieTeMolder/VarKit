##' @export
get_format_types <- function(x) {
  UseMethod("get_format_types", x)
}

##' @export
get_format_types.varkit <- function(x) {
  frmt <- site(x, cols = "format")
  if (any(is.na(frmt)))
    stop("The FORMAT column contains missing values!")

  uniq <- as.character(unique(frmt))

  if (length(uniq) > 1)
    message("!FORMAT column is variable, number of combinations found: ", length(uniq))

  splits <- stringr::str_split(uniq, ":")
  splits <- sort(unique(unlist(splits)))
  splits <- tibble::tibble(type = splits)

  if ("FORMAT" %in% meta(x)$name) {
    metadata <- parse_meta_type(x, "FORMAT")
    metadata <- data.frame(type = metadata$ID, description = metadata$Description)
    return(dplyr::left_join(splits, metadata, by = "type"))
  }
  splits
}
