##' @export
format_summary <- function(x) {
  UseMethod("format_summary", x)
}

##' @export
format_summary.varkit <- function(x) {
  if (any(is.na(x$fixed$format)))
    stop("The FORMAT column contains missing values!")

  uniq <- levels(x$fixed$format)
  # Some values might be dropped when masking, so we recompute the unique here
  if (length(uniq) != 1)
    uniq <- unique(fixed(x)$format)

  if (length(uniq) > 1)
    message("!FORMAT column is variable, number of combinations found: ", length(uniq))

  splits <- stringr::str_split(uniq, ":")
  splits <- unique(unlist(splits))
  splits <- tibble::tibble(type = splits)

  if ("FORMAT" %in% meta(x)$name) {
    metadata <- parse_meta_type(x, "FORMAT")
    metadata <- data.frame(type = metadata$ID, description = metadata$Description)
    return(dplyr::left_join(splits, metadata, by = "type"))
  }
  splits
}
