##' @export
get_format_types <- function(x, check_consistent = FALSE) {
  UseMethod("get_format_types", x)
}

##' @export
get_format_types.varkit <- function(x, check_consistent = FALSE) {
  if (any(is.na(x$fixed$format)))
    stop("The FORMAT column contains missing values!")
  uniq <- levels(x$fixed$format)
  if (length(uniq) != 1)
    uniq <- unique(fixed(x)$format)
  if (check_consistent && length(uniq) > 1)
    stop("The FORMAT column differs between sites!")
  splits <- strsplit(uniq, ":")
  unique(unlist(splits))
}
