##' @export
parse_meta_type <- function(x, type) {
  UseMethod("parse_meta_type", x)
}

##' @export
parse_meta_type.varkit_header <- function(x, type) {
  parse_meta_type(meta(x), type)
}

##' @export
parse_meta_type.varkit_meta <- function(x, type) {
  # Checks
  stopifnot(all(c("name", "value") %in% names(x)))

  x <- dplyr::filter(x, name == type)

  if (nrow(x) == 0)
    stop(paste0("The type '", type, "' could not be found in meta()$name!"))

  # We assume here that the values of the key-value pairs in a header line are
  # either all unquoted, or that it it starts off with one ore more unquoted
  # values after which all values are quoted.

  # Check if the first line only contains unquoted values. Unquoted key-value
  # pairs should not contain any apostrophes.
  if (!stringr::str_detect(x$value[1], "\""))
    return(.match_key_value(x$value, pattern = "<?([^=]+)=([^,]+)(?:,|>)"))

  # Split line based on transition from unquoted to quoted values
  x <- stringr::str_match(x$value, "<(.*[^\"]),([^=]+=\".*)>")

  # Parse unquoted and quoted key-value pairs separately
  unquoted <- .match_key_value(
    x[, 2],
    pattern = "([^=]+)=([^,]+)(?:,|$)"
  )
  quoted <- .match_key_value(
    x[, 3],
    pattern = "([^=]+)=\"(.+?)\"(?:,|$)"
  )

  dplyr::bind_cols(unquoted, quoted)
}
