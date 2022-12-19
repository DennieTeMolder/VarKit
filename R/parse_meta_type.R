##' @export
parse_meta_type <- function(x, type) {
  UseMethod("parse_meta_type", x)
}

##' @export
parse_meta_type.varkit_header <- function(x, type) {
  parse_meta_type(meta(x), type)
}

##' @export
parse_meta_type.default <- function(x, type) {
  # Checks
  stopifnot(colnames(x) == c("name", "value"))

  values <- x$value[x$name == type]

  if (length(values) == 0)
    stop(paste0("The type '", type, "' could not be found in meta(x)$name!"))

  values <- stringr::str_remove_all(values, "(^<|>$)")

  # First group of the regex should match the key, second should match the corresponding value
  values <- .match_key_value(values, pattern = '([^="]+)=("(?:[^"\\\\]|\\\\.)+"|[^,]+)(?:,|$)')

  purrr::map_dfc(values, .cleanup_col)
}

.match_key_value <- function(str, pattern) {
  res <- stringr::str_match_all(str, pattern)
  res <- lapply(res, .build_df)
  do.call(dplyr::bind_rows, res)
}

.build_df <- function(mat) {
  res <- matrix(mat[, 3], nrow = 1)
  colnames(res) <- mat[, 2]
  as.data.frame(res)
}

.cleanup_col <- function(col) {
  stopifnot(is.character(col))
  # Strip surrounding double quotes
  res <- ifelse(
    stringr::str_starts(col, '"') & stringr::str_ends(col, '"'),
    stringr::str_sub(col, 2, -2),
    col
  )
  utils::type.convert(res, as.is = TRUE, na.strings = c("NA", "."))
}
