##' @export
parse_meta <- function(x, type = NULL, na = ".", add_index = FALSE) {
  if (!missing(type) && is.null(type))
    warning("The 'type' argument is NULL, returning possible options.")
  UseMethod("parse_meta", x)
}

##' @export
parse_meta.varkit <- function(x, type = NULL, na = ".", add_index = FALSE) {
  validate_varkit(x)
  .parse_meta_internal(meta.varkit(x), type = type, na = na, add_index = add_index)
}

##' @export
parse_meta.varkit_header <- function(x, type = NULL, na = ".", add_index = FALSE) {
  validate_varkit_header(x)
  .parse_meta_internal(meta.varkit_header(x), type = type, na = na, add_index = add_index)
}

.parse_meta_internal <- function(meta, type, na, add_index) {
  stopifnot(is.character(na))
  if (!is.null(type)) {
    .stopifnotsingle(type, what = "character")
  } else {
    return(.get_meta_types(meta))
  }
  .stopifnotsingle(add_index, what = "logical")

  if (add_index)
    meta$index <- seq_len(nrow(meta))

  meta_type <- meta[meta$name == type, ]
  if (nrow(meta_type) == 0)
    return(NULL)

  # Values should be surrounded by <...> according to the VCF spec, else abort
  if (!all(startsWith(meta_type$value, "<") & endsWith(meta_type$value, ">"), na.rm = TRUE))
    return(meta_type)

  # Strip <...>
  meta_type$value <- stringr::str_sub(meta_type$value, 2L, -2L)

  # First group of the regex should match the key, second should match the corresponding value
  res <- .match_key_value(meta_type$value, pattern = '([^="]+)=("(?:[^"\\\\]|\\\\.)+"|[^,]+)(?:,|$)')

  res <- lapply(res, .cleanup_meta_col, na = na)
  if (add_index)
    res <- c(res, list(index = meta_type$index))

  do.call(tibble::tibble, res)
}

.get_meta_types <- function(meta) {
  df <- as.data.frame(table(meta$name), stringsAsFactors = FALSE)
  colnames(df) <- c("type", "n")
  tibble::as_tibble(df[order(df$type), ])
}

.match_key_value <- function(str, pattern) {
  res <- stringr::str_match_all(str, pattern)
  # Unpack
  res <- lapply(res, function(mat) {
    vec <- mat[, 3L]
    names(vec) <- mat[, 2L]
    vec
  })
  do.call(dplyr::bind_rows, res)
}

.cleanup_meta_col <- function(col, na = ".") {
  stopifnot(is.character(col))

  # Strip enclosing double quotes
  which_strip <- which(startsWith(col, '"') & endsWith(col, '"'))
  if (length(which_strip) > 0L)
    col[which_strip] <- stringr::str_sub(col[which_strip], start = 2L, end = -2L)

  utils::type.convert(col, as.is = TRUE, na.strings = na)
}

