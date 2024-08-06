##' @export
parse_meta <- function(x, type = NULL, na = ".") {
  UseMethod("parse_meta", x)
}

##' @export
parse_meta.varkit <- function(x, type = NULL, na = ".") {
  .validate_varkit_meta(meta(x))
  stopifnot(is.character(na))
  if (!is.null(type)) {
    .stopifnotsingle(type, what = "character")
  } else {
    warning("Please provide the 'type' argument, returning possible options.")
    return(.get_meta_types(x))
  }

  values <- with(meta(x), value[name == type])

  if (length(values) == 0)
    stop("The type '", type, "' could not be found in meta(x)$name!")

  # Values should be surrounded by <...> according to the VCF spec, else abort
  if (!all(startsWith(values, "<") & endsWith(values, ">"), na.rm = TRUE))
    return(tibble::tibble(value = values))

  # Strip <...>
  values <- stringr::str_sub(values, 2L, -2L)

  # First group of the regex should match the key, second should match the corresponding value
  values <- .match_key_value(values, pattern = '([^="]+)=("(?:[^"\\\\]|\\\\.)+"|[^,]+)(?:,|$)')

  values <- lapply(values, .cleanup_meta_col, na = na)
  do.call(tibble::tibble, values)
}

.get_meta_types <- function(vcf) {
  df <- as.data.frame(table(meta(vcf)$name), stringsAsFactors = FALSE)
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
  # Strip surrounding double quotes
  res <- ifelse(
    startsWith(col, '"') & endsWith(col, '"'),
    substr(col, 2L, nchar(col) - 1L),
    col
  )
  utils::type.convert(res, as.is = TRUE, na.strings = na)
}

##' @export
parse_meta.varkit_header <- parse_meta.varkit
