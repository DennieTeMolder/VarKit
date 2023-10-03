##' @export
parse_meta <- function(x, type = NULL, na = ".") {
  UseMethod("parse_meta", x)
}

##' @export
parse_meta.varkit_header <- function(x, type = NULL, na = ".") {
  stopifnot(colnames(meta(x)) == c("name", "value"))
  stopifnot(is.character(na))
  if (!is.null(type)) {
    .stopifnotsingle(type, class = "character")
  } else {
    warning("Please provide the 'type' argument, returning possible options.")
    return(.get_meta_types(x))
  }

  values <- meta(x)$value[meta(x)$name == type]

  if (length(values) == 0)
    stop("The type '", type, "' could not be found in meta(x)$name!")

  # Values should be surrounded by <...> according to the VCF spec, else abort
  if (any(!.str_starts(values, "<") | !.str_ends(values, ">")))
    return(tibble::tibble(value = values))

  # Strip <...>
  values <- stringr::str_sub(values, 2, -2)

  # First group of the regex should match the key, second should match the corresponding value
  values <- .match_key_value(values, pattern = '([^="]+)=("(?:[^"\\\\]|\\\\.)+"|[^,]+)(?:,|$)')

  .mutate_all(values, .cleanup_meta_col, na = na)
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
    vec <- mat[, 3]
    names(vec) <- mat[, 2]
    vec
  })
  do.call(dplyr::bind_rows, res)
}

.cleanup_meta_col <- function(col, na = ".") {
  stopifnot(is.character(col))
  # Strip surrounding double quotes
  n_char <- nchar(col)
  res <- ifelse(
    .str_starts(col, '"') & .str_ends(col, '"', n_char = n_char),
    substr(col, 2, n_char - 1),
    col
  )
  utils::type.convert(res, as.is = TRUE, na.strings = na)
}
