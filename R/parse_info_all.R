##' @export
parse_info_all <- function(x, use_meta = TRUE) {
  UseMethod("parse_info_all", x)
}

##' @export
parse_info_all.varkit <- function(x, use_meta = TRUE) {
  validate_varkit(x, check_info = TRUE)
  .stopifnotsingle(use_meta, what = "logical")

  if (use_meta) {
    col_types <- get_info_types.varkit(x, meta_type = "INFO")
    if (is.null(col_types))
        stop("The `$meta$name` column has no INFO entries, this is required when `use_meta = TRUE`!")
  } else {
    col_types <- NULL
  }

  info <- site.varkit(x, "info")

  if (is.null(col_types)) {
    keys <- .collect_all_keys(info)
  } else {
    keys <- names(col_types)
  }

  if (all(is.na(keys)))
    return(tibble::tibble(.rows = n_sites.varkit(x)))

  names(keys) <- keys
  tibble::as_tibble(lapply(keys, function(key) {
    .parse_info_internal(info, type = key, col_types = col_types)
  }))
}
