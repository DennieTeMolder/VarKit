##' @export
parse_info_all <- function(x, use_meta = TRUE) {
  UseMethod("parse_info_all", x)
}

##' @export
parse_info_all.varkit <- function(x, use_meta = TRUE) {
  validate_varkit(x, check_info = TRUE)
  .stopifnotsingle(use_meta, what = "logical")

  if (use_meta && !any(x$meta$name == "INFO")) {
    warning("The `$meta$name` column has no INFO entries, reverting to `use_meta = FALSE`!")
    use_meta <- FALSE
  }

  info <- site.varkit(x, "info")

  if (use_meta) {
    col_types <- get_info_types.varkit(x, meta_type = "INFO")
    keys <- names(col_types)
  } else {
    col_types <- NULL
    keys <- .collect_all_keys(info)
  }

  if (is.null(keys))
    return(tibble::tibble(.rows = length(info)))

  names(keys) <- keys
  tibble::as_tibble(lapply(keys, function(key) {
    .parse_info_internal(info, type = key, col_types = col_types)
  }))
}
