##' @export
parse_info_all <- function(x, use_meta = TRUE) {
  UseMethod("parse_info_all", x)
}

##' @export
parse_info_all.varkit <- function(x, use_meta = TRUE) {
  validate_varkit(x, check_info = TRUE)
  if (use_meta && !"INFO" %in% meta.varkit(x)$name)
      stop("The `$meta$name` column has no INFO entries, this is required when `use_meta = TRUE`!")

  info <- site.varkit(x, "info")

  if (use_meta) {
    meta_info <- parse_meta.varkit(x, "INFO")
    keys <- meta_info[[1L]]
  } else {
    meta_info <- NULL
    keys <- .collect_all_keys(info)
  }

  if (all(is.na(keys)))
    return(tibble::tibble(.rows = n_sites.varkit(x)))

  keys <- sort(keys)
  names(keys) <- keys

  tibble::as_tibble(lapply(keys, function(key) {
    .parse_info_internal(info, type = key, meta_info = meta_info)
  }))
}
