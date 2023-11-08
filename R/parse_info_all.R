##' @export
parse_info_all <- function(x, use_meta = TRUE) {
  UseMethod("parse_info_all", x)
}

##' @export
parse_info_all.varkit <- function(x, use_meta = TRUE) {
  validate_varkit(x, check_info = TRUE)
  info <- site.varkit(x, "info")
  if (all(is.na(info)))
    stop("The INFO column only contains NAs!")

  if (use_meta) {
    meta_info <- parse_meta.varkit(x, "INFO")
    keys <- meta_info[[1L]]
  } else {
    meta_info <- NULL
    keys <- .collect_all_keys(info)
  }

  keys <- sort(keys)
  names(keys) <- keys

  res <- lapply(keys, function(key) .parse_info_internal2(info, type = key, meta_info = meta_info))
  do.call(tibble::tibble, res)
}
