##' @export
parse_info_all <- function(x, use_meta = TRUE) {
  UseMethod("parse_info_all", x)
}

##' @export
parse_info_all.varkit <- function(x, use_meta = TRUE) {
  info <- site(x, "info")
  if (all(is.na(info)))
    stop("The INFO column only contains NAs!")

  if (use_meta) {
    meta_info <- parse_meta(x, "INFO")
    keys <- meta_info[[1]]
  } else {
    meta_info <- NULL
    keys <- .collect_all_keys(info)
  }

  keys <- sort(keys)
  names(keys) <- keys
  tibble::as_tibble(lapply(keys, function(key) .parse_info_internal(info, type = key, meta_info = meta_info)))
}
