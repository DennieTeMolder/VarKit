##' @export
parse_info_all <- function(x, use_meta = TRUE) {
  UseMethod("parse_info_all", x)
}

##' @export
parse_info_all.varkit <- function(x, use_meta = TRUE) {
  if (use_meta && !any(meta.varkit(x)$name == "INFO"))
    stop("The `meta()` slot has no INFO entry, this is required when `use_meta = TRUE`!")
  validate_varkit(x, check_info = TRUE)

  info <- site.varkit(x, info = "info")
  drmodel <- drm_get(info)
  info <- drm_do_detach(info[[1L]])

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
  info <- tibble::as_tibble(lapply(keys, function(key) {
    .parse_info_internal(info, type = key, meta_info = meta_info)
  }))

  if (is.null(drmodel))
    return(info)

  drm_do_attach(
    info,
    drmodel = drmodel,
    dim_links = c("sites", NA),
    label = "parse_info_all()"
  )
}
