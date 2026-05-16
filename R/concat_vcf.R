##' @export
concat_vcf <- function(...) {
  if (...length() < 1L)
    return(NULL)

  # The user can provide a mix of varkit objects & lists of varkit objects
  # Here we ensure all arguments are wrapped in a list so we can unlist() safely
  dots <- .unlist(lapply(list(...), function(x) {
    if (is_varkit(x))
      return(list(x))
    if (!is.list(x) || !all(vapply(x, is_varkit, logical(1L))))
      stop("Not all provided arguments are (lists of) 'varkit' objects!", call. = FALSE)
    x
  }))

  if (length(dots) == 1L)
    return(dots[[1L]])

  # Unclass to ignore masking
  dots <- lapply(dots, unclass)

  new_varkit(
    meta = dots[[1L]][["meta"]],
    site = do.call(rbind, lapply(dots, function(x) x[["site"]])),
    geno = do.call(cbind, lapply(dots, function(x) x[["geno"]])),
    sample_indices = .indices(dots[[1L]])[["samples"]],
    site_indices = do.call(c, lapply(dots, function(x) .indices(x)[["sites"]])),
    sample_keys = attr(dots[[1L]], "keys")[["samples"]],
    site_keys = do.call(rbind, lapply(dots, function(x) attr(x, "keys")[["sites"]]))
  )
}
