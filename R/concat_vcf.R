##' @export
concat_vcf <- function(...) {
  if (...length() < 1L)
    stop("No arguments provided, there is noting to concatenate!")
  dots <- list(...)

  # The user can provide a mix of varkit objects & lists of varkit objects
  # Here we ensure all arguments are wrapped in a list so we can unlist() safely
  dots <- lapply(dots, function(x) {
    if (is_varkit(x))
      return(list(x))
    if (!(is.list(x) && all(vapply(x, is_varkit, logical(1L)))))
      stop("Not all provided arguments are (lists of) 'varkit' objects!", call. = FALSE)
    x
  })
  dots <- unlist(dots, recursive = FALSE, use.names = FALSE)

  if (length(dots) == 1L)
    return(dots[[1L]])

  new_varkit(
    meta = unclass(dots[[1L]])$meta,
    site = do.call(rbind, lapply(dots, function(x) unclass(x)$site)),
    geno = do.call(cbind, lapply(dots, function(x) unclass(x)$geno)),
    sample_indices = .indices(dots[[1L]])$samples,
    site_indices = do.call(c, lapply(dots, function(x) .indices(x)$sites))
  )
}
