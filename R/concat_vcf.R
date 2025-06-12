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

  drm_preselect(
    varkit(
      meta = meta.varkit(dots[[1L]]),
      site = do.call(rbind, lapply(dots, function(x) x$site)),
      geno = varkit_geno(do.call(cbind, lapply(dots, function(x) x$geno)))
    ),
    samples = .drm_if_else(dots[[1L]], node = "samples"),
    sites = do.call(c, lapply(dots, .drm_if_else, node = "sites"))
  )
}
