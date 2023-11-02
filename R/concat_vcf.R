##' @export
concat_vcf <- function(...) {
  dots <- list(...)
  if (length(dots) < 1L)
    stop("No arguments provided, there is noting to concatenate!")

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

  structure(
    list(
      meta = meta(dots[[1L]]),
      site = do.call(rbind, lapply(dots, function(x) x$site)),
      geno = varkit_geno(do.call(cbind, lapply(dots, function(x) x$geno))),
      use_site = unlist(lapply(dots, function(x) x$use_site), use.names = FALSE)
    ),
    class = c("varkit", "varkit_header")
  )
}
