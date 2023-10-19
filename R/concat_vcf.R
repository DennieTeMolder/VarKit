##' @export
concat_vcf <- function(...) {
  dots <- list(...)
  if (length(dots) < 1)
    stop("No arguments provided, there is noting to concatenate!")

  # The user can provide a mix of varkit objects & lists of varkit objects
  # Here we ensure all arguments are wrapped in a list so we can unlist() safely
  dots <- lapply(dots, function(x) {
    if (is_varkit(x))
      return(list(x))
    if (!(is.list(x) && all(vapply(x, is_varkit, logical(1)))))
      stop("Not all provided arguments are (lists of) 'varkit' objects!", call. = FALSE)
    x
  })
  dots <- unlist(dots, recursive = FALSE, use.names = FALSE)

  if (length(dots) == 1)
    return(dots[[1]])

  names(dots) <- NULL
  structure(
    list(
      meta = meta(dots[[1]]),
      site = do.call(rbind, lapply(dots, function(x) x$site)),
      geno = varkit_geno(do.call(cbind, lapply(dots, function(x) x$geno))),
      use_site = unlist(lapply(dots, function(x) x$use_site))
    ),
    class = c("varkit", "varkit_header")
  )
}
