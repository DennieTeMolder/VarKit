##' @export
concat_vcf <- function(...) {
  dots <- list(...)
  if (length(dots) < 1)
    stop("No arguments provided, there is noting to concatenate!")
  if (!all(vapply(dots, is_varkit, logical(1))))
    stop("Not all provided arguments have the 'varkit' class!")

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
