##' @export
geno <- function(x, by_chrom = FALSE) {
  UseMethod("geno", x)
}

##' @export
geno.varkit <- function(x, by_chrom = FALSE) {
  if (by_chrom) {
    chroms <- unique(fixed(x, cols = "chrom"))
    return(sapply(chroms, simplify = FALSE, FUN = function(chr) {
      varkit_filter(x$geno, site = x$fixed$chrom == chr & x$use_site)
    }))
  }
  varkit_filter(x$geno, site = x$use_site)
}
