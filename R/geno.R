##' @export
geno <- function(x, by_chrom = FALSE) {
  UseMethod("geno", x)
}

##' @export
geno.varkit <- function(x, by_chrom = FALSE) {
  if (by_chrom) {
    chroms <- unique(site(x, cols = "chrom"))
    return(lapply(chroms, function(chr) {
      varkit_filter(x$geno, site = x$site$chrom == chr & x$use_site)
    }))
  }
  varkit_filter(x$geno, site = x$use_site)
}
