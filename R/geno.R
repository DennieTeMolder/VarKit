##' @export
geno <- function(x, by_chrom = FALSE) {
  UseMethod("geno", x)
}

##' @export
geno.varkit <- function(x, by_chrom = FALSE) {
  if (by_chrom) {
    chrom <- unique(fixed(x, only_positions = TRUE)$chrom)
    my_fun <- function(chr)
      varkit_filter(x$geno, site = x$fixed$chrom == chr & x$use_site)
    return(sapply(chrom, my_fun, simplify = FALSE))
  }
  varkit_filter(x$geno, site = x$use_site)
}
