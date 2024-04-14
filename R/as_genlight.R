##' @export
as_genlight <- function(x, ploidy, cores = 1, check_geno = TRUE) {
  UseMethod("as_genlight", x)
}

##' @export
as_genlight.varkit <- function(x, ploidy, cores = 1, check_geno = TRUE) {
  .ensure_pkg("adegenet")
  validate_varkit(x)
  stopifnot(length(ploidy) == 1L, is.numeric(ploidy), ploidy >= 1L)
  ploidy <- as.integer(ploidy)

  if (any(is_indel.varkit(x)))
    warning("The provided VCF contains INDELs, which are not explicitly supported by 'genlight'.")
  if (!is.numeric(x$geno))
    stop("The `geno()` matrix should contain numeric values describing ALT allele dosage!")

  gt <- geno.varkit(x)
  if (check_geno && !max(gt, na.rm = TRUE) > ploidy - 1L) {
    warning("`geno(x)` doesn't seem to be a dosage! Expected range: 0 to ", ploidy,
            " (ploidy), current range: ", .collapse(range(gt, na.rm = TRUE), " to "))
  }

  gt <- as_integer(unclass(gt))

  if (requireNamespace("adegenet")) {
    gt <- methods::new(Class = "genlight", gt, ploidy = ploidy, n.cores = cores)
  } else {
    stop("adegenet is not installed!")
  }

  adegenet::chromosome(gt) <- site.varkit(x, "chrom")
  adegenet::position(gt) <- site.varkit(x, "pos")

  if (any(colnames(x$site) == "id")) {
    lnames <- site.varkit(x, "id")
    which_has_id <- which(!is.na(lnames))
    if (length(which_has_id) > 0L)
      adegenet::locNames(gt)[which_has_id] <- lnames[which_has_id]
  }

  if (all(c("ref", "alt") %in% colnames(x$site))) {
    alleles <- tolower(paste0(site.varkit(x, "ref"), "/", site.varkit(x, "alt")))
    if (all(nchar(alleles) == 3L))
      adegenet::alleles(gt) <- alleles
  }

  gt
}
