##' @export
as_genind <- function(x, ploidy, type = c("codom", "PA"), check_geno = TRUE) {
  UseMethod("as_genind", x)
}

##' @export
as_genind.varkit <- function(x, ploidy, type = c("codom", "PA"), check_geno = TRUE) {
  .ensure_pkg("adegenet")
  validate_varkit(x)
  if (!is.numeric(x$geno))
    stop("The `geno()` matrix should contain numeric values describing ALT allele counts!")
  stopifnot(length(ploidy) == 1, ploidy >= 1)
  ploidy <- as.integer(ploidy)
  type <- match.arg(type)

  gt <- as_integer(geno(x))

  expected_max <- if (type == "codom") ploidy else 1L
  if (check_geno && max(gt, na.rm = TRUE) != expected_max) {
    warning("`geno(x)` doesn't seem to be a dosage! Expected range: 0 to ", expected_max,
            " (ploidy), Current range: ", .collapse(range(gt, na.rm = TRUE), " to "))
  }

  gt <- unclass(gt)
  colnames(gt) <- paste0(
    "chr",
    as.character(as.integer(factor(site(x, "chrom")))),
    ".",
    site(x, "pos")
  )

  adegenet::genind(
    tab = gt,
    ploidy = ploidy,
    type = type
  )
}
