##' @export
as_genind <- function(x, ploidy, type = c("codom", "PA"), check_geno = TRUE) {
  UseMethod("as_genind", x)
}

##' @export
as_genind.varkit <- function(x, ploidy, type = c("codom", "PA"), check_geno = TRUE) {
  stopifnot(is.numeric(x$geno))
  ploidy <- as.integer(ploidy)
  stopifnot(length(ploidy) == 1, ploidy >= 1)
  type <- match.arg(type)

  gt <- geno(x)
  storage.mode(gt) <- "integer"

  expected_max <- if (type == "codom") ploidy else 1
  if (check_geno && max(gt, na.rm = TRUE) != expected_max) {
    warning("`geno(x)` doesn't seem to be a dosage! Expected range: 0 to ", expected_max,
            " (ploidy), Current range: ", .collapse(range(gt, na.rm = TRUE), " to "))
  }

  colnames(gt) <- paste0(
    "chr",
    as.character(as.integer(factor(site(x, "chrom")))),
    ".",
    site(x, "pos")
  )
  gt <- unclass(gt)

  adegenet::genind(
    tab = gt,
    ploidy = ploidy,
    type = type
  )
}
