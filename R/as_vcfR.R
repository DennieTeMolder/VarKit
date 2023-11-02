##' @export
as_vcfR <- function(x) {
  UseMethod("as_vcfR", x)
}

##' @export
as_vcfR.varkit <- function(x) {
  .ensure_pkg("vcfR")
  validate_varkit(x, check_format = TRUE)

  vcfr <- new(Class = "vcfR")
  vcfr@meta <- paste0("##", x$meta$name, "=", x$meta$value)

  x <- standardize_cols.varkit(x)
  fix <- site(x, 1:8)
  frmt <- site(x, 9)
  gt <- geno(x)
  rm(x)

  gt <- cbind(FORMAT = frmt, t(unclass(gt)))
  storage.mode(gt) <- "character"
  vcfr@gt <- gt

  fix <- lapply(fix, as.character)
  fix <- do.call(cbind, fix)
  colnames(fix) <- toupper(.fixed_columns()[1:8])
  vcfr@fix <- fix

  vcfr
}
