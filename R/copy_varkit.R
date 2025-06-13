##' @export
copy_varkit <- function(vcf) {
  validate_varkit(vcf)
  new_varkit(
    meta = vcf$meta,
    site = vcf$site,
    geno = vcf$geno,
    drmodel = drm_copy(drm_get(vcf))
  )
}
