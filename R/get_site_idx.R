##' @export
get_site_idx <- function(site, vcf) {
  stopifnot(is_varkit(vcf))
  UseMethod("get_site_idx", site)
}

##' @export
get_site_idx.logical <- function(site, vcf) {
  if (length(site) != sum(vcf$use_site))
    stop("Length of 'site' not equal to current number of unmasked sites!")
  which(vcf$use_site)[site]
}

##' @export
get_site_idx.data.frame <- function(site, vcf) {
  if (!all(c("chrom", "pos") %in% colnames(site)))
    stop("The data frame 'site' should contain columns 'chrom' and 'pos'!")
  current <- site(vcf, cols = c("chrom", "pos"), add_index = TRUE)
  current <- dplyr::semi_join(current, site, by = c("chrom", "pos"))
  if (nrow(site) != nrow(current)) {
    warning("Not all of the provided sites could be found, ",
            "perhaps some were masked previously?")
  }
  current$idx
}

##' @export
get_site_idx.link_list <- function(site, vcf) {
  if (length(site) != sum(vcf$use_site))
    stop("Length of 'site' not equal to current number of unmasked sites!")
  idx <- which_remove(site, translate_out = TRUE)
  if (!all(idx %in% which(vcf$use_site)))
    stop("Indexes of 'link_list' don't align with the current mask!")
  idx
}
