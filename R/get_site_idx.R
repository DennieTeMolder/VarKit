get_site_idx <- function(x, vcf) {
  stopifnot(is_varkit(vcf))
  UseMethod("get_site_idx", x)
}

##' @export
get_site_idx.logical <- function(x, vcf) {
  if (length(x) != sum(vcf$use_site))
    stop("Length of 'x' not equal to current number of unmasked sites!")
  which(vcf$use_site)[x]
}

##' @export
get_site_idx.data.frame <- function(x, vcf) {
  if (!all(c("chrom", "pos") %in% colnames(x)))
    stop("The data frame 'x' should contain columns 'chrom' and 'pos'!")
  current <- site(vcf, cols = c("chrom", "pos"), add_index = TRUE)
  current <- dplyr::inner_join(current, x, by = c("chrom", "pos"))
  if (nrow(x) != nrow(current)) {
    warning("Not all of the provided sites could be found, ",
            "perhaps some were masked previously?")
  }
  current$idx
}

get_site_idx.link_list <- function(x, vcf) {
  if (length(x) != sum(vcf$use_site))
    stop("Length of 'x' not equal to current number of unmasked sites!")
  idx <- which_remove(x, translate_out = TRUE)
  if (!all(idx %in% which(vcf$use_site)))
    stop("Indexes of 'link_list' don't align with the current mask!")
  idx
}
