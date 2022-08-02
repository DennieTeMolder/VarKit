get_site_idx <- function(site, current_sites) {
  stopifnot(is.data.frame(current_sites))
  stopifnot("idx" %in% colnames(current_sites))
  UseMethod("get_site_idx", site)
}

##' @export
get_site_idx.logical <- function(site, current_sites) {
  if (length(site) != nrow(current_sites))
    stop("Length of 'site' not equal to current number of unmasked sites!")
  current_sites$idx[site]
}

##' @export
get_site_idx.data.frame <- function(site, current_sites) {
  stopifnot(c("chrom", "pos") %in% colnames(current_sites))
  if (any(colnames(site) != c("chrom", "pos")))
    stop("The 'site' data frame should consists of columns 'chrom' and 'pos'!")
  current_sites <- dplyr::inner_join(
    current_sites, site,
    by = c("chrom", "pos")
  )
  if (nrow(site) != nrow(current_sites)) {
    warning("Not all of the provided sites could be found, ",
            "perhaps some were masked previously?")
  }
  current_sites$idx
}
