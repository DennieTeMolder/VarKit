get_site_idx <- function(x, current_sites) {
  stopifnot(is.data.frame(current_sites))
  stopifnot("idx" %in% colnames(current_sites))
  UseMethod("get_site_idx", x)
}

##' @export
get_site_idx.logical <- function(x, current_sites) {
  if (length(x) != nrow(current_sites))
    stop("Length of 'x' not equal to current number of unmasked sites!")
  current_sites$idx[x]
}

##' @export
get_site_idx.data.frame <- function(x, current_sites) {
  stopifnot(c("chrom", "pos") %in% colnames(current_sites))
  if (any(colnames(x) != c("chrom", "pos")))
    stop("The 'x' data frame should consists of columns 'chrom' and 'pos'!")
  current_sites <- dplyr::inner_join(
    current_sites, x,
    by = c("chrom", "pos")
  )
  if (nrow(x) != nrow(current_sites)) {
    warning("Not all of the provided sites could be found, ",
            "perhaps some were masked previously?")
  }
  current_sites$idx
}

get_site_idx.link_list <- function(x, current_sites) {
  if (length(x) != nrow(current_sites))
    stop("Length of 'x' not equal to current number of unmasked sites!")
  idx <- which_remove(x, translate_out = TRUE)
  if (!all(idx %in% current_sites$idx))
    stop("Indexes of 'link_list' don't align with the current mask!")
  idx
}
