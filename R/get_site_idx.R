##' @export
get_site_idx <- function(site, vcf) {
  stopifnot(is_varkit(vcf))
  UseMethod("get_site_idx", site)
}

##' @export
get_site_idx.logical <- function(site, vcf) {
  if (length(site) != sum(vcf$use_site))
    stop("Length of 'site' not equal to current number of unmasked sites!", call. = FALSE)
  which(vcf$use_site)[site]
}

##' @export
get_site_idx.data.frame <- function(site, vcf) {
  if (nrow(site) == 0L)
    return(integer())

  join_by <- c("chrom", "pos", "alt")
  if (!all(join_by %in% colnames(vcf$site)))
    stop("When 'site' is a data frame, 'site(vcf)' must contain columns: ", .quote_collapse(join_by), "!", call. = FALSE)
  if (!all(join_by %in% colnames(site)))
    stop("The data frame 'site' must contain columns: ", .quote_collapse(join_by), "!", call. = FALSE)

  current <- site(vcf, tidyselect::all_of(join_by), .add_index = TRUE)
  current <- dplyr::semi_join(current, site, by = join_by, na_matches = "never")

  if(nrow(current) == 0L)
    stop("No sites could be found! Check if the following columns overlap: ", .quote_collapse(join_by), call. = FALSE)
  if (nrow(site) != nrow(current)) {
    warning("Not all of the provided sites could be found, ",
            "perhaps some were masked previously?", call. = FALSE)
  }

  current$idx
}

##' @export
get_site_idx.varkit_linkage_graph <- function(site, vcf) {
  .ensure_igraph()

  if (is.null(get_origin(site))) {
    stop("The linkage graph 'site' does not have a 'varkit_origin' attribute!\n",
         "The linkage graph is most likely corrupted.", call. = FALSE)
  } else if (!originates_from(site, origin = vcf)) {
    stop("The provided linkage graph does not originate from 'vcf'!\n",
         "Likely either the mask or samples(vcf) differs between the two.", call. = FALSE)
  }

  # All nodes that should be removed have a single in-going edge
  deg <- igraph::degree(site, mode = "in", loops = FALSE)
  if (any(deg >= 2))
    stop("The linkage graph appears to be corrupted!", call. = FALSE)
  sort(as.integer(names(deg)[deg == 1L]))
}
