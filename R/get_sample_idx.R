get_sample_idx <- function(sample, all_samples, use_sample) {
  UseMethod("get_sample_idx", sample)
}

##' @export
get_sample_idx.logical <- function(sample, all_samples, use_sample) {
  if (length(sample) != sum(use_sample))
    stop("Length of 'sample' not equal to current number of unmasked samples")
  which(use_sample)[sample]
}

##' @export
get_sample_idx.character <- function(sample, all_samples, use_sample) {
  if (!all(sample %in% all_samples[use_sample])) {
    warning("Not all of the provided samples could be found, ",
            "perhaps some were masked previously?")
  }
  which(all_samples %in% sample)
}
