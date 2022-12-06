##' @export
find_linked_cols <- function(x,
                             pos = NULL,
                             cutoff = .9,
                             window = NULL,
                             llist = NULL) {
  .compare_cols(x, FUN = .compare_r2, pos = pos, window = window, llist = llist, cutoff = cutoff)
}

.compare_r2 <- function(a, b, cutoff) {
  r2 <- .cor_pair_silent(a, b)^2
  r2 >= cutoff & !is.na(r2)
}

.cor_pair_silent <- function(x, y = NULL) {
  suppressWarnings(stats::cor(x, y, use = "pairwise.complete.obs"))
}
