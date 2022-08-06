##' @export
link_list <- function(names) {
  stopifnot(is.vector(names))
  n <- length(names)
  if (n < 1)
    stop("The input is zero length!")
  ll <- as.list(rep(0, n))
  class(ll) <- "link_list"
  if (!all(names == 1:n))
    attr(ll, "translation") <- names
  ll
}

##' @export
print.link_list <- function(x, n = 5) {
  len <- length(x)
  n <- min(n, len)
  trans <- attr(x, "translation")
  cat("*A link_list object*\n")
  for (i in 1:n) {
    cat("[[", i, "]]: ", .collapse(x[[i]]), "\n", sep = "")
  }
  if (len != n)
    cat(" ...\n")
  cat("# Total length:", len, "\n")
  if (!is.null(trans))
    cat("\nWith a translation layer:\n", .collapse(trans[1:n]), "...\n")
}

##' @export
summary.link_list <- function(x) {
  tibble::tibble(
    n_total = length(x),
    n_node = sum(contains_link(x)),
    n_unlinked = sum(is_unlinked(x)),
    n_keep = n_node + n_unlinked,
    n_remove = n_total - n_keep,
    frac_remove = n_remove / n_total
  )
}

##' @export
is_link_list <- function(x) {
  inherits(x, "link_list")
}
