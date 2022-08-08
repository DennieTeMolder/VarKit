##' @export
link_list <- function(names) {
  stopifnot(is.vector(names))
  n <- length(names)
  if (n < 1)
    stop("The input is zero length!")
  llist <- as.list(rep(0, n))
  class(llist) <- "link_list"
  if (!all(names == 1:n))
    attr(llist, "translation") <- names
  llist
}

##' @export
print.link_list <- function(x, ..., n = 5) {
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
##' @importFrom rlang .data
summary.link_list <- function(object, ...) {
  tibble::tibble(
    n_total = length(object),
    n_node = sum(contains_link(object)),
    n_unlinked = sum(is_unlinked(object)),
    n_keep = .data$n_node + .data$n_unlinked,
    n_remove = .data$n_total - .data$n_keep,
    frac_remove = .data$n_remove / .data$n_total
  )
}

##' @export
is_link_list <- function(x) {
  inherits(x, "link_list")
}
