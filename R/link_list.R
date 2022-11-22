##' @export
link_list <- function(names, check = TRUE) {
  stopifnot(is.vector(names))
  n <- length(names)
  if (check && n < 2)
    stop("'names' should have a minimum length of 2!")
  llist <- as.list(rep(0, n))
  class(llist) <- "link_list"
  if (!all(names == 1:n))
    attr(llist, "translation") <- names
  llist
}

##' @export
is_link_list <- function(x) {
  inherits(x, "link_list")
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
    cat("\nWith a", class(trans), "translation layer:\n",
        .collapse(trans[1:n]), "...\n")
}

##' @export
##' @importFrom rlang .data
summary.link_list <- function(object, ...) {
  tbl <- table(get_types(object))

  tibble::tibble(
    n_total = sum(tbl),
    n_node = tbl["node"],
    n_unlinked = tbl["unlinked"],
    n_keep = .data$n_node + .data$n_unlinked,
    n_remove = .data$n_total - .data$n_keep,
    frac_remove = .data$n_remove / .data$n_total
  )
}
