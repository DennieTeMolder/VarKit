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
  cat("# Total length:", len, "\n")
  if (!is.null(trans))
    cat("\nWith a translation layer:\n", .collapse(trans[1:n]), "...\n")
}
