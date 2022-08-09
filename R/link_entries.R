##' @export
link_entries <- function(x, idx, links) {
  UseMethod("link_entries", x)
}

##' @export
link_entries.link_list <- function(x, idx, links) {
  # Checks
  stopifnot(is.numeric(idx))
  stopifnot(is.numeric(links))
  if (any(duplicated(c(idx, links))))
    stop("Duplicated or shared values in 'idx' and/or in 'links'!")
  # Get links of idx and links of links
  values <- lookup(x, c(idx, links), translate = "none")
  # Add links to idx and copy links of links
  x[[idx]] <- c(links, values)
  # Indicate linked entries wit NA
  x[links] <- NA
  x
}
