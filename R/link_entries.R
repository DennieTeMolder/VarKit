##' @export
link_entries <- function(x, i, links) {
  UseMethod("link_entries", x)
}

##' @export
link_entries.link_list <- function(x, i, links) {
  # Checks
  stopifnot(is.numeric(i))
  stopifnot(is.numeric(links))
  if (any(duplicated(c(i, links))))
    stop("Duplicated or shared values in 'i' and/or in 'links'!")
  # Get links of i and links of links
  values <- lookup(x, c(i, links), translate = FALSE)
  # Add links to i and copy links of links
  x[[i]] <- c(links, values)
  # Indicate linked entries wit NA
  x[links] <- NA
  x
}
