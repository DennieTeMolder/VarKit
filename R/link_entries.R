##' @export
link_entries <- function(ll, i, links) {
  UseMethod("link_entries", ll)
}

##' @export
link_entries.link_list <- function(ll, i, links) {
  # Checks
  stopifnot(is.numeric(i))
  stopifnot(is.numeric(links))
  if (any(duplicated(c(i, links))))
    stop("Duplicated or shared values in 'i' and/or 'links'!")
  # Get links of i and links of links
  values <- lookup(ll, c(i, links), translate = FALSE)
  # Add links to i and copy links of links
  ll[[i]] <- c(links, values)
  # Indicate linked entries wit NA
  ll[links] <- NA
  ll
}
