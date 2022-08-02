##' @export
link_list <- function(names) {
  stopifnot(is.vector(names))
  n <- length(names)
  if (n < 1)
    stop("The input is zero length!")
  ll <- as.list(rep(NA, n))
  names(ll) <- names
  class(ll) <- "link_list"
  return(ll)
}

##' @export
lookup <- function(ll, names) {
  UseMethod("lookup", ll)
}

##' @export
lookup.link_list <- function(ll, names) {
  names <- as.character(names)
  entries <- ll[names]
  # Ensure all entries exist in list
  if (any(is.na(names(entries))))
    stop("Not all 'names' found in link_list!")
  entries <- unlist(entries, use.names = F)
  entries[!is.na(entries)]
}

##' @export
link_entries <- function(ll, entry, links) {
  UseMethod("link_entries", ll)
}

##' @export
link_entries.link_list <- function(ll, entry, links) {
  entry <- as.character(entry)
  links <- as.character(links)
  # No duplicates allowed
  if (any(duplicated(c(entry, links))))
    stop("Duplicated or shared values in 'entry' and/or 'links'!")
  # Get links of entry and links of links
  values <- lookup(ll, c(entry, links))
  # Add links to entry and copy links of links
  ll[[entry]] <- c(links, values)
  # Remove linked entries from list, ensure all links are present
  ll[links] <- NULL
  ll
}

##' @export
which_remove <- function(ll) {
  UseMethod("which_remove", ll)
}

##' @export
which_remove <- function(ll) {
  all_links <- unlist(ll, use.names = F)
  as.integer(all_links[!is.na(all_links)])
}

## WriteLinkedList <- function(linkList, file) {
##   write(rjson::toJSON(linkList), file)
## }

## ReadLinkedList <- function(file) {
##   linkList <- rjson::fromJSON(file = file)
##   linkList[linkList == "NA"] <- NA
##   return(linkList)
## }
