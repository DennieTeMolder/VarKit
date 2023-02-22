# This code is currently unused, it was part of the discontinued 'recursive' linking algorithm

resolve_linked_cols <- function(linked_cols, data, llist) {
  if (all(is.null(linked_cols)))
    return(llist)

  n <- length(linked_cols)

  # Index the links of all of the columns for lookup speed
  index <- sapply(
    seq_len(n), .get_all_linked,
    linked_cols = linked_cols, simplify = FALSE
  )

  # Keep track of which cols to skip
  skip <- rep(FALSE, n)
  for (i in seq_len(n - 1)) {
    if (skip[i])
      next()

    links <- .find_links_of_links(i, index = index)

    if (is.null(links))
      next()

    llist <- link_entries(llist, idx = links[1], links = links[-1])
    skip[links] <- TRUE
  }
  redistribute(llist, data = data, keep = "least_na", tiebreaker = "middle")
}

.get_all_linked <- function(i, linked_cols) {
  linked_before <- rep(FALSE, i - 1)
  for (j in seq_along(linked_before)) {
    if (i %in% linked_cols[[j]])
      linked_before[[j]] <- TRUE
  }
  linked_after <- linked_cols[[i]]
  if (is.null(linked_after)) {
    if (any(linked_before))
      return(c(which(linked_before), i))
    return(NULL)
  }
  # Assume that if link_after != null, it contains 'i'
  c(which(linked_before), linked_after)
}

.find_links_of_links <- function(i, index) {
  # Retrieve directly linked columns
  a <- index[[i]]
  if (is.null(a))
    return(NULL)
  # Retrieve all indirectly linked columns by iteratively expanding array
  len <- length(a)
  repeat {
    last_len <- len
    a <- unique(unlist(index[a]))
    len <- length(a)
    # Break loop if the array stops growing
    if (len == last_len)
      break()
  }
  a
}
