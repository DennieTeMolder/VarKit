##' @export
redistribute <- function(x, data, keep = "least_na", tiebreaker = "middle") {
  UseMethod("redistribute", x)
}

##' @export
redistribute.link_list <- function(x, data, keep = "least_na", tiebreaker = "middle") {
  stopifnot(is.matrix(data) || is.data.frame(data))
  stopifnot(ncol(data) == length(x))
  stopifnot(tiebreaker != "none")

  if (keep == "least_na") {
    keep_fun <- function(y)
      .which_least_na(y, data = data, tiebreaker = tiebreaker)
  } else {
    stop("Value keep = '", keep, ' not recognised!')
  }

  .check_link_list(x)

  skip <- rep(FALSE, length(x))
  for (i in seq_along(x)) {
    if (skip[i])
      next()

    links <- x[[i]]
    if (length(links) == 1 && (is.na(links) || links == 0))
      next()

    skip[links] <- TRUE

    combined <- c(i, links)
    keep_which <- keep_fun(combined)
    i_keep <- combined[keep_which]

    if (i_keep == i)
      next()

    # Transfer links to new position
    x[[i]] <- NA
    x[[i_keep]] <- combined[-keep_which]
  }
  x
}

.which_least_na <- function(cols, data, tiebreaker = "none") {
  n_na <- colSums(is.na(data[, cols, drop = FALSE]))
  least_na <- which(n_na == min(n_na))

  if (tiebreaker == "none")
    return(least_na)

  if (tiebreaker == "left")
    return(utils::head(least_na, n = 1))

  if (tiebreaker == "middle")
    return(.middle(least_na))

  if (tiebreaker == "right")
    return(utils::tail(least_na, n = 1))

  stop("Value tiebreaker = '", tiebreaker, "' not recognised!")
}
