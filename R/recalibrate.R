##' @export
redistribute <- function(x, data, keep = "middle") {
  UseMethod("redistribute", x)
}

##' @export
redistribute.link_list <- function(x, data, keep = "middle") {
  stopifnot(is.matrix(data) || is.data.frame(data))
  stopifnot(ncol(data) == length(x))

  if (keep %in% c("left", "right", "middle")) {
    keep_fun <- function(y)
      .which_least_na(y, data = data, tiebreaker = keep)
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
