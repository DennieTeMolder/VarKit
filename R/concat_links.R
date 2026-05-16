##' @export
concat_links <- function(x, y) {
  if (!is.list(x))
    stop("'x' must be a dataframe or list!")
  if (!identical(class(x), class(y)))
    stop("'x' and 'y' must be of the same class!")

  .concat_links_internal <- function(name = NULL) {
    if (!is.null(name)) {
      x <- x[[name]]
      y <- y[[name]]
    }

    if (is.null(x))
      return(y)
    if (is.null(y))
      return(x)

    if (!is.data.frame(x))
      stop("All elements of 'x' must be a dataframe!")
    if (!is.data.frame(y))
      stop("All elements of 'x' must be a dataframe!")
    if (!identical(colnames(x), colnames(y)))
      stop("The dataframes in 'x' and 'y' must have identical column names!")
    if (!all(c("row", "col") %in% names(x)))
      stop("The dataframes must contain columns named: 'row' and 'col'!")

    dplyr::distinct(rbind(x, y), .data$row, .data$col, .keep_all = TRUE)
  }

  if (is.data.frame(x)) {
    res <- .concat_links_internal()
  } else {
    chroms <- union(names(x), names(y))
    names(chroms) <- chroms
    res <- purrr::map(chroms, .concat_links_internal)
  }

  # Combine origins to create a lineage
  origin <- c(get_origin(x), get_origin(y))
  if (!is.null(origin)) {
    origin <- origin[!duplicated(origin, fromLast = TRUE)]
    attr(res, "varkit_origin") <- origin
  }

  res
}
