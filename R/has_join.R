##' @export
has_join <- function(x, y, by = NULL, ...) {
  stopifnot(is.data.frame(x), is.data.frame(y))

  if (is.null(by)) {
    by_x <- by_y <- intersect(names(x), names(y))
  } else if (inherits(by, "dplyr_join_by")) {
    by_x <- by$x
    by_y <- by$y
  } else if (is.character(by)) {
    by_x <- if (rlang::is_named(by)) names(by) else by
    by_y <- unname(by)
  } else {
    stop("The 'by' argument should be a character vector or `dplyr::join_by()`!")
  }

  # Subset X and Y to save on memory
  x <- x[by_x]
  y <- y[by_y]

  # Add a dummy column for checking if the join was successful
  y[[ncol(y) + 1L]] <- TRUE
  join_result <- dplyr::left_join(
    x = x,
    y = y,
    by = by,
    multiple = "any",
    suffix = c(".x", ""),
    ...
  )

  # Unsuccessful joins return NA
  !is.na(join_result[[ncol(join_result)]])
}
