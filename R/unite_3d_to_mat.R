##' @export
unite_3d_to_mat <- function(x, delim = ",") {
  stopifnot(is.array(x), length(dim(x)) == 3L)
  .stopifnotsingle(delim, what = "character")

  in_dim <- dim(x)
  in_names <- names(x)

  # Convert to long character format with blank NAs
  storage.mode(x) <- "character"
  x[is.na(x)] <- ""
  dim(x) <- c(in_dim[1L] * in_dim[2L], 1L, in_dim[3L])

  # Join strings over the z-axis and strip trailing delimiters
  x <- asplit(x, 3L)
  x <- do.call(paste, c(x, list(sep = delim)))
  x <- stringr::str_remove(x, paste0("(?:", delim, ")+$"))

  # Restore attributes and return
  dim(x) <- in_dim[1:2]
  dimnames(x) <- in_names[1:2]
  x
}
