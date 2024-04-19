##' @export
seq_index <- function(x, skip_header = TRUE) {
  UseMethod("seq_index", x)
}

##' @export
seq_index.varkit_index <- function(x, skip_header = TRUE) {
  if (nrow(x) < 1L)
    stop("x is zero length!")

  pos <- .seek_next(x, skip_header = skip_header)
  if (pos$n_lines < 1L) # Pointer outside of x
    return(integer())

  i <- as.integer(rownames(pos))
  result <- seq(i, nrow(x))
  result <- result[x$n_lines[result] >= 1L]

  ids <- x$id[result]
  if (!skip_header)
    ids <- c("header", ids)
  names(result) <- ids

  result
}
