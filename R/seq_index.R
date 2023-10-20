##' @export
seq_index <- function(x, skip_header = TRUE) {
  UseMethod("seq_index", x)
}

##' @export
seq_index.varkit_index <- function(x, skip_header = TRUE) {
  if (nrow(x) < 1)
    stop("x is zero length!")

  current <- seek(x)
  if (current$n_lines < 1) # Pointer outside of x
    return(integer())

  i <- as.integer(rownames(current))
  if (skip_header) # Omit header
    i <- max(1, i)

  result <- seq(i, nrow(x))

  ids <- x$id[result]
  if (!skip_header)
    ids <- c("header", ids)
  names(result) <- ids

  result
}
