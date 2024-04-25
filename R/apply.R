##' @export
apply_sites <- function(x, FUN, ..., simplify = TRUE) {
  UseMethod("apply_sites", x)
}

##' @export
apply_sites.varkit_geno <- function(x, FUN, ..., simplify = TRUE) {
  res <- apply(x, MARGIN = 2, FUN = FUN, ..., simplify = simplify)
  if (is.matrix(res)) {
    if (all(dim(res) == dim(x)))
      return(varkit_geno(res))
  } else if (nrow(x) == 1 && is.vector(res) && !is.list(res)) {
    res <- matrix(res, nrow = 1, dimnames = dimnames(x))
    return(varkit_geno(res))
  }
  res
}

##' @export
apply_samples <- function(x, FUN, ..., simplify = TRUE) {
  UseMethod("apply_samples", x)
}

##' @export
apply_samples.varkit_geno <- function(x, FUN, ..., simplify = TRUE) {
  res <- apply(x, MARGIN = 1, FUN = FUN, ..., simplify = simplify)
  # The apply transposes the matrix, undo this if a matrix is returned
  if (is.matrix(res)) {
    res <- t(res)
    if (all(dim(res) == dim(x)))
      return(varkit_geno(res))
  } else if (nrow(x) == 1 && is.vector(res) && !is.list(res)) {
    res <- matrix(res, nrow = 1, dimnames = dimnames(x))
    return(varkit_geno(res))
  }
  res
}

##' @export
apply_index <- function(x, FUN, ..., .from_start = TRUE, .progress = TRUE) {
  UseMethod("apply_index", x)
}

##' @export
apply_index.varkit_index <- function(x, FUN, ..., .from_start = TRUE, .progress = TRUE) {
  stopifnot(is.logical(.from_start), length(.from_start) == 1, !anyNA(.from_start))
  stopifnot(is.logical(.progress) | is.character(.progress), length(.progress) == 1, !anyNA(.progress))

  if (is.character(.progress)) {
    msg <- .progress
    .progress <- TRUE
  } else {
    msg <- "Processing VCF"
  }

  if (.from_start)
    seek.varkit_index(x, 1L)

  idx <- seq_index.varkit_index(x)
  n_chunks <- length(idx)

  if (.progress) {
    i <- 0L
    cli::cli_progress_step(
      paste0(msg, "... {i}/{n_chunks} | ETA: {cli::pb_eta}"),
      paste0(msg, " finished, processed { .frmt_big(cli::pb_total)} lines @{cli::pb_rate}"),
      paste0(msg, " failed at index row {idx[i]}")
    )
    cli::cli_progress_update(
      set = 0L,
      total = sum(x$n_lines[idx]),
      force = TRUE
    )
  }

  res <- vector("list", length = n_chunks)
  for (i in seq_along(res)) {
    res[[i]] <- FUN(suppressMessages(next_chunk.varkit_index(x)), ...)
    if (.progress)
      cli::cli_progress_update(inc = x$n_lines[idx[i]], force = TRUE)
  }
  res
}
