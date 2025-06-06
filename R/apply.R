##' @export
apply_sites <- function(x, FUN, ..., simplify = TRUE) {
  lifecycle::deprecate_stop("v0.8.6", "apply_sites()", I("apply(MARGIN = 'sites')"))
}

##' @export
apply_samples <- function(x, FUN, ..., simplify = TRUE) {
  lifecycle::deprecate_stop("v0.8.6", "apply_samples()", I("apply(MARGIN = 'samples')"))
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
      paste0(msg, " finished, processed { .frmt_big(cli::pb_total)} variants @{cli::pb_rate}"),
      paste0(msg, " failed at chunk {i}/{n_chunks}, after { .frmt_big(cli::pb_current)} variants)")
    )
    cli::cli_progress_update(
      set = 0L,
      total = sum(x$n_lines[unique(idx)]),
      force = TRUE
    )
  }

  res <- vector("list", length = n_chunks)
  names(res) <- names(idx)
  for (i in seq_along(res)) {
    vcf_chunk <- suppressMessages(next_chunk.varkit_index(x))
    res[[i]] <- FUN(vcf_chunk, ...)
    if (.progress)
      cli::cli_progress_update(inc = n_sites.varkit(vcf_chunk, mask = FALSE), force = TRUE)
  }
  res
}
