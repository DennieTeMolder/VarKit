##' @export
find_identical_sites <- function(x,
                                 tolerance = .01,
                                 method = "fast",
                                 by_chrom = TRUE,
                                 cores = 1,
                                 debug = FALSE) {
  UseMethod("find_identical_sites", x)
}

##' @export
##' @import foreach
find_identical_sites.varkit <- function(x,
                                        tolerance = .01,
                                        method = "fast",
                                        by_chrom = TRUE,
                                        cores = 1,
                                        debug = FALSE) {
  stopifnot(is.logical(by_chrom))

  # Short-circuit
  if (!by_chrom) {
    llist <- link_list(fixed(x, add_index = TRUE)$idx)
    if (cores > 1)
      warning("The 'cores' option only has effect when  `by_chrom = TRUE`.")
    return(find_identical_cols(
      geno(x), tolerance = tolerance, method = method, llist = llist
    ))
  }

  if (debug)
    cores <- 1

  # Register cores for parallel computing if required
  cl <- .register_workers(cores)

  # Get required data from varkit object
  fix_chrom <- fixed(x, add_index = TRUE, by_chrom = TRUE, only_positions = TRUE)
  gt_chrom <- geno(x, by_chrom = TRUE)

  # Order contigs most to least sites for efficiency
  idx <- order(sapply(fix_chrom, nrow), decreasing = TRUE)
  fix_chrom <- fix_chrom[idx]
  gt_chrom <- gt_chrom[idx]

  # Add progress bar to foreach
  if (debug) {
    opts <- NULL
  } else {
    pb <- utils::txtProgressBar(max = length(fix_chrom), style = 3)
    pb_set <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = pb_set)
  }

  # Compute identical sites within each contig in parallel
  llist_chrom <- foreach::foreach(
    fix = fix_chrom,
    gt = gt_chrom,
    .options.snow = opts,
    .packages = "varkit"
  ) %dopar% {
    if (debug)
      message("Processing CHROM: ", fix$chrom[1])
    llist <- link_list(fix$idx, check = FALSE)
    if (length(llist) > 1) {
      llist <- find_identical_cols(
        gt, tolerance = tolerance, method = method, llist = llist
      )
    }
    if (is.null(cl) && !debug)
      .pb_increment(pb)
    llist
  }

  # Cleanup
  if (!debug)
    close(pb)
  .unregister_workers(cl)

  # Add names and revert order
  names(llist_chrom) <- names(fix_chrom)
  llist_chrom <- llist_chrom[idx]

  # Concatenate the per chromosome link_lists
  concat_link_lists(llist_chrom)
}
