##' @export
find_identical_sites <- function(x,
                                 tolerance = .01,
                                 method = "fast",
                                 by_chrom = TRUE,
                                 cores = 1) {
  UseMethod("find_identical_sites", x)
}

##' @export
##' @import foreach
find_identical_sites.varkit <- function(x,
                                        tolerance = .01,
                                        method = "fast",
                                        by_chrom = TRUE,
                                        cores = 1) {
  stopifnot(is.logical(by_chrom))

  # Short-circuit
  if (!by_chrom) {
    llist <- link_list(fixed(x, add_index = TRUE)$idx)
    return(find_identical_cols(
      geno(x), tolerance = tolerance, method = method, llist = llist
    ))
  }

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
  pb <- utils::txtProgressBar(max = length(fix_chrom), style = 3)
  pb_set <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress = pb_set)

  # Compute identical sites within each contig in parallel
  llist_chrom <- foreach::foreach(
    fix = fix_chrom,
    gt = gt_chrom,
    .options.snow = opts,
    .packages = "varkit"
  ) %dopar% {
    llist <- link_list(fix$idx)
    llist <- find_identical_cols(
      gt, tolerance = tolerance, method = method, llist = llist
    )
    if (is.null(cl))
      .pb_increment(pb)
    llist
  }

  # Cleanup
  close(pb)
  .unregister_workers(cl)

  # Add names and revert order
  names(llist_chrom) <- names(fix_chrom)
  llist_chrom <- llist_chrom[idx]

  # Merge the per chromosome link_lists into one
  .merge_link_list(llist_chrom)
}
