##' @export
find_linked_sites <- function(x,
                              cutoff = .9,
                              window = 5e4,
                              method = "recursive",
                              by_chrom = TRUE,
                              cores = 1) {
  UseMethod("find_linked_sites", x)
}

##' @export
##' @import foreach
find_linked_sites.varkit <- function(x,
                                     cutoff = .9,
                                     window = 5e4,
                                     method = "recursive",
                                     by_chrom = TRUE,
                                     cores = 1) {
  stopifnot(is.logical(by_chrom))

  # Short-circuit
  if (!by_chrom) {
    loc <- fixed(x, add_index = TRUE)
    llist <- link_list(loc$idx)
    if (cores > 1)
      warning("The 'cores' option only has effect when  `by_chrom = TRUE`.")
    if (window > 0)
      warning("The window option is only supported when `by_chrom = TRUE`, disabling window...")
    return(find_linked_cols(
      geno(x), pos = loc$pos, cutoff = cutoff, window = -1, method = method
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
    llist <- link_list(fix$idx, check = FALSE)
    if (length(llist) > 1) {
      llist <- find_linked_cols(
        gt, pos = fix$pos, cutoff = cutoff, window = window, method = method, llist = llist
      )
    }
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

  # Concatenate the per chromosome link_lists
  .concat_link_lists(llist_chrom)
}
