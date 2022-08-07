##' @export
find_linked_sites <- function(x,
                              cutoff = .9,
                              window = NULL,
                              method = "recursive",
                              by_chrom = TRUE,
                              cores = 1) {
  UseMethod("find_linked_sites", x)
}

##' @export
##' @import foreach
find_linked_sites.varkit <- function(x,
                                     cutoff = .9,
                                     window = NULL,
                                     method = "recursive",
                                     by_chrom = TRUE,
                                     cores = 1) {
  stopifnot(is.logical(by_chrom))

  # Missing arguments
  if (is.null(window))
    window <- .window_default(use_bp = TRUE)

  # Short-circuit
  if (!by_chrom) {
    llist <- link_list(fixed(vcf, add_index = TRUE)$idx)
    return(find_linked_cols(
      geno(vcf), pos = fixed(vcf)$pos, cutoff = cutoff, window = window, method = method
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
  pb <- txtProgressBar(max = length(fix_chrom), style = 3)
  pb_set <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = pb_set)

  # Compute identical sites within each contig in parallel
  llist_chrom <- foreach::foreach(
    fix = fix_chrom,
    gt = gt_chrom,
    .options.snow = opts,
    .packages = "varkit"
  ) %dopar% {
    llist <- link_list(fix$idx)
    llist <- find_linked_cols(
      gt, pos = fix$pos, cutoff = cutoff, window = window, method = method
    )
    if (is.null(cl))
      pb_set(getTxtProgressBar(pb) + 1)
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
