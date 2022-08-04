##' @export
find_identical_sites <- function(x,
                                 tolerance = .01,
                                 by_chrom = TRUE,
                                 cores = 1) {
  UseMethod("find_identical_sites", x)
}

##' @export
##' @import foreach
find_identical_sites.varkit <- function(x,
                                        tolerance = .01,
                                        by_chrom = TRUE,
                                        cores = 1) {
  # TODO clean-up this function
  stopifnot(is.logical(by_chrom))

  # Short-circuit
  if (!by_chrom) {
    llist <- link_list(fixed(vcf, add_index = TRUE)$idx)
    return(find_identical_cols(geno(vcf), tolerance = tolerance, llist = llist))
  }

  stopifnot(is.numeric(tolerance) && tolerance >= 0)
  stopifnot(is.numeric(cores) && cores >= 1)

  # Register cores for parallel computing if required
  parallel <- cores > 1
  if (parallel) {
    log_file <- tempfile(fileext = ".log")
    message("Writing parallel worker logs to: ", log_file)
    cl <- snow::makeCluster(cores, outfile = log_file)
    doSNOW::registerDoSNOW(cl = cl)
  } else {
    foreach::registerDoSEQ()
  }

  # Get required data from varkit object
  fix_chrom <- fixed(x, add_index = TRUE, by_chrom = TRUE, only_positions = TRUE)
  gt_chrom <- geno(x, by_chrom = TRUE)

  # Order contigs most to lest for efficiency
  idx <- order(sapply(fix_chrom, nrow), decreasing = TRUE)
  fix_chrom <- fix_chrom[idx]
  gt_chrom <- gt_chrom[idx]

  # Add progress bar to foreach
  pb <- txtProgressBar(max = length(fix_chrom), style = 3)
  pb_set <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = pb_set)

  # Compute linkage within each contig in parallel
  llist_chrom <- foreach::foreach(
    fix = fix_chrom,
    gt = gt_chrom,
    .options.snow = opts,
    .packages = "varkit"
  ) %dopar% {
    llist <- link_list(fix$idx)
    llist <- find_identical_cols(gt, tolerance = tolerance, llist = llist)
    if (!parallel)
      pb_set(getTxtProgressBar(pb) + 1)
    llist
  }

  # Cleanup
  close(pb)
  if (parallel)
    snow::stopCluster(cl)

  # Add names and revert order
  names(llist_chrom) <- names(fix_chrom)
  llist_chrom <- llist_chrom[idx]

  # Prepare list of link_lists for merge
  for (i in seq_along(llist_chrom)) {
    current <- llist_chrom[[i]]

    cur_trans <- attr(current, "translation")
    if (is.null(cur_trans))
      cur_trans <- seq_along(current)

    if (i == 1) {
      trans <- cur_trans
      list_end <- length(current)
      next()
    }

    needs_adjust <- contains_link(current)
    current[needs_adjust] <- lapply(current[needs_adjust], function(x) x + list_end)
    llist_chrom[[i]] <- current

    trans <- c(trans, cur_trans)
    list_end <- list_end + length(current)
  }

  # Merge and adjust attributes
  llist <- unlist(llist_chrom, recursive = FALSE)
  if (!all(trans == seq_along(llist)))
    attr(llist, "translation") <- trans
  class(llist) <- "link_list"
  llist
}
