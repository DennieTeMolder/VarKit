# FUN should take in a genotype matrix (samples x sites) as its first argument,
# a vector with chromosomal position in bp as its second argument, and a
# link_list as its third argument. It should return this link_list with the
# sites that are considered identical linked.
.compare_sites <- function(x,
                           FUN,
                           by_chrom = TRUE,
                           cores = 1,
                           debug = FALSE,
                           ...) {
  stopifnot(is_varkit(x))
  stopifnot(is.function(FUN))
  .stopifnotsingle(by_chrom, class = "logical")
  .stopifnotsingle(cores, class = "numeric")
  .stopifnotsingle(debug, class = "logical")

  # Short-circuit
  if (!by_chrom) {
    loc <- fixed(x, add_index = TRUE)
    llist <- link_list(loc$idx)
    if (cores > 1)
      warning("The 'cores' option only has effect when  `by_chrom = TRUE`.")
    return(do.call(FUN, list(geno(x), loc$pos, llist, ...)))
  }

  if (debug)
    cores <- 1

  # Register cores for parallel computing if required
  cl <- .register_workers(cores)

  # Get required data from varkit object
  fix_chrom <- fixed(x, cols = "pos", add_index = TRUE, by_chrom = TRUE)
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
      message("Processing CHROM:", fix$chrom[1])

    llist <- link_list(fix$idx, check = FALSE)
    llist <- do.call(FUN, list(gt, fix$pos, llist, ...))

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

.register_workers <- function(cores) {
  stopifnot(is.numeric(cores) && cores >= 1)
  if (cores > 1) {
    log_file <- tempfile(fileext = ".log")
    message("Writing parallel worker logs to: ", log_file)
    cl <- snow::makeCluster(cores, outfile = log_file)
    doSNOW::registerDoSNOW(cl = cl)
  } else {
    foreach::registerDoSEQ()
    cl <- NULL
  }
  cl
}

.unregister_workers <- function(cl) {
  if (!is.null(cl))
    snow::stopCluster(cl)
}

.pb_increment <- function(pb, n = 1) {
  val <- utils::getTxtProgressBar(pb)
  utils::setTxtProgressBar(pb, value = val + n)
}
