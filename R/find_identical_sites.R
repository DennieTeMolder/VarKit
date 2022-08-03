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
  # Register cores for parallel computing if required
  parallel <- cores > 1
  if (parallel) {
    if (verbose)
      stop("The 'verbose' option only supports cores = 1!")
    log_file <- tempfile(fileext = ".log")
    message("Writing parallel worker logs to: ", log_file)
    cl <- snow::makeCluster(cores, outfile = log_file)
    doSNOW::registerDoSNOW(cl = cl)
  } else {
    foreach::registerDoSEQ()
  }

  # Get required dat from varkit object
  fix_chrom <- fixed(vcf, add_index = TRUE, by_chrom = TRUE, only_positions = TRUE)
  gt_chrom <- geno(vcf, by_chrom = TRUE)

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
    .export = c("link_list", "lookup", "link_entries", "find_identical_cols")
  ) %dopar% {
    llist <- link_list(fix$idx)
    find_identical_cols(gt, tolerance = tolerance, llist = llist)
  }

  # Cleanup
  close(pb)
  if (parallel)
    snow::stopCluster(cl)

  # TODO Unpack the top level of the resulting list and translate
}
