##' @export
find_linked_sites <- function(x,
                              cutoff = 0.9,
                              windows = 1,
                              size_bp = 1e5,
                              by_chrom = TRUE,
                              return_r2 = FALSE,
                              cores = 1,
                              min_ncol = 20
                              ) {
  UseMethod("find_linked_sites", x)
}

##' @export
find_linked_sites.varkit <- function(x,
                                     cutoff = 0.9,
                                     windows = 1,
                                     size_bp = 1e5,
                                     by_chrom = TRUE,
                                     return_r2 = FALSE,
                                     cores = 1,
                                     min_ncol = 20
                                     ) {
  validate_varkit(x)
  stopifnot(is.numeric(cutoff), length(cutoff) == 1)
  stopifnot(is.numeric(size_bp), length(size_bp) == 1)
  stopifnot(is.numeric(windows), length(windows) == 1)
  stopifnot(is.numeric(min_ncol), length(min_ncol) == 1)
  if (!(is.matrix(x$geno) && is.numeric(x$geno)))
    stop("geno(x) should be a numeric matrix!")

  # Label the columns with their index so they can be mapped back later
  colnames(x$geno) <- seq_len(ncol(x$geno))

  mat_list <- splitm(geno(x),
    f = if (by_chrom) site(x, "chrom") else NULL, # Split by CHROM
    margin = 2,                                   # Split over the columns
    distances = site(x, "pos"),                   # Further split based on bp distance
    size = size_bp,                               # Max bases covered by a split
    by_na_pattern = TRUE,                         # Further split by NA pattern across samples
    min_size = min_ncol                           # Merge NA splits with fewer then MIN_NCOL
  )

  if (by_chrom) {
    # Undo nesting by chrom
    chroms <- unlist(lapply(names(mat_list), function(nm) rep(nm, length(mat_list[[nm]]))))
    mat_list <- unlist(mat_list, recursive = FALSE, use.names = FALSE)
  }

  # Compute the required comparisons
  comps <- compute_comparisons(mat_list, windows = windows)

  if (by_chrom) {
    # Get the CHROM of each comparison, exclude cross-CHROM comparisons
    comps$chrom <- chroms[comps$i]
    comps <- comps[chroms[comps$j] == comps$chrom, ]
  }

  results <- .r2_cutoff_parallel(mat_list, comps = comps, cutoff = cutoff, return_r2 = return_r2, cores = cores)

  .msg_step("Collecting results")

  # Combine output according to by_chrom
  if (by_chrom) {
    results <- split(results, comps$chrom)
    results <- lapply(results, function(chrom_result) do.call(rbind, chrom_result))
  } else {
    results <- do.call(rbind, results)
  }

  set_origin(results, x)
}
