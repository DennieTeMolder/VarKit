##' @export
find_linked_sites <- function(x,
                              cutoff = 0.9,
                              window_size_bp = 1e5,
                              n_window = 1,
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
                                     window_size_bp = 1e5,
                                     n_window = 1,
                                     by_chrom = TRUE,
                                     return_r2 = FALSE,
                                     cores = 1,
                                     min_ncol = 20
                                     ) {
  stopifnot(is.numeric(cutoff), length(cutoff) == 1)
  stopifnot(is.numeric(window_size_bp), length(window_size_bp) == 1)
  stopifnot(is.numeric(n_window), length(n_window) == 1)
  stopifnot(is.numeric(min_ncol), length(min_ncol) == 1)

  # Ensure the matrix has col names so it can be reconstructed
  gt <- geno(x)
  if (!(is.matrix(gt) && is.numeric(gt)))
    stop("geno(vcf) should be a numeric matrix!")
  colnames(gt) <- seq_len(ncol(gt))

  mat_list <- splitm(gt,
    f = site(x, "chrom"),       # Split by CHROM
    margin = 2,                   # Split over the columns
    distances = site(x, "pos"), # Further split based on bp distance
    size = window_size_bp,        # Max bases covered by a split
    by_na_pattern = TRUE,         # Further split by NA pattern across samples
    min_size = min_ncol
  )
  rm(gt)

  if (by_chrom) {
    # Make a dataframe of lists that contains all requested comparisons
    comps <- lapply(names(mat_list), function(chrom) {
      comp <- .generate_comparisons(mat_list[[chrom]], window = n_window)
      comp$chrom <- chrom
      comp
    })
    comps <- do.call(rbind, comps)
  } else {
    # When removing the by chrom grouping, windows extend into adjacent chromosomes
    mat_list <- unlist(mat_list, recursive = FALSE, use.names = FALSE)
    comps <- .generate_comparisons(mat_list, window = n_window)
  }

  results <- .r2_cutoff_parallel(comps$x, comps$y, cutoff = cutoff, return_r2 = return_r2, cores = cores)

  # Combine output according to by_chrom
  if (!by_chrom)
    return(set_origin(do.call(rbind, results), x))

  results <- split(results, comps$chrom)
  results <- lapply(results, function(chrom_result) do.call(rbind, chrom_result))
  set_origin(results, x)
}
