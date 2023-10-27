.r2_cutoff_parallel <- function(mat_list, comps, cutoff, return_r2, cores) {
  cores <- min(floor(cores), nrow(comps))

  cli::cli_alert_info(c("Running ", .frmt_big(nrow(comps)), " comparisons between ", .frmt_big(length(mat_list)), " windows"))

  if (cores < 2) {
    return(purrr::map2(comps$i, comps$j, function(i, j) {
      x <- mat_list[[i]]
      y <- if (i == j) NULL else mat_list[[j]]
      r2_cutoff(x, y, cutoff = cutoff, return_r2 = return_r2, use_colnames = TRUE)
    }, .progress = TRUE))
  }

  cli::cli_progress_step("Setting up parallel workers")

  # Rearrange lower half for better data usage by clusters
  half <- max(comps$i) / 2
  comps <- split(comps[, c("i", "j")], ifelse(comps$j <= half, "upper", "lower"))
  comps <- rbind(comps$upper[order(comps$upper$j), ], comps$lower)

  # Divide into CORES * EACH lists, resetting the indices within each split
  each <- 4
  split_comp <- splitm(comps, size = ceiling(nrow(comps) / cores / each))
  split_mat_list <- vector("list", length(split_comp))
  for (i in seq_along(split_comp)) {
    curr <- split_comp[[i]]
    curr_fct <- as.factor(as.matrix(curr))
    split_mat_list[[i]] <- mat_list[as.integer(levels(curr_fct))]
    df <- data.frame(matrix(as.integer(curr_fct), ncol = 2))
    split_comp[[i]] <- setNames(df, names(curr))
  }

  # Sort splits on the size of the most computationally intensive matrix
  sizes <- sapply(split_mat_list, function(x) {
    sum(sapply(x, function(xx) max(0, ncol(xx$below_min_size))))
  })
  size_order <- order(sizes, decreasing = TRUE)
  split_comp <- split_comp[size_order]
  split_mat_list <- split_mat_list[size_order]

  # Start workers, PSOCK is much more memory efficient then FORK
  cl <- parallel::makeCluster(cores, type = "PSOCK")
  on.exit(tryCatch(parallel::stopCluster(cl), error = function(e) NULL))

  # Setup BLAS threads on worker env
  total_threads <- min(blas_threads(), parallel::detectCores(), na.rm = TRUE)
  blas_threads <- max(floor(total_threads / cores), 1)
  parallel::clusterExport(cl, "blas_threads", envir = rlang::current_env())
  parallel::clusterEvalQ(cl, varkit::blas_threads(blas_threads))

  cli::cli_progress_done()

  # Inform the user about memory usage if above 1 Gb
  session_size <- 100e6
  mem_usage <- sum(sapply(split_mat_list[seq_len(cores)], utils::object.size))
  mem_usage <- mem_usage * 2 + cores * session_size
  class(mem_usage) <- "object_size"
  if (mem_usage > 1074e6) {
    cli::cli_alert_warning(c(
      "A minimum ", format(mem_usage, units = "Gb", digits = 2L),
      " of RAM will be needed for parallel computations"))
  }

  cli::cli_progress_step("Running parallel computations")

  # NOTE Using an anonymous function here will copy the enclosing env to the workers, using a lot of memory
  res <- parallel::clusterMap(cl, .r2_cutoff_worker,
    split_mat_list, split_comp, MoreArgs = list(cutoff = cutoff, return_r2 = return_r2),
    RECYCLE = FALSE, USE.NAMES = FALSE, .scheduling = "dynamic"
  )
  parallel::stopCluster(cl)

  cli::cli_progress_done()

  # Combine results per core and restore old order
  res <- res[match(seq_along(res), size_order)]
  res <- unlist(res, recursive = FALSE, use.names = FALSE)
  res[order(comps$i, comps$j)]
}

.r2_cutoff_worker <- function(data, comp, cutoff, return_r2) {
  lapply_rowwise(comp, function(i, j) {
    x <- data[[i]]
    y <- if (i == j) NULL else data[[j]]
    r2_cutoff(x, y, cutoff = cutoff, return_r2 = return_r2, use_colnames = TRUE)
  })
}
