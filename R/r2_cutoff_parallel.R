.r2_cutoff_parallel <- function(mat_list, comps, cutoff, return_r2, cores) {
  cores <- min(cores, nrow(comps))

  cli::cli_alert_info(c("Running ", .frmt_big(nrow(comps)), " comparisons between ", .frmt_big(length(mat_list)), " windows"))

  if (cores < 2) {
    return(purrr::map2(comps$i, comps$j, function(i, j) {
      x <- mat_list[[i]]
      y <- if (i == j) NULL else mat_list[[j]]
      r2_cutoff(x, y, cutoff = cutoff, return_r2 = return_r2, check_colnames = FALSE)
    }, .progress = TRUE))
  }

  cli::cli_progress_step("Setting up parallel workers")

  # Rearrange lower half for better data usage by clusters
  half <- max(comps$i) / 2
  comps <- split(comps[, c("i", "j")], ifelse(comps$j <= half, "upper", "lower"))
  comps <- rbind(comps$upper[order(comps$upper$j), ], comps$lower)

  # Split per core, resetting the indices within each split
  split_comp <- splitm(comps, size = ceiling(nrow(comps) / cores))
  split_mat_list <- vector("list", length(split_comp))
  for (i in seq_along(split_comp)) {
    curr <- split_comp[[i]]
    curr_fct <- as.factor(as.matrix(curr))
    split_mat_list[[i]] <- mat_list[as.integer(levels(curr_fct))]
    df <- data.frame(matrix(as.integer(curr_fct), ncol = 2))
    split_comp[[i]] <- setNames(df, names(curr))
  }

  # Start workers
  cl <- parallel::makeCluster(cores, type = if (parallelly::supportsMulticore()) "FORK" else "PSOCK")
  on.exit(parallel::stopCluster(cl))

  # Setup worker envs
  blas_threads <- max(floor(RhpcBLASctl::blas_get_num_procs() / cores), 1)
  parallel::clusterExport(cl, c("cutoff", "return_r2", "blas_threads"), envir = rlang::current_env())
  parallel::clusterEvalQ(cl, RhpcBLASctl::blas_set_num_threads(blas_threads))
  parallel::clusterEvalQ(cl, library(varkit))
  ## parallel::clusterEvalQ(cl, devtools::load_all())

  cli::cli_progress_done()

  # Inform the user about memory usage is needed
  session_size <- 2e8
  mem_usage <- sum(sapply(split_comp, function(x) {
    length(unique(as.vector(unlist(x, use.names = FALSE))))
  })) / length(mat_list) + 2
  mem_usage <- mem_usage * utils::object.size(mat_list) + cores * session_size
  class(mem_usage) <- "object_size"
  if (mem_usage > 5e8) # 0.5 Gb
    cli::cli_alert_warning(c("Estimated memory usage: ", format(mem_usage, units = "Gb", digits = 2L)))

  cli::cli_progress_step("Running parallel computations")

  res <- parallel::clusterMap(cl, function(data, comp) {
    lapply_rowwise(comp, function(i, j) {
      x <- data[[i]]
      y <- if (i == j) NULL else data[[j]]
      r2_cutoff(x, y, cutoff = cutoff, return_r2 = return_r2, check_colnames = FALSE)
    })
  }, split_mat_list, split_comp, RECYCLE = FALSE, .scheduling = "static")

  cli::cli_progress_done()

  res <- unlist(res, recursive = FALSE, use.names = FALSE)
  res[order(comps$i, comps$j)]
}
