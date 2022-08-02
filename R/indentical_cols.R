FindIdenticalColsContig <- function(
  mat, loc, tolerance = .01, cores = 1, progress = T, verbose = F
) {
  require(foreach)

  # Checks
  stopifnot(is.matrix(mat) && is.numeric(mat))
  stopifnot(is.data.frame(loc))
  stopifnot(ncol(loc) >= 2)
  stopifnot(nrow(loc) == ncol(mat))

  # Unpack the loc dataframe (1st column = contig, 2nd = position bp/cM)
  contigs <- loc[[1]]
  pos <- loc[[2]]

  # Register cores for parallel computing if required
  parallel <- cores > 1
  if (parallel) {
    logFile <- tempfile()
    message("Writing parallel worker logs to: ", logFile)
    cl <- snow::makeCluster(cores, outfile = logFile)
    doSNOW::registerDoSNOW(cl = cl)
    # Disable verbosity as it doesn't work in parallel
    verbose <- F
  } else {
    foreach::registerDoSEQ()
  }

  # Find unique contigs, order from most to least positions for efficiency
  table <- table(contigs)
  order <- names(sort(table[table > 0], decreasing = T))

  if (progress) {
    pb <- txtProgressBar(max = length(order), style = 3)
    PbSet <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = PbSet)
  } else {
    opts <- NULL
  }

  # Compute linkage within each contig in parallel
  linkList <- foreach::foreach(
    contig = order,
    .options.snow = opts,
    .export = c("CreateNaList", "GetEntries", "LinkEntries", "FindIdenticalCols")
  ) %dopar% {
    if (verbose) {
      message(sprintf("\nAnalyzing contig %s", contig))
    }

    idx <- which(contigs == contig)

    linkList <- FindIdenticalCols(mat, pos, idx, tolerance)

    if (progress & !parallel)
      PbSet(getTxtProgressBar(pb) + 1)

    return(linkList)
  }

  # Cleanup
  if (parallel)
    snow::stopCluster(cl)
  if (progress)
    close(pb)

  # Unpack the top level of the resulting list
  linkList <- unlist(linkList, recursive = F)
  return(linkList)
}

FindIdenticalCols <- function(mat, pos = NULL, idx = NULL, tolerance = .01) {
  # Checks
  stopifnot(is.matrix(mat) && is.numeric(mat))
  if (!is.null(pos)) {
    stopifnot(is.numeric(pos))
    stopifnot(length(pos) == ncol(mat))
  }
  if (!is.null(idx))
    stopifnot(is.integer(idx))

  # Create vars not supplied
  if (is.null(idx))
    idx <- 1:ncol(mat)

  # List for storing results
  linkList <- CreateNaList(idx)

  n <- length(idx)
  # Keep track of indexes that can be skipped
  skip <- rep(F, n)
  # Skip last column as it has already been compared to everything
  for (i in seq(1, n - 1)) {
    # Continue to next if current column was already removed
    if (skip[i])
      next()

    # Only check columns after as prior have been compared already
    toCheck <- seq(i + 1, n)
    toCheck <- toCheck[!skip[toCheck]]
    if (length(toCheck) == 0)
      next()

    # Compute equality using matrix arithmetic for speed
    diff <- mat[,idx[toCheck],drop=F] - mat[,idx[i]]
    sums <- colMeans(abs(diff), na.rm = T)
    identical <- toCheck[!is.na(sums) & sums <= tolerance]
    if (length(identical) == 0)
      next()

    # If there is more than one link, keep the most central column
    if (length(identical) > 1 && !is.null(pos)) {
      combined <- c(idx[i], idx[identical])
      combinedPos <- pos[combined]
      iMean <- which.min(abs(combinedPos - mean(combinedPos)))
      linkList <- LinkEntries(combined[iMean], combined[-iMean], linkList)
    } else {
      # Keep first if there is a single link or positions are unknown
      linkList <- LinkEntries(idx[i], idx[identical], linkList)
    }
    skip[identical] <- T
  }
  return(linkList)
}
