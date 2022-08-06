FindLinkedColsContig <- function(
  mat, loc, cutoff = .9, window = NULL, cores = 1, progress = T, debug = F
) {
  require(foreach)

  # Checks
  stopifnot(is.matrix(mat) && is.numeric(mat))
  stopifnot(is.data.frame(loc))
  stopifnot(ncol(loc) >= 2)
  stopifnot(nrow(loc) == ncol(mat))

  parallel <- cores > 1

  if (debug) {
    if (parallel)
      stop("Debugging only supported when cores = 1")
    progress <- F
  }

  # Unpack the loc dataframe (1st column = contig, 2nd = position bp/cM)
  contigs <- loc[[1]]
  pos <- loc[[2]]

  # Register cores for parallel computing if required
  if (parallel) {
    logFile <- tempfile()
    message("Writing parallel worker logs to: ", logFile)
    cl <- snow::makeCluster(cores, outfile = logFile)
    doSNOW::registerDoSNOW(cl = cl)
  } else {
    foreach::registerDoSEQ()
  }

  # Find unique contigs, order from most to least positions for efficiency
  table <- table(contigs)
  order <- names(sort(table[table > 0], decreasing = T))

  # Initiate the progress barr
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
    .export = c(
      "CreateNaList", "GetEntries", "LinkEntries", "CorPairwiseSilent",
      "FindLinkedCols", ".ResolveLinkedCols", ".IndexLinkedCols",
      ".IndexGet"
    )
  ) %dopar% {
    if (debug)
      message(sprintf("\nAnalyzing contig %s", contig))

    idx <- which(contigs == contig)
    linkList <- FindLinkedCols(
      mat = mat, pos = pos, cutoff = cutoff, window = window, idx = idx
    )

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
