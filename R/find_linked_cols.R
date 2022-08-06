FindLinkedCols <- function(
  mat, pos = NULL, cutoff = .9, window = NULL, idx = NULL, debug = F
) {
  # Checks
  stopifnot(is.matrix(mat) && is.numeric(mat))
  if (!is.null(pos)) {
    stopifnot(is.numeric(pos))
    stopifnot(length(pos) == ncol(mat))
  }
  if (!is.null(window)) {
    stopifnot(is.numeric(window) && length(window) == 1)
    stopifnot(!is.null(pos))
  }
  if (!is.null(idx))
    stopifnot(is.integer(idx))

  # Create missing vars
  if (is.null(idx))
    idx <- 1:ncol(mat)

  n <- length(idx)

  # Tabulate all linked cols
  linkedCols <- data.frame()
  for (i in seq(1, n - 1)) {
    current <- idx[i]

    if (debug)
      message(paste("Checking column with index:", current))

    # Find columns right of sample and within window if specified
    if (!is.null(window)) {
      check <- which(pos[idx] > pos[current] & pos[idx] <= pos[current] + window)
    } else {
      check <- seq(i + 1, n)
    }

    if (length(check) == 0)
      next()

    # Find cols above linkage cutoff
    r2 <- CorPairwiseSilent(mat[,current], mat[,idx[check],drop=F])^2
    above <- check[r2 >= cutoff & !is.na(r2)]

    if (length(above) == 0)
      next()

    ## if (debug)
    ##   message(paste("  Detected", length(above), "column indexes >= threshold"))

    # Store linked columns
    df <- data.frame(i, j = above)
    linkedCols <- rbind(linkedCols, df)
  }

  if (debug)
    message("Resolving linked columns")

  # Create list to track links
  linkList <- CreateNaList(idx)

  # From linked sets keep the one column with the highest correlation
  if (nrow(linkedCols) > 0)
    linkList <- .ResolveLinkedCols(linkedCols, mat, linkList, idx)

  return(linkList)
}

.cor_pair_silent <- function(x, y = NULL) {
  suppressWarnings(cor(x, y, use = "pairwise.complete.obs"))
}

.ResolveLinkedCols <- function(linkedCols, mat, linkList, idx) {
  # Index the links of all of the columns for lookup speed
  allCols <- unique(c(linkedCols$i, linkedCols$j))
  allCols <- as.character(allCols)
  index <- sapply(allCols, .IndexLinkedCols, df = linkedCols, simplify = F)

  # Keep track of which cols to skip
  skip <- rep(F, length(idx))
  for (current in allCols) {
    if (skip[as.integer(current)])
      next()
    message(paste("Idx:", current))
    # Retrieve directly linked columns
    a <- index[[current]]
    # Retrieve all indirectly linked columns by iteratively expanding array
    currentLength <- length(a)
    while (T) {
      previousLength <- currentLength
      a <- .IndexGet(a, index)
      currentLength <- length(a)
      # Break loop if the array stops growing
      if (currentLength == previousLength)
        break()
    }
    # TODO involve number of missing calls in decision process
    # If there is more than one link, keep column closest to mean pos
    if (length(a) > 2) {
      combinedPos <- pos[a]
      iMean <- which.min(abs(combinedPos - mean(combinedPos)))
    } else {
      iMean <- 1
    }
    linkList <- LinkEntries(idx[a[iMean]], idx[a[-iMean]], linkList)
    skip[as.integer(a)] <- T
  }
  return(linkList)
}

.IndexLinkedCols <- function(i, df) {
  i <- as.integer(i)
  linked <- c(i, df$j[df$i == i], df$i[df$j == i])
  return(unique(linked))
}

.IndexGet <- function(i, index) {
  x <- unlist(index[as.character(i)], use.names = F)
  return(unique(x))
}
