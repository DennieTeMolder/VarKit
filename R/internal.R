## Helper functions ------
.fixed_columns <- function() {
  c("chrom", "pos", "id", "ref", "alt", "qual", "filter", "info", "format")
}

.collapse <- function(x, delim = ", ") {
  paste(x, collapse = delim)
}

.linked_only <- function(x, allow_na = TRUE) {
  if (allow_na) {
    x <- na.omit(x)
  } else {
    if (any(is.na(x)))
      stop("Removed indexes (idx) requested, this not allowed!")
  }
  x[as.logical(x)]
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

.merge_link_list <- function(llist_list) {
  # Prepare list of link_lists for merge
  for (i in seq_along(llist_list)) {
    current <- llist_list[[i]]

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
    llist_list[[i]] <- current

    trans <- c(trans, cur_trans)
    list_end <- list_end + length(current)
  }

  # Merge and adjust attributes
  llist <- unlist(llist_list, recursive = FALSE)
  if (!all(trans == seq_along(llist)))
    attr(llist, "translation") <- trans
  class(llist) <- "link_list"
  llist
}

## Unused functions ------
.bi_gt2dosage <- function(mat) {
  stopifnot(is.matrix(mat))
  stopifnot(inherits(mat[[1,1]], "character"))
  dims <- dim(mat)
  res <- matrix(NA, dims[1], dims[2], dimnames = dimnames(mat))
  for (j in 1:dims[2]) {
    alleles <- stringr::str_split(mat[, j], "[/|]")
    for(i in 1:dims[1]) {
      if (all(alleles[[i]] %in% c("0", "1")))
        res[i,j] <- sum(alleles[[i]] == "1")
    }
  }
  class(res) <- class(mat)
  res
}

.not_variable <- function(x) {
  all(duplicated(na.omit(x))[-1])
}

## .MissedCorrelations <- function(corMat, cutoff = .9, linkList = NULL) {
##   stopifnot(is.matrix(corMat))
##   dims <- dim(corMat)
##   stopifnot(dims[1] == dims[2])

##   # Ensure matrix is truly symmetric
##   checkMax <- 1000
##   if (dims[1] > checkMax) {
##     testMat <- corMat[1:checkMax, 1:checkMax]
##     stopifnot(all.equal(testMat, t(testMat)))
##   } else {
##     stopifnot(all.equal(corMat, t(corMat)))
##   }

##   if (!is.null(linkList)) {
##     rm <- RemoveWhich(linkList)
##     corMat <- corMat[-rm, -rm]
##   }

##   corMat[lower.tri(corMat, diag = T)] <- NA
##   miss <- apply(corMat, 1, function(row) {
##     any(row >= cutoff, na.rm = T)
##   })
##   return(sum(miss))
## }
