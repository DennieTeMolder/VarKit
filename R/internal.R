## Helper functions ------
.fixed_columns <- function() {
  c("chrom", "pos", "id", "ref", "alt", "qual", "filter", "info", "format")
}

.collapse <- function(x, delim = ", ") {
  paste(x, collapse = delim)
}

.linked_only <- function(x, allow_na = TRUE) {
  if (allow_na) {
    x <- x[!is.na(x)]
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

.pb_increment <- function(pb, n = 1) {
  val <- utils::getTxtProgressBar(pb)
  utils::setTxtProgressBar(pb, value = val + n)
}

.concat_link_lists <- function(llist_list) {
  # Collect all translations and prepare llist_list for concatenating
  n <- NULL
  trans_list <- vector(mode = "list", length = length(llist_list))
  for (i in seq_along(llist_list)) {
    cur_len <- length(llist_list[[i]])

    # Store translations
    cur_trans <- attr(llist_list[[i]], "translation")
    if (is.null(cur_trans)) {
      trans_list[[i]] <- 1:cur_len
    } else {
      trans_list[[i]] <- cur_trans
    }

    # First llist does not need to be adjusted
    if (i == 1) {
      n <- cur_len
      next()
    }

    # Adjust entries that contain links
    needs_adjust <- contains_link(llist_list[[i]])
    for (j in which(needs_adjust)) {
      llist_list[[i]][[j]] <- llist_list[[i]][[j]] + n
    }

    n <- n + cur_len
  }

  # Perform the concatenation
  llist <- unlist(llist_list, recursive = FALSE, use.names = FALSE)
  rm(llist_list)
  trans <- unlist(trans_list)
  rm(trans_list)

  if (any(duplicated(trans)))
    stop("Duplicated translations detected, cannot concat!")

  # Add attributes
  if (!all(trans == seq_along(llist)))
    attr(llist, "translation") <- trans
  class(llist) <- "link_list"

  llist
}

.which_least_na_middle <- function(cols, data) {
  n_na <- colSums(is.na(data[, cols]))
  least_na <- which(n_na == min(n_na))
  # Take the middle value
  if (length(least_na) > 1)
    return(least_na[ceiling(length(least_na) / 2)])
  least_na
}

.all_null <- function(x) {
  for (i in seq_along(x)) {
    if (!is.null(x[[i]]))
      return(FALSE)
  }
  return(TRUE)
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
  x <- stats::na.omit(x)
  if (length(x) < 2)
    return(TRUE)
  all(duplicated(x)[-1])
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
