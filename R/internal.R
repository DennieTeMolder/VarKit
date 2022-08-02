.fixed_columns <- function() {
  c("chrom", "pos", "id", "ref", "alt", "qual", "filter", "info", "format")
}

.parse_col_names <- function(x) {
  col_names <- stringr::str_remove(x, "#")
  col_names <- stringr::str_split(col_names, "\t")[[1]]

  if (length(col_names) < 5)
    stop("The number of columns is too low, make sure the file is TAB delimited.")

  # Standardise the names of fixed columns
  fixed_names <- .fixed_columns()
  fixed_idx <- na.omit(match(fixed_names, tolower(col_names)))
  col_names[fixed_idx] <- fixed_names[seq_along(fixed_names) %in% fixed_idx]

  if (!all(c("chrom", "pos") %in% col_names))
    warning("The VCF header is missing the CHROM and/or POS column(s)!")

  col_names
}

.match_key_value <- function(x, pattern) {
  # Match
  x <- stringr::str_match_all(x, pattern)
  # Convert
  x <- t(sapply(x, function(x) x[, -1]))
  # First half == colnames; second half == values
  i <- seq(1, ncol(x) / 2)
  df <- as.data.frame(x[, -i])
  colnames(df) <- x[1, i]
  # Return as tibble
  tibble::tibble(df)
}

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

.cor_pair_silent <- function(x, y = NULL) {
  suppressWarnings(cor(x, y, use = "pairwise.complete.obs"))
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
