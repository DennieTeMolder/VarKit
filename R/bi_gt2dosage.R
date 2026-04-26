# REVIEW rework using matrixStats and export?
bi_gt2dosage <- function(mat) {
  stopifnot(is.matrix(mat), is.character(mat))
  dims <- dim(mat)
  res <- matrix(NA, dims[1L], dims[2L], dimnames = dimnames(mat))
  for (j in seq_len(dims[2L])) {
    alleles <- stringr::str_split(mat[, j], "[/|]")
    for(i in seq_len(dims[1L])) {
      if (all(alleles[[i]] %in% c("0", "1")))
        res[i,j] <- sum(alleles[[i]] == "1")
    }
  }
  class(res) <- class(mat)
  res
}
