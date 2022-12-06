## Helper functions ------
.fixed_columns <- function() {
  c("chrom", "pos", "id", "ref", "alt", "qual", "filter", "info", "format")
}

.stopifnotsingle <- function(x, class = NULL) {
  if (length(x) != 1)
    stop("Argument should have a length of 1!")
  if (!is.null(class) && !inherits(x, what = class))
    stop("Argument should be of class '", class, "'!")
}

.collapse <- function(x, delim = ", ") {
  paste(x, collapse = delim)
}

.not_variable <- function(x, na_rm = TRUE) {
  stopifnot(is.vector(x))
  if (na_rm)
    x <- x[!is.na(x)]
  if (length(x) <= 1)
    return(TRUE)
  all(duplicated(x)[-1])
}

.str_split_get_idx <- function(string, char, idx) {
  stopifnot(is.character(string))
  if (is.na(idx))
    return(rep(NA, length(string)))
  stopifnot(is.character(char) && length(char) == 1)
  stopifnot(is.numeric(idx) && length(idx) == 1)

  if (idx > 1) {
    match_before <- paste0("([^", char, "]+", char, "){", idx - 1, "}")
    string <- stringr::str_remove(string, pattern = match_before)
  }

  match_after <- paste0(char, ".*")
  stringr::str_remove(string, pattern = match_after)
}

.maxnchar <- function(x) {
  max(nchar(x), na.rm = TRUE)
}

.max_print_col <- function(mat, width = getOption("width")) {
  stopifnot(is.matrix(mat))

  # Reduce size of matrix to a reasonable size
  check_ncol <- min(c(300, ncol(mat)))
  mat <- mat[, seq_len(check_ncol), drop = FALSE]

  if (is.null(rownames(mat))) {
    rname_len <- nchar(nrow(mat)) + 3
  } else {
    rname_len <- .maxnchar(rownames(mat))
  }

  if (is.null(colnames(mat))) {
    # We add 3 for the [,] and for the space before the column
    cname_len <- nchar(seq_len(ncol(mat))) + 4
  } else {
    # We add 2 here for the potential `` around var names
    cname_len <- nchar(colnames(mat)) + 3
  }

  col_len <- apply(mat, 2, .maxnchar) + 1
  if (is.character(mat[1]))
    col_len <- col_len + 2

  # Check if col name or col value is widest, add row name width to first element
  col_widths <- pmax.int(cname_len, col_len)
  col_widths[1] <- col_widths[1] + rname_len

  # Return the index of the last column that fits within width
  which_fit <- which(cumsum(col_widths) < width)
  if (length(which_fit) == 0)
      return(1)
  max(which_fit)
}

.middle <- function(x, on_even = "left") {
  stopifnot(is.vector(x))
  len <- length(x)
  if (on_even == "right") {
    len <- len + 1
  } else if (on_even != "left") {
    stop("Option on_even = '", on_even, "' not recognised!")
  }
  x[ceiling(len / 2)]
}

.check_link_list <- function(llist) {
  len <- length(llist)

  skip <- rep(FALSE, len)
  for (i in seq_along(llist)) {
    if (skip[i])
      next()

    links <- llist[[i]]
    if (length(links) == 1 && (is.na(links) || links == 0))
      next()
    if (any(is.na(links)))
      stop("Unexpected NA value(s) found at index: ", i)

    if (any(links < 0 | links > len))
      stop("Out of bounds ([0,", len, "]) link value found at index: ", i)

    for (j in links) {
      if (!is.na(llist[[j]]))
        stop("Expected NA at position: ", j, " (link of ", i, ")!")
    }
    skip[links] <- TRUE
  }

  invisible(llist)
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

## Unused functions ------
.bi_gt2dosage <- function(mat) {
  stopifnot(is.matrix(mat))
  stopifnot(is.character(mat))
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

.dosage2gt <- function(mat, ploidy, cutoffs = c(.5, 1.5), na = NA) {
  stopifnot(is.matrix(mat) && is.numeric(mat))
  if (ploidy != 2)
    stop("This function only supports a 'ploidy' of 2")
  stopifnot(length(cutoffs) == ploidy)
  if (max(mat, na.rm = TRUE) < cutoffs[2])
    warning("No values pass the upper cutoff, ensure 'cutoffs' are correct.")

  gt <- matrix("0/1", nrow = nrow(mat), ncol = ncol(mat), dimnames = dimnames(mat))
  gt[is.na(mat)] <- na
  gt[mat < cutoffs[1]] <- "0/0"
  gt[mat >= cutoffs[2]] <- "1/1"
  class(gt) <- class(mat)
  gt
}

.missed_correlations <- function(cor_mat, cutoff = .9, llist = NULL) {
  stopifnot(is.matrix(cor_mat))
  stopifnot(nrow(cor_mat) == ncol(cor_mat))
  if (!is.null(llist)) {
    stopifnot(is_link_list(llist))
    stopifnot(length(llist) == ncol(cor_mat))

    rm <- which_remove(llist)
    cor_mat <- cor_mat[-rm, -rm]
  }

  cor_mat[lower.tri(cor_mat, diag = T)] <- NA
  missed <- apply(cor_mat, 2, function(col) {
    any(col >= cutoff, na.rm = TRUE)
  })
  missed
}
