## Helper functions ------
.fixed_columns <- function() {
  c("chrom", "pos", "id", "ref", "alt", "qual", "filter", "info", "format")
}

.stopifnotsingle <- function(x, class = NULL) {
  if (!is.null(class) && !inherits(x, what = class))
    stop("Argument '", substitute(x), "' should be of class '", class, "'!")
  if (length(x) != 1)
    stop("Argument '", substitute(x), "' should have a length of 1!")
}

.collapse <- function(x, delim = ", ") {
  paste(x, collapse = delim)
}

.str_starts <- function(x, what) {
  substr(x, 1, nchar(what)) == what
}

.str_ends <- function(x, what, n_char = nchar(x)) {
  substr(x, n_char - nchar(what) + 1, n_char) == what
}

.str_split_i <- function(string, on, i) {
  stringr::str_split_i(string, pattern = stringr::fixed(on), i = i)
}

.not_variable <- function(x, na_rm = TRUE) {
  stopifnot(is.vector(x))
  if (na_rm)
    x <- x[!is.na(x)]
  if (length(x) <= 1)
    return(TRUE)
  all(duplicated(x)[-1])
}

.mutate_all <- function(.tbl, .fns, ...) {
  dplyr::mutate(.tbl, dplyr::across(dplyr::everything(), .fns, ...))
}

.print_tbl <- function(x, ...) {
  if (FALSE) pillar::pillar() # Silence imports checker
  do.call(utils::getS3method("print", "tbl", envir = getNamespace("pillar")), c(list(x), ...))
}

.maxnchar <- function(x) {
  if (all(is.na(x)))
    return(2)
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

.ad2aaf <- function(vcf, type = "AD") {
  stopifnot(is_varkit(vcf))

  # Extract allelic depth (AD)
  ad <- parse_format_type(vcf, type)
  n_sites <- ncol(ad)
  n_samples <- nrow(ad)
  n_allele <- max(stringr::str_count(ad[1, ], stringr::fixed(","))) + 1

  # Split into one matrix per site of [samples x alleles]
  ad <- stringr::str_split_fixed(ad, stringr::fixed(","), n = n_allele)
  ad <- utils::type.convert(ad, as.is = TRUE, na.strings = c("", "."))
  ad <- split.data.frame(ad, rep(seq_len(n_sites), each = n_samples))
  names(ad) <- NULL

  # Remove NA columns
  ad <- lapply(ad, function(site) site[, !apply(is.na(site), 2, all)])

  res <- lapply(ad, function(site) {
    if (ncol(site) == 2) {
      # Simple case if site is bi-allelic (bi_dp == dp)
      alt_idx <- 1
      bi_dp <- rowSums(site)
      bi_frac <- ifelse(bi_dp == 0, NA, 1)
    } else {
      # For multi-allelic sites, use the most abundant alt allele
      alt_idx <- which.max(colSums(site[, -1, drop = FALSE]))
      # Bi-allelic read depth
      bi_dp <- rowSums(site[, c(1, alt_idx + 1), drop = FALSE])
      # Fraction of total reads that support bi-allelic sites
      # 0 if there are only multi-allelic reads, NA if there are no reads
      dp <- rowSums(site)
      bi_frac <- bi_dp / ifelse(dp == 0, NA, dp)
    }

    # Alternative Allele Fraction (AAF)
    aaf <- site[, alt_idx + 1] / ifelse(bi_dp == 0, NA, bi_dp)
    aaf <- round(aaf, 3)

    list(aaf = aaf, bi_dp = bi_dp, bi_frac = bi_frac, alt_idx = alt_idx)
  })
  rm(ad)

  # Unpack and convert to matrix (last entry in each list is a vector not matrix)
  alt_idx <- unlist(lapply(res, utils::tail, n = 1), use.names = FALSE)
  col_names <- utils::head(names(res[[1]]), n = -1)
  res <- lapply(seq_along(col_names), function(i) {
    mat <- sapply(res, function(x) x[[i]], simplify = "array")
    rownames(mat) <- samples(vcf)
    varkit_geno(mat)
  })
  names(res) <- col_names

  # Indicate which alt allele was used
  res$alt_idx <- alt_idx

  res
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
