## Helper functions ------
.fixed_columns <- function() {
  c("chrom", "pos", "id", "ref", "alt", "qual", "filter", "info", "format")
}

.scan_fields <- function(con, sep = "\t", nlines = 0, skip = 0, na.strings = "") {
  scan(con,
    sep = sep,
    nlines = nlines,
    skip = skip,
    na.strings = na.strings,
    what = character(),
    quote = "",
    strip.white = TRUE,
    blank.lines.skip = FALSE,
    quiet = TRUE
  )
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
  if (na_rm) {
    not_na <- !is.na(x)
    if (sum(not_na) < 2)
      return(TRUE)
    x <- x[not_na]
    return(all(x == x[1]))
  }
  if (length(x) < 2)
    return(TRUE)
  all(duplicated(x)[-1])
}

.is_repeated <- function(x) {
  if (is.character(x))
    x <- as.factor(x)
  if (is.factor(x))
    x <- as.integer(x)
  c(FALSE, diff(x) == 0)
}

.mutate_all <- function(.tbl, .fns, ...) {
  dplyr::mutate(.tbl, dplyr::across(dplyr::everything(), .fns, ...))
}

.frmt_big <- function(x, scientific = FALSE, ...) {
  big.mark <- if (getOption("OutDec", ".") == ".") "," else "."
  format(x, big.mark = big.mark, scientific = scientific, ...)
}

.msg_info <- function(...) {
  cat(paste0("# ", cli::symbol$info, " ", ..., "\n"))
}

.msg_h1 <- function(...) {
  print(cli::rule(left = paste0(...), line = 2))
}

.msg_h2 <- function(...) {
  print(cli::rule(left = paste0(...), line = 1))
}

.print_tbl <- function(x, ...) {
  if (FALSE) pillar::pillar() # Silence imports checker
  do.call(utils::getS3method("print", "tbl", envir = getNamespace("pillar")), c(list(x), ...))
}

# Use with on.exit(restore_fn())
.set_cores <- function(cores) {
  stopifnot(cores > 1)
  old_blas_threads <- RhpcBLASctl::blas_get_num_procs()
  RhpcBLASctl::blas_set_num_threads(1)
  old_plan <- future::plan(future::multisession, workers = cores)

  restore_fn <- function() {
    RhpcBLASctl::blas_set_num_threads(old_blas_threads)
    future::plan(old_plan)
  }
  restore_fn
}

.max_print_col <- function(mat, width = getOption("width")) {
  stopifnot(is.matrix(mat))

  .max_nchar <- function(x) {
    max(nchar(format(x, trim = TRUE)))
  }

  # Reduce size of matrix to a reasonable size
  n_max <- max(1, floor(width / 2) - 1)
  if (ncol(mat) > n_max)
    mat <- mat[, seq_len(n_max), drop = FALSE]

  if (is.null(rownames(mat))) {
    rname_len <- nchar(nrow(mat)) + 3
  } else {
    rname_len <- .max_nchar(rownames(mat))
  }

  if (is.null(colnames(mat))) {
    # We add 3 for the [,] and for the space before the column
    cname_len <- nchar(seq_len(ncol(mat))) + 4
  } else {
    # We add 2 here for the potential `` around var names
    cname_len <- nchar(colnames(mat)) + 3
  }

  col_len <- apply(mat, 2, .max_nchar) + 1
  if (is.character(mat))
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

  if (is.null(type)) {
    ad <- geno(vcf)
  } else {
    # Extract allelic depth (AD)
    ad <- parse_geno(vcf, type)
  }

  if (storage.mode(ad) != "character")
    stop(if (is.null(type)) "geno(vcf)" else "parse_geno(vcf, type)", " should return a character matrix!")

  n_sites <- ncol(ad)
  n_samples <- nrow(ad)
  n_allele <- max(stringr::str_count(ad[1, ], stringr::fixed(","))) + 1

  if (n_allele < 2)
    stop("Could not detect multiple allelic depths delimited by a ','!")

  # Variables for storing results
  aaf <- matrix(NA_real_, nrow = n_samples, ncol = n_sites, dimnames = dimnames(ad))
  bi_dp <- matrix(NA_integer_, nrow = n_samples, ncol = n_sites, dimnames = dimnames(ad))
  bi_frac <- matrix(NA_real_, nrow = n_samples, ncol = n_sites, dimnames = dimnames(ad))
  alt_idx <- vector("integer", n_sites)

  # Split into one matrix per site of [samples x alleles]
  ad <- stringr::str_split_fixed(ad, stringr::fixed(","), n = n_allele)
  storage.mode(ad) <- "integer"
  ad <- split.data.frame(ad, rep(seq_len(n_sites), each = n_samples))
  names(ad) <- NULL

  for (i in seq_along(ad)) {
    current <- ad[[i]]
    is_na <- is.na(current[1, ])
    if (any(is_na))
      current <- current[, !is_na, drop = FALSE]

    if (ncol(current) == 2) {
      # Simple case if current is bi-allelic (bi_dp == dp)
      current_idx <- 2
      current_dp <- current[, 1] + current[, current_idx]
      current_frac <- ifelse(current_dp > 0, 1, NaN)
    } else {
      # For multi-allelic sites, use the most abundant alt allele
      current_idx <- which.max(colSums(current[, -1, drop = FALSE])) + 1
      # Bi-allelic read depth
      current_dp <- current[, 1] + current[, current_idx]
      # Fraction of total reads that support bi-allelic sites
      # 0 if there are only multi-allelic reads, NA if there are no reads
      current_frac <- current_dp / rowSums(current)
    }

    # Alternative Allele Fraction (AAF)
    aaf[, i] <- current[, current_idx] / current_dp
    bi_dp[, i] <- current_dp
    bi_frac[, i] <- current_frac
    alt_idx[i] <- current_idx - 1
  }

  list(aaf = varkit_geno(aaf), bi_dp = varkit_geno(bi_dp), bi_frac = varkit_geno(bi_frac), alt_idx = alt_idx)
}
