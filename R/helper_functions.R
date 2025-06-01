## Helper functions ------
.fixed_columns <- function() {
  c("chrom", "pos", "id", "ref", "alt", "qual", "filter", "info", "format")
}

# Dim functions that also work on vectors
.dim <- function(x) {
  res <- dim(x)
  if (!is.null(res))
    return(res)
  length(x)
}

.ndim <- function(x) {
  length(.dim(x))
}

# Simplified version of withr::defer()
.defer <- function(expr, envir = parent.frame(), after = FALSE) {
  thunk <- as.call(list(function() expr))
  do.call(base::on.exit, args = list(thunk, TRUE, after), envir = envir)
}

.stopifnotsingle <- function(x, what = NULL, allow_na = FALSE) {
  # Check class
  if (is.null(what)) {
    # Do nothing
  } else if (is.function(what)) {
    if (!isTRUE(what(x)))
      stop("`", substitute(what), "(", substitute(x), ")` is not TRUE!", call. = FALSE)
  } else if (!inherits(x, what = what)) {
    stop("`", substitute(x), "` should be one of what: ", .collapse(what), "!", call. = FALSE)
  }

  # Check length
  if (length(x) != 1L)
    stop("Argument '", substitute(x), "' should have a length of 1!", call. = FALSE)

  # Check NA
  if (!isTRUE(allow_na) && is.na(x))
    stop("`", substitute(x), "` cannot be NA!", call. = FALSE)

  invisible(x)
}

.is_connection <- function(x) {
  if (!inherits(x, "connection"))
    return(FALSE)
  if (!is_open.connection(x))
    stop("The provided connection '", substitute(x), "' is not open!")
  return(TRUE)
}

.round <- function(x, digits) {
  if (is.null(digits))
    return(x)

  if (is.array(x)) {
    if (is.numeric(x))
      x <- round(x, digits = digits)
  } else if (is.list(x)) {
    for (i in seq_along(x)) {
      if (is.numeric(x[[i]]))
        x[[i]] <- round(x[[i]], digits = digits)
    }
  } else {
    warning("Failed to apply 'digits' argument. Please round numeric values manually.", call. = FALSE)
  }

  x
}

.collapse <- function(x, delim = ", ") {
  paste(x, collapse = delim)
}

.quote_collapse <- function(x, quote = "'", ...) {
  x <- ifelse(is.na(x), "NA", paste0(quote, x, quote))
  .collapse(x, ...)
}

.str_split_i <- function(x, on, i) {
  pattern <- sprintf("(?:[^%s]+%s){%i}([^%s]+).*", on, on, i - 1L, on)
  stringr::str_replace(x, pattern, "\\1")
}

.frmt_big <- function(x, scientific = FALSE, ...) {
  big.mark <- if (getOption("OutDec", ".") == ".") "," else "."
  format(x, big.mark = big.mark, scientific = scientific, ...)
}

.curr_time <- function() {
  format(Sys.time(), "%a %H:%M")
}

.msg_vec <- function(x, before = NULL, after = NULL, sep = ", ", last = sep, n_max = 100L) {
  x <- cli::cli_vec(x, style = list(`vec-sep` = sep, `vec-last` = last, `vec-trunc` = n_max))
  cli::cli_text(c(before, "{x}", after))
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

.msg_step <- function(..., .envir = parent.frame()) {
  msg <- paste0(...)
  msg_time <- paste0(msg, " <", .curr_time(), ">")
  cli::cli_progress_step(msg_time, msg_done = msg, .envir = .envir)
}

.max_print_col <- function(mat, width = getOption("width")) {
  stopifnot(is.matrix(mat))

  .max_nchar <- function(x) {
    max(nchar(format(x, trim = TRUE)))
  }

  # Reduce size of matrix to the maximum possible number of columns
  n_max <- max(1L, as.integer(floor(width / 2L)) - 1L)
  if (ncol(mat) > n_max)
    mat <- mat[, seq_len(n_max), drop = FALSE]

  if (is.null(rownames(mat))) {
    rname_len <- nchar(nrow(mat)) + 3L
  } else {
    rname_len <- .max_nchar(rownames(mat))
  }

  if (is.null(colnames(mat))) {
    # We add 3 for the [,] and 1 for the space before the column
    cname_len <- nchar(seq_len(ncol(mat))) + 4L
  } else {
    # We add 2 here for the potential `` around var names + 1 for the space
    cname_len <- nchar(colnames(mat)) + 3L
  }

  col_len <- apply(mat, 2L, .max_nchar) + 1L
  if (is.character(mat))
    col_len <- col_len + 2L

  # Check if col name or col value is widest, add row name width to first element
  col_widths <- pmax.int(cname_len, col_len)
  col_widths[1L] <- col_widths[1L] + rname_len

  # Return the index of the last column that fits within width
  which_fit <- which(cumsum(col_widths) < width)
  if (length(which_fit) == 0L)
      return(1L)
  max(which_fit)
}

# X should be ordered from large to small (computationally)
.split_load_balance <- function(x, cores, each = 4L) {
  stopifnot(is.vector(x))
  cores <- as.integer(cores)
  each <- as.integer(each)

  len <- length(x)
  stopifnot(isTRUE(len >= cores))

  n <- min(cores * each, len)
  size <- floor(len / n)
  group <- c(
    rep(seq_len(n), times = size),
    rep(n + 1L, len %% n)
  )

  unname(split.default(x, f = group))
}

.ensure_igraph <- function(...) {
  rlang::check_installed("igraph", ...)
}

## Unused functions ------
.is_repeated <- function(x) {
  if (is.character(x))
    x <- as.factor(x)
  if (is.factor(x))
    x <- as.integer(x)
  c(FALSE, diff(x) == 0L)
}

.middle <- function(x, on_even = "left") {
  stopifnot(is.vector(x))
  len <- length(x)
  if (on_even == "right") {
    len <- len + 1L
  } else if (on_even != "left") {
    stop("Option on_even = '", on_even, "' not recognised!")
  }
  x[ceiling(len / 2L)]
}

.bi_gt2dosage <- function(mat) {
  stopifnot(is.matrix(mat))
  stopifnot(is.character(mat))
  dims <- dim(mat)
  res <- matrix(NA, dims[1L], dims[2L], dimnames = dimnames(mat))
  for (j in 1:dims[2L]) {
    alleles <- stringr::str_split(mat[, j], "[/|]")
    for(i in 1:dims[1L]) {
      if (all(alleles[[i]] %in% c("0", "1")))
        res[i,j] <- sum(alleles[[i]] == "1")
    }
  }
  class(res) <- class(mat)
  res
}
