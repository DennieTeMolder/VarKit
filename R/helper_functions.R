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

.is_connection <- function(x) {
  if (!inherits(x, "connection"))
    return(FALSE)
  if (!is_open.connection(x))
    stop("The provided connection '", substitute(x), "' is not open!")
  return(TRUE)
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

.skip_lines <- function(con, n, block_size = 5e3L, progress = TRUE) {
  stopifnot(is_open(con))
  n <- as.integer(n)
  block_size <- as.integer(block_size)

  # Jump through the file in blocks
  times <- n %/% block_size
  mod <- n %% block_size

  if (times > 0L) {
    purrr::walk(seq_len(times), function(i) {
      .skip_lines_internal(con, n = block_size)
    }, .progress = progress)
  }
  if (mod > 0L)
    .skip_lines_internal(con, n = mod)

  invisible(con)
}

.skip_lines_internal <- function(con, n) {
  if (is_varkit_index(con)) {
    readLines(env(con)$con, n = n)
    env(con)$curr_line <- env(con)$curr_line + n
  } else {
    readLines(con, n = n)
  }
  invisible(NULL)
}

.ensure_pkg <- function(pkg, version = "0.0.0") {
  if (length(find.package(pkg, quiet = TRUE)) > 0) {
    if (utils::packageVersion(pkg) < version) {
      message("This function requires a newer version of the following package:")
      utils::update.packages(oldPkgs = pkg)
    }
    if (utils::packageVersion(pkg) >= version)
      return(TRUE)
  } else {
    message("This function requires the '", pkg, "' package.")
    response <- readline(paste0("Install it into ", .libPaths()[1L], "? [y/N] "))

    if (tolower(response) == "y") {
      utils::install.packages(pkg)
      return(TRUE)
    }
  }
  stop("Package '", pkg, "' (>= ", version, ") is required but not available!")
}

.ensure_igraph <- function() {
  .ensure_pkg("igraph")
}

## Unused functions ------
.is_repeated <- function(x) {
  if (is.character(x))
    x <- as.factor(x)
  if (is.factor(x))
    x <- as.integer(x)
  c(FALSE, diff(x) == 0)
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
