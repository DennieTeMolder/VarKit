##' @export
varkit_index <- function(path, index = NULL) {
  if (is.null(index))
    index <- index_vcf(path)

  attr(index, "file") <- file(path, "r")
  class(index) <- c("varkit_index", class(index))

  goto(index, index$chrom[1])
  index
}

##' @export
print.varkit_index <- function(x, ...) {
  con <- attr(x, "file")
  cat("*VarKit Index Object*\n")
  print(tibble::as_tibble(x))
  cat("# Located at offset", seek(con), "in", summary(con)$description, "\n")
  invisible(x)
}

##' @export
goto <- function(x, ...) {
  UseMethod("goto", x)
}

##' @export
goto.varkit_index <- function(x, chrom = NULL) {
  if (is.null(chrom)) {
    target <- x$linum[1]
  } else {
    stopifnot(is.character(chrom))
    target <- x$linum[match(chrom, x$chrom)]
    if (is.na(target))
      stop("The value of 'chrom' is not part of the index!")
  }

  # NOTE this is better achieved using seek(where = ...) but this doesn't seem
  # to work well on windows.
  curr_line <- get_current(x)$linum
  if (is.na(curr_line) || target < curr_line) {
    x <- reset(x)
    curr_line <- 1
  }

  # Move pointer if needed
  if (target > curr_line)
    .skip_lines(x, target - curr_line)
  invisible(x)
}

##' @export
reset <- function(x, ...) {
  UseMethod("reset", x)
}

##' @export
reset.varkit_index <- function(x) {
  info <- summary(attr(x, "file"))
  close(attr(x, "file"))
  attr(x, "file") <- file(info$description, info$mode)
  invisible(x)
}

##' @export
get_current <- function(x) {
  UseMethod("get_current", x)
}

##' @export
get_current.varkit_index <- function(x) {
  pointer <- seek(attr(x, "file"))
  if (pointer == 0)
    return(tibble::tibble(chrom = NA, offset = 0, linum = 1, n_lines = x$linum[1] - 1))
  i <- match(pointer, x$offset)
  if (is.na(i))
    return(tibble::tibble(chrom = NA, offset = pointer, linum = NA, n_lines = NA))
  x[i, ]
}

##' @export
read <- function(x, ...) {
  UseMethod("read", x)
}

##' @export
read.varkit_index <- function(x, n = NULL, ...) {
  if (is.null(n)) {
    n <- get_current(x)$n_lines
    if (is.na(n))
      stop("File connection is out of sync with index. Run `goto()` to fix.")
  }
  readLines(attr(x, "file"), n = n, warn = FALSE, ...)
}

.skip_lines <- function(index, n) {
  for (i in seq_len(n)) read(index, n = 1)
  invisible(index)
}
