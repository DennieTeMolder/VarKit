##' @export
varkit_index <- function(index, file) {
  stopifnot(is.data.frame(index), nrow(index) > 0)
  stopifnot(c("chrom", "linum", "offset") %in% colnames(index))
  if (is.unsorted(index$offset) || is.unsorted(index$linum))
    stop("The provided 'index' appears to be unsorted!")
  if (utils::tail(index$chrom, n = 1) != "EOF")
    stop("The End Of File (EOF) is not in the index!")

  index <- tibble::as_tibble(index)
  index$n_lines <- c(diff(index$linum), NA)

  attr(index, "file") <- file
  class(index) <- c("varkit_index", class(index))
  index
}

##' @export
is_varkit_index <- function(x) {
  inherits(x, "varkit_index")
}

##' @export
print.varkit_index <- function(x, ...) {
  cat("*VarKit Index Object*\n")
  print(tibble::as_tibble(x))
  if (is_open(x)) {
    con <- attr(x, "con")
    cat("# With a pointer at offset", seek(con), "in", summary(con)$description, "\n")
  } else {
    cat("# For file", attr(x, "file"), "\n")
  }
  invisible(x)
}

##' @export
is_open <- function(x) {
  UseMethod("is_open", x)
}

##' @export
is_open.connection <- function(x) {
  summ <- tryCatch(summary.connection(x), error = function(e) {})
  !is.null(summ)
}

##' @export
is_open.varkit_index <- function(x) {
  is_open.connection(attr(x, "con"))
}

##' @export
open.varkit_index <- function(con, open = "r", ...) {
  if (is_open(con))
    return(con)
  attr(con, "con") <- file(attr(con, "file"), open = open, ...)
  con
}

##' @export
close.varkit_index <- function(con, ...) {
  if (is_open(con))
    close(attr(con, "con"))
  invisible(con)
}

##' @export
get_current <- function(x) {
  UseMethod("get_current", x)
}

##' @export
get_current.varkit_index <- function(x) {
  if (is_open(x)) {
    pointer <- seek(attr(x, "con"))
  } else {
    pointer <- 0
  }

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
  if (!is_open(x))
    stop("Index has no open connection. Run `x <- open(x)` to fix.")

  curr <- get_current(x)
  if (isTRUE(curr$chrom == "EOF")) {
    close(x)
    return(NULL)
  }

  if (is.null(n)) {
    n <- curr$n_lines
    if (is.na(n))
      stop("File connection is out of sync with index. Run `goto(x)` to fix.")
  }

  readLines(attr(x, "con"), n = n, warn = FALSE)
}

##' @export
goto <- function(x, chrom = NULL) {
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
    x <- open(close(x))
    curr_line <- 1
  }

  # Move pointer if needed
  if (target > curr_line)
    .skip_lines(x, target - curr_line)
  invisible(x)
}

.skip_lines <- function(index, n) {
  for (i in seq_len(n)) read(index, n = 1)
  invisible(index)
}
