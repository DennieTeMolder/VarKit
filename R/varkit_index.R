##' @export
varkit_index <- function(index, file) {
  stopifnot(is.data.frame(index), nrow(index) > 0)
  stopifnot(c("id", "linum") %in% colnames(index))
  if (is.unsorted(index$linum))
    stop("The provided 'index' appears to be unsorted!")
  if (utils::tail(index$id, n = 1) != "EOF")
    stop("The End Of File (EOF) is not in the index!")
  stopifnot(file.exists(file))

  index <- tibble::as_tibble(index)
  index$n_lines <- c(diff(index$linum), 0)

  props <- new.env(parent = rlang::empty_env())
  props$file <- file
  props$curr_line <- 1
  attr(index, "props") <- props
  class(index) <- c("varkit_index", class(index))
  index
}

##' @export
is_varkit_index <- function(x) {
  inherits(x, "varkit_index")
}

##' @export
print.varkit_index <- function(x, ...) {
  props <- .get_props(x)
  cat("*VarKit Index Object*\n")
  .print_tbl(x, ...)
  if (is_open(x)) {
    cat("# With a pointer at line", props$curr_line, "in", props$file, "\n")
  } else {
    cat("# For file", props$file, "\n")
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
  props <- .get_props(x)
  if (!exists("con", envir = props))
    return(FALSE)
  is_open.connection(props$con)
}

##' @export
open.varkit_index <- function(con, open = "r", ...) {
  if (!is_open(con)) {
    props <- .get_props(con)
    props$con <- file(props$file, open = open, ...)
    props$curr_line <- 1
  }
  invisible(con)
}

##' @export
close.varkit_index <- function(con, ...) {
  if (is_open(con)) {
    props <- .get_props(con)
    close(props$con)
    rm("con", envir = props)
    props$curr_line <- 1
  }
  invisible(con)
}

##' @export
get_current <- function(x) {
  UseMethod("get_current", x)
}

##' @export
get_current.varkit_index <- function(x) {
  props <- .get_props(x)
  if (!is_open(x))
    props$curr_line <- 1

  i <- max(0, which(x$linum <= props$curr_line))
  res <- tibble::as_tibble(x[max(i, 1), ])
  res$linum <- props$curr_line

  # Pointer before start of index
  if (i == 0)
    res$id <- NA

  i_next <- min(nrow(x), i + 1)
  res$n_lines <- x$linum[i_next] - res$linum
  res
}

##' @export
next_lines <- function(x, ...) {
  UseMethod("next_lines", x)
}

##' @export
next_lines.varkit_index <- function(x, n = NULL, ...) {
  if (!is.null(n))
    stopifnot(is.numeric(n), length(n) == 1, n > 0)

  curr <- get_current(x)
  if (curr$n_lines < 1) {
    close(x)
    return(NULL)
  }

  if (is.null(n))
    n <- curr$n_lines
  n <- min(n, utils::tail(x$linum, n = 1) - curr$linum)

  open(x)
  props <- .get_props(x)
  props$curr_line <- props$curr_line + n
  readLines(props$con, n = n, warn = FALSE)
}

##' @export
goto <- function(x, where = NULL) {
  UseMethod("goto", x)
}

##' @export
goto.varkit_index <- function(x, where = NULL) {
  if (!is.null(where)) {
    stopifnot(is.character(where) || is.numeric(where))
  } else {
    where <- x$linum[1]
  }

  if (is.character(where)) {
    where <- x$linum[match(where, x$id)]
    if (is.na(where))
      stop("The value of 'where' is not present in the 'id' column!")
  }

  if (where > utils::tail(x$linum, n = 1))
    stop("The value of 'where' is larger then the index!")

  curr_line <- get_current(x)$linum

  # Reload file if target line is before current line
  if (where < curr_line) {
    open(close(x))
    curr_line <- 1
  }

  # Move pointer if needed
  if (where > curr_line)
    .skip_lines(x, where - curr_line)
  invisible(x)
}

## Helpers ------
.get_props <- function(x) {
  attr(x, "props")
}

.skip_lines <- function(index, n, block_size = 5e3) {
  # Jump trought the file in blocks
  by <- rep(block_size, n / block_size)
  mod <- n %% block_size
  if (mod > 0)
    by <- c(by, mod)

  for (n_lines in by) next_lines(index, n = n_lines)
  invisible(index)
}
