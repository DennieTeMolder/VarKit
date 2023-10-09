##' @export
varkit_index <- function(index, file, ...) {
  stopifnot(is.data.frame(index), nrow(index) > 0)
  stopifnot(c("id", "linum", "n_lines") %in% colnames(index))
  if (any(is.na(index$linum)) || any(is.na(index$n_lines)))
    stop("The 'linum' and/or 'n_lines' columns contain NA values!")
  if (is.unsorted(index$linum, strictly = TRUE))
    stop("The provided 'index' appears to be unsorted!")
  if (any(diff(index$linum) != utils::head(index$n_lines, n = -1)))
    stop("The 'n_lines' column id not congruent with 'linum'!")
  stopifnot(file.exists(file))

  index <- tibble::as_tibble(index)

  props <- new.env(parent = rlang::empty_env())
  if (!missing(...)) {
    args <- list(...)
    if (is.null(names(args)) || any(names(args) == ""))
      stop("All additional arguments should be named!")
    for (current in names(args))
      props[[current]] <- args[[current]]
  }
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
  .msg_h1("VarKit Index Object")
  .print_tbl(x, ...)
  if (is_open(x)) {
    .msg_info("With a pointer at line ", props$curr_line, " in ", props$file)
  } else {
    .msg_info("For file ", props$file)
  }
  invisible(x)
}

##' @export
is_open <- function(x) {
  UseMethod("is_open", x)
}

##' @export
is_open.connection <- function(x) {
  summ <- tryCatch(summary.connection(x), error = function(e) NULL)
  !is.null(summ)
}

##' @export
is_open.varkit_index <- function(x) {
  props <- .get_props(x)
  if (!exists("con", envir = props))
    return(FALSE)
  is_open(props$con)
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

  if (!is_open.varkit_index(x))
    props$curr_line <- 1

  i <- max(0, which(x$linum <= props$curr_line))
  res <- tibble::as_tibble(x[max(i, 1), ])

  # Pointer before start of index
  if (i == 0) {
    res$n_lines <- res$linum - props$curr_line
    for (idx in which(!names(res) %in% c("linum", "n_lines")))
      res[[idx]] <- NA
  } else {
    res$n_lines <- res$linum + res$n_lines - props$curr_line
  }

  res$linum <- props$curr_line
  res
}

##' @export
next_lines <- function(x, n = NULL, warn = FALSE, ...) {
  UseMethod("next_lines", x)
}

##' @export
next_lines.varkit_index <- function(x, n = NULL, warn = FALSE, ...) {
  if (!is.null(n)) {
    stopifnot(is.numeric(n), length(n) == 1, n > 0)
  } else {
    n <- get_current(x)$n_lines
    if (n < 1)
      n <- utils::tail(x$n_lines)
  }

  open(x)
  props <- .get_props(x)
  lines <- readLines(props$con, n = n, warn = FALSE, ...)

  n <- length(lines)
  if (n == 0)
    return(NULL)

  props$curr_line <- props$curr_line + n
  lines
}

##' @export
goto <- function(x, where = NULL) {
  UseMethod("goto", x)
}

##' @export
goto.varkit_index <- function(x, where = NULL) {
  if (!is.null(where))
    stopifnot(is.character(where) || is.numeric(where))

  # Handle non-numeric values of WHERE
  if (is.null(where)) {
    where <- x$linum[1]
  } else if (is.character(where)) {
    where <- x$linum[match(where, x$id)]
    if (is.na(where))
      stop("The value of 'where' is not present in the 'id' column!")
  }

  props <- .get_props(x)

  # Reload file if target line is before current line
  if (where < props$curr_line)
    open(close(x))

  # Move pointer if needed
  if (where > props$curr_line)
    .skip_lines(x, n = where - props$curr_line)

  invisible(x)
}

## Helpers ------
.get_props <- function(x) {
  attr(x, "props")
}

.skip_lines <- function(index, n, block_size = 5e3) {
  # Jump through the file in blocks
  times <- n / block_size
  mod <- n %% block_size

  if (times >= 1) {
    cli::cli_progress_bar(total = n)
    for (n_lines in by) {
      if (is.null(next_lines(index, n = n_lines)))
        return(invisible(index))
      cli::cli_progress_update(inc = n_lines)
    }
  }
  if (mod > 0) {
    next_lines(index, n = mod)
  }

  invisible(index)
}
