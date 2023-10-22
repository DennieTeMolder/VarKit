##' @export
varkit_index <- function(index, file, col_select = NULL, ...) {
  index <- tibble::as_tibble(index)
  file <- as.character(file)

  dots <- list(...)
  if (!rlang::is_named2(dots))
    stop("All additional arguments must be named!", call. = FALSE)

  col_select <- rlang::enexpr(col_select)
  if (!is.null(col_select))
    dots$col_select <- col_select

  new_varkit_index(index, file = file, con = NA, curr_line = 1, read_vcf_args = dots)
}

##' @export
new_varkit_index <- function(index = data.frame(id = character(), linum = integer(), n_lines = integer()), ...) {
  stopifnot(is.data.frame(index))
  structure(index,
    env = list2env(list(...), parent = rlang::empty_env()),
    class = unique(c("varkit_index", class(index)))
  )
}

##' @export
validate_varkit_index <- function(index) {
  if (!(is.data.frame(index) && all(c("id", "linum", "n_lines") %in% colnames(index))))
    stop("The index should be a data frame with columns 'id', 'linum', and 'n_lines'!")
  if (anyNA(index$linum) || anyNA(index$n_lines))
    stop("The 'linum' and/or 'n_lines' columns contain NA values!")
  if (min(index$linum, index$n_lines) < 1)
    stop("The index contains values smaller then 1!")
  if (is.unsorted(index$linum, strictly = TRUE))
    stop("The provided 'index' should be sorted on 'linum'!")
  if (!all(index$n_lines >= 1))
    stop("The index contains entries with zero lines!")
  if (any(diff(index$linum) != utils::head(index$n_lines, n = -1)))
    stop("The 'n_lines' column is not congruent with 'linum'!")
  if (is.null(env(index)$file))
    stop("The index env has no associated 'file'!")
  if (!file.exists(env(index)$file))
    stop("The 'file' associated to the index does not seem to exists!")
  index
}

`env<-` <- function(x, value) {
  attr(x, "env") <- value
  x
}

env <- function(x) {
  attr(x, "env")
}

##' @export
is_varkit_index <- function(x) {
  inherits(x, "varkit_index")
}

##' @export
print.varkit_index <- function(x, ...) {
  .msg_h1("VarKit Index Object")
  NextMethod("print", x)
  if (is_open(x)) {
    .msg_info("With a pointer at line ", env(x)$curr_line, " in ", env(x)$file)
  } else {
    .msg_info("For file ", env(x)$file)
  }
  invisible(x)
}

##' @export
open.varkit_index <- function(con, open = "r", blocking = TRUE, ...) {
  if (open != "r")
    stop("Indexes should be read-only (open = 'r')!")
  if (!blocking)
    stop("Indexes should be blocking (blocking = TRUE)!")

  if (!is_open.varkit_index(con)) {
    validate_varkit_index(con)
    env(con)$con <- file(env(con)$file, open = open, blocking = blocking, ...)
    env(con)$curr_line <- 1
  }
  invisible(con)
}

##' @export
close.varkit_index <- function(con, ...) {
  if (is_open.varkit_index(con)) {
    close(env(con)$con)
  }
  env(con)$con <- NA
  env(con)$curr_line <- 1
  invisible(con)
}

##' @export
seek.varkit_index <- function(con, where = NA, origin = "start", ...) {
  if (!is.na(where))
    stopifnot(is.character(where) || is.numeric(where))
  origin <- match.arg(origin, c("start", "current", "end"))

  if (!is_open.varkit_index(index))
    env(index)$curr_line <- 1

  # Handle non-numeric values of WHERE
  if (is.na(where)) {
    return(.current_index_position(con))
  } else if (is.character(where)) {
    where <- con$linum[match(where, con$id)]
    if (is.na(where))
      stop("The value of 'where' is not present in the index's 'id' column!")
  } else {
    if (origin == "current") {
      where <- where + env(con)$curr_line
    } else if (origin == "end") {
      last_row <- nrow(con)
      where <- con$linum[last_row] + con$n_lines[last_row] - where
    }
  }

  # Reload file if target line is before current line
  if (where < env(con)$curr_line) {
    close(con)
    env(con)$con <- file(env(con)$file, open = "r", blocking = TRUE)
  }

  # Move pointer if needed
  if (where > env(con)$curr_line)
    .skip_lines(con, n = where - env(con)$curr_line)

  invisible(con)
}

.current_index_position <- function(index) {
  i <- max(0, which(index$linum <= env(index)$curr_line))
  res <- as.data.frame(index[max(i, 1), ])
  rownames(res) <- i

  # Pointer before start of index
  if (i == 0) {
    res$n_lines <- res$linum - env(index)$curr_line
    for (idx in which(!names(res) %in% c("linum", "n_lines")))
      res[[idx]] <- NA
  } else {
    res$n_lines <- res$linum + res$n_lines - env(index)$curr_line
  }

  res$linum <- env(index)$curr_line
  res
}

.skip_lines <- function(index, n, block_size = 5e3) {
  # Jump through the file in blocks
  times <- n / block_size
  mod <- n %% block_size

  if (times >= 1) {
    cli::cli_progress_bar(total = n)
    for (i in seq_len(times)) {
      if (is.null(next_lines(index, n = 1, skip = block_size - 1)))
        return(invisible(index))
      cli::cli_progress_update(inc = block_size)
    }
  }
  if (mod >= 1) {
    next_lines(index, n = 1, skip = mod - 1)
  }

  invisible(index)
}
