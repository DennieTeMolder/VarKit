##' @export
varkit_index <- function(index, file, col_select = NULL, ...) {
  index <- tibble::as_tibble(index)
  file <- as.character(file)

  dots <- list(...)
  if (!rlang::is_named2(dots))
    stop("All additional arguments must be named!", call. = FALSE)

  col_select <- rlang::enquo(col_select)
  if (!rlang::quo_is_null(col_select))
    dots$col_select <- col_select

  # Skip validation as its performed by open.varkit_index() later
  new_varkit_index(
    index = index,
    file = file,
    con = NA,
    curr_line = 1L,
    read_vcf_args = dots
  )
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
  if (any(index$linum < 1L))
    stop("The 'linum' column contains values smaller then 1!")
  if (any(index$n_lines < 0L))
    stop("The 'n_lines' column contains negative values!")
  if (is.unsorted(index$linum))
    stop("The provided 'index' should be sorted on 'linum'!")
  if (any(diff(index$linum) < utils::head(index$n_lines, n = -1)))
    stop("The 'n_lines' column is larger then the difference in 'linum'!")
  if (is.null(.env(index)$file))
    stop("The index env has no associated 'file'!")
  if (!file.exists(.env(index)$file))
    stop("The 'file' associated to the index does not seem to exists!")
  index
}

`.env<-` <- function(x, value) {
  attr(x, "env") <- value
  x
}

.env <- function(x) {
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
  if (is_open.varkit_index(x)) {
    .msg_info("With a pointer at line ", .env(x)$curr_line, " in ", .env(x)$file)
  } else {
    .msg_info("For file ", .env(x)$file)
  }
  invisible(x)
}

##' @export
open.varkit_index <- function(con, open = "r", blocking = TRUE, ...) {
  if (!(identical(open, "r") || identical(open, "rt")))
    stop("Indexes should be read-only (open = 'r')!")
  if (!isTRUE(blocking))
    stop("Indexes should be blocking (blocking = TRUE)!")

  if (!is_open.varkit_index(con)) {
    validate_varkit_index(con)
    .env(con)$con <- file(.env(con)$file, open = open, blocking = blocking, ...)
    .env(con)$curr_line <- 1L
  }
  invisible(con)
}

##' @export
close.varkit_index <- function(con, ...) {
  if (is_open.varkit_index(con)) {
    close(.env(con)$con)
  }
  .env(con)$con <- NA
  .env(con)$curr_line <- 1L
  invisible(con)
}

##' @export
seek.varkit_index <- function(con, where = NA, origin = "start", ..., max_header_lines = 1e6) {
  if (!is.na(where))
    stopifnot(length(where) == 1L, is.character(where) || is.numeric(where))
  origin <- match.arg(origin, c("start", "current", "end"))

  # Make sure the current line is not out of sync
  if (!is_open.varkit_index(con))
    .env(con)$curr_line <- 1L

  # Handle non-numeric values of WHERE
  if (is.na(where)) {
    return(.current_index_position(con))
  } else if (is.character(where)) {
    where <- con$linum[match(where, con$id)]
    if (is.na(where))
      stop("The value of 'where' is not present in the index's 'id' column!")
  } else if (is.numeric(where)) {
    if (origin == "current") {
      where <- where + .env(con)$curr_line
    } else if (origin == "end") {
      where <- with(utils::tail(con, n = 1L), linum + n_lines) - where
    }
  } else {
    stop("Unexpected!")
  }

  # Close file if target line is before current line
  if (where < .env(con)$curr_line)
    close.varkit_index(con)
  open.varkit_index(con)

  # Deal with the VCF header, storing it if not provided by the user
  if (.seek_passes_header(con, where)) {
    if (is.null(read_vcf_args(con)$header)) {
      header <- read_vcf_header(.env(con)$con, max_lines = max_header_lines)

      # Adjust the current line number for the number of header lines read
      .env(con)$curr_line <- .env(con)$curr_line + nrow(meta.varkit_header(header)) + 1L

      if (.env(con)$curr_line != con$linum[1L]) {
        stop("The header size differs from the index! Expected: ", .frmt_big(con$linum[1L]),
            ", but got: ", .frmt_big(.env(con)$curr_line))
      }

      read_vcf_args(con)$header <- header
    } else {
      # Skip past the header
      skip_lines(con, n = con$linum[1L] - .env(con)$curr_line)
    }
  }

  # When outside of the header we should count data lines only for parity with index_vcf()
  if (where > con$linum[1L]) {
    skip_lines(
      con,
      n = where - .env(con)$curr_line,
      blank.lines.skip = TRUE,
      comment.char = "#"
    )
  } else {
    skip_lines(con, n = where - .env(con)$curr_line)
  }

  invisible(con)
}

.current_index_position <- function(index) {
  i <- max(0L, which(index$linum <= .env(index)$curr_line))
  res <- as.data.frame(index[max(i, 1L), , drop = FALSE])
  rownames(res) <- i

  # Check if pointer is outside current entry
  res_diff <- .env(index)$curr_line - res$linum
  if (res_diff < 0L || res_diff >= res$n_lines) {
    for (idx in seq_along(res)) res[[idx]] <- NA
    if (i == nrow(index)) {
      res$n_lines <- index$n_lines[i] - res_diff
    } else {
      res$n_lines <- index$linum[i + 1L] - .env(index)$curr_line
    }
  } else {
    res$n_lines <- res$linum + res$n_lines - .env(index)$curr_line
  }

  res$linum <- .env(index)$curr_line
  res
}

.seek_passes_header <- function(con, where) {
  .env(con)$curr_line < con$linum[1L] &&  where >= con$linum[1L]
}
