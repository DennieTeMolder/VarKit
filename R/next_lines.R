##' @export
next_lines <- function(x, n = NULL, skip = 0) {
  UseMethod("next_lines", x)
}

##' @export
next_lines.varkit_index <- function(x, n = NULL, skip = 0) {
  if (is.null(n)) {
    n <- .seek_next(x, skip_header = FALSE)$n_lines
  } else {
    stopifnot(is.numeric(n), length(n) == 1L, n >= 1)
  }
  stopifnot(length(skip) == 1L, is.numeric(skip))
  skip <- max(0, skip)

  open.varkit_index(x)

  lines <- scan(env(x)$con,
    what = character(),
    sep = "\n",
    nmax = n,
    skip = skip,
    na.strings = "",
    quote = "",
    strip.white = FALSE,
    blank.lines.skip = FALSE,
    quiet = TRUE
  )

  lines_read <- length(lines)
  env(x)$curr_line <- env(x)$curr_line + lines_read + skip

  if (lines_read == 0L)
    return(NULL)
  lines
}
