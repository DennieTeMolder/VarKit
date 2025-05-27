##' @export
next_lines <- function(x, n = NULL, skip = 0) {
  UseMethod("next_lines", x)
}

##' @export
next_lines.varkit_index <- function(x, n = NULL, skip = 0) {
  if (is.null(n)) {
    n <- .seek_next(x, skip_header = TRUE)$n_lines
  } else {
    stopifnot(is.numeric(n), length(n) == 1L, n >= 1)
  }
  stopifnot(length(skip) == 1L, is.numeric(skip))
  skip <- as.integer(max(0, skip))

  open.varkit_index(x)

  lines <- scan(env(x)$con,
    what = character(),
    sep = "\n",
    skip = skip,
    nlines = n,
    na.strings = "",
    quote = "",
    strip.white = FALSE,
    blank.lines.skip = TRUE,
    quiet = TRUE,
    # FIXME this will skip the header
    comment.char = "#"
  )

  # Increment index
  env(x)$curr_line <- env(x)$curr_line + length(lines) + skip

  if (length(lines) == 0L)
    return(NULL)
  lines
}
