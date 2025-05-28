##' @export
next_lines <- function(x, n = NULL) {
  UseMethod("next_lines", x)
}

##' @export
next_lines.varkit_index <- function(x, n = NULL) {
  if (is.null(n)) {
    n <- .seek_next(x, skip_header = TRUE)$n_lines
  } else {
    stopifnot(is.numeric(n), length(n) == 1L, n >= 1)
  }

  open.varkit_index(x)

  lines <- scan(env(x)$con,
    what = character(),
    sep = "\n",
    nlines = n,
    na.strings = "",
    quote = "",
    strip.white = FALSE,
    blank.lines.skip = TRUE,
    quiet = TRUE,
    comment.char = "#"
  )

  # Increment index
  env(x)$curr_line <- env(x)$curr_line + length(lines)

  if (length(lines) == 0L)
    return(NULL)
  lines
}
