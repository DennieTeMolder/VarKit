##' @export
next_lines <- function(x, n = NULL, skip = 0) {
  UseMethod("next_lines", x)
}

##' @export
next_lines.varkit_index <- function(x, n = NULL, skip = 0) {
  if (!is.null(n)) {
    stopifnot(is.numeric(n), length(n) == 1)
  } else {
    n <- seek.varkit_index(x)$n_lines
    if (n < 1)
      n <- utils::tail(x$n_lines, n = 1)
  }

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

  n <- length(lines)
  env(x)$curr_line <- env(x)$curr_line + n + skip

  if (n == 0)
    return(NULL)
  lines
}
