##' @export
next_lines <- function(x, n_max = NULL) {
  UseMethod("next_lines", x)
}

##' @export
next_lines.varkit_index <- function(x, n_max = NULL) {
  # Open index to ensure it is valid
  open.varkit_index(x)
  next_idx <- .seek_next(x, skip_header = TRUE)

  if (is.null(n_max))
    n_max <- read_vcf_args(x)$n_max

  if (!is.null(n_max) && !(is.numeric(n_max) && length(n_max) == 1L))
    stop("Please provide a single numeric value for the 'n_max' argument!")

  # Ignore negative N_MAX
  if (!isTRUE(n_max >= 1L))
    n_max <- NULL

  # Stay below N_MAX whilst never reading more than one chunk
  n_max <- min(as.integer(n_max), next_idx$n_lines, na.rm = TRUE)

  lines <- scan(.env(x)$con,
    what = character(),
    sep = "\n",
    nlines = n_max,
    na.strings = "",
    quote = "",
    strip.white = FALSE,
    blank.lines.skip = TRUE,
    quiet = TRUE,
    comment.char = "#"
  )

  n <- length(lines)

  # Check if we got the expected number of sites as long as we are within the index's range
  if (n != n_max && .env(x)$curr_line <= x$linum[nrow(x)]) {
    warning("At index row ", rownames(seek(x)), ": expected ", .frmt_big(n_max),
            " sites, but got ", .frmt_big(n), " sites.")
    n <- n_max
  }

  # Increment index
  .env(x)$curr_line <- .env(x)$curr_line + n

  if (n == 0L)
    return(NULL)
  lines
}
