# Skip lines of CON in blocks with optional progress updates.
# Increment CURRENT_LINE of CON if it is an index.
##' @export
skip_lines <- function(con,
                       n,
                       quote = "",
                       blank.lines.skip = FALSE,
                       comment.char = "",
                       block_size = 5e3L,
                       progress = TRUE) {
  stopifnot(inherits(con, c("connection", "varkit_index")))
  if (!is_open(con)) open(con)
  n <- max(0L, as.integer(n), na.rm = TRUE)
  if (is.null(quote)) quote <- ""
  if (is.null(comment.char)) comment.char <- ""
  block_size <- as.integer(block_size)

  # Named progress bar
  if (isTRUE(progress)) {
    if (is_varkit_index(con)) {
      progress <- "Moving index pointer:"
    } else {
      progress <- "Skipping lines:"
    }
  }

  # Variables used by .skip_lines_internal()
  real_lines <- identical(comment.char, "") && identical(quote, "") && isFALSE(blank.lines.skip)
  index_p <- is_varkit_index(con)

  .skip_lines_internal <- function(con, n) {
    if (real_lines) {
      readLines(if (index_p) env(con)$con else con, n = n)
    } else {
      # This is about 30% slower than readLines()
      scan(
        if (index_p) env(con)$con else con,
        what = list(NULL),
        sep = "\n",
        nlines = n,
        na.strings = "",
        quote = quote,
        strip.white = FALSE,
        blank.lines.skip = blank.lines.skip,
        quiet = TRUE,
        comment.char = comment.char
      )
    }

    if (index_p)
      env(con)$curr_line <- env(con)$curr_line + n

    invisible(NULL)
  }

  # Jump through the file in blocks
  times <- n %/% block_size
  mod <- n %% block_size

  if (times > 0L) {
    purrr::walk(seq_len(times), function(i) {
      .skip_lines_internal(con, n = block_size)
    }, .progress = progress)
  }
  if (mod > 0L)
    .skip_lines_internal(con, n = mod)

  invisible(con)
}
