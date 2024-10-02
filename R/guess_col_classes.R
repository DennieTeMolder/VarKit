##' @export
guess_col_classes <- function(file,
                              nmax = 1000,
                              simplify = TRUE,
                              sep = "\t",
                              col.names = NULL,
                              header = is.null(col.names),
                              na.strings = "NA",
                              skip = 0,
                              fill = !blank.lines.skip,
                              strip.white = FALSE,
                              blank.lines.skip = TRUE,
                              comment.char = "#",
                              allowEscapes = FALSE,
                              flush = FALSE,
                              encoding = "unknown",
                              skipNul = FALSE,
                              ...) {
  stopifnot(is.numeric(nmax), length(nmax) == 1)
  nmax <- max(2, nmax)

  if (.is_connection(file)) {
    lines <- scan(file, character(), sep = "\n", nmax = nmax, quote = "",
                  skip = skip, na.strings = na.strings, quiet = TRUE, fill = fill,
                  strip.white = strip.white, blank.lines.skip = blank.lines.skip,
                  multi.line = FALSE, comment.char = comment.char,
                  allowEscapes = allowEscapes, flush = flush, encoding = encoding,
                  skipNul = skipNul)
    pushBack(lines, connection = file)

    con <- textConnection(lines, encoding = "UTF-8")
    .defer(close(con))
  } else {
    stopifnot(file.exists(file))
    con <- file(file, "r")
    .defer(close(con))

    if (skip > 0L) .skip_lines(con, n = skip, progress = FALSE)
  }

  data <- utils::read.table(
    file = con,
    nrows = nmax,
    sep = sep,
    header = header,
    na.strings = na.strings,
    skip = 0,
    fill = fill,
    strip.white = strip.white,
    blank.lines.skip = blank.lines.skip,
    comment.char = comment.char,
    allowEscapes = allowEscapes,
    flush = flush,
    encoding = encoding,
    skipNul = skipNul,
    ...
  )
  col_classes <- lapply(data, class)

  if (!is.null(col.names))
    names(col_classes) <- col.names

  if (!simplify)
    return(col_classes)
  sapply(col_classes, `[`, 1L)
}
