##' @export
next_chrom <- function(x, ...) {
  UseMethod("next_chrom", x)
}

##' @export
next_chrom.varkit_index <- function(x, ...) {
  open(x)
  props <- .get_props(x)

  if (props$curr_line < x$linum[1]) {
    n_max <- x$n_lines[1]
    step_size <- x$linum[1] - props$curr_line + n_max
  } else {
    n_max <- step_size <- get_current(x)$n_lines
  }

  if (n_max < 1)
    return(NULL)

  # FIXME handle trying to read past EOF
  chrom <- read_vcf(props$con, header = props$header, n_max = n_max, ...)
  props$curr_line <- props$curr_line + step_size

  # Store header for further reading
  if (is.null(props$header))
    props$header <- new_varkit_header(meta = meta(chrom), col_names = col_names(chrom))

  chrom
}
