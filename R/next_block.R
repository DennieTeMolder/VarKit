##' @export
next_block <- function(x, ...) {
  UseMethod("next_block", x)
}

##' @export
next_block.varkit_index <- function(x, ...) {
  open(x)

  if (env(x)$curr_line < x$linum[1]) {
    n_max <- x$n_lines[1]
    step_size <- x$linum[1] - env(x)$curr_line + n_max
  } else {
    n_max <- step_size <- seek.varkit_index(x)$n_lines
  }

  if (n_max < 1)
    return(NULL)

  # FIXME handle trying to read past EOF
  chrom <- read_vcf(env(x)$con, header = env(x)$header, n_max = n_max, ...)
  env(x)$curr_line <- env(x)$curr_line + step_size

  # Store header for further reading
  if (is.null(env(x)$header))
    env(x)$header <- new_varkit_header(meta = meta(chrom), col_names = col_names(chrom))

  chrom
}
