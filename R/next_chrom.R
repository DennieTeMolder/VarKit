##' @export
next_chrom <- function(x, ...) {
  UseMethod("next_chrom", x)
}

##' @export
next_chrom.varkit_index <- function(x, ...) {
  props <- .get_props(x)
  if (!exists("header", envir = props) || !exists("spec", envir = props))
    stop("Index is missing 'header' and/or 'spec' properties!")

  if (props$curr_line < x$linum[1])
    goto(x)

  chrom <- next_lines(x)
  if (is.null(chrom))
    return(NULL)

  read_vcf(file = I(chrom), header = props$header, spec = props$spec, ...)
}
