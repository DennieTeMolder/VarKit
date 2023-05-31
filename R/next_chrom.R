##' @export
next_chrom <- function(x, progress = FALSE) {
  UseMethod("next_chrom", x)
}

##' @export
next_chrom.varkit_index <- function(x, progress = FALSE) {
  props <- .get_props(x)
  if (!exists("header", envir = props) || !exists("spec", envir = props))
    stop("Index is missing 'header' and/or 'spec' properties!")

  if (is.na(get_current(x)$id))
    goto(x)

  chrom <- next_lines(x)
  if (is.null(chrom))
    return(NULL)

  read_vcf(I(chrom), header = props$header, spec = props$spec, progress = progress)
}
