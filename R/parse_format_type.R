##' @export
parse_format_type <- function(x, type) {
  UseMethod("parse_format_type", x)
}

##' @export
parse_format_type.varkit <- function(x, type) {
  frmt <- get_format_types.varkit(x, check_consistent = TRUE)
  idx <- which(frmt == type)
  if (length(idx) != 1)
    stop("No matching value for `type` found in FORMAT!")
  len <- length(frmt)
  apply_sites(geno(x), .str_split_fixed_idx, pattern = ":", n = len, idx = idx)
}
