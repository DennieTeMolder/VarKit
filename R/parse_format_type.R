##' @export
parse_format_type <- function(x, type) {
  UseMethod("parse_format_type", x)
}

##' @export
parse_format_type.varkit <- function(x, type) {
  # Check te FORMAT field for index of type
  frmt <- get_format_types.varkit(x, check_consistent = TRUE)
  idx <- which(frmt == type)
  if (length(idx) != 1)
    stop("No matching value for `type` found in FORMAT!")

  # Parse type from genotype fields
  len <- length(frmt)
  res <- apply_each(geno(x), .str_split_fixed_idx, pattern = ":", n = len, idx = idx)

  # Convert NA values
  is_na <- stringr::str_detect(res , pattern ="^[.,|/]+$")
  res[is_na] <- NA

  # Convert to numeric if possible
  is_numeric <- stringr::str_detect(res , pattern = "^[0-9]+(.[0-9]+)?$")
  if (all(is_numeric, na.rm = TRUE))
    res <- apply_each(res, as.numeric)

  res
}

