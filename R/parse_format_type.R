##' @export
parse_format_type <- function(x, type, allow_missing = FALSE) {
  UseMethod("parse_format_type", x)
}

##' @export
parse_format_type.varkit <- function(x, type, allow_missing = FALSE) {
  stopifnot(is.character(type) && length(type) == 1)

  # Check te FORMAT field for index of type
  frmt <- .index_format(x, type)
  if (!allow_missing && any(is.na(frmt)))
    stop("`type` not found in all FORMAT fields!")

  # Parse type from genotype fields
  if (length(unique(frmt)) == 1) {
    idx <- frmt[[1]]
    res <- apply_each(geno(x), .str_split_get_idx, char = ":", idx = idx)
  } else {
    gt <- geno(x)
    idxs <- frmt[fixed(x)$format]
    res <- sapply(seq_along(idxs), function(i) {
      .str_split_get_idx(gt[, i], char = ":", idx = idxs[[i]])
    })
    dimnames(res) <- dimnames(gt)
    res <- varkit_geno(res)
  }

  # Convert NA values
  is_na <- stringr::str_detect(res , pattern ="^[.,|/]+$")
  res[is_na] <- NA

  # Convert to numeric if possible
  is_numeric <- stringr::str_detect(res , pattern = "^[0-9]+(.[0-9]+)?$")
  if (all(is_numeric, na.rm = TRUE))
    res <- apply_each(res, as.numeric)

  res
}

.index_format <- function(x, type) {
  frmt <- fixed(x)$format
  if (any(is.na(frmt)))
    stop("The FORMAT column contains missing values!")

  uniq <- levels(frmt)
  # Some values might be dropped when masking, so we recompute the unique here
  if (length(uniq) != 1)
    uniq <- as.character(unique(frmt))

  splits <- stringr::str_split(uniq, ":")
  names(splits) <- uniq

  lapply(splits, function(y) match(type, y))
}

