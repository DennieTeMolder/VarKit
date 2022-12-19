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
    # For fixed format loop over samples to save time while remaining memory efficient
    idx <- frmt[[1]]
    res <- apply_samples(geno(x), .str_split_get_idx, char = ":", idx = idx)
  } else {
    idxs <- unlist(frmt[fixed(x, cols = "format")], use.names = FALSE)
    gt <- geno(x)
    res <- sapply(seq_along(idxs), function(i) {
      .str_split_get_idx(gt[, i], char = ":", idx = idxs[i])
    })
    dimnames(res) <- dimnames(gt)
  }

  # Convert NA values
  is_na <- stringr::str_detect(res , pattern ="^[.,|/]*$")
  res[is_na] <- NA

  varkit_geno(utils::type.convert(res, as.is = TRUE, na.strings = ""))
}

.index_format <- function(x, type) {
  frmt <- fixed(x, cols = "format")
  if (any(is.na(frmt)))
    stop("The FORMAT column contains missing values!")

  uniq <- as.character(unique(frmt))
  splits <- stringr::str_split(uniq, ":")
  names(splits) <- uniq

  lapply(splits, function(y) match(type, y))
}

