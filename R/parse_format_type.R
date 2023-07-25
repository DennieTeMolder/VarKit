##' @export
parse_format_type <- function(x, type = NULL, allow_missing = FALSE) {
  UseMethod("parse_format_type", x)
}

##' @export
parse_format_type.varkit <- function(x, type = NULL, allow_missing = FALSE) {
  if (!is.null(type)) {
    .stopifnotsingle(type, class = "character")
  } else {
    message("Please provide the 'type' argument, returning possible options.")
    return(get_format_types(x))
  }

  # Check te FORMAT field for index of type
  frmt <- .index_format(x, type)
  if (!allow_missing && any(is.na(frmt)))
    stop("`type` not found in all FORMAT fields!")

  # Parse type from genotype fields
  if (all(frmt == frmt[[1]])) {
    # For fixed format loop over samples to save time while remaining memory efficient
    idx <- frmt[[1]]
    res <- as.matrix(apply_samples(geno(x), .str_split_i, on = ":", i = idx))
  } else {
    idxs <- unname(frmt[site(x, cols = "format")])
    gt <- geno(x)
    res <- vapply(seq_along(idxs), function(i) {
      .str_split_i(gt[, i], on = ":", i = idxs[i])
    }, character(length(samples(x))))
    dimnames(res) <- dimnames(gt)
  }

  # Convert NA values
  is_na <- stringr::str_detect(res , pattern ="^[.,|/]*$")
  res[is_na] <- NA

  varkit_geno(utils::type.convert(res, as.is = TRUE, na.strings = ""))
}

.index_format <- function(x, type) {
  frmt <- site(x, cols = "format")
  if (any(is.na(frmt)))
    stop("The FORMAT column contains missing values!")

  uniq <- as.character(unique(frmt))
  splits <- stringr::str_split(uniq, stringr::fixed(":"))
  names(splits) <- uniq

  vapply(splits, function(y) match(type, y), integer(1))
}

