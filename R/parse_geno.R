##' @export
parse_geno <- function(x, type = NULL, allow_missing = FALSE) {
  UseMethod("parse_geno", x)
}

##' @export
parse_geno.varkit <- function(x, type = NULL, allow_missing = FALSE) {
  validate_varkit(x, check_format = TRUE)
  if (!is.null(type)) {
    .stopifnotsingle(type, class = "character")
  } else {
    warning("Please provide the 'type' argument, returning possible options.")
    return(.get_format_types(x))
  }

  # Check te FORMAT field for index of type
  frmt <- .index_format(x, type)
  if (!allow_missing && any(is.na(frmt)))
    stop("The type '", type, "' is not found in all FORMAT fields! If this is expected use `allow_missing = TRUE`.")


  # Parse type from genotype fields
  if (all(frmt == frmt[[1]])) {
    # For fixed format loop over samples to save time while remaining memory efficient
    idx <- frmt[[1]]
    res <- as.matrix(apply_samples(geno(x), .str_split_i, on = ":", i = idx))
  } else {
    idxs <- unname(frmt[site(x, "format")])
    gt <- geno.varkit(x)
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

.get_format_types <- function(x) {
  frmt <- site(x, "format")
  if (any(is.na(frmt)))
    stop("The FORMAT column contains missing values!")

  uniq <- as.character(unique(frmt))

  # Split the unique combinations and check if specific types appear at a fixed position/index
  uniq <- stringr::str_split(uniq, ":")
  uniq_mat <- sapply(uniq, `[`, seq_len(min(sapply(uniq, length))))
  fixed_position <- uniq_mat[apply(uniq_mat, 1, .not_variable), 1L]
  types <- sort(unique(unlist(uniq)))
  results <- tibble::tibble(type = types, index = match(types, fixed_position))

  if ("FORMAT" %in% meta(x)$name) {
    metadata <- parse_meta(x, "FORMAT")
    metadata <- tibble::tibble(type = metadata$ID, description = metadata$Description)
    return(dplyr::right_join(metadata, results, by = "type"))
  }

  results
}

.index_format <- function(x, type) {
  frmt <- site(x, "format")
  if (any(is.na(frmt)))
    stop("The FORMAT column contains missing values!")

  uniq <- as.character(unique(frmt))
  splits <- stringr::str_split(uniq, stringr::fixed(":"))
  names(splits) <- uniq

  vapply(splits, function(y) match(type, y), integer(1))
}

