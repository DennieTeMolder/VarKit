##' @export
parse_geno <- function(x, type = NULL, allow_missing = FALSE) {
  if (!missing(type) && is.null(type))
    warning("The 'type' argument is NULL, returning possible options.")
  UseMethod("parse_geno", x)
}

##' @export
parse_geno.varkit <- function(x, type = NULL, allow_missing = FALSE) {
  validate_varkit(x, check_format = TRUE)

  # We first check TYPE because the errors generated are more intuitive to the user
  if (!is.null(type)) {
    .stopifnotsingle(type, what = "character")
  } else {
    return(.get_format_types(x))
  }

  # Elements of X can be referenced directly w/o parsing, the FORMAT/TAG always triggers parsing
  if (stringr::str_detect(type, "^(FORMAT|FMT)/")) {
    type <- stringr::str_remove(type, "[^/]*/")
  } else if (type %in% names(x)) {
    res <- x[[type]]
    if (!is.matrix(res)) {
      stop("Expected cached value in `$", type, "` to be a matrix!")
    } else if (any(dim(res) != rd_dim(x, i = "geno"))) {
      stop("Expected dimensions for cached value in `$", type, "`: ", .collapse(.frmt_big(rd_dim(x, i = "geno"))),
           "; but got: ", .collapse(.frmt_big(dim(res))))
    }
    return(res)
  }

  # Check if parsing is required
  frmt <- site.varkit(x, "format")
  if (all(frmt == type))
    return(geno.varkit(x))

  # Only character matrices can/need to be parsed
  if (!is.character(rd_unclass(x)$geno))
    stop("The matrix in the `geno()` slot should be of type character!")

  # Check if any/all FORMAT fields contain TYPE
  frmt_idx <- .index_format(frmt, type)
  if (all(frmt_idx == 0L))
    stop("The type '", type, "' is not in any $site$format fields!")
  if (!allow_missing && any(frmt_idx == 0L))
    stop("The type '", type, "' is not found in all $site$format fields! If this is expected use `allow_missing = TRUE`.")

  # Parse type from genotype fields
  gt <- unclass(geno.varkit(x))

  if (all(frmt_idx == frmt_idx[[1L]])) {
    # Simple case if all variants have the same FORMAT
    res <- matrix(
      .str_split_i(gt, on = ":", i = frmt_idx[[1L]]),
      nrow = nrow(gt), dimnames = dimnames(gt)
    )
    rm(gt)
  } else {
    # Complex case where variants differ in FORMAT
    idxs <- unname(frmt_idx[frmt])
    old_names <- colnames(gt)
    colnames(gt) <- seq_len(ncol(gt))

    # Split the genotype by FORMAT and loop
    gt <- splitm(gt, f = idxs, margin = 2)
    res <- lapply(names(gt), function(curr) {
      dims <- dim(gt[[curr]])
      if (curr == "0") {
        res <- NA_character_
      } else {
        res <- .str_split_i(gt[[curr]], on = ":", i = as.integer(curr))
      }
      matrix(res, nrow = dims[1L], ncol = dims[2L], dimnames = dimnames(gt[[curr]]))
    })
    rm(gt)

    # Combine and restore the original order using the colnames
    res <- do.call(cbind, res)
    res <- res[, order(as.integer(colnames(res))), drop = FALSE]
    colnames(res) <- old_names
  }

  # Convert NA values manually using regex and cast types
  res[stringr::str_detect(res , pattern ="^[.,|/]*$")] <- NA_character_
  varkit_geno(utils::type.convert(res, as.is = TRUE, na.strings = ""))
}

.get_format_types <- function(x) {
  uniq <- as.character(unique(site.varkit(x, "format")))

  # Split the unique combinations and check if specific types appear at a fixed position/index
  uniq <- stringr::str_split(uniq, ":")
  uniq_mat <- sapply(uniq, `[`, seq_len(min(sapply(uniq, length))))
  which_fixed_pos <- which(has_no_variation(uniq_mat, margin = 1))
  names(which_fixed_pos) <- uniq_mat[which_fixed_pos, 1L]

  types <- sort(unique(unlist(uniq)))
  results <- dplyr::arrange(tibble::tibble(
    type = types,
    index = which_fixed_pos[types]
  ), index)

  metadata <- parse_meta(x, "FORMAT")
  if (!is.null(metadata)) {
    metadata <- tibble::tibble(
      type = metadata$ID,
      description = metadata$Description
    )
    return(dplyr::left_join(results, metadata, by = "type"))
  }

  results
}

.index_format <- function(frmt, type) {
  uniq <- as.character(unique(frmt))
  splits <- stringr::str_split(uniq, stringr::fixed(":"))
  names(splits) <- uniq
  vapply(splits, function(y) match(type, y, 0L), integer(1))
}

