##' @export
parse_geno <- function(x, type = NULL, allow_missing = FALSE) {
  UseMethod("parse_geno", x)
}

##' @export
parse_geno.varkit <- function(x, type = NULL, allow_missing = FALSE) {
  validate_varkit(x, check_format = TRUE)

  # We first check TYPE because the errors generated are more intuitive to the user
  if (!is.null(type)) {
    .stopifnotsingle(type, what = "character")
  } else {
    warning("Please provide the 'type' argument, returning possible options.")
    return(.get_format_types(x))
  }

  # Check if parsing is required
  if (all(site.varkit(x, "format") == type))
    return(geno.varkit(x))

  # Check te FORMAT field for index of type
  frmt <- .index_format(x, type)
  if (all(frmt == 0L))
    stop("The type'", type, "' is not in any FORMAT fields!")
  if (!allow_missing && any(frmt == 0L))
    stop("The type '", type, "' is not found in all FORMAT fields! If this is expected use `allow_missing = TRUE`.")

  # Only character matrices can/need to be parsed
  if (!is.character(x$geno))
    stop("The matrix in the `geno()` slot should be of type character!")

  # Parse type from genotype fields
  gt <- unclass(geno.varkit(x))
  drmodel <- drm_get(gt)

  if (all(frmt == frmt[[1L]])) {
    res <- matrix(
      .str_split_i(gt, on = ":", i = frmt[[1L]]),
      nrow = nrow(gt), dimnames = dimnames(gt)
    )
    rm(gt)
  } else {
    idxs <- unname(frmt[site.varkit(x, "format")])

    old_names <- colnames(gt)
    colnames(gt) <- seq_len(ncol(gt))
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

    res <- do.call(cbind, res)
    res <- res[, order(as.integer(colnames(res))), drop = FALSE]
    colnames(res) <- old_names
  }

  # Convert NA values manually using regex and cast types
  res[stringr::str_detect(res , pattern ="^[.,|/]*$")] <- NA_character_
  res <- varkit_geno(utils::type.convert(res, as.is = TRUE, na.strings = ""))

  # Attach the data relation model if any
  if (is.null(drmodel))
    return(res)

  drm_do_attach(
    res,
    drmodel = drmodel,
    dim_links = c("samples", "sites"),
    label = paste0("parse_geno(", type, ")")
  )
}

.get_format_types <- function(x) {
  uniq <- as.character(unique(site.varkit(x, "format")))

  # Split the unique combinations and check if specific types appear at a fixed position/index
  uniq <- stringr::str_split(uniq, ":")
  uniq_mat <- sapply(uniq, `[`, seq_len(min(sapply(uniq, length))))
  which_fixed_pos <- which(has_no_variation(uniq_mat, margin = 1))
  names(which_fixed_pos) <- uniq_mat[which_fixed_pos, 1L]

  types <- sort(unique(unlist(uniq)))
  results <- tibble::tibble(type = types, index = which_fixed_pos[types])

  if ("FORMAT" %in% meta(x)$name) {
    metadata <- parse_meta(x, "FORMAT")
    metadata <- tibble::tibble(type = metadata$ID, description = metadata$Description)
    return(dplyr::right_join(metadata, results, by = "type"))
  }

  results
}

.index_format <- function(x, type) {
  uniq <- as.character(unique(site.varkit(x, "format")))
  splits <- stringr::str_split(uniq, stringr::fixed(":"))
  names(splits) <- uniq
  vapply(splits, function(y) match(type, y, 0L), integer(1))
}

