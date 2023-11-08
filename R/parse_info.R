##' @export
parse_info <- function(x, type = NULL, use_meta = TRUE) {
  UseMethod("parse_info", x)
}

##' @export
parse_info.varkit <- function(x, type = NULL, use_meta = TRUE) {
  if (is.null(type)) {
    warning("Please provide the 'type' argument, returning possible options.")
    if (!is.null(meta.varkit(x)$info))
      return(dplyr::transmute(parse_meta.varkit(x, "INFO"), type = .data$ID, description = .data$Description))
    return(.collect_all_keys(site.varkit(x, "info")))
  } else {
    .stopifnotsingle(type, class = "character")
    validate_varkit(x, check_info = TRUE)
  }

  if (use_meta) {
    meta_info <- parse_meta.varkit(x, "INFO")
    if (!type %in% meta_info$ID)
      stop("The type '", type, "' could not be found in parse_meta(x, 'INFO')$ID!")
  } else {
    meta_info <- NULL
  }

  info <- site.varkit(x, "info")
  if (all(is.na(info)))
    stop("The INFO column only contains NAs!")

  .parse_info_internal(info, type, meta_info)
}

.collect_all_keys <- function(str) {
  keys <- stringr::str_remove_all(str, pattern = "=[^;]*")
  keys <- unique(keys) # This seems to speed up the procedure
  keys <- stringr::str_split(keys, ";")
  keys <- unlist(keys)
  unique(keys)
}

.parse_info_internal <- function(info, type, meta_info = NULL) {
  locs <- stringr::str_locate(info, sprintf("(?:^|;)%s(?:;|$|=[^;]*)", type))
  if (all(is.na(locs[, 1L])))
    return(rep(NA, length(info)))

  delim_locs <- locs[, "start"] + nchar(type) + (locs[, "start"] != 1L)
  delim <- stringr::str_sub(info, delim_locs, delim_locs)

  if (any(delim == "=", na.rm = TRUE)) {
    locs[, "start"] <- delim_locs + 1L
    locs[locs[, "start"] > locs[, "end"], ] <- NA_integer_
    info <- stringr::str_sub(info, locs)
  } else {
    # Type appears to be a flag & we report presence absence
    info <- !is.na(delim)
  }

  if (!is.null(meta_info))
    return(.convert_using_meta(info, meta_info, type))
  utils::type.convert(info, na.strings = c("", "."), as.is = TRUE, tryLogical = TRUE)
}

.convert_using_meta <- function(info, meta_info, type) {
  convert_to <- .get_info_col_types(meta_info)[type]

  # Convert NAs for parity w/ type.convert()
  info[info == "."] <- NA_character_

  switch(convert_to,
    l = as.logical(info),
    i = as.integer(info),
    d = as.double(info),
    ifelse(info == "", NA_character_, info)
  )
}
