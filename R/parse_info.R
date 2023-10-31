##' @export
parse_info <- function(x, type = NULL, use_meta = TRUE) {
  UseMethod("parse_info", x)
}

##' @export
parse_info.varkit <- function(x, type = NULL, use_meta = TRUE) {
  validate_varkit(x, check_info = TRUE)
  if (!is.null(type)) {
    .stopifnotsingle(type, class = "character")
  } else {
    warning("Please provide the 'type' argument, returning possible options.")
    if (!is.null(meta(x)) && "INFO" %in% meta(x)$name)
      return(dplyr::transmute(parse_meta(x, "INFO"), type = .data$ID, description = .data$Description))
    return(.collect_all_keys(site(x, "info")))
  }

  if (use_meta) {
    meta_info <- parse_meta(x, "INFO")
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

.parse_info_internal <- function(info, type, meta_info = NULL) {
  pattern <- paste0("(?:^|;)", type, "(?:=([^;]*))?(?:;|$)")
  matches <- stringr::str_match(info, pattern = pattern)

  if (!is.null(meta_info)) {
    return(.convert_using_meta(matches, meta_info, type))
  }

  # If the second column is all NA, type is likely a flag and we report presence/absence
  is_na <- is.na(matches)
  if (all(is_na[, 2L])) {
    if (all(is_na[, 1L]))
      return(matches[, 2L])
    return(!is_na[, 1L])
  }

  utils::type.convert(matches[, 2L], as.is = TRUE, na.strings = "")
}

.collect_all_keys <- function(str) {
  keys <- stringr::str_remove_all(str, pattern = "=[^;]*")
  keys <- unique(keys) # This seems to speed up the procedure
  keys <- stringr::str_split(keys, ";")
  keys <- unlist(keys)
  unique(keys)
}

.convert_using_meta <- function(info, meta_info, info_type) {
  meta_info <- meta_info[meta_info$ID == info_type, , drop = FALSE]
  convert_to <- .get_info_col_types(meta_info)[[1L]]

  if (convert_to == "l") {
    return(!is.na(info[, 1L]))
  } else if (convert_to == "i") {
    return(as.integer(info[, 2L]))
  } else if (convert_to == "d") {
    return(as.double(info[, 2L]))
  }

  # Return as character, replace NAs for parity w/ type.convert()
  info <- info[, 2L]
  info[info == ""] <- NA
  info
}
