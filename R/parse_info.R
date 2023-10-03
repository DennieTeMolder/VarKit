##' @export
parse_info <- function(x, type = NULL, use_meta = TRUE) {
  UseMethod("parse_info", x)
}

##' @export
parse_info.varkit <- function(x, type = NULL, use_meta = TRUE) {
  if (!is.null(type)) {
    .stopifnotsingle(type, class = "character")
  } else {
    warning("Please provide the 'type' argument, returning possible options.")
    return(dplyr::transmute(parse_meta(x, "INFO"), type = .data$ID, description = .data$Description))
  }

  if (use_meta) {
    meta_info <- parse_meta(x, "INFO")
    if (!type %in% meta_info$ID)
      stop("The type '", type, "' could not be found in parse_meta(x, 'INFO')$ID!")
  }

  info <- site(x, "info")
  if (all(is.na(info)))
    stop("The INFO column only contains NAs!")

  pattern <- paste0("(?:^|;)", type, "(?:=([^;]*))?(?:;|$)")
  info <- stringr::str_match(info, pattern = pattern)

  if (use_meta) {
    return(.convert_using_meta(info, meta_info, type))
  }

  is_na <- is.na(info)
  if (all(is_na[, 2])) {
    if (all(is_na[1, ]))
      return(info[, 2])
    return(!is.na(info[, 2]))
  }

  utils::type.convert(info[, 2], as.is = TRUE, na.strings = "")
}

.convert_using_meta <- function(info, meta_info, info_type) {
  meta_info <- meta_info[meta_info$ID == info_type, , drop = FALSE]
  convert_to <- .get_info_col_types(meta_info)[[1]]

  if (convert_to == "l") {
    return(!is.na(info[, 1]))
  } else if (convert_to == "i") {
    return(as.integer(info[, 2]))
  } else if (convert_to == "d") {
    return(as.double(info[, 2]))
  }

  # Return as character, replace NAs for parity w/ type.convert()
  info <- info[, 2]
  info[info == ""] <- NA
  info
}
