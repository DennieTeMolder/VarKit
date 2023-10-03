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

  pattern <- paste0("(?:^|;)", type, "=([^;]*)")
  res <- stringr::str_extract(site(x, "info"), pattern = pattern, group = 1)

  if (use_meta) {
    return(.convert_using_meta(res, meta_info, type))
  }
  utils::type.convert(res, as.is = TRUE, na.strings = "")
}

.convert_using_meta <- function(str, meta_info, info_type) {
  meta_info <- meta_info[meta_info$ID == info_type, , drop = FALSE]
  convert_to <- .get_info_col_types(meta_info)[[1]]

  if (convert_to == "l") {
    str <- as.logical(str)
    if (anyNA(str))
      str[is.na(str)] <- FALSE
    return(str)
  } else if (convert_to == "i") {
    return(as.integer(str))
  } else if (convert_to == "d") {
    return(as.double(str))
  }

  # Conversion to charater is not needed but we check for NAs
  str[str == ""] <- NA
  str
}
