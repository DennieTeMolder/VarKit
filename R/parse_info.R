##' @export
parse_info <- function(x, type = NULL, use_meta = TRUE) {
  if (!missing(type) && is.null(type))
    warning("The 'type' argument is NULL, returning possible options.")
  UseMethod("parse_info", x)
}

##' @export
parse_info.varkit <- function(x, type = NULL, use_meta = TRUE) {
  validate_varkit(x, check_info = TRUE)
  if (!is.null(type))
    .stopifnotsingle(type, what = "character")
  .stopifnotsingle(use_meta, what = "logical")

  if (use_meta && !any(x$meta$name == "INFO")) {
    warning("The `$meta$name` column has no INFO entries, reverting to `use_meta = FALSE`!")
    use_meta <- FALSE
  }

  # When TYPE is NULL, return all possible values
  if (is.null(type)) {
    if (use_meta) {
      return(dplyr::transmute(
        parse_meta.varkit(x, type = "INFO"),
        type = .data$ID,
        description = .data$Description
      ))
    }

    return(tibble::tibble(
      type = .collect_all_keys(site.varkit(x, "info"))
    ))
  }

  # Columns of x$site can be referenced directly w/o parsing, the INFO/TAG always triggers parsing
  if (stringr::str_detect(type, "^INFO/")) {
    type <- stringr::str_remove(type, "INFO/")
  } else if (type %in% rd_dimnames(x, i = "site", use_indices = FALSE, validate = FALSE)[[2L]]) {
    return(rd_get(x, i = "site", , type, .drop = TRUE, .validate = FALSE))
  }

  if (use_meta) {
    col_types <- get_info_types.varkit(x, meta_type = "INFO")
    if (!type %in% names(col_types))
      stop("The type '", type, "' could not be found in parse_meta(x, 'INFO')$ID!")
  } else  {
    col_types <- NULL
  }

  info <- site.varkit(x, "info")
  if (all(is.na(info)))
    stop("The $site$info column only contains NAs!")

  .parse_info_internal(info, type = type, col_types = col_types)
}

.collect_all_keys <- function(str) {
  is_na <- is.na(str)
  if (all(is_na))
    return(NULL)
  if (any(is_na))
    str <- str[!is_na]

  keys <- stringr::str_remove_all(str, pattern = "=[^;]*")
  keys <- unique(keys) # This seems to speed up the procedure
  keys <- stringr::str_split(keys, ";")
  keys <- .unlist(keys)
  sort(unique(keys))
}

.parse_info_internal <- function(info, type, col_types = NULL) {
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

  .convert_info(info, convert_to = col_types[type])
}

.convert_info <- function(info, convert_to = NULL, na = c("", ".")) {
  if (!is.character(info))
    stop("Unexpected!")
  if (is.null(convert_to))
    return(utils::type.convert(info, na.strings = na, as.is = TRUE, tryLogical = TRUE))

  # Convert NAs for parity w/ type.convert()
  info[info %in% na] <- NA

  switch(convert_to,
    l = as.logical(info),
    i = as.integer(info),
    d = as.double(info),
    info
  )
}
