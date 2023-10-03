##' @export
parse_info_all <- function(x) {
  UseMethod("parse_info_all", x)
}

##' @export
parse_info_all.varkit <- function(x) {
  # Convert to dataframe assuming ";" and "=" can only be used as delimiters
  res <- stringr::str_split(site(x, cols = "info"), stringr::fixed(";"))
  res <- lapply(res, .split_key_value)
  res <- .bind_rows_list(res)
  res <- dplyr::relocate(res, sort(colnames(res)))
  .mutate_all(res, .convert_info_col)
}

.split_key_value <- function(str) {
  if (anyNA(str))
    return(NA)

  splits <- stringr::str_split_fixed(str, pattern = stringr::fixed("="), n = 2)

  res <- splits[, 2]
  res <- ifelse(res == "", "TRUE", res)
  names(res) <- splits[, 1]
  res
}

.bind_rows_list <- function(lst) {
  # Give NAs a name attribute, so bind_rows will handle them correctly
  is_na <- is.na(lst)
  if (any(is_na)) {
    non_na_idx <- which(!is_na)[1]
    valid_name <- names(lst[[non_na_idx]])[1]

    for (idx in which(is_na))
      names(lst[[idx]]) <- valid_name
  }

  do.call(dplyr::bind_rows, lst)
}

.convert_info_col <- function(col) {
  col <- utils::type.convert(col, as.is = TRUE, na.strings = "")
  if (is.logical(col))
    col[is.na(col)] <- FALSE
  col
}
