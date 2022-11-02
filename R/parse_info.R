##' @export
parse_info <- function(x) {
  UseMethod("parse_info", x)
}

##' @export
parse_info.varkit <- function(x) {
  # Convert to dataframe assuming ";" and "=" can only be used as delimiters
  res <- stringr::str_split(x$fixed$info[x$use_site], ";")
  res <- lapply(res, .split_key_value)
  res <- .bind_rows_list(res)

  # Use INFO metadata to convert specific columns to numeric
  if ("INFO" %in% x$meta$name) {
    types <- parse_meta_type(x, "INFO")
    types <- dplyr::left_join(data.frame(ID = colnames(res)), types, by = "ID")
    which_numeric <- which(tolower(types$Type) %in% c("integer", "float"))
    for (idx in which_numeric) {
      # Do not convert columns with multi value fields
      if (!any(stringr::str_detect(res[[idx]], ","), na.rm = TRUE))
        res[[idx]] <- as.numeric(res[[idx]])
    }
  }

  res
}

.split_key_value <- function(x) {
  if (all(is.na(x)))
    return(NA)

  x <- stringr::str_split_fixed(x, pattern = "=", n = 2)
  res <- x[, 2]
  names(res) <- x[, 1]

  res
}

##' @importFrom dplyr bind_rows
.bind_rows_list <- function(lst) {
  is_na <- is.na(lst)
  if (any(is_na)) {
    non_na_idx <- which(!is_na)[1]
    valid_name <- names(lst[[non_na_idx]])[1]

    for (idx in which(is_na))
      names(lst[[idx]]) <- valid_name
  }

  do.call("bind_rows", lst)
}
