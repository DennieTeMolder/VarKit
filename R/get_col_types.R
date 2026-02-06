##' @export
get_col_types <- function(x, meta_type = "INFO", ...) {
  UseMethod("get_col_types", x)
}

##' @export
get_col_types.varkit <- function(x, meta_type = "INFO", ...) {
  info <- parse_meta(x, type = meta_type)
  if (is.null(info))
    return(NULL)

  if (!all(c("ID", "Number", "Type") %in% colnames(info)))
    stop("Calling `parse_meta(x, meta_type)` should return the columns ID, Number, and Type!")

  info$Type <- tolower(info$Type)

  col_types <- with(info, dplyr::case_when(
    Type   == "flag"    ~ "l",
    Number != "1"       ~ "c",
    Type   == "integer" ~ "i",
    Type   == "float"   ~ "d",
    .default = "c"
  ))

  names(col_types) <- info$ID
  col_types
}

##' @export
get_col_types.varkit_header <- get_col_types.varkit
