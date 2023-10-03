##' @export
get_info_col_types <- function(x, meta_type = "INFO", ...) {
  UseMethod("get_info_col_types", x)
}

##' @export
get_info_col_types.varkit_header <- function(x, meta_type = "INFO", ...) {
  meta_info <- parse_meta(x, type = meta_type)
  if (!all(c("ID", "Number", "Type") %in% colnames(meta_info)))
    stop("Parsing error: `parse_meta(vcf, meta_type)` does not have columns ID, Number, and Type!")

  col_types <- .get_info_col_types(meta_info)
  do.call(readr::cols, c(col_types, ...))
}

.get_info_col_types <- function(meta_info) {
  col_types <- lapply(seq_along(meta_info$ID), function(i) {
    cur_number <- as.character(meta_info$Number[i])
    cur_type <- tolower(meta_info$Type[i])
    if (isTRUE(cur_type == "flag"))
      return("l")
    if (isTRUE(cur_number == "1")) {
      if (isTRUE(cur_type == "integer"))
        return("i")
      if (isTRUE(cur_type == "float"))
        return("d")
    }
    "c"
  })
  names(col_types) <- meta_info$ID
  col_types
}
