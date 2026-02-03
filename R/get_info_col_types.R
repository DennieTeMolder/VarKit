##' @export
get_info_col_types <- function(x, meta_type = "INFO", ...) {
  UseMethod("get_info_col_types", x)
}

##' @export
get_info_col_types.varkit <- function(x, meta_type = "INFO", ...) {
  meta_info <- parse_meta(x, type = meta_type)
  if (is.null(meta_info))
    stop("The $meta$name columns has no INFO entries!")
  # REVIEW improve error message
  if (!all(c("ID", "Number", "Type") %in% colnames(meta_info)))
    stop("Parsing error: `parse_meta(vcf, meta_type)` does not have columns ID, Number, and Type!")

  .get_info_col_types(meta_info)
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
  setNames(do.call(c, col_types), nm = meta_info$ID)
}

##' @export
get_info_col_types.varkit_header <- get_info_col_types.varkit
