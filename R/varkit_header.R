##' @export
varkit_header <- function(meta = data.frame(), col_names = character()) {
  meta <- as.data.frame(meta)
  col_names <- as.character(col_names)
  header <- new_varkit_header(meta = meta, col_names = col_names)
  validate_varkit_header(header)
}

##' @export
new_varkit_header <- function(meta = data.frame(), col_names = character()) {
  stopifnot(is.character(col_names))
  if (!is.null(meta)) {
    stopifnot(is.data.frame(meta))
    if (any(dim(meta) == 0L)) {
      meta <- NULL
    } else {
      meta <- tibble::as_tibble(meta)
    }
  }

  structure(
    list(meta = meta, col_names = col_names),
    class = "varkit_header"
  )
}

##' @export
validate_varkit_header <- function(x) {
  stopifnot(is.list(x))

  .validate_varkit_meta(x$meta)

  if (length(x[["col_names"]]) < 1L)
    stop("The '$col_names' slot is empty!")
  if (!is.character(x$col_names))
    stop("The '$col_names' slot must be a character vector!")
  if (.any_na(x$col_names) && any(nchar(x$col_names) < 1))
    stop("The '$col_names' slot cannot be NA or a zero-length string!")
  if (anyDuplicated(x$col_names) != 0L)
    warning("Duplicate '$col_names' detected.")

  x
}

.validate_varkit_meta <- function(x) {
  if (is.null(x))
    return(x)

  if (!tibble::is_tibble(x))
    stop("The '$meta' slot must be a tibble!")
  if (!any(colnames(x) == "name"))
    stop("The '$meta$name' column is missing!")
  if (.any_na(x$name))
    stop("The '$meta$name' column contains NAs!")
  if (!any(colnames(x) == "value"))
    stop("The '$meta$value' column is missing!")
  x
}

##' @export
is_varkit_header <- function(x) {
  inherits(x, "varkit_header")
}

##'@export
print.varkit_header <- function(x, ..., n = NULL) {
  .msg_h1("VarKit Header Object")
  if (!is.null(meta(x))) {
    .msg_h2("Header metadata, `meta(header)`")
    print(meta(x), ..., n = n)
    cat("\n")
  }
  .msg_h2("Column names (for 'col_select'), `header$col_names`")
  cnames <- cli::cli_vec(x$col_names, style = list("vec-sep" = ", ", "vec-last" = ", ", "vec-trunc" = 100))
  cli::cli_text("{cnames}")
  .msg_info("Use `samples(header)` to get only columns recognised as sample names")
}
