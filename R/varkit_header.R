##' @export
varkit_header <- function(meta = data.frame(), col_names = character()) {
  meta <- as.data.frame(meta)
  col_names <- as.character(col_names)
  header <- new_varkit_header(meta = meta, col_names = col_names)
  validate_varkit_header(header)
}

##' @export
new_varkit_header <- function(meta = data.frame(name = character(), value = character()),
                              col_names = character()
                              ) {
  stopifnot(is.character(col_names))
  if (is.null(meta)) {
    meta <- data.frame(name = character(), value = character())
  } else {
    stopifnot(is.data.frame(meta))
  }

  structure(
    list(meta = tibble::as_tibble(meta), col_names = col_names),
    class = c("varkit_header", "list")
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
  if (.any_na(x$col_names) && any(nchar(x$col_names) < 1L))
    stop("The '$col_names' slot cannot be NA or a zero-length string!")
  if (anyDuplicated(x$col_names) != 0L)
    warning("Duplicate '$col_names' detected.")

  x
}

.validate_varkit_meta <- function(x) {
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

  .msg_h2("$meta, Header metadata")
  print(meta.varkit_header(x), ..., n = n)

  cat("\n")
  .msg_h2("$col_names, Column names (for 'col_select')")
  if (length(x$col_names) > 0L) {
    cnames <- cli::cli_vec(x$col_names, style = list(
      "vec-sep" = ", ", "vec-last" = ", ", "vec-trunc" = 100
    ))
    cli::cli_text("{cnames}")
    .msg_info("Use `samples()` to get only columns recognized as sample names")
  }
}
