##' @export
varkit_header <- function(meta = data.frame(), col_names = character()) {
  meta <- as.data.frame(meta)
  col_names <- as.character(col_names)
  header <- new_varkit_header(meta = meta, col_names = col_names)
  validate_varkit_header(header)
}

##' @export
new_varkit_header <- function(meta = data.frame(), col_names = character()) {
  stopifnot(is.null(meta) || is.data.frame(meta))
  stopifnot(is.character(col_names))
  structure(
    list(meta = tibble::as_tibble(meta), col_names = col_names),
    class = "varkit_header"
  )
}

##' @export
validate_varkit_header <- function(x) {
  stopifnot(c("meta", "col_names") %in% names(x))

  if (!is.null(x$meta))
    .validate_varkit_meta(x$meta)

  if (!is.character(x$col_names))
    stop("The 'col_names' slot must be a character vector!", call. = FALSE)
  if (anyNA(x$col_names) && any(nchar(x$col_names) < 1))
    stop("The 'col_names' slot cannot be NA or a zero-length string!")
  if (anyDuplicated(x$col_names))
    warning("Duplicate 'col_names' detected.")

  x
}

.validate_varkit_meta <- function(meta) {
  if (!tibble::is_tibble(meta))
    stop("The 'meta' slot must be a tibble!", call. = FALSE)
  if (all(colnames(meta) != c("name", "value")))
    stop("The 'meta' slot should only have a 'name' & 'value' column!", call. = FALSE)
  if (anyNA(meta$name))
    stop("The 'meta$name' contains NAs!", call. = FALSE)
  meta
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
  .msg_h2("Column names (for 'skip_cols'), `col_names(header)`")
  cnames <- cli::cli_vec(col_names(x), style = list("vec-sep" = ", ", "vec-last" = ", ", "vec-trunc" = 100))
  cli::cli_text("{cnames}")
  .msg_info("Use `samples(header)` to get only columns recognised as sample names")
}
