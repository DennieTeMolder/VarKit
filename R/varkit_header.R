##' @export
varkit_header <- function(x) {
  stopifnot(is.character(x))
  stopifnot(length(x) > 1)

  if (!.str_starts(x[1], "##fileformat=VCF"))
    warning("No valid VCF fileformat meta information detected on the first line!")

  if (!all(.str_starts(x, "#")))
    stop("Invalid VCF. Not all meta lines start with a '#'!")

  header <- list(
    meta = .parse_meta(utils::head(x, n = -1)),
    col_names = .parse_col_names(utils::tail(x, n = 1))
  )
  class(header) <- c("varkit_header", "list")
  header
}

.parse_meta <- function(x) {
  x <- stringr::str_remove(x, "^##")
  x <- stringr::str_split_fixed(x, "=", n = 2)
  tibble::tibble(name = x[, 1], value = x[, 2])
}

.parse_col_names <- function(x) {
  if (substr(x, 2, 2) == "#")
    stop("The last line does not appear to be the header line")
  col_names <- stringr::str_remove(x, "^#")
  col_names <- stringr::str_split_1(col_names, "\t")

  # Standardise the names of fixed columns
  fixed_names <- .fixed_columns()
  fixed_idx <- stats::na.omit(match(fixed_names, tolower(col_names)))
  col_names[fixed_idx] <- fixed_names[seq_along(fixed_names) %in% fixed_idx]
  col_names
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
