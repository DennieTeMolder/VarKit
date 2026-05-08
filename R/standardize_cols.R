##' @export
standardize_cols <- function(x, message = TRUE) {
  UseMethod("standardize_cols", x)
}

##' @export
standardize_cols.varkit <- function(x, message = TRUE) {
  stopifnot("site" %in% names(x))

  all_site <- rd_unclass(x)$site
  if (!is.data.frame(all_site))
    stop("The '$site' slot should be a dataframe!")

  missing <- .find_missing_cols(colnames(all_site), message = message)
  if (length(missing) > 0L)
    all_site[, missing] <- rep(NA, nrow(all_site))

  # Remove all non-fixed columns from $sites as the violate the VCF spec
  all_site <- all_site[.fixed_columns()]
  x$site <- all_site

  x
}

##' @export
standardize_cols.varkit_header <- function(x, message = TRUE) {
  if (message) .find_missing_cols(x$col_names, message = message)
  x$col_names <- c(.fixed_columns(), samples.varkit_header(x))
  x
}

.find_missing_cols <- function(col_names, message) {
  missing <- setdiff(.fixed_columns(), tolower(col_names))

  if (message && length(missing) > 0L)
    .msg_info("Creating missing columns: ", toupper(.collapse(missing)))

  missing
}
