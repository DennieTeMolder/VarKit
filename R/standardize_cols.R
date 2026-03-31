##' @export
standardize_cols <- function(x, message = TRUE) {
  UseMethod("standardize_cols", x)
}

##' @export
standardize_cols.varkit <- function(x, message = TRUE) {
  all_site <- rd_unclass(x)$site
  missing <- .find_missing_cols(colnames(all_site), message = message)

  if (length(missing) > 0) {
    all_site[, missing] <- rep(NA, nrow(all_site))
  }

  x$site <- dplyr::relocate(all_site, tidyselect::all_of(.fixed_columns()))
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
    cli::cli_alert_info(c("Creating missing columns: ", toupper(.collapse(missing))))

  missing
}
