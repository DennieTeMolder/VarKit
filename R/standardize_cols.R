##' @export
standardize_cols <- function(x) {
  UseMethod("standardize_cols", x)
}

##' @export
standardize_cols.varkit <- function(x) {
  missing <- .find_missing_cols(x)

  if (length(missing) > 0) {
    x$site[, missing] <- rep(NA, nrow(x$site))
  }

  x$site <- dplyr::relocate(x$site, tidyselect::all_of(.fixed_columns()))
  x
}

##' @export
standardize_cols.varkit_header <- function(x) {
  .find_missing_cols(x)
  x$col_names <- c(.fixed_columns(), samples(x))
  x
}

.find_missing_cols <- function(x, silent = FALSE) {
  fxd_cols <- .fixed_columns()
  missing <- fxd_cols[!fxd_cols %in% tolower(col_names(x))]

  if (length(missing) == 0)
    return(character(0))

  if (!silent) {
    if ("format" %in% missing)
      cli::cli_alert_danger("The FORMAT column is missing!")

    cli::cli_alert_info(c("Creating missing columns: ", toupper(.collapse(missing))))
  }

  missing
}
