##' @export
write_vcf <- function(x, file, append = FALSE, replace_filter = FALSE, na_geno = NULL) {
  UseMethod("write_vcf", x)
}

##' @export
write_vcf.varkit <- function(x, file, append = FALSE, replace_filter = FALSE, na_geno = NULL) {
  if (!is.null(na_geno))
    .stopifnotsingle(na_geno, class = "character")

  if (!append || !file.exists(file)) {
    cli::cli_progress_step("Writing VCF header")
    write_vcf_header(x, file)
    cli::cli_process_done
  }

  ## Fixed fields ------
  fxd <- x$site

  # Add missing columns
  fixed_names <- .fixed_columns()
  missing <- fixed_names[!fixed_names %in% tolower(colnames(fxd))]

  if ("format" %in% missing)
    cli::cli_alert_danger("The FORMAT column is missing!")

  if (length(missing) > 0) {
    cli::cli_alert_info(c("Creating missing columns: ", .collapse(missing)))
    fxd[missing] <- rep(NA, nrow(fxd))
  }

  fxd$filter <- .check_filter(fxd$filter, x$use_site, replace_filter)

  # Fxd ordering and convert
  fxd <- dplyr::relocate(fxd, tidyselect::all_of(fixed_names))
  colnames(fxd) <- toupper(colnames(fxd))

  ## Genotype fields ------
  gt <- t(x$geno)
  if (is.null(na_geno)) {
    non_na <- as.character(gt[which(!is.na(gt))[1]])
    na_geno <- stringr::str_replace_all(non_na, "[^|/:]+", ".")
    cli::cli_alert_info(c("Representing missing genotypes as: '", na_geno, "'"))
  }
  gt[is.na(gt)] <- na_geno

  cli::cli_progress_step("Writing VCF body")
  body <- cbind(fxd, gt)
  rm(fxd, gt)
  readr::write_tsv(body, file, append = TRUE, col_names = FALSE, na = ".")
}

.check_filter <- function(fltr, use_site, replace_filter) {
  is_na <- is.na(fltr)
  if (all(is_na))
    return(ifelse(use_site, "PASS", "MASKED"))

  if (any(is_na))
    fltr[is_na] <- "PASS"

  pass_filter <- fltr == "PASS"
  if (any(pass_filter != use_site)) {
    if (replace_filter) {
      fltr[use_site] <- "PASS"
      return(ifelse(pass_filter & !use_site, "MASKED", fltr))
    }
    cli::cli_alert_warning("FILTER column does not reflect currently masked sites!")
    cli::cli_alert_warning("Use `replace_filter = TRUE` if this is desired.")
  }
  fltr
}
