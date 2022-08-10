##' @export
write_vcf <- function(x, path, replace_filter = FALSE) {
  UseMethod("write_vcf", x)
}

##' @export
write_vcf.varkit <- function(x, path, replace_filter = FALSE) {
  ## Header
  if (!is.null(x$meta)) {
    header <- x$meta
    # Replace fileformat and timestamp
    header <- rbind(
      data.frame(name = "fileformat", value = "VCFv4.2"),
      header[header$name != "fileformat", ],
      data.frame(
        name = "VarKit",
        value = paste0("Modified and written to file by 'write_vcf';Date=", date())
      )
    )
    header <- paste0("##", header$name, "=", header$value)
  } else {
    header <- '##fileformat=VCFv4.2'
  }

  ## Fixed fields
  fxd <- x$fixed

  # Add missing columns
  fixed_names <- .fixed_columns()
  missing <- fixed_names[!fixed_names %in% tolower(colnames(fxd))]

  if ("format" %in% missing)
    warning("The FORMAT column is missing!")

  if (length(missing) > 0) {
    message("Creating missing columns: ", paste(missing, collapse = ", "))
    fxd[missing] <- rep(NA, nrow(fxd))
  }

  if (replace_filter || all(is.na(fxd$filter)))
    fxd$filter <- ifelse(x$use_site, "PASS", "MASKED")

  # Fxd ordering and convert
  fxd <- dplyr::relocate(fxd, tidyselect::all_of(fixed_names))
  colnames(fxd) <- toupper(colnames(fxd))

  ## Genotype fields
  gt <- x$geno

  if (is.character(gt))
    gt[is.na(gt)] <- paste(rep(".", x$ploidy), collapse = "/")
  gt <- as.data.frame(gt)

  # Combine
  gt <- cbind(fxd, gt)

  ## Writing
  # Header
  readr::write_lines(header, path)
  col_names <- paste0("#", paste(colnames(gt), collapse = "\t"))
  readr::write_lines(col_names, path, append = TRUE)

  # data
  readr::write_tsv(gt, path, append = TRUE, col_names = FALSE, na = ".")
}
