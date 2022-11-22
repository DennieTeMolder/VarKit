##' @export
write_vcf <- function(x, file, replace_filter = FALSE, na_gt = NULL) {
  UseMethod("write_vcf", x)
}

##' @export
write_vcf.varkit <- function(x, file, replace_filter = FALSE, na_gt = NULL) {
  if (all(is.na(x$geno)))
    stop("All genotype fields are NA!")
  .stopifnotsingle(file, class = "character")
  if (stringr::str_ends(file, "\\.(gz|gzip)"))
    warning("Varkit writes regular g-zipped files, not the block g-zip format ",
            "expected by many tools!\n If the latter is desired, write to plain ",
            "vcf and convert using an external tool like 'bgzip'.")
  if (!is.null(na_gt))
    stopifnot(is.character(na_gt), length(na_gt) == 1)

  ## Header
  header <- x$meta
  if (!is.null(header))
    header <- header[header$name != "fileformat", ]
  # Replace fileformat and add info
  header <- rbind(
    data.frame(name = "fileformat", value = "VCFv4.2"),
    header,
    data.frame(
      name = "VarKit",
      value = paste0("Modified and written to file by 'write_vcf';Date=", date())
    )
  )
  header <- paste0("##", header$name, "=", header$value)

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

  if (replace_filter
      || all(is.na(fxd$filter))
      || all(fxd$filter == "PASS", na.rm = TRUE)) {
    fxd$filter <- ifelse(x$use_site, "PASS", "MASKED")
  } else if (!all(fxd$filter[x$use_site] == "PASS", na.rm = TRUE)) {
    warning("FILTER column does not reflect currently masked sites!\n",
            "Use `replace_filter = TRUE` if this is desired.")
  }

  # Fxd ordering and convert
  fxd <- dplyr::relocate(fxd, tidyselect::all_of(fixed_names))
  colnames(fxd) <- toupper(colnames(fxd))

  ## Genotype fields
  gt <- t(x$geno)
  if (is.null(na_gt)) {
    non_na <- as.character(gt[which(!is.na(gt))[1]])
    na_gt <- stringr::str_replace_all(non_na, "[^|/:]+", ".")
    message("Representing missing genotypes as: '", na_gt, "'")
  }
  gt[is.na(gt)] <- na_gt

  # Combine
  body <- cbind(fxd, gt)
  rm(fxd, gt)

  ## Writing
  # Header
  readr::write_lines(header, file)
  col_names <- paste0("#", paste(colnames(body), collapse = "\t"))
  readr::write_lines(col_names, file, append = TRUE)

  # data
  readr::write_tsv(body, file, append = TRUE, col_names = FALSE, na = ".")
}
