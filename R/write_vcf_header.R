##' @export
write_vcf_header <- function(x, file) {
  UseMethod("write_vcf_header", x)
}

##' @export
write_vcf_header.varkit_header <- function(x, file) {
  .stopifnotsingle(file, class = "character")
  if (stringr::str_ends(file, "\\.gz(ip)?"))
    warning("Varkit writes regular g-zipped files, not the block g-zip format ",
            "expected by many tools!\n If the latter is desired, write to plain ",
            "vcf and convert using an external tool like 'bgzip'.")

  header <- meta(x)
  # Replace fileformat and add info
  if (!is.null(header))
    header <- header[header$name != "fileformat", ]
  header <- rbind(
    data.frame(name = "fileformat", value = "VCFv4.2"),
    header,
    data.frame(
      name = "VarKit",
      value = paste0("Modified and written to file by 'write_vcf()';Date=", date())
    )
  )

  # Standardise the column names of the fixed fields and convert to upper
  x <- standardize_cols.varkit_header(x)
  which_to_upper <- seq_along(.fixed_columns())
  x$col_names[which_to_upper] <- toupper(x$col_names[which_to_upper])

  header <- c(
    paste0("##", header$name, "=", header$value),
    paste0("#", .collapse(x$col_names, "\t"))
  )

  readr::write_lines(header, file)
}
