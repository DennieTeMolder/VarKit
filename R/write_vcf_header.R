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

  header <- x$meta
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

  header <- c(
    paste0("##", header$name, "=", header$value),
    paste0("#", .collapse(col_names(x), "\t"))
  )

  readr::write_lines(header, file)
}
