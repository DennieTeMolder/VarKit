##' @export
seqminer2varkit <- function(seq_list, ...) {
  stopifnot(is.list(seq_list))

  if (is.null(names(seq_list)))
    return(NULL)

  # Only named elements with a length > 0 can be converted
  seq_list[is.na(names(seq_list)) | names(seq_list) == ""] <- NULL
  seq_list[purrr::map_int(seq_list, length) == 0L] <- NULL

  if (length(seq_list) == 0L)
    return(NULL)

  # Get sample names
  samples <- seq_list[["sampleId"]]
  seq_list[["sampleId"]] <- NULL

  # Values from 'vcfIndv' are returned as arrays
  is_array <- purrr::map_lgl(seq_list, is.array)
  if (!any(is_array))
    return(NULL)

  array_list <- seq_list[is_array]
  seq_list[is_array] <- NULL

  # Add samples to rownames and pick an array for the $geno slot (prefers FORMAT/GT)
  i_geno <- if (is.matrix(array_list[["GT"]])) match("GT", names(array_list))
  for (i in seq_along(array_list)) {
    if (nrow(array_list[[i]]) != length(samples))
      next()
    if (is.null(i_geno) && is.matrix(array_list[[i]]))
      i_geno <- i
    rownames(array_list[[i]]) <- samples
  }

  if (is.null(i_geno))
    return(NULL)

  # Find the fixed columns based on their name
  which_fix <- match(.fixed_columns(), tolower(names(seq_list)))
  which_fix <- which_fix[!is.na(which_fix)]
  if (length(which_fix) == 0L)
    return(NULL)

  names(seq_list)[which_fix] <- tolower(names(seq_list)[which_fix])
  if (!all(c("chrom", "pos") %in% names(seq_list)[which_fix]))
    return(NULL)

  # Type convert strings where possible
  convert_fn <- function(x) {
    if (!is.character(x))
      return(x)
    utils::type.convert(x, na.strings = c("", "."), as.is = TRUE)
  }
  seq_list <- purrr::map(seq_list, convert_fn)
  array_list <- purrr::map(array_list, convert_fn)

  # Remaining columns are the INFO fields
  seq_list <- as.data.frame.list(c(seq_list[which_fix], seq_list[-which_fix]))

  # Adjust FORMAT column to match $geno
  seq_list[["format"]] <- names(array_list)[[i_geno]]

  vcf <- varkit(site = seq_list, geno = array_list[[i_geno]], ...)
  if (length(array_list) == 1L)
    return(vcf)

  # Add additional slots to the VCF/related_data object
  nsamples <- n_samples.varkit(vcf)
  nsites <- n_sites.varkit(vcf)
  array_list[[i_geno]] <- NULL
  for (iname in names(array_list)) {
    ivalue <- array_list[[iname]]
    idim <- dim(ivalue)
    islot <- c(
      if (idim[1L] == nsamples) "samples" else "",
      if (idim[2L] == nsites) "sites" else "",
      rep("", length(idim) - 2L)
    )

    attr(vcf, "slots")[[iname]] <- islot
    vcf[[iname]] <- if (identical(islot, c("samples", "sites"))) {
      varkit_geno(ivalue)
    }  else if (is.matrix(ivalue)) {
      as_mtrx(ivalue)
    } else {
      ivalue
    }
  }

  vcf
}
