##' @export
seqminer2varkit <- function(seq_list, ...) {
  stopifnot(is.list(seq_list))
  .parsed_seqminer2varkit(.parse_seqminer(seq_list))
}

.parse_seqminer <- function(seq_list) {
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

  # Type convert strings where possible
  seq_list <- purrr::map(seq_list, function(x) {
    if (!is.character(x))
      return(x)
    x[stringr::str_detect(x , pattern ="^[.,|/]*$")] <- NA_character_
    utils::type.convert(x, as.is = TRUE, na.strings = "")
  })

  # Values from 'vcfIndv' are returned as arrays
  is_array <- purrr::map_lgl(seq_list, is.array)
  if (any(is_array)) {
    array_list <- seq_list[is_array]
    seq_list[is_array] <- NULL

    # Add samples to rownames and pick an array for the $geno slot (prefers FORMAT/GT)
    i_geno <- if (is.matrix(array_list[["GT"]])) match("GT", names(array_list))
    for (i in seq_along(array_list)) {
      if (!is.matrix(array_list[[i]]))
        next()

      array_list[[i]] <- as_mtrx(array_list[[i]])

      if (nrow(array_list[[i]]) == length(samples)) {
        rownames(array_list[[i]]) <- samples
        if (is.null(i_geno))
          i_geno <- i
      }
    }
  } else {
    array_list <- NULL
    i_geno <- NULL
  }

  if (length(seq_list) > 0L) {
    # Find the fixed columns based on their name
    which_fix <- match(.fixed_columns(), tolower(names(seq_list)))
    which_fix <- which_fix[!is.na(which_fix)]
    if (length(which_fix) > 0L) {
      names(seq_list)[which_fix] <- tolower(names(seq_list)[which_fix])
      # Remaining columns are the INFO fields, they go last
      seq_list <- c(seq_list[which_fix], seq_list[-which_fix])
    }

    site <- tibble::as_tibble(seq_list)
  } else {
    site <- NULL
  }

  list(site = site, geno = array_list, i_geno = i_geno)
}

.parsed_seqminer2varkit <- function(parsed_list, ...) {
  if (is.null(parsed_list) || any(purrr::map_lgl(parsed_list, is.null)))
    return(NULL)
  if (!all(c("chrom", "pos") %in% names(parsed_list$site)))
    return(NULL)

  # Adjust FORMAT column to match GENO
  parsed_list$site$format <- with(parsed_list, names(geno)[[i_geno]])
  geno <- with(parsed_list, geno[[i_geno]])

  vcf <- varkit(site = parsed_list$site, geno = geno, ...)
  if (length(parsed_list$geno) == 1L)
    return(vcf)

  # Add additional slots to the VCF/related_data object
  parsed_list$geno[[parsed_list$i_geno]] <- NULL
  nsamples <- n_samples.varkit(vcf)
  nsites <- n_sites.varkit(vcf)
  for (iname in names(parsed_list$geno)) {
    ivalue <- parsed_list$geno[[iname]]
    idim <- dim(ivalue)
    islot <- c(
      if (idim[1L] == nsamples) "samples" else "",
      if (idim[2L] == nsites) "sites" else "",
      rep("", length(idim) - 2L)
    )

    if (identical(islot, c("samples", "sites")))
      ivalue <- varkit_geno(ivalue)

    attr(vcf, "slots")[[iname]] <- islot
    vcf[[iname]] <- ivalue
  }

  vcf
}
