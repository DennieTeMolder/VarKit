##' @export
query_vcf <- function(path,
                      range = NULL,
                      gene_name = NULL,
                      gene_path = NULL,
                      anno_type = NULL,
                      site = NULL,
                      info = NULL,
                      geno = NULL,
                      use_filter = TRUE,
                      silent = FALSE
                      ) {
  rlang::check_installed("seqminer", version = "8.0.0")

  # Read header using Seqminer for BCF support
  header <- seqminer::tabix.read.header(path)[["header"]]
  if (is.null(header))
    stop("Failed to read header! Does the file have Tabix index (ending in .tbi)?")
  header <- varkit_header(
    meta = .parse_header_meta(header[-length(header)]),
    col_names = .parse_header_line(header[length(header)])
  )

  if (!is.null(range)) {
    .stopifnotsingle(range, what = "character")
  } else {
    # HACK: this value passes seqminer::isTabixRange() and returns all variants
    range <- ":"
    silent <- TRUE
  }

  if (!is.null(gene_name)) {
    if (is.null(gene_path))
      stop("Specifying 'gene_name' requires providing a 'gene_path' with gene annotations!")
    range <- NULL
  } else {
    # Patch in support for selecting entire chromosomes/contigs
    range <- .fix_seqminer_range(range, header)
  }

  if (is.null(anno_type))
    anno_type <- ""

  # When SITE, INFO, or GENO are null, all values are used
  if (is.null(site)) {
    site <- toupper(intersect(
      header$col_names,
      setdiff(.fixed_columns(), c("info", "format"))
    ))
  } else if (is.character(site)) {
    # Convert to upper case and always include CHROM and POS
    site <- union(c("CHROM", "POS"), toupper(site))
  }
  if (is.null(info))
    info <- parse_meta.varkit_header(header, type = "INFO")[["ID"]]
  if (is.null(geno))
    geno <- parse_meta.varkit_header(header, type = "FORMAT")[["ID"]]

  # Setting SITE, INFO, or GENO to FALSE excludes them
  if (isFALSE(site))
    site <- NULL
  if (isFALSE(info))
    info <- NULL
  if (isFALSE(geno))
    geno <- NULL

  # Enable silencing of Seqminer's C++ code
  sink_fn <- if (isTRUE(silent)) .with_sink else identity

  # Call the right seqminer function
  if (!is.null(range)) {
    seq_list <- sink_fn(seqminer::readVCFToListByRange(
      path, range = range, annoType = anno_type,
      vcfColumn = site, vcfInfo = info, vcfIndv = geno
    ))
  } else if (!is.null(gene_name)) {
    seq_list <- sink_fn(seqminer::readVCFToListByGene(
      path, geneFile = gene_path, geneName = gene_name, annoType = anno_type,
      vcfColumn = site, vcfInfo = info, vcfIndv = geno
    ))
  } else {
    stop("Unexpected!")
  }

  # The query failed if all elements except the 'sampleId' are zero length
  i_sample <- match("sampleId", names(seq_list))
  lens <- purrr::map_int(seq_list[-i_sample], length)
  if (all(lens == 0L))
    return(NULL)

  # Attempt to convert to a VarKit object
  vcf <- seqminer2varkit(seq_list, meta = header$meta, use_filter = use_filter)

  if (is.null(vcf))
    return(seq_list)
  vcf
}

.fix_seqminer_range <- function(range, header) {
  splits <- stringr::str_split_1(range, stringr::fixed(","))
  splits <- stringr::str_trim(splits)

  only_chrom <- !stringr::str_detect(splits, stringr::fixed(":"))
  if (!any(only_chrom))
    return(paste(splits, collapse = ","))

  # Determine CHROM end position using the HEADER
  chrom_sizes <- parse_meta.varkit_header(header, type = "contig")
  ends <- chrom_sizes$length[match(splits[only_chrom], chrom_sizes[["ID"]])]

  if (is.null(ends) || all(is.na(ends))) {
    if (all(only_chrom))
      stop("Chromosome(s) were not described in the VCF header: ", .quote_collapse(splits), "!", call. = FALSE)
    return(paste(splits, collapse = ","))
  }

  # Add the interval that spans the entire CHROM
  intervals <- paste0(":0-", ends)
  intervals[is.na(ends)] <- ""
  splits[only_chrom] <- paste0(splits[only_chrom], intervals)

  paste(splits, collapse = ",")
}
