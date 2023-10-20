##' Load A VCF File From Disk
##'
##' @title Load A VCF File From Disk
##' @param file String, path to the vcf(.bgz) file to load
##' @param col_select Columns to include in the results. You can use the same mini-language as ‘dplyr::select()’ to refer to the columns by name. Use ‘c()’ to use more than one selection expression.
##' @param header varkit_header object, used to determine the column names of 'file'.
##' @param n_max Integer, maximum number of variants to load. Zero or negative will load all.
##' @param skip Integer, number of variants to skip before loading 'n_max' variants.
##' @param na_geno Character vector, exact string(s) to convert to NA in the genotype fields
##' @return A VarKit object
##' @export
##' @author Dennie te Molder
##' @examples
##' file <- system.file("extdata", "celegans.vcf.bgz", package = "varkit")
##' vcf <- read_vcf(file)
read_vcf <- function(file,
                     col_select = NULL,
                     header = NULL,
                     n_max = -1,
                     skip = 0,
                     na_geno = c("./.", ".|.")
                     ) {
  if (!is.null(header))
    validate_varkit_header(header)

  # Open the file if needed
  if (inherits(file, "connection")) {
    if (!is_open(file))
      stop("The provided 'file' connection is invalid!")
    con <- file
  } else {
    con <- file(file, "r")
    on.exit(close(con))
  }

  # Handle the header
  if (seek(con) != 0) {
    if (is.null(header))
      stop("A header should be supplied if connection 'file' is not at the start!")
  } else if (!is.null(header)) {
    skip <- skip + find_header_line(summary(con)$description)
  } else {
    header <- read_vcf_header(con)
    if (length(header$col_names) < 3)
      stop("At line ", length(header$meta) + 1, ": Invalid VCF, the header has fewer then three tab-separated fields!")
  }

  body <- .read_vcf_body(
    con, col_names = col_names(header), col_select = {{ col_select }},
    n_max = n_max, skip = skip, na_geno = na_geno
  )
  if (nrow(body) < 1)
    return(NULL)

  varkit(body, meta = meta(header))
}

.read_vcf_body <- function(con, col_names, col_select = NULL, n_max = -1,
                           skip = 0, na_geno = c("./.", ".|.")) {
  if (!inherits(con, "connection"))
    stop("'con' should be a file connection!")
  na_geno <- unique(c("", ".", na_geno))

  col_classes <- .make_col_classes(col_names, col_select = {{ col_select }})
  tibble::as_tibble(utils::read.delim(
    con,
    col.names = col_names,
    colClasses = col_classes,
    nrow = n_max,
    skip = skip,
    na.strings = na_geno,
    header = FALSE,
    check.names = FALSE,
    quote = "",
    as.is = TRUE,
    fill = FALSE,
    strip.white = TRUE,
    comment.char = "#"
  ))
}

.make_col_classes <- function(col_names, col_select = NULL, default = NA_character_) {
  stopifnot(is.character(col_names))
  col_select <- rlang::enquo(col_select)

  fixed_classes <- list(
    chrom = "character",
    pos = "integer",
    id = "character",
    ref = "character",
    alt = "character",
    qual = "numeric",
    filter = "character",
    info = "character",
    format = "character"
  )

  col_classes <- as.list(rep_len(default, length(col_names)))
  names(col_classes) <- col_names
  col_classes <- utils::modifyList(col_classes, fixed_classes)
  col_classes <- col_classes[col_names]

  if (rlang::quo_is_null(col_select))
    return(col_classes)

  keep <- colnames(dplyr::select(as.data.frame(col_classes, check.names = FALSE), !!col_select))
  col_classes[!names(col_classes) %in% keep] <- "NULL"
  col_classes
}
