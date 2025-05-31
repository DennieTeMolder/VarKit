##' Load A VCF File From Disk
##'
##' @title Load A VCF File From Disk
##' @param file String, path to the vcf(.bgz) file to load
##' @param col_select Columns to include in the results. You can use the same mini-language as ‘dplyr::select()’ to refer to the columns by name. Use ‘c()’ to use more than one selection expression.
##' @param header varkit_header object, used to determine the column names of 'file'.
##' @param n_max Integer, maximum number of variants to load. Zero or negative will load all.
##' @param na_geno Character vector, exact string(s) to convert to NA. Empty strings and "." are always converted to NA
##' @param guess_max Integer, number of variants to use for guessing the type/class of the genotype matrix, negative to disable.
##' @param geno_ptype Vector/array, prototype of the genotype matrix (i.e. double()). When provided guess_max is ignored.
##' @return A VarKit object
##' @export
##' @author Dennie te Molder
##' @examples
##' file_path <- system.file("extdata", "celegans.vcf.bgz", package = "varkit")
##' vcf <- read_vcf(file_path)
read_vcf <- function(file,
                     col_select = NULL,
                     header = NULL,
                     n_max = -1,
                     na_geno = c("./.", ".|."),
                     guess_max = 100,
                     geno_ptype = NULL
                     ) {
  stopifnot(is.numeric(n_max), length(n_max) == 1)
  if (!is.null(na_geno)) stopifnot(is.character(na_geno))
  stopifnot(is.numeric(guess_max), length(guess_max) == 1)
  col_select <- rlang::enquo(col_select)
  na.strings <- unique(c("", ".", na_geno))

  # Disable guessing if a prototype is provided or fewer variants are requested
  if (!is.null(geno_ptype) || (n_max > 0L && guess_max >= n_max))
    guess_max <- -1L

  # Turn file into a connection
  if (!.is_connection(file)) {
    file <- file_con(file, "r")
    .defer(close(file))
  }

  # NOTE the header part of the file is skipped by "comment.char" if not read here
  if (is.null(header)) {
    .msg_step("Reading VCF header")
    header <- read_vcf_header(file)
  }

  validate_varkit_header(header)

  # Apply 'col_select' to header$col_names, which allows the user to rename columns
  colClasses <- rlang::rep_named(header$col_names, NA_character_)
  colClasses <- .col_select_classes(colClasses, col.select = col_select)

  if (!all(c("chrom", "pos") %in% names(colClasses)))
    stop("The 'chrom' and/or 'pos' columns are missing from the VCF!")

  # Fill in known classes, chrom & pos cannot be skipped, others are optional
  colClasses[c("chrom", "pos")] <- c("c", "i")
  na_cols <- names(which(is.na(colClasses)))
  optional_classes <- c(id = "c", ref = "c", alt = "c", qual = "n", filter = "c", info = "c", format = "c")
  colClasses[na_cols] <- optional_classes[na_cols]

  # The remaining NAs represent the genotype columns
  if (!anyNA(colClasses)) {
    null_geno <- setdiff(names(which(colClasses == "NULL")), names(optional_classes))
    if (length(null_geno) > 0L)
      stop("All genotypes were removed by 'col_select'!")
    stop("The VCF only contains fixed columns, genotypes are missing!")
  }

  # Shared arguments for reading the body
  shared_args <- list(
    file = file, na.strings = na.strings, header = FALSE, check.names = FALSE,
    quote = "", as.is = TRUE, fill = FALSE, strip.white = TRUE, comment.char = "#"
  )

  # The genotypes will be combined into a single matrix, coercing all values to a shared type
  # Knowing the type beforehand improves the speed & memory usage of read_tbl & simplifies coercion
  geno_class <- if (is.null(geno_ptype)) NA_character_ else typeof(geno_ptype)

  if (guess_max >= 1L) {
    .msg_step("Guessing data type of genotype matrix")
    skip_known <- rlang::rep_named(names(which(!is.na(colClasses))), "NULL")
    geno_col_classes <- do.call(guess_col_classes, c(shared_args, list(
      nmax = guess_max, col.names = names(colClasses), colClasses = skip_known
    )))

    geno_class <- lapply(geno_col_classes, function(class) methods::as(0.0, class))
    geno_class <- class(do.call(c, geno_class))[1L]
  }

  colClasses[is.na(colClasses)] <- geno_class

  .msg_step("Reading VCF body")
  body <- do.call(read_tbl, c(shared_args, list(col.names = colClasses, nrows = n_max)))

  if (nrow(body) < 1)
    return(NULL)

  .msg_step("Collecting results")

  # Move fixed fields to a separate df and convert genotypes to matrix
  geno_cols <- setdiff(na_cols, names(optional_classes))
  fxd <- body[, !colnames(body) %in% geno_cols]
  body <- body[, geno_cols]
  body <- varkit_geno(body)

  # Convert columns that only contains NAs to logical for memory efficiency
  fxd <- dplyr::mutate(fxd,
    dplyr::across(tidyselect::where(~ all(is.na(.x))), as.logical)
  )

  varkit(site = fxd, geno = body, meta = meta(header))
}
