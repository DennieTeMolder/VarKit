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
##' @param use_filter TRUE/FALSE, whether to mask all sites where the FILTER column != PASS
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
                     geno_ptype = NULL,
                     use_filter = TRUE) {
  .stopifnotsingle(n_max, what = is.numeric)
  if (!is.null(na_geno)) stopifnot(is.character(na_geno))
  .stopifnotsingle(guess_max, what = is.numeric)
  .stopifnotsingle(use_filter, what = "logical")
  col_select <- rlang::enquo(col_select)
  na.strings <- unique(c("", ".", na_geno))

  # Disable guessing if fewer variants are requested
  if (n_max >= 1 && guess_max >= n_max)
    guess_max <- -1

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
  optional_classes <- c(id = "c", ref = "c", alt = "c", qual = "n", filter = "c", info = "c", format = "c")
  optional_cols <- intersect(names(which(is.na(colClasses))), names(optional_classes))
  colClasses[optional_cols] <- optional_classes[optional_cols]

  # The remaining NAs represent the genotype columns
  geno_cols <- names(which(is.na(colClasses)))

  if (length(geno_cols) == 0L) {
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
  if (!is.null(geno_ptype)) {
    geno_class <- typeof(geno_ptype)
  } else if (guess_max >= 1) {
    .msg_step("Guessing data type of genotype matrix")
    skip_known <- rlang::rep_named(names(which(!is.na(colClasses))), "NULL")
    geno_col_classes <- do.call(guess_col_classes, c(shared_args, list(
      nmax = guess_max, col.names = names(colClasses), colClasses = skip_known
    )))

    # Try to determine the common type, this is not bullet proof, but should work for atomic types
    geno_class <- purrr::map(geno_col_classes, function(class) methods::new(class))
    geno_class <- typeof(do.call(c, geno_class))
  } else {
    geno_class <- NA_character_
  }

  colClasses[geno_cols] <- geno_class

  .msg_step("Reading VCF body")

  # We have already performed the guessing if requested, disable GUESS.MAX here
  body <- do.call(read_tbl, c(shared_args, list(col.names = colClasses, nrows = n_max, guess.max = -1)))

  if (nrow(body) < 1)
    return(NULL)

  .msg_step("Collecting results")

  # Convert columns that only contains NAs to logical for memory efficiency
  all_na <- which(purrr::map_lgl(body, function(col) all(is.na(col))))
  if (any(all_na)) body[all_na] <- NA

  as_varkit.data.frame(body, meta = header$meta, use_filter = use_filter)
}
