##' @export
next_chunk <- function(x, ...) {
  UseMethod("next_chunk", x)
}

##' @export
next_chunk.varkit_index <- function(x, col_select = NULL, ...) {
  args <- list(...)
  if (!rlang::is_named2(args))
    stop("All additional arguments must be named!")

  open.varkit_index(x)

  # Read the header when possible
  if (env(x)$curr_line < x$linum[1] && x$linum[1] > 1) {
    vcf_header <- read_vcf_header(env(x)$con)
    env(x)$curr_line <- env(x)$curr_line + nrow(meta(vcf_header)) + 1
    if (x$linum[1] != env(x)$curr_line)
      stop("The index should start right below the header line! Expected start: ", env(x)$curr_line)
  } else {
    vcf_header <- NULL
  }

  # NOTE Keep repeating the last element when outside of the index
  # This allows for "open ended" indexes like from index_vcf_partial()
  n_max <- seek.varkit_index(x)$n_lines
  if (n_max < 1)
    n_max <- utils::tail(x$n_lines, n = 1)

  # Fetch arguments stored on the index & merge with ...
  if (is.null(env(x)$read_vcf_args))
    env(x)$read_vcf_args <- list()
  args <- utils::modifyList(env(x)$read_vcf_args, args)

  if (is.null(args$header)) {
    if (is.null(vcf_header))
      stop("A header must be supplied if the index is not before the first entry!")
    args$header <- vcf_header
  }

  # These arguments are always overwritten
  if (!missing(col_select))
    args <- utils::modifyList(args, list(col_select = rlang::enexpr(col_select)))
  args <- utils::modifyList(args, list(file = env(x)$con, n_max = n_max))

  vcf <- do.call(read_vcf, args)

  # Store the header for subsequent calls
  if (is.null(env(x)$read_vcf_args$header))
    env(x)$read_vcf_args$header <- args$header

  if (is.null(vcf))
    return(vcf)

  # Increment index
  env(x)$curr_line <- env(x)$curr_line + n_max

  vcf
}
