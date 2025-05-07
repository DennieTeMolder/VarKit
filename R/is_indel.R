##'@export
is_indel <- function(x) {
  UseMethod("is_indel", x)
}

##'@export
is_indel.varkit <- function(x) {
  x <- site.varkit(x, "ref", "alt")
  if (anyNA(x$ref))
    stop("The site$ref column contains missing values")
  if (anyNA(x$alt))
    stop("The site$alt column contains missing values")

  # INDELs have more than 1 character (ALT can be comma separated) or has a '*' at the ALT position
  nchar(x$ref) > 1L | stringr::str_detect(x$alt, "\\*|[^,]{2}")
}
