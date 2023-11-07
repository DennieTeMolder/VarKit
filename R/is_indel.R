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

  # INDELs have a '*' at the alt position or have more then 1 character (between commas)
  nchar(x$ref) > 1L | stringr::str_detect(x$alt, "\\*|[^,]{2,}")
}
