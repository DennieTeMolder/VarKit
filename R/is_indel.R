##'@export
is_indel <- function(x) {
  UseMethod("is_indel", x)
}

##'@export
is_indel.varkit <- function(x) {
  .is_indel(site.varkit(x, "ref"), site.varkit(x, "alt"))
}

.is_indel <- function(ref, alt) {
  if (.any_na(ref))
    stop("The site$ref column contains missing values")
  if (.any_na(alt))
    stop("The site$alt column contains missing values")

  # INDELs have more than 1 character (ALT can be comma separated) or has a '*' at the ALT position
  nchar(ref) > 1L | stringr::str_detect(alt, "\\*|[^,]{2}")
}
