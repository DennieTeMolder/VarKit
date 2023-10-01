##'@export
is_indel <- function(x) {
  UseMethod("is_indel", x)
}

##'@export
is_indel.varkit <- function(x) {
  ref <- site(x, "ref")
  if (anyNA(ref))
    stop("The site$ref column contains missing values")
  alt <- site(x, "alt")
  if (anyNA(alt))
    stop("The site$alt column contains missing values")

  alt <- stringr::str_split(alt, ",")
  alt_indel <- sapply(alt, function(current) {
    if ("*" %in% current)
      return(TRUE)
    any(nchar(current) > 1)
  })
  alt_indel | nchar(site(x, "ref")) > 1
}
