##' @export
meta <- function(x) {
  UseMethod("meta", x)
}

##' @export
meta.varkit <- function(x) {
  linked_data_get(x, i = "meta")
}

##' @export
meta.varkit_header <- meta.varkit

##' @export
`meta<-` <- function(x, value) {
  if (is_varkit_header(x)) {
    if (!is.data.frame(value))
      stop("The new value must be a data frame!")
    if (!all(c("name", "value") %in% colnames(value)))
      stop("The new value should have two columns named 'name' & 'value'!")
    x$meta <- tibble::as_tibble(value)
  } else if (is_varkit(x)) {
    # Validation is performed by `[<-.varkit`
    `$<-.varkit`(x, "meta", value)
  } else {
    stop("`meta() <-` is only supported on 'varkit' & 'varkit_header' objects!")
  }

  x
}
