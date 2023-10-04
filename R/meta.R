##' @export
meta <- function(x) {
  UseMethod("meta", x)
}

##' @export
meta.varkit_header <- function(x) {
  x$meta
}

##' @export
`meta<-` <- function(x, value) {
  if (!is_varkit_header(x))
    stop("`meta() <-` is only supported on 'varkit_header' objects!")
  if (!is.data.frame(value))
    stop("The new value must be a data frame!")
  if (any(colnames(value) != c("name", "value")))
    stop("The new value should have two columns named 'name' & 'value'!")

  x$meta <- value
  x
}
