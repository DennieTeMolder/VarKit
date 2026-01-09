##' @export
meta <- function(x) {
  UseMethod("meta", x)
}

##' @export
meta.varkit <- function(x) {
  x$meta
}

##' @export
meta.varkit_header <- meta.varkit

##' @export
`meta<-` <- function(x, value) {
  if (!(is_varkit(x) || is_varkit_header(x)))
    stop("`meta() <-` is only supported on 'varkit' & 'varkit_header' objects!")
  if (!is.data.frame(value))
    stop("The new value must be a data frame!")
  if (!identical(colnames(value), c("name", "value")))
    stop("The new value should have two columns named 'name' & 'value'!")

  x$meta <- value
  x
}
