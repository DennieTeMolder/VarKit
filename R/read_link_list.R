##' @export
read_link_list <- function(file) {
  ll <- rjson::fromJSON(file = file)
  class(ll) <- "link_list"
  trans <- ll$translation
  if (!is.null(trans))
    attr(ll, "translation") <- trans
  ll$translation <- NULL
  ll
}
