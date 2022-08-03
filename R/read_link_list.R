##' @export
read_link_list <- function(file) {
  ll <- rjson::fromJSON(file = file)
  ll[ll == "NA"] <- NA
  class(ll) <- "link_list"
  trans <- ll$translation
  if (!is.null(trans))
    attr(ll, "translation") <- trans
  ll$translation <- NULL
  ll
}
