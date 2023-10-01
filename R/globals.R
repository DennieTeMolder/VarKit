# 'Alias' functions from bundled R packages
head <- utils::head
tail <- utils::tail
setNames <- stats::setNames
quantile <- stats::quantile

# This fixes missing bindings withing tidy-eval
utils::globalVariables(".data")
