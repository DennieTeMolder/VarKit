# Ignore these when missing from global env during devools::check()
# These variables are assigned by foreach::foreach and are therefore missed.
utils::globalVariables(c("fix", "gt"))
