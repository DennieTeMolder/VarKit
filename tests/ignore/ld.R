### Setup ------

# TODO Load VCF here

### Single chromosome ------
# TODO Subset for single chromosome testing

mat <- wmpp[,siteFiltered$chrom == chrom]
pos <- siteFiltered$pos[siteFiltered$chrom == chrom]
length(pos)

## system.time(res <- FindLinkedCols(mat, pos, window = 3e4))
## length(RemoveWhich(res))

# Compute linkage on full matrix for comparison
ldTime <- system.time(ld <- .cor_pair_silent(mat)^2)
print(ldTime[["elapsed"]])

# Baseline for benchmark
baseline <- data.frame(s = 1, time = 0)
baseline$miss <- .missed_correlations(ld)

# Binsizes to benchmark
maxSize <- diff(range(pos))
sizes <- c(1e3, 1e4, 3e4, 5e4, 7e4, 1e5, maxSize)

# Benchmark
benchmark <- map_df(sizes, function(s) {
  cat(s)
  # Run function with current size
  time <- system.time(
    res <- find_linked_cols(mat, pos, window = s)
  )
  time <- time[["elapsed"]]
  cat(": ", time, "s", sep = "")
  # Check for missed correlations
  miss <- .missed_correlations(ld, linkList = res)
  cat(",", miss, "missed\n")
  # Return as df row
  return(c(s = s, miss = miss, time = time))
})

benchmark %>%
  bind_rows(baseline) %>%
  ggplot(aes(time, miss, label = s)) +
    geom_line() +
    geom_label() +
    ylim(c(0, baseline$miss)) +
    ggtitle("Bin size trade-off when detecting linkage", subtitle = chrom) +
    xlab("Computation Time (s)") +
    ylab("# Correlated Variables Remaining")

## ggsave("../Figures/window_size_tradeoff.pdf", width = 7, height = 5)

# Absolutely linked columns
system.time(res <- find_identical_cols(mat, pos))
length(which_remove(res))
.missed_correlations(ld, 1, res)

### Multiple chromosomes ------
# Subset for parallel testing
chrom <- siteFiltered %>%
  group_by(chrom) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  tail(100) %>%
  .$chrom

mat <- wmpp[,siteFiltered$chrom %in% chrom]
loc <- siteFiltered[siteFiltered$chrom %in% chrom,]
nrow(loc)

## Identical columns ------
system.time(find_identical_sites(mat, loc, cores = 1, verbose = F))
system.time(res2 <- find_identical_sites(mat, loc, cores = 10))

# Check if there are no duplicates in the resulting list
total <- c(as.integer(names(res2)), which_remove(res2))
stopifnot(!duplicated(total))
stopifnot(length(total) == nrow(loc))

# Check if all linked columns fall in the same contig
tmp <- map2(names(res2), res2, ~as.integer(c(.x, .y[!is.na(.y)])))
stopifnot(map_lgl(tmp, function (x) {
  length(unique(loc$chrom[x])) == 1
}))

## Linked columns ------
system.time(find_linked_sites(mat, loc, window = 5e4, cores = 1, verbose = F))
system.time(res3 <- find_linked_sites(mat, loc, window = 5e4, cores = 10))

# Check if there are no duplicates in the resulting list
total <- c(as.integer(names(res3)), which_remove(res3))
stopifnot(!duplicated(total))
stopifnot(length(total) == nrow(loc))

# Check if all linked columns fall in the same contig
tmp <- map2(names(res3), res3, ~as.integer(c(.x, .y[!is.na(.y)])))
stopifnot(map_lgl(tmp, function (x) {
  length(unique(loc$chrom[x])) == 1
}))
