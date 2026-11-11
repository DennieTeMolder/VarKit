# Changelog
## v0.9.1
Breaking changes:
 - `get_col_types()`: rename to `get_info_types()`
 - `varkit_linkage_graph`: switch to using absolute/unmasked indices
 - `sha1.varkit()`: remove function in favor of `varkit_hash()`
 - `find_linked_sites()/find_linked_cols()`: change default arguments to `windows = 2` & `return_r2 = TRUE`
 - `resolve_links()`: change default algorithm to "greedy"
 - `summary.varkit_linkage_graph()`: remove `vcf` argument
 - `index_vcf()`: include `size_bp` in the 'id' column

Bugfixes:
 - `[<-.varkit()`: retain sample indices when possible
 - `standardize_cols.varkit()`: drop unrecognized columns in `$site` for parity with `standardize_cols.varkit_header()`
 - `parse_geno()`: fix no slot registered for type error
 - `match_rd()/rd_set()`: fix incorrect results when multiple dimensions shared the same index
 - `splitm()`: fix error when empty levels are present

Changes:
 - `parse_*()`: don't raise a warning when `type` is omitted
 - `collapse_alt()`: allow 'alt_idx' to be a scalar and check all values are in range
 - `new_varkit(): add `sample_keys` & `site_keys` arguments

Features:
 - `rd_shrink()`: new function to remove filtered/masked data
 - `seqminer2varkit()`: new function to convert lists from the Seqminer2 package to varkit objects
 - `query_vcf()`: wrapper around Seqminer that allows fast partial access to Tabix-indexed VCF files
 - `unmask_linked()`: new function to unmask linked sites of visible sites using a 'varkit_linkage_graph'
 - `bi_gt2dosage()`: new function to convert GT fields to dosage
 - `concat_links()`: new function for merging results from `find_linked_*()`
 - `within_vcf()/within_rd()`: new functions, the `within()` equivalent of `with_vcf()/with_rd()`
 
## v0.9.0
Breaking changes:
- `varkit`: now extends the `related_data` class meaning methods like `$` and `[` now also exclude masked sites
- `summary.varkit()`: now returns a list of two dataframes with different statistics
- `data_relation_model`: remove class and associated functions/methods in favor of the new `related_data` class
- `varkit_geno`: remove class and associated methods
- `varkit_geno()`: no longer transposes dataframes, use `as_varkit()` instead
- `remove_masked()`: remove deprecated function
- `get_info_col_types()`: renamed to `get_col_types()`
- `[read|write]_graph()`: renamed to `[read|write]_linkage_graph()`

Bug fixes:
- `print.mtrx()`: fix warning when matrix is empty
- `print.varkit()`: fix `$geno` not having the right width when a mask is active
- `str.mtrx()`: fix errors for small matrices
- `write_vcf()`: only overwrite NA values in the FILTER column when `replace_filter=TRUE`
- `mask()`: fix `na_mask=TRUE` not masking NAs

Changes:
- `get_site_idx()`: deprecate function in favor of `has_join()` and `is_linked()`
- `new_varkit()`: cast elements to the expected types (tibble/mtrx)
- `print.mtrx()`: no longer reports percentage NA, only if NAs are present
- `with_vcf()`: now uses lazy evaluation internally for better efficiency

Features:
- `read_vcf()`: add the `use_filter` argument to control if FILTER column is used for initial masking
- `as_varkit()`: new function for converting dataframes, reverse complement of `as.data.frame.varkit()`
- `print.mtrx()`: add the `tooltip` argument to control printing of hint comments
- `site()`: add `.drop` argument for parity with `geno()`
- `parse_info()`: allow prefixing tags in the `type` argument with 'INFO/'
- `parse_geno()`: allow prefixing tags in the `type` argument with 'FORMAT/' or 'FMT/'
- `related_data`: new class that generalizes VarKit's masking functionality for use with non-VCF data of all dimensions
  - `related_data()`: create an (empty) `related_data` object
  - `rd_register()`: register new slots and indices to a `related_data` object
  - `rd_set()`: populate slots with data whilst reordering data based on keys provided as `...` argument
  - `rd_get()`: retrieve data associated to a slot whilst allowing efficient merging of external indices
  - `rd_filter()`: filter/mask indices with support for tidy evaluation for fast access to slot data
  - `rd_reset()`: undo filtering/masking
  - `rd_trim()`: remove unused indices and keys to save on memory
  - `rd_shrink()`: bake filters into the underlying data so they cannot be undone to save on memory
  - `rd_dim()`: efficiently obtain the dimensions of populated slots
  - `rd_dimnames()`:  efficiently obtain the dimnames of populated slots
  - `with_rd()`: implementation of `with()` for `related_data` objects 
  - `match_rd()`: key-match an external object to a `related_data` object
  
## v0.8.7
Bug fixes:
- `print.mtrx()`: fix 'row_idx' error when no rows are hidden (also affects `print.varkit_geno()` & `print.varkit()` by extenstion)

Changes:
- `[.varkit_geno()`: default to 'drop = FALSE'
- `mtrx()`: mimic behavior & arguments of `matrix()`

Features:
- `as_mtrx()`: new function, analogue of `as.matrix()`
- `[.mtrx()`: new method, retains 'mtrx' class using 'drop = FALSE' as a default

## v0.8.6
Bug fixes:
- `mask_sites_without_variation()/has_no_variation()`: fix incorrect results when 'margin' == 2
- `parse_geno()`: fix error when `n_samples() == 1`
- `next_chunk()`: fix index getting out of sync on masked/soft-filtered files
- `seek.varkit_index()/next_lines()`: fix index getting out of sync when file contains comments
- `summary.varkit_linkage_graph()`: fix incorrect 'n_singletons'

Changes:
- `read_vcf()`: remove 'skip' argument
- `next_lines()`: remove 'skip' argument
- `apply_samples()`: deprecate function in favor of `apply(MARGIN = "samples")`
- `apply_sites()`: deprecate function in favor of `apply(MARGIN = "sites")`

Features:
- `get_site_idx.data.frame()`: add 'join_by' argument
- `find_header_line()`: support connections as input types
- `skip_lines()`: new function to skip lines from a connection in chunks with progress updates
- `next_chunk()/seq_index()/apply_index()`: add support for read_vcf()'s 'n_max' argument
- `read_vcf_args()`: new function to get/set arguments associated with an index
- `has_join()`: new function to check if a row has a `dplyr::*_join()` partner for use in `mask/retain()`
- `is_linked()`: new function to linkage prune using a 'varkit_linkage_graph'
- `mtrx`: new class that generalizes the pretty printing from `varkit_geno` 
- `data_relation_model`: new class with associated methods and additional functions

## v0.8.4-v0.8.5
Bug fixes:
- `splitm()`: fix `seq_len(n)` error when `is.vector(x)`
- `retain()`: fix early evaluation errors for 'site' argument
- `geno()`: fix tidyselect renaming error when `drop = TRUE` has effect
- `ad2aaf()`: produce valid output if FORMAT/AD has missing values

Changes:
- `write_vcf()`: package version is now included in the timestamp
- `guess_col_classes()`: now assumes the first line contains 'col.names' if not provided similar to `read_tbl()`
- `get_first_non_na_rowwise()`: rename to `get_frist_non_na()` and add 'margin' argument
- `parse_info_all()`: return somewhat sensible values when the INFO column is NA or when no INFO types are declared

Features:
- `write_vcf()`: faster NA checking and replacement of the genotype matrix
- `write_[tbl|vcf]`: add the 'nsignif' argument that defaults to `getOption("digits")`
- `with_vcf()`: new function to tidyeval an expresion with data pronouns ('.', 'meta', 'site', 'geno', and all columns of 'site')
- `mask()/retain()`: add support for data pronouns of `with_vcf()`
- `read_vcf_header()`: add 'max_lines' argument (default increased from 1e5 to 1e6)

## v0.8.3
Bug fixes:
- `write_vcf()`: fix guessing 'na_geno' when sites with an identical format had different numbers of possible alleles

Changes: 
- `write_vcf()`: 'na_geno' can now be vector giving the NA value for each site

Features:
- `write_tbl/vcf()`: add 'digits' arguments (defaults to 6 for `write_vcf()`) to save disk space
- `get_first_non_na_rowwise()`: new function that returns the first non NA value in each row

## v0.8.2
Breaking changes:
 - `remove_masked()`: Renamed to `delete_masked_sites()`
 - `has_no_variation()`: Swapped order of 2nd and 3rd argument ('na.rm' and 'margin')
 - `mask()`: Now mask NA values in 'site' by default
 - `retain()`: Now retains NA values in 'site' by default
 - `as_genind()`: Replace with `as_genlight()`
 - `read_vcf()`: Using non-seekable file connection now requires specifying the 'append' argument

Changes:
 - `get_site_idx.data.frame()`: Now also uses the ALT column for matching when available
 - `apply_index()`: Now uses a custom progress message that does not disappear randomly

Bug fixes:
 - `read_tbl()`: Fix missing TRUE/FALSE error
 - `next_chunk()`: Fix malformed arguments in certain cases when ... contains a data.frame

Features:
 - `ad2aaf()`: New arguments 'ad\_cap', to limit the weight of individual samples on selecting the most abundant alt allele, and 'alt\_idx' for user selection of the alternative allele.
 - `collapse_alt()`: New function to select single alleles in the ALT column using 'alt\_idx'
 - `seek.varkit_index()`: Store the VCF header when passing over it, so the user doesn't need to supply it when calling `next_chunk()`
 - `has_no_variation()`: Improved performance when 'na.rm = TRUE'
 - `get_site_idx()`: Add 'na\_get' argument (FALSE by default)
 - `mask()`: Add 'na\_mask' argument (TRUE by default)
 - `retain()`: Add 'na\_retain' argument (TRUE by default)
 - `write_vcf[_header]()`: Add the 'timestamp' argument to control the addition of a new line to the VCF header
 - `which_nth_lowest()`: New function for finding specific ranks within matrices
 - `separate_mat_to_3d()`: New function to split a delimited character matrix into a 3d (numerical) array
 - `unite_3d_to_mat()`: New function that is the reverse operation of `separate_mat_to_3d()`
 - `write_tbl()`: Add 'scipen' argument similar to base R option

## v6.0.1-v0.8.1
Breaking changes:
 - `find_linked_*()`: add `size_bp` which impacts the value of `window`, it no longer returns a 'link_list' but data frame of linked indices. Leverage BLAS and add the `return_r2` option. Within CHROM parallelization is now possible.
 - `link_list`: remove class and related functions (replaced by Igraph)
 - `spec_vcf()`: remove function as Readr is no longer a dependency
 - `parse_*()`: replaces the `get_*_type()` and `parse_*_type()` functions. Lists options if `type = NULL`
 - `not_variable()`: rename to `has_no_variation()`, increase speed 5-fold and reduce memory usage
 - `parse_info()`: renamed to `parse_info_all()`
 - `to_tibble()`: replaced with `as_tibble()`
   
Features:
 - Add constructors and validators for all VarKit classes
 - `index_vcf()`: creates a line-wise CHROM index of a vcf. Use `size_bp` to subset into chunks. 
 - `seek.varkit_index()`: move to a specific line or chunk without reading the entire file
 - `index_vcf_header()`: quickly creates sparse index that allows you to load a file in chunks without having to index it fully first.
 - `read/write_vcf_index()`: IO for index objects
 - `get_vcf_index()`: wrapper that will load the index from disk if available, else it will index the file and store the index.
 - `next_block/lines()`: read data in chunks from a index
 - `retain()`: complement to `mask()` which masks all sites not provided
 - `is_indel()`: predicate function listing for each site if it is an INDEL
 - `ad2aaf()`: compute the Alternative Allele Fraction (AAF) from Allelic Depth (AD) field.
 - `num2gt()`: convert numeric values to GenoType (GT) calls using `cutoff`
 - `parse_info()`: extract a single sub field from the INFO column
 - `split.varkit()`: new method for `split()` that can be used to split varkit objects by variant
 - `concat_vcf()`: combine multiple 'varkit' objects.
 - `resolve_links()`: processes the output of `find_linked_*()` and returns an Igraph indicating how to optimally remove linkage. This graph can be passed to `mask()`
 - `find_header_line()`: returns the line number of the header line
 - `spltim()`: extension of the base R `split()` function that has a `margin` argument and enables additional splitting by size, distance, and NA pattern.
 - `compute_comparisons()`: returns an array of indices of all pair-wise comparisons within a single or between two vectors, the `window` argument restricts the range of comparisons
 - `pcor_pairwise()`: fast version of `cor(use = "pairwise.complete.obs", method = "pearson")` that leverages BLAS
 - `r2_cutoff()`: faster version of `pcor_pairwise()` that only returns values with a SQUARED correlation above `cutoff`
 - `read_tbl()`: wrapper of `read.delim()` that has the `guess.max` option which enables faster and more memory efficient reading and uses a `dplyr::read_*()` like syntax for `colClasses`. It supports reading from connections
 - `guess_col_classes`: generate a value for `read_tbl()/read_delim()` `colClasses` argument
 - `write_tbl()`: thin wrapper around `write.table()` with more sensible defaults
 - `get_info_col_types()`: get the types of the info sub-fields based on `meta()`
 - `as_genind()`: convert a 'varkit' object to a 'genind' object
 - `as_vcfR()`: convert a 'varkit' object to a 'vcfR' object
   
Bug fixes:
 - `varkit_help()`: fix some functions not being printed
 - `parse_geno()`: fix error when `type = NULL` and VCF had a single site
 - `write_vcf()`: fix incorrect NA conversion when the FORMAT column was variable
   
Changes:
 - `read/write_vcf()`: use `read/write_tbl()` internally to support reading from connections, add `col_select` argument similar to `readr::read_*()`, and enable appending for non compressed files, add argument to control level of compression.
 - `meta/site/geno/samples()`: allow assignment using '<-'
 - `site()`: enable selection of columns using `dplyr::select()` syntax, remove the `by_chrom` argument (replaced by `splitm()`)
 - `geno()`: enable selection of samples using `dplyr::select()` syntax, remove the `by_chrom` argument (replaced by `splitm()`)
 - `parse_*`: improve speed & reduce string pool utilisation
 - `mask_sites_without_variation()`: add `parse_geno_type` argument
 - `import_mask()`: new `add` argument for combining with existing mask
 - Use CLI to improve formatting of many print functions & add progress indicators
 
## v0.5.0
Changes:
- BREAKING changed `fixed()` to `site()` to avoid collision with tidyverse/stringr 

## v0.4.1
- This changelog also covers the v0.4.0 release

Features:
- `find_identical_cols()/find_linked_cols()`: Rework algorithm to reduce the impact of NAs
- `parse_format_type()`: improve memory efficiency
- `mask()`: add ability to modify the FILTER column for masked sites
- `fixed()`: efficiently subset columns using the 'cols' arguments
- `read_vcf_header()`: properly detect header line, drop the 'max_header_lines' argument
- `parse_meta_type()/parse_info()`: improve parsing robustness and speed, convert types where possible

Bug fixes:
- Fix `geno()` and `fixed()` not always being in the same order when using `by_chrom = TRUE`

Changes:
- BREAKING `read/write_link_list()`: new format with more speed/efficiency, drop "rjson" dependency
- BREAKING Change all factors to character vectors
- `read_vcf()`: drop 'ploidy' argument in favor of 'na_geno'
- `compare_cols()`: unify code base of find_x_cols() functions
- `compare_sites()`: unify code base of find_x_sites() functions

## v0.3.1-v0.3.2
Features:
- `to_tibble()` now works for `varkit` objects

Bug fixes:
- Fix print width of `varkit_geno` matrices
- Fix !is.matrix error when there is only one sample in `print.varkit_geno()`

Changes:
- Deprecate the `varkit_meta` class
- Rename `summary.varkit_meta()` -> `get_meta_types()` 

## v0.3.0
Features:
- Add functions to parse complex fields:
  - Convert the INFO column to a df with `parse_info()` 
  - Convert a specific FORMAT from the GENO fields using `parse_format_type()`
  - List available FORMATs using `format_summary()`
- `varkit_help()`: only list varkit methods
-  Improved printing of varkit specific objects

Bug fixes:
- `apply_...()`: correctly handle single sample VCFs

Changes:
- Remove `as.data.frame.varkit_geno()` as it had non standard behavior
- `apply_...()`: Drop support for "varkit" objects, use `geno(vcf)` instead
- `print.varkit_geno()`: Standardise arguments, drop "n_samples", rename "n_sites" -> "n"

## v0.2.7
Features:
- New functions to parse format fields (`parse_format_type()`)
- Improved UX and NA recognition for `write_vcf()`
- Many fixed columns are now stored as factors for memory efficientcy

## v0.2.6
Features:
- Add `write_bcftools_regions()` and `write_bcftools_samples()`
- Expanded vignette varkit.Rmd
- Minor speed improvements

## v0.2.5
Features:
- New introduction vignette: varkit.Rmd
- Add `apply_samples()` and `apply_sites()` functions

Bug fixes:
- Handle chromosomes with a single site without crashing in `find_identical_sites()` and `find_linked_sites()`
- Ensure that `varkit_filter()` always returns a matrix

## v0.2.2-v0.2.4
Features:
- New function for `vakit` objects: `mask_sites_without_variation()`
- Rename 'sites_frac' column in `summary.varkit()` to 'visible_frac'
- Improved speed of `fixed()` for objects without masks

Bug fixes:
- Correctly handle NAs in the FILTER field in `write_vcf()` and `varkit()` (which is used by `read_vcf()`)
- Convert NAs back from string to logical in `read_link_list()`

## v0.2.1
Feature changes:
- Rename `translate()` -> `index2key()`
- Provide the reverse operation with `key2index()`
- Rework how 'translation' is specified in `lookup()`
- Rename `.merge_link_list()` -> `.concat_link_lists()` and rework internals

Bug fixes:
- Resolve duplicates in translation layer of the 'link_list' returned by `find_linked_sites()`

## v0.1.1
The function `read_vcf_header()` no longer raises any errors and thus can be
used to diagnose/validate VCF files before loading. All of the checks are
instead offloaded to `read_vcf()`. These changes make it possible to load
headerless TSV files, although a warning will be given.

## v0.1.0
Initial (Beta) release
