# NA

## IMPORTANT

Important Information for Developers:

In order to deal with `data.frame`, `data.table` and `tibble` objects at
the same time we overwrite the `"[.data.frame"` accessor. Hence, when
executing code manually the file `R/subset.R` needs to be sourced.
