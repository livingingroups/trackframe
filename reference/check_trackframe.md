# Checkmate style check if x is a trackframe object

Checkmate style check if x is a trackframe object

## Usage

``` r
check_trackframe(x, ..., unsorted.ok = TRUE)

assert_trackframe(
  x,
  ...,
  unsorted.ok = TRUE,
  .var.name = checkmate::vname(x),
  add = NULL
)

expect_trackframe(x, ..., unsorted.ok = TRUE, info = NULL, label = vname(x))
```

## Arguments

- x:

  Object to check

- ...:

  arguments passed to checkmate::check_data_frame

- unsorted.ok:

  Is a trackframe not in order([id](tf_id.md), timestamp) allowed?
  Default: TRUE

- .var.name:

  `character(1)`  
  Name of the checked object to print in assertions. Defaults to the
  heuristic implemented in
  [`vname`](https://mllg.github.io/checkmate/reference/vname.html).

- add:

  [`checkmate::AssertCollection`](https://mllg.github.io/checkmate/reference/AssertCollection.html)  
  Collection to store assertion messages. See
  [`AssertCollection`](https://mllg.github.io/checkmate/reference/AssertCollection.html).

- info:

  `character(1)`  
  Extra information to be included in the message for the testthat
  reporter. See
  [`expect_that`](https://testthat.r-lib.org/reference/expect_that.html).

- label:

  `character(1)`  
  Name of the checked object to print in messages. Defaults to the
  heuristic implemented in
  [`vname`](https://mllg.github.io/checkmate/reference/vname.html).

## Value

Depending on the function prefix: If the check is successful, the
functions `asserttrackframe`/`assert_trackframe` return `x` invisibly,
whereas `checktrackframe`/`check_trackframe` and
`testtrackframe`/`test_trackframe` return `TRUE`. If the check is not
successful, `asserttrackframe`/`assert_trackframe` throws an error
message, `testtrackframe`/`test_trackframe` returns `FALSE`, and
`checktrackframe`/`check_trackframe` return a string with the error
message. The function `expect_trackframe` always returns an
[`expectation`](https://testthat.r-lib.org/reference/expectation.html).

## Examples

``` r
check_trackframe(tf_mini)
#> [1] TRUE
expect_trackframe(tf_mini)
#> Loading required namespace: testthat
assert_trackframe(tf_mini)
```
