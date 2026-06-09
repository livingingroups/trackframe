expect_most_elements_equal <- function(current, target, omit = c(), ...) {
  filter_list <- function(l) {
    l <- l[!names(l) %in% omit]
    l[sort(names(l))]
  }
  expect_equal(filter_list(current), filter_list(target), ...)
}
