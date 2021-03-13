test_that("`mem_if_not()` memoises a new function", {
  skip_if_not_installed("memoise")
  f <- function(x) return(x)
  f_memoised <- mem_if_not(f)
  expect_false(memoise::is.memoised(f))
  expect_true(memoise::is.memoised(f_memoised))
})

test_that("`mem_if_not()` does not memoise already memoised functions", {
  skip_if_not_installed("memoise")
  f_mem        <- memoise::memoise(function(x) return(x))
  f_mem_if_not <- mem_if_not(f_mem)
  expect_equal(f_mem, f_mem_if_not)
})

test_that("`read_inv_positive()` is memoised", {
  skip_if_not_installed("memoise")
  expect_true(memoise::is.memoised(read_inv_positive))
})

test_that("`read_pcr_positive()` is memoised", {
  skip_if_not_installed("memoise")
  expect_true(memoise::is.memoised(read_pcr_positive))
})

test_that("`add_in_nbs()` is memoised", {
  skip_if_not_installed("memoise")
  expect_true(memoise::is.memoised(add_in_nbs))
})

test_that("`add_recent_test()` is memoised", {
  skip_if_not_installed("memoise")
  expect_true(memoise::is.memoised(add_recent_test))
})
