test_that("check_integer", {
  err <- rlang::catch_cnd(check_integer("a", name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "numeric scalar")
  expect_equal(err$not, "character")

  err <- rlang::catch_cnd(check_integer(1:2, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "length 1")
  expect_equal(err$not, 2L)

  err <- rlang::catch_cnd(check_integer(NA_integer_, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "non-missing")

  err <- rlang::catch_cnd(check_integer(NULL, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "numeric scalar")

  err <- rlang::catch_cnd(check_integer(-1, lb = 0L, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "greater than 0")

  err <- rlang::catch_cnd(check_integer(1, ub = 0L, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "less than 0")

  err <- rlang::catch_cnd(check_integer(4L, lb = 0L, ub = 3L, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "between 0 and 3")

  err <- rlang::catch_cnd(check_integer(0, lb = 0, inclusive = FALSE,
                                        name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "greater than 0")

  expect_equal(check_integer(5, name = "check1"), 5L)
  expect_equal(check_integer(5L, name = "check1"), 5L)
  expect_equal(check_integer(0, lb = 0, inclusive = TRUE, name = "check1"), 0L)
  expect_equal(check_integer(6, lb = 0, name = "check1"), 6L)
  expect_equal(check_integer(NULL, name = "check1", allow_null = TRUE), NULL)
})

test_that("check_double", {
  err <- rlang::catch_cnd(check_double("a", name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "numeric scalar")
  expect_equal(err$not, "character")

  err <- rlang::catch_cnd(check_double(list(c(1, 2), 3), name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "length 1")
  expect_equal(err$not, 2L)

  err <- rlang::catch_cnd(check_double(NA_real_, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "non-missing")

  err <- rlang::catch_cnd(check_double(-1, lb = 0L, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "greater than 0")

  err <- rlang::catch_cnd(check_double(1, ub = 0L, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "less than 0")

  err <- rlang::catch_cnd(check_double(4L, lb = 0L, ub = 3L, name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "between 0 and 3")

  err <- rlang::catch_cnd(check_double(0, lb = 0, inclusive = FALSE,
                                       name = "check1"))
  expect_s3_class(err, "error_bad_argument")
  expect_equal(err$arg, "check1")
  expect_match(err$message, "greater than 0")

  expect_equal(check_double(0.98, name = "check1"), 0.98)
  expect_equal(check_double(0.1, name = "check1"), 0.1)
  expect_equal(check_double(0, lb = 0, inclusive = TRUE, name = "check1"), 0L)
  expect_equal(check_double(0.975, lb = 0, ub = 1, name = "check1"), 0.975)
})
