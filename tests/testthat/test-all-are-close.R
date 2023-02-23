testthat::test_that( "Vectors are close", {
  # Generate v1 with length 100 and ~ U(2,5)
  v1 <- runif(100,2,5)
  # The absolute error is 1e-6
  # Since every element in |v1| > 1, relative error < absolute error
  v2 <- v1 + 1e-6
  testthat::expect_true(are_all_close(v1,v2,abs_tol=1e-5,rel_tol=1e-5))
  }
)

testthat::test_that( "Vectors have large absolute error", {
  # Generate v1 with length 100 and ~ U(2,5)
  v1 <- runif(100,2,5)
  # The absolute error is 1e-6
  # Since every element in |v1| > 1, relative error < absolute error
  v2 <- v1 + 1e-6
  testthat::expect_true(are_all_close(v1,v2,abs_tol=1e-10,rel_tol=1e-5))
}
)

testthat::test_that( "Vectors have large relative error", {
  # Generate v1 with length 100 and ~ U(0,1)
  v1 <- runif(100,0,1)
  # The absolute error is 1e-6
  # Since every element in |v1| < 1, relative error > absolute error
  v2 <- v1 + 1e-6
  testthat::expect_true(are_all_close(v1,v2,abs_tol=1e-5,rel_tol=1e-10))
}
)
