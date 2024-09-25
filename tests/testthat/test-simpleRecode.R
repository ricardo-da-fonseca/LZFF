test_that("Recoding", {
  expect_equal(is.data.frame(a<-simpleRecode(c("2","3","a34"))),TRUE)
  })
