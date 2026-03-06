test_that("Effects columns are formatted correctly",{
  expect_equal(nrow(udata) * length(traits), nrow(d1))
  expect_equal(length(udata) - length(traits), length(d1))
            })

test_that("Traits column is created", {
  expect_equal(nrow(udata) * length(traits), length(d2))
  expect_equal(class(d2), "numeric")
})

test_that("Number of traits column is created", {
  expect_equal(length(traits) * nrow(udata), length(d3))
  expect_equal(d3[1:(length(d2)/length(traits))], rep(1,length(d2)/length(traits)))
})
