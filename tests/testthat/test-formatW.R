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

test_that("formatW mantém o alinhamento correto entre d1 e d2", {
    data_mock <- data.frame(
    ID = 1:3,
    FIXO = c("A", "B", "A"),
    T1 = c(10, 20, 30),
    T2 = c(15, 25, 35)
  )
  dataList <- list(data = data_mock)

  tmp <- file.path(tempdir(), "fw_test")
  withr::defer(unlink(tmp))

  formatW(dataList = dataList, of = tmp, traits = c(3, 4))

  result <- read.table(tmp, header = FALSE)

  # 3 animais × 2 traits = 6 linhas
  expect_equal(nrow(result), 6)


  # Coluna de característica (d3): deve ter valores 1 e 2
  expect_setequal(unique(result[[1]]), c(1, 2))

  # Coluna de observação (d2): deve conter os valores de T1 e T2
  expect_setequal(result[[ncol(result)]], c(10, 20, 30, 15, 25, 35))
})
