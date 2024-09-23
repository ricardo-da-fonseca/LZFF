test_that("extrair extensão", {
  expect_equal(stringr::str_extract("testeNome.xlsx","(\\w+)$"), "xlsx")
})
