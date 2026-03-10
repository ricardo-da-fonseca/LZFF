test_that("formatB is working correctly", {
  # Simulating rrcData output
  simudata<-list(
    map = data.frame(code = letters[1:9], recode = 1:9),
    ped = data.frame(id = 7:9, sire = 4:6, dam = 1:3),
    data = data.frame(id = 7:9, trait = c(5.1, 8.9, 12.3)))

  temp_file<-tempfile(fileext = ".txt")

  formatB(dataList = simudata, of = temp_file)

  # Reading the formatB output
  dataRead <- readLines(temp_file)

  # checking if the file exists
  expect_true(file.exists(temp_file))

  # Checking the content
  expect_equal(length(dataRead), 3)
  expect_equal(dataRead[1], "7 5.1")
  expect_equal(dataRead[2], "8 8.9")
  expect_equal(dataRead[3], "9 12.3")

  # Clear the temporary file
  unlink(temp_file)
})

test_that("formatB throw an error when file's name contains a #", {
  # Creating data
  simudata <- data.frame(id = 1:2, trait = c(10, 20))

  # Attempting to use a file's name with a #
  expect_error(
    formatB(datalist = simudata, of = "filewitha#.txt")
  )
})
