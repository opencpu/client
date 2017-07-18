context("External: Java")

test_that("Use read xlsx works",{
  # Check if server has xlsx
  req <- ocpu('/library/xlsx/info', stop_on_error = FALSE)
  if(req$status != 200)
    skip("Skipping Java test, 'xlsx' is not installed")

  # Tests xlsx::read.xlsx2()
  args <- list(sheetIndex = 1, file = I('system.file("tests", "test_import.xlsx", package = "xlsx")'))
  test_import1 <- ocpu_post_multipart('/library/xlsx/R/read.xlsx2', args)
  expect_is(test_import1, "data.frame")
  expect_equal(nrow(test_import1), 50)

  test_import2 <- ocpu_post_multipart('/library/xlsx/R/read.xlsx', args)
  expect_is(test_import2, "data.frame")
  expect_equal(nrow(test_import2), 50)
})

test_that("Genrate doc with ReporteRs", {
  # Check if server has ReporteRs
  req <- ocpu('/library/ReporteRs/info', stop_on_error = FALSE)
  if(req$status != 200)
    skip("Skipping Java test, 'ReporteRs' is not installed")

  code <- "
library(ReporteRs)
library(ggplot2)
library(magrittr)

myggplot <- qplot(Sepal.Length, Petal.Length, data = iris, color = Species, size = Petal.Width )

doc <- docx(title = 'My document') %>%
addTitle('First 5 lines of iris', level = 1)  %>%
addFlexTable(vanilla.table(iris[1:5, ]))  %>%
addTitle('ggplot2 example', level = 1)  %>%
addPlot(fun = print, x = myggplot)   %>%
addTitle('Text example', level = 1)   %>%
addParagraph('My tailor is rich.', stylename = 'Normal')   %>%
writeDoc('output.docx')"

  req <- ocpu_post_multipart('/library/base/R/identity', list(x = I(code)))



})
