testthat::test_that("Simple renames work.", {
  z = list(workflow::column(name = 'a', standard_name = 'b'),
           workflow::column(name = 'a', standard_name = 'c'),
           workflow::column(name = 'a', standard_name = 'c', units = 'meters'),
           workflow::column(name = 'b', standard_name = 'd'),
           workflow::column(name = 'd', standard_name = 'q'),
           workflow::column(name = 'z', standard_name = 'b'))
  zz = workflow::combine_definitions(z)
})
