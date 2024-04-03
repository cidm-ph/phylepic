test_that("finds s3 classes", {
  expect_contains(
    get_s3_classes(tidygraph::as_tbl_graph),
    c("data.frame", "phylepic", "phylo")
  )
})
