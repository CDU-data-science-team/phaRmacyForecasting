test_that("multiplication works", {
  
  reorder_frame <- inventory_reorder(site = 240, supplier = "STO")
  
  expect_gt(nrow(reorder_frame), 0)
  
})

