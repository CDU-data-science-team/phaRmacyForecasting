test_that("multiplication works", {
  
  # If you're looking at the most recent data then 
  # site 100 (stores) uses the suppliers AAH, MAW, PHOX and 
  # sites 240, 242 & 958 use STO. 
  # If you're looking at the very early data (2011) 
  # then only site 240 existed at that point so try AAH & UNICH
  
  reorder_frame <- inventory_reorder(site = 240, supplier = "STO")
  
  expect_gt(nrow(reorder_frame), 0)
  
})

