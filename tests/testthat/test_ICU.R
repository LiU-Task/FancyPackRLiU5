context("ICU")



test_that("Generate errors in abnormal scenarioes", {
   
  expect_error(x<-ICU$new(land=se-7,date="2015-11",mode="geo"))
  
})

test_that("Generate id number", {
  x<-ICU$new(land="world-2",date="2015-11",mode="geo") 
  z<-x$guo$Countries$coordinates
  enti<-x$guo$Entities
  gj<-x$guo$Countries
  
  expect_equal(enti[grep("sweden",enti$name,ignore.case=TRUE),]$id,"455") 
  expect_equal(enti[grep("china",enti$name,ignore.case=TRUE),]$id,"507")
  
})
