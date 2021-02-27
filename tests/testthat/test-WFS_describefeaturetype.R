test_that("WFS_describefeaturetype checks", {
  fte      <- 'x y z'
  e        <-tibble::tibble(typename=fte,name='*NOT_FOUND*')

  version  <- '1.1.0'
  xml_doc  <- WFS_getcapabilities(version=version)
  ftn      <- WFS_featuretypes(xml_doc,
       filternames=stringr::fixed("bomen", ignore_case = T))$layer

  r       <- WFS_describefeaturetype(fte,version=version)
  expect_identical(r,e)
  r       <- WFS_describefeaturetype(c(fte,fte),version=version)
  expect_identical(r,rbind(e,e))
  r       <- WFS_describefeaturetype(c(fte,fte),version=version)
  expect_identical(r,rbind(e,e))
  r       <- WFS_describefeaturetype(ftn,version=version)
  expect_identical(names(r),c("typename", "name", "maxOccurs", "minOccurs", "type" ))
  expect_identical(unique(r$typename),ftn)
  expect_identical(length(grep('*NOT_FOUND*',r$name,fixed = T,ignore.case =F)),0L)

  version <- '2.0.0'
  xml_doc  <- WFS_getcapabilities(version=version)
  ftn      <- WFS_featuretypes(xml_doc,
       filternames=stringr::fixed("bomen", ignore_case = T))$layer

  r       <- WFS_describefeaturetype(fte,version=version)
  expect_identical(r,e)
  r       <- WFS_describefeaturetype(c(fte,fte),version=version)
  expect_identical(r,rbind(e,e))
  r       <- WFS_describefeaturetype(c(fte,fte),version=version)
  expect_identical(r,rbind(e,e))
  r       <- WFS_describefeaturetype(ftn,version=version)
  expect_identical(names(r),c("typename", "name", "maxOccurs", "minOccurs", "type" ))
  expect_identical(unique(r$typename),ftn)
  expect_identical(length(grep('*NOT_FOUND*',r$name,fixed = T,ignore.case =F)),0L)

  version <- 'x.y.z'

  r       <- WFS_describefeaturetype(fte,version=version)
  expect_identical(r,"only version '1.1.0' and '2.0.0' are allowed")
})
