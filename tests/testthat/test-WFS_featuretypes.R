test_that("WFS_featuretypes checks", {
  v_in     <- '1.1.0'
  ft_names <- c("layer", "defaultcrs", "lc_wgs84", "uc_wgs84")
  xml_doc  <- WFS_getcapabilities(version=v_in)
  ft0      <- WFS_featuretypes(xml_doc)
  ft0d     <- dim(ft0)
  expect_equal(ft0d[2],4)
  expect_equal(names(ft0),ft_names)
  ix       <- grep('bomen',ft0$layer,ignore.case = T)
  ft0b     <- ft0[ix,]
  ft1      <- WFS_featuretypes(xml_doc,
       filternames=stringr::fixed("bomen", ignore_case = T))
  ft1d     <- dim(ft1)
  expect_equal(ft1d[2],4)
  expect_equal(names(ft1),ft_names)
  expect_equal(ft0b,ft1)
})
