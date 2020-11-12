
test_that("WFS_getfeature2 checks", {
  # library(HOQCwfs)
  # library(testthat)



  typename <- 'topp:gidw_groenbomen'
  version1 <- '1.1.0'
  wfs1      <- HOQCwfs:::WFS_getfeature2(typename, version=version1,
              startindex=3,maxfeatures=5,debug=F,verbose=rep(F,4))
  wfs2      <- HOQCwfs:::WFS_getfeature2(typename, version=version1,
              startindex=3,maxfeatures=5,debug=F,verbose=rep(T,4))
  wfs3      <- HOQCwfs:::WFS_getfeature2(typename, version=version1,
              startindex=3,maxfeatures=5,debug=F,verbose=rep(T,1))

  wfs0      <- WFS_getfeature(typename, version=version1,
              startindex=3,maxfeatures=5)
  wfs1      <- HOQCwfs:::WFS_getfeature2(typename, version=version1,
              startindex=3,maxfeatures=5)
  expect_equal(wfs0,wfs1)

})
