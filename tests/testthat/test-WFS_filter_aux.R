test_that("WFS_filter_aux checks", {
  # convert_bbox
 bbox_28992 <- c(119103, 480726, 119160, 481078)
 bbox_4326 <- convert_bbox(bbox_28992,"EPSG:28992","EPSG:4326")
 a1 <- convert_bbox(bbox_4326,"EPSG:4326","EPSG:28992")
 expect_equal( bbox_28992,as.numeric(a1))
 expect_equal( names(a1),c("xmin", "ymin", "xmax", "ymax"))
 expect_equal( sf::st_crs(a1)$input,"EPSG:28992")

 # build_filter

 # version other than '1.1.0' and '2.0.0'
 expect_equal( build_filter(version='1.0.0'),
               "only version '1.1.0' and '2.0.0' are allowed")

 # generate correct prefix
 f1a <- build_filter(version='1.1.0','x',sep='')
 f1b <- "<Filter xmlns:gml=\"http://www.opengis.net/gml\" xmlns:ogc=\"http://www.opengis.net/ogc\">x</Filter>"
 expect_equal( f1a,f1b)

 f2a <- build_filter(version='2.0.0','x',sep='')
 f2b <- "<Filter xmlns:gml=\"http://www.opengis.net/gml/3.2\" xmlns:fes=\"http://www.opengis.net/fes/2.0\">x</Filter>"
 expect_equal( f2a,f2b)

 # save version and sep over function
 oversion <- WFS_set_version('2.0.0')
 osep     <- WFS_set_sep('xyz')
 f3a <- build_filter(version='1.1.0','x',sep='abc')
 expect_equal( WFS_get_version(), oversion)
 expect_equal( WFS_get_sep(), osep)

 oversion <- WFS_set_version('1.1.0')
 osep     <- WFS_set_sep('xyz')
 f3b <- build_filter(version='2.0.0','x',sep='def')
 expect_equal( WFS_get_version(), oversion)
 expect_equal( WFS_get_sep(), osep)

 # generate xml for correct version

f4a <- build_filter(version='1.1.0', propeq_xml('x',"y"),sep='')
f4b <- paste0("<Filter xmlns:gml=\"http://www.opengis.net/gml\" xmlns:ogc=\"http://www.opengis.net/ogc\"",
         "><PropertyIsEqualTo><PropertyName>x</PropertyName><Literal>y</Literal></PropertyIsEqualTo></Filter>")
expect_equal(f4a,f4b)

f4c <- build_filter(version='2.0.0', propeq_xml('x',"y"),sep='')
f4d <- paste0( "<Filter xmlns:gml=\"http://www.opengis.net/gml/3.2\" xmlns:fes=\"http://www.opengis.net/fes/2.0\"",
         "><PropertyIsEqualTo><ValueReference>x</ValueReference><Literal>y</Literal></PropertyIsEqualTo></Filter>")
expect_equal(f4c,f4d)

})
