library(testthat)
library(HOQCwfs)

testthat::context("WFS_filter_aux checks")

test_that("WFS_filter_aux checks", {

  # convert_bbox
 bbox_28992 <- c(119103, 480726, 119160, 481078)
 bbox_4326 <- convert_bbox(bbox_28992,"EPSG:28992","EPSG:4326")
 a1 <- convert_bbox(bbox_4326,"EPSG:4326","EPSG:28992")
 expect_equal( bbox_28992,as.numeric(a1))
 expect_equal( names(a1),c("xmin", "ymin", "xmax", "ymax"))
 expect_equal( sf::st_crs(a1)$input,"EPSG:28992")

  # convert_points
 points_28992  <- c(119103, 480726, 119160, 481078)
 points_28992M <- matrix(points_28992,ncol=2,byrow = T)
 points_4326   <- convert_points(points_28992,"EPSG:28992","EPSG:4326")
 points_4326M  <- convert_points(points_28992M,"EPSG:28992","EPSG:4326")
 expect_identical( points_4326 ,points_4326M)
 a1 <- convert_points(points_4326,"EPSG:4326","EPSG:28992")
 a2 <- a1 ; dimnames(a2) <- NULL
 expect_equal(points_28992M,a2)
 expect_identical( dimnames(a1)[[2]]  ,c( "X", "Y"))
 a3 <- points_4326M ; dimnames(a3) <- NULL
 expect_equal(a3,matrix(bbox_4326,ncol=2,byrow = T))

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
 WFS_set_sep('\n')

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

test_that("spat functions checks", {
   context("spat functions checks")
  # version 1.1.0 point

coords    <- c(119103.4, 480726.0)
my_point  <- sf::st_sfc(sf::st_point(coords),crs='EPSG:28992')
my_coords <- sf::st_coordinates(my_point)[,c('X','Y')]
xml_query <- build_filter(version='1.1.0',
     spat_xml('geometrie',
              spat_feature('point','EPSG:28992',my_coords),
              50)
  )
typename  <- 'topp:gidw_groenbomen'
fields    <- 'boom_omschrijf,objec_omschrijf'
f5        <-  WFS_getfeature(typename
             ,version='1.1.0'
             ,filter=xml_query
             ,propertyname=fields)
expect_true(dim(f5)[1]>1)
expect_equal(dim(f5)[2],4)
expect_true(all(
     sf::st_distance(f5,my_point,by_element=T)<=units::set_units(50,'m')
     ))

# version 1.1.0 linestring

coords_wgs84 <- c( 4.86, 52.31, 4.86, 52.316, 4.867, 52.316)
coords       <- convert_points(coords_wgs84,"EPSG:4326","EPSG:28992")
my_line  <- sf::st_sfc(sf::st_linestring(coords),crs='EPSG:28992')
my_coords <- sf::st_coordinates(my_line)[,c('X','Y')]
xml_query <- build_filter(version='1.1.0',
     spat_xml('geometrie',spat_feature('LineString','EPSG:28992',my_coords),50)
  )
typename  <- 'topp:gidw_groenbomen'
fields    <- 'boom_omschrijf,objec_omschrijf'
f6        <-  WFS_getfeature(typename
             ,version='1.1.0'
             ,filter=xml_query
             ,propertyname=fields)
expect_gte(dim(f6)[1],1)
expect_equal(dim(f6)[2],4)

expect_true(all(
    sf::st_distance(f6,my_line,by_element=T)<=units::set_units(50,'m')
    ))

# version 1.1.0 polygon

coords_wgs84 <- c( 4.86, 52.31, 4.86, 52.316, 4.867, 52.316, 4.86, 52.31)
coords       <- convert_points(coords_wgs84,"EPSG:4326","EPSG:28992")
# coords       <- round(coords,2)
my_poly      <- sf::st_sfc(sf::st_polygon(list(coords)),crs='EPSG:28992')
my_coords    <- sf::st_coordinates(my_poly)[,c('X','Y')]
xml_query    <- build_filter(version='1.1.0',
     spat_xml('geometrie',spat_feature('Polygon','EPSG:28992',my_coords),spat_fun='DWithin',50)
  )
typename  <- 'topp:gidw_groenbomen'
fields    <- 'boom_omschrijf,objec_omschrijf'
f6        <-  WFS_getfeature(typename
             ,version='1.1.0'
             ,filter=xml_query
             ,propertyname=fields)

expect_gte(dim(f6)[1],1)
expect_equal(dim(f6)[2],4)

expect_true(all(
    sf::st_distance(f6,my_poly,by_element=T)<=units::set_units(50,'m')
    ))

# version 1.1.0 polygon Intersects

coords_wgs84 <- c( 4.86, 52.31, 4.86, 52.316, 4.867, 52.316, 4.86, 52.31)
coords       <- convert_points(coords_wgs84,"EPSG:4326","EPSG:28992")
# coords       <- round(coords,2)
my_poly      <- sf::st_sfc(sf::st_polygon(list(coords)),crs='EPSG:28992')
my_coords    <- sf::st_coordinates(my_poly)[,c('X','Y')]
xml_query    <- build_filter(version='1.1.0',
     spat_xml('geometrie',spat_feature('Polygon','EPSG:28992',my_coords),spat_fun='Intersects')
  )
typename  <- 'topp:gidw_groenbomen'
fields    <- 'boom_omschrijf,objec_omschrijf'
f7        <-  WFS_getfeature(typename
             ,version='1.1.0'
             ,filter=xml_query
             ,propertyname=fields)

expect_gte(dim(f7)[1],1)
expect_equal(dim(f7)[2],4)

expect_true(all(
    sf::st_distance(f7,my_poly,by_element=T)==units::set_units(0,'m')
    ))



})
