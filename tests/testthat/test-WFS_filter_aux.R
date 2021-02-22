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

 testmat <- expand.grid(out_matrix=c(T,F),keep_names=c(T,F))

 test_convert_points <- function(in_data, testmat) {
   purrr::walk(seq(1, dim(testmat)[1]),
               function(i) {
                 out_matrix = testmat[i, 1]
                 keep_names = testmat[i, 2]
                 x <- convert_points(in_data,
                     "EPSG:28992",
                     "EPSG:4326",
                     out_matrix = out_matrix,
                     keep_names = keep_names)
                 y <- convert_points(x,
                     "EPSG:4326",
                     "EPSG:28992",
                     out_matrix = out_matrix,
                     keep_names = keep_names)
                 if (is.matrix(y) && (!is.matrix(in_data)))
                   y = as.vector(t(y))
                 if ( is.matrix(in_data) && (!is.null(dimnames(in_data)[2])) &&( keep_names == F))
                   attributes(y) = attributes(in_data)
                 expect_equal(in_data, y)
               })
 }

 in_data  <- c(119103, 480726, 119160, 481078)
 test_convert_points(in_data, testmat)
 in_data2 <- matrix(in_data,ncol=2,byrow = T)
 test_convert_points(in_data2, testmat)
 in_data3 <- in_data2
 dimnames(in_data3) <- list(NULL,c('myx','myy'))
 test_convert_points(in_data3, testmat)

 # create_coord

orgsep <-WFS_get_sep()
WFS_set_sep('')
a <- create_coord(1:4,'pos',version='1.1.0')
b <- create_coord(1:4,'poslist',version='1.1.0')
c <- create_coord(1:4,'pos',version='2.0.0')
d <- create_coord(1:4,'posli',version='2.0.0')
a1 <- "<gml:coordinates decimal=\".\" cs=\",\" ts=\" \">1,2 3,4</gml:coordinates>"
b1 <- "<gml:coordinates decimal=\".\" cs=\",\" ts=\" \">1,2 3,4</gml:coordinates>"
c1 <- "<gml:pos decimal=\".\" cs=\",\" ts=\" \">1 2 3 4</gml:pos>"
d1 <- "<gml:posList decimal=\".\" cs=\",\" ts=\" \">1 2 3 4</gml:posList>"
expect_equal(a,a1)
expect_equal(b,b1)
expect_equal(c,c1)
expect_equal(d,d1)
WFS_set_sep(orgsep)

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
 WFS_set_version('2.0.0')
 WFS_set_sep('xyz')
 f3a <- build_filter(version='1.1.0','x',sep='abc')
 expect_equal( WFS_get_version(), '2.0.0')
 expect_equal( WFS_get_sep(), 'xyz')

 WFS_set_version('1.1.0')
 osep     <- WFS_set_sep('abc')
 f3b <- build_filter(version='2.0.0','x',sep='def')
 expect_equal( WFS_get_version(), '1.1.0')
 expect_equal( WFS_get_sep(), 'abc')
 WFS_set_sep('\n')

 # generate xml for correct version

f4a <- build_filter(version='1.1.0', propeq_xml('x',"y"),sep='')
f4b <- paste0("<Filter xmlns:gml=\"http://www.opengis.net/gml\" xmlns:ogc=\"http://www.opengis.net/ogc\"",
         "><ogc:PropertyIsEqualTo><ogc:PropertyName>x</ogc:PropertyName><ogc:Literal>y</ogc:Literal></ogc:PropertyIsEqualTo></Filter>")
expect_equal(f4a,f4b)

f4c <- build_filter(version='2.0.0', propeq_xml('x',"y"),sep='')
f4d <- paste0( "<Filter xmlns:gml=\"http://www.opengis.net/gml/3.2\" xmlns:fes=\"http://www.opengis.net/fes/2.0\"",
         "><fes:PropertyIsEqualTo><fes:ValueReference>x</fes:ValueReference><fes:Literal>y</fes:Literal></fes:PropertyIsEqualTo></Filter>")
expect_equal(f4c,f4d)
})

test_that("spat functions checks", {
   context("spat functions checks")

  point_test <- function (version){ #DWithin
coords    <- c(119103.4, 480726.0)
my_point  <- sf::st_sfc(sf::st_point(coords),crs='EPSG:28992')
my_coords <- sf::st_coordinates(my_point)[,c('X','Y')]
xml_query <- build_filter(version=version,
     spat_xml('geometrie',
              spat_feature('point','EPSG:28992',my_coords),
              50)
  )
typename  <- 'topp:gidw_groenbomen'
fields    <- 'boom_omschrijf,objec_omschrijf'
f5        <-  WFS_getfeature(typename
             ,version=version
             ,filter=xml_query
             ,propertyname=fields)
expect_true(dim(f5)[1]>1)
expect_equal(dim(f5)[2],4)
expect_true(all(
     sf::st_distance(f5,my_point,by_element=T)<=units::set_units(50,'m')
))
  }
  point_test('1.1.0')
  point_test('2.0.0')


  envelope_test <- function (version){  #DWithin
coords_wgs84 <- c( 4.86, 52.31,  4.867, 52.316)
coords       <- convert_points(coords_wgs84,"EPSG:4326","EPSG:28992",out_matrix=F)
my_env  <- sf::st_as_sfc( convert_bbox(coords,crs_in='EPSG:28992',crs_out='EPSG:28992'))

xml_query <- build_filter(version=version,
     spat_xml('geometrie',
              spat_feature('Envelope','EPSG:28992',coords ),
              50)
  )
typename  <- 'topp:gidw_groenbomen'
fields    <- 'boom_omschrijf,objec_omschrijf'
f5a        <-  WFS_getfeature(typename
             ,version=version
             ,filter=xml_query
             ,propertyname=fields)
expect_gte(dim(f5a)[1],1)
expect_equal(dim(f5a)[2],4)

expect_true(all(
    sf::st_distance(f5a,my_env,by_element=T)<=units::set_units(50,'m')
    ))
  }
  envelope_test('1.1.0')
  envelope_test('2.0.0')

  linestring_test <- function (version){  #DWithin
coords_wgs84 <- c( 4.86, 52.31, 4.86, 52.316, 4.867, 52.316)
coords       <- convert_points(coords_wgs84,"EPSG:4326","EPSG:28992")
my_line  <- sf::st_sfc(sf::st_linestring(coords),crs='EPSG:28992')
my_coords <- sf::st_coordinates(my_line)[,c('X','Y')]
xml_query <- build_filter(version=version,
     spat_xml('geometrie',
              spat_feature('LineString','EPSG:28992',my_coords),
              50)
  )
typename  <- 'topp:gidw_groenbomen'
fields    <- 'boom_omschrijf,objec_omschrijf'
f6        <-  WFS_getfeature(typename
             ,version=version
             ,filter=xml_query
             ,propertyname=fields)
expect_gte(dim(f6)[1],1)
expect_equal(dim(f6)[2],4)

expect_true(all(
    sf::st_distance(f6,my_line,by_element=T)<=units::set_units(50,'m')
    ))
  }
  linestring_test('1.1.0')
  linestring_test('2.0.0')

  poly_test1 <- function (version){  #DWithin
coords_wgs84 <- c( 4.86, 52.31, 4.86, 52.316, 4.867, 52.316, 4.86, 52.31)
coords       <- convert_points(coords_wgs84,"EPSG:4326","EPSG:28992")
my_poly      <- sf::st_sfc(sf::st_polygon(list(coords)),crs='EPSG:28992')
my_coords    <- sf::st_coordinates(my_poly)[,c('X','Y')]
xml_query    <- build_filter(version=version,
     spat_xml('geometrie',
              spat_feature('Polygon','EPSG:28992',list(my_coords)),
              spat_fun='DWithin',50)
  )
typename  <- 'topp:gidw_groenbomen'
fields    <- 'boom_omschrijf,objec_omschrijf'
f6        <-  WFS_getfeature(typename
             ,version=version
             ,filter=xml_query
             ,propertyname=fields)

expect_gte(dim(f6)[1],1)
expect_equal(dim(f6)[2],4)

expect_true(all(
    sf::st_distance(f6,my_poly,by_element=T)<=units::set_units(50,'m')
    ))
  }

  poly_test1('1.1.0')
  poly_test1('2.0.0')

  poly_test2 <- function (version){  #Intersects
coords_wgs84 <- c( 4.86, 52.31, 4.86, 52.316, 4.867, 52.316, 4.86, 52.31)
coords       <- convert_points(coords_wgs84,"EPSG:4326","EPSG:28992")
my_poly      <- sf::st_sfc(sf::st_polygon(list(coords)),crs='EPSG:28992')
my_coords    <- sf::st_coordinates(my_poly)[,c('X','Y')]
xml_query    <- build_filter(version=version,
     spat_xml('geometrie',
              spat_feature('Polygon','EPSG:28992',list(my_coords)),
              spat_fun='Intersects')
  )
typename  <- 'topp:gidw_groenbomen'
fields    <- 'boom_omschrijf,objec_omschrijf'
f7        <-  WFS_getfeature(typename
             ,version=version
             ,filter=xml_query
             ,propertyname=fields)

expect_gte(dim(f7)[1],1)
expect_equal(dim(f7)[2],4)

expect_true(all(
    sf::st_distance(f7,my_poly,by_element=T)==units::set_units(0,'m')
    ))
  }

  poly_test2('1.1.0')
  poly_test2('2.0.0')


  poly_test3 <- function (version){  # Intersects with hole
out_wgs84    <- c( 4.86, 52.31, 4.86, 52.316, 4.88, 52.316, 4.88, 52.31, 4.86, 52.31)
out_28992    <- convert_points(out_wgs84,"EPSG:4326","EPSG:28992")
in_wgs84    <- c( 4.87, 52.312, 4.87, 52.314, 4.875, 52.314, 4.875, 52.312, 4.87, 52.312)
in_28992    <- convert_points(in_wgs84,"EPSG:4326","EPSG:28992")

my_poly      <- sf::st_sfc(sf::st_polygon(list(out_28992,in_28992)),crs='EPSG:28992')
xml_query    <- build_filter(version=version,
     spat_xml('geometrie',
              spat_feature('Polygon','EPSG:28992',list(out_28992,in_28992)),
              spat_fun='Intersects')
  )

typename  <- 'topp:gidw_groenbomen'
fields    <- 'boom_omschrijf,objec_omschrijf'
f8        <-  WFS_getfeature(typename
             ,version=version
             ,filter=xml_query
             ,propertyname=fields)

expect_gte(dim(f8)[1],1)
expect_equal(dim(f8)[2],4)

expect_true(all(
    sf::st_distance(f8,my_poly,by_element=T)==units::set_units(0,'m')
    ))
  }

  poly_test3('1.1.0')
  poly_test3('2.0.0')

  mp_test1 <- function (version){  # multipoint
coords_wgs84 <- c( 4.86, 52.31, 4.86, 52.316, 4.88, 52.316)
coords_28992 <- convert_points(coords_wgs84,"EPSG:4326","EPSG:28992")

my_mp      <- sf::st_sfc(sf::st_multipoint(coords_28992),crs='EPSG:28992')
xml_query    <- build_filter(version=version,
     spat_xml('geometrie',
              spat_feature('Multipoint','EPSG:28992',coords_28992),
              spat_fun='DWithin',
              50)
  )

typename  <- 'topp:gidw_groenbomen'
fields    <- 'boom_omschrijf,objec_omschrijf'
f9        <-  WFS_getfeature(typename
             ,version=version
             ,filter=xml_query
             ,propertyname=fields)

expect_gte(dim(f9)[1],1)
expect_equal(dim(f9)[2],4)

expect_true(all(
    sf::st_distance(f9,my_mp,by_element=T)<=units::set_units(50,'m')
    ))
  }

  mp_test1('1.1.0')
  mp_test1('2.0.0')


  mls_test1 <- function (version){  # multipoint
coords_wgs84a <- c( 4.86, 52.31, 4.86, 52.316, 4.88, 52.316)
coords_28992a <- convert_points(coords_wgs84a,"EPSG:4326","EPSG:28992")
coords_wgs84b <- c( 4.87, 52.31, 4.87, 52.306, 4.89, 52.316)
coords_28992b <- convert_points(coords_wgs84b,"EPSG:4326","EPSG:28992")
coords_28992 <- list(coords_28992a,coords_28992b)

my_mls      <- sf::st_sfc(sf::st_multilinestring(coords_28992),crs='EPSG:28992')
# sf::st_write(my_mls,'mls110.gml')
# sf::st_write(my_mls,'mls200.gml',dataset_options = 'FORMAT=GML3.2')

xml_query    <- build_filter(version=version,
     spat_xml('geometrie',
              spat_feature('MultiLinestring','EPSG:28992',coords_28992),
              spat_fun='DWithin',
              50)
  )

typename  <- 'topp:gidw_groenbomen'
fields    <- 'boom_omschrijf,objec_omschrijf'
f10        <-  WFS_getfeature(typename
             ,version=version
             ,filter=xml_query
             ,propertyname=fields)

expect_gte(dim(f10)[1],1)
expect_equal(dim(f10)[2],4)

expect_true(all(
    sf::st_distance(f10,my_mls,by_element=T)<=units::set_units(50,'m')
    ))
  }

  mls_test1('1.1.0')
  mls_test1('2.0.0')


  mpoly_test1 <- function (version){  # multipolygonwith hole Intersects
out_wgs84    <- c( 4.86, 52.31, 4.86, 52.316, 4.88, 52.316, 4.88, 52.31, 4.86, 52.31)
out_28992    <- convert_points(out_wgs84,"EPSG:4326","EPSG:28992")
in_wgs84    <- c( 4.87, 52.312, 4.87, 52.314, 4.875, 52.314, 4.875, 52.312, 4.87, 52.312)
in_28992    <- convert_points(in_wgs84,"EPSG:4326","EPSG:28992")

pol1 <- list(out_28992,in_28992)

out_wgs84    <-  c( 4.86, 52.30, 4.86, 52.306, 4.88, 52.306, 4.88, 52.30, 4.86, 52.30)
out_28992    <- convert_points(out_wgs84,"EPSG:4326","EPSG:28992")

pol2 <- list(out_28992)

mpol1        <- list(pol1,pol2)
my_mpol      <- sf::st_sfc(sf::st_multipolygon(mpol1),crs='EPSG:28992')

 # sf::st_write(my_mpol,'mpol110.gml')
 # sf::st_write(my_mpol,'mpol200.gml',dataset_options = 'FORMAT=GML3.2')



xml_query    <- build_filter(version=version,
     spat_xml('geometrie',
              spat_feature('MultiPolygon','EPSG:28992',mpol1),
              spat_fun='Intersects')
  )

typename  <- 'topp:gidw_groenbomen'
fields    <- 'boom_omschrijf,objec_omschrijf'
f11        <-  WFS_getfeature(typename
             ,version=version
             ,filter=xml_query
             ,propertyname=fields)

expect_gte(dim(f11)[1],1)
expect_equal(dim(f11)[2],4)

expect_true(all(
    sf::st_distance(f11,my_mpol,by_element=T)==units::set_units(0,'m')
    ))
  }

  mpoly_test1('1.1.0')
  mpoly_test1('2.0.0')


})
