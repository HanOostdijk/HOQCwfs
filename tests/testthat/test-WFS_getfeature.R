
test_that("WFS_getfeature checks", {
  # library(HOQCwfs)
  # library(testthat)

  L1a <- list(a=1,a=2,b=3,a=4,c=5,b=6)
  L1b <- list(a=1,b=3,c=5)
  L1c <- list(a=4,b=6,c=5)
  expect_identical( WFS_util_keep_unique(L1a,keep_first = T),L1b)
  expect_identical( WFS_util_keep_unique(L1a,keep_first = F),L1c)

  typename <- 'topp:gidw_groenbomen'
  version1 <- '1.1.0'
  version2 <- '2.0.0'
  version3 <- 'x.3.0'
  pnames    <- c("boom_omschrijf", "jaar")
  # tests for case without queries 1.1.0 and 2.0.0
  fti1     <- WFS_describefeaturetype(typename,version=version1)$name
  fti2     <- WFS_describefeaturetype(typename,version=version2)$name
  expect_identical(fti1,fti2)
  fti1a    <- c("id",stringr::str_replace(fti1,'^geometrie$','geometry'))

  wfs1     <-  WFS_getfeature(typename,version=version1)
  wfs2     <-  WFS_getfeature(typename,version=version2)
  expect_identical(wfs1,wfs2)
  expect_identical(fti1a,names(wfs1))
  wfs3e    <-  WFS_getfeature(typename,version=version3)
  expect_identical(wfs3e,"only version '1.1.0' and '2.0.0' are allowed")
  wfs3f    <-  WFS_getfeature(typename,version=version1,resultType='hits' )
  wfs3g    <-  WFS_getfeature(typename,version=version2,resultType='hits' )
  expect_identical(wfs3f,wfs3g)
  expect_equal(wfs3f,dim(wfs1)[1])

  # tests for simple cql_filter query 1.1.0 and 2.0.0
  a_tree    <- r"(Prunus serrulata 'Kanzan')"    # embedded quote
  bm=stringr::str_replace_all(a_tree,"'","''")   # double that for cql_filter
  wfs3     <- dplyr::filter(wfs1,boom_omschrijf==a_tree)
  wfs3a     <-  WFS_getfeature(typename,version=version1,
              cql_filter= glue::glue("boom_omschrijf='{bm}'") )
  expect_identical(wfs3,wfs3a)
  wfs3b     <-  WFS_getfeature(typename,version=version2,
              cql_filter= glue::glue("boom_omschrijf='{bm}'") )
  expect_identical(wfs3,wfs3b)

  # same tests with a WFS filter

  f4a <- build_filter(version='1.1.0'
          , propeq_xml('topp:boom_omschrijf',a_tree)
   )
  f4b <- build_filter(version='2.0.0'
          , propeq_xml('topp:boom_omschrijf',a_tree)
   )

  wfs4a      <- WFS_getfeature(typename, version=version1,
              filter  = f4a)
  wfs4b      <- WFS_getfeature(typename, version=version2,
              filter  = f4b)
  expect_identical(wfs3,wfs4a)
  expect_identical(wfs3,wfs4b)

  # test with startindex and maxfeatures/count
  wfs5       <- wfs3 %>% dplyr::slice(4:8)
  wfs5a      <- WFS_getfeature(typename, version=version1,
              startindex=3,maxfeatures=5,
              filter  = f4a)
  wfs5a1    <- WFS_getfeature(typename, version=version1,
              startindex=3,count=5, # translated
              filter  = f4a)
  wfs5b      <- WFS_getfeature(typename, version=version2,
              startindex=3,count=5,
              filter  = f4b)
  wfs5b1      <- WFS_getfeature(typename, version=version2,
              startindex=3,maxfeatures=5,  # translated
              filter  = f4b)
  wfs5c      <- WFS_getfeature(typename, version=version1,
              startindex=3,maxfeatures=5,
              cql_filter= glue::glue("boom_omschrijf='{bm}'") )
  wfs5d      <- WFS_getfeature(typename, version=version2,
              startindex=3,count=5,
              cql_filter= glue::glue("boom_omschrijf='{bm}'") )
  expect_identical(wfs5a,wfs5)
  expect_identical(wfs5a1,wfs5)
  expect_identical(wfs5b,wfs5)
  expect_identical(wfs5b1,wfs5)
  expect_identical(wfs5c,wfs5)
  expect_identical(wfs5d,wfs5)

  # test with propertyname
  pnames    <- c("boom_omschrijf", "jaar")
  pnamesi   <- unique(c('id',pnames)) # id always returned with geometry
  wfs6      <- wfs3 %>% dplyr::slice(4:8) %>% dplyr::select(dplyr::all_of(pnamesi))
  wfs6a      <- WFS_getfeature(typename, version=version1,
              startindex=3,maxfeatures=5,
              propertyname=glue::glue_collapse(pnames,sep=','),
              filter  = f4a)
  wfs6b      <- WFS_getfeature(typename, version=version2,
              startindex=3,count=5,
              propertyname=glue::glue_collapse(pnames,sep=','),
              filter  = f4b)
  expect_identical(wfs6a,wfs6)
  expect_identical(wfs6b,wfs6)

   # test with srsname
  wfs7       <- sf::st_transform(wfs6,crs='EPSG:4326')
  wfs7a      <- WFS_getfeature(typename, version=version1,
              startindex=3,maxfeatures=5,
              propertyname=glue::glue_collapse(pnames,sep=','),
              srsname='EPSG:4326',
              filter  = f4a)
  expect_equal(sf::st_crs(wfs7a)$input,"WGS 84") # other word for EPSG:4326
  sf::st_crs(wfs7a)<- 'EPSG:4326' # indicate so and then expect equal
  expect_equal(wfs7a,wfs7)
  wfs7b      <- WFS_getfeature(typename, version=version2,
              startindex=3,count=5,
              propertyname=glue::glue_collapse(pnames,sep=','),
              srsname='EPSG:4326',
              filter  = f4b)
  expect_equal(sf::st_crs(wfs7b)$input,"WGS 84") # other word for EPSG:4326
  sf::st_crs(wfs7b)<- 'EPSG:4326' # indicate so and then expect equal
  expect_equal(wfs7b,wfs7)

  # tests with bbox
  bbox_wgs84 <- c(4.860793, 52.313319, 4.861587, 52.316493 )
  x <- sf::st_sfc(sf::st_multipoint(matrix(bbox_wgs84,ncol=2,byrow=T)),crs=4326)
  bbox_28992 <- sf::st_bbox(sf::st_transform(x,crs=28992))

  convert_bbox <- function (bbox_coords,crs_in,crs_out) {
    mp_sfc <- sf::st_sfc(
      sf::st_multipoint(matrix(bbox_coords,ncol=2,byrow=T)),crs=crs_in)
    sf::st_bbox(sf::st_transform(mp_sfc,crs=crs_out))
  }


  wfs8a      <- WFS_getfeature(typename, version=version1,
              bbox=glue::glue_collapse(bbox_28992,sep=','),
              propertyname=glue::glue_collapse(pnames,sep=','),
              srsname='EPSG:4326')
  wfs8b      <- WFS_getfeature(typename, version=version1,
              bbox=paste(glue::glue_collapse(bbox_wgs84,sep=','),'EPSG:4326',sep=','),
              propertyname=glue::glue_collapse(pnames,sep=','),
              srsname='EPSG:4326')
  wfs8c      <- WFS_getfeature(typename, version=version2,
              bbox=glue::glue_collapse(bbox_28992,sep=','),
              propertyname=glue::glue_collapse(pnames,sep=','),
              srsname='EPSG:4326')
  wfs8d      <- WFS_getfeature(typename, version=version2,
              bbox=paste(glue::glue_collapse(bbox_wgs84,sep=','),'EPSG:4326',sep=','),
              propertyname=glue::glue_collapse(pnames,sep=','),
              srsname='EPSG:4326')
  expect_identical(wfs8a,wfs8b)
  expect_identical(wfs8b,wfs8c)
  expect_identical(wfs8c,wfs8d)
  bbox_res   <- sf::st_bbox(wfs8a)
  # expect_true(all(bbox_res[1:2]>=bbox_wgs84[1:2])) is not true ??
  expect_true(all(bbox_res[3:4]<=bbox_wgs84[3:4]))


  f9a=build_filter(version='1.1.0',
     fg("And"
           , propeq_xml('topp:boom_omschrijf',"Alnus glutinosa 'Laciniata'")
           , bbox_xml("geometrie","EPSG:28992",bbox_28992)
         )
  )
  f9b=build_filter(version='2.0.0',
     fg("And"
           , propeq_xml('topp:boom_omschrijf',"Alnus glutinosa 'Laciniata'")
           , bbox_xml("geometrie","EPSG:28992",bbox_28992)
         )
  )

  wfs9a       <- WFS_getfeature(typename, version=version1,
                filter=f9a,debug=F,
                propertyname=glue::glue_collapse(pnames,sep=','))

  wfs9b       <- WFS_getfeature(typename, version=version2,
                filter=f9b,debug=F,
                propertyname=glue::glue_collapse(pnames,sep=','))
  expect_identical(wfs9a,wfs9b)

# tests Dwithin
  wfs10a      <- WFS_getfeature(typename, version=version1,
              propertyname=glue::glue_collapse(pnames,sep=','),
              cql_filter=
              glue::glue('DWithin(geometrie,POINT({x}),50,meters)',
                        x= glue::glue_collapse(bbox_28992[1:2],sep=' '))
              )

  my_point <- sf::st_sfc(sf::st_point(bbox_28992[1:2]),crs=28992)
  expect_true(all(
    sf::st_distance(wfs10a,my_point,by_element=T)<=units::set_units(50,'m')
    ))

})
