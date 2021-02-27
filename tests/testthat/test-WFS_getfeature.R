# library(testthat)
# library(HOQCwfs)

test_that("WFS_getfeature set 1", {
  # test that there are no differences for version 1.1.0 and 2.0.0
  #   with GET/POST for simple query  with only maxfeatures/count and startindex
  # test that maxfeatures/count are accepted by both versions and
  # only versions 1.1.0 and 2.0.0 are accepted
  # NB without startindex can give different results than startindex=0 : the latter apparently sorts !?

  # problem with version 2.0.0 POST for Amstelveen data is included
  typename <- "wijkenbuurten2019:cbs_buurten_2019" # or typename <- "cbs_buurten_2019"
  version1 <- '1.1.0'
  version2 <- '2.0.0'
  version3 <- '3.0.0'
  alturl   <- "https://geodata.nationaalgeoregister.nl/wijkenbuurten2019/wfs"

  wfs0a      <- WFS_getfeature(typename, url=alturl,
                  httrType="GET",version=version1,
                  startindex=0,maxfeatures=8)
  wfs0b      <- WFS_getfeature(typename, url=alturl,
                  httrType="POST",version=version1,
                  startindex=0,count=8)
  expect_true(inherits(wfs0a,'sf'))
  expect_equal(dim(wfs0a)[1],8)
  expect_equal(wfs0a,wfs0b)

  comb     <- expand.grid(version  = c('1.1.0','2.0.0','3.0.0'),
                      httrType = c("GET","POST"))
  res    <- purrr::map(purrr::array_branch(comb,1), function(vp){
           WFS_getfeature(typename,url=alturl,
                     version=vp[1],httrType=vp[2],
                      startindex=3,maxfeatures=5)
  })

  expect_equal(dim(res[[1]])[1],5)
  expect_equal(res[[1]],dplyr::slice(wfs0a,4:8))
  expect_equal(res[[2]],res[[1]])
  expect_equal(res[[4]],res[[1]])
  expect_equal(res[[5]],res[[1]])
  expect_equal(res[[3]],"only version '1.1.0' and '2.0.0' are allowed")
  expect_equal(res[[6]],"only version '1.1.0' and '2.0.0' are allowed")

  expect_error( WFS_getfeature(typename, url=alturl,
                 httrType="GOST",version=version1,
                 startindex=3,maxfeatures=5),
              "'arg' should be one of \"GET\", \"POST\"")
              #"'arg' should be one of “GET”, “POST”")

  expect_error( WFS_getfeature(typename, url=alturl,
                 httrType="GOST",version=version2,
                 startindex=3,maxfeatures=5),
              "'arg' should be one of \"GET\", \"POST\"")
              #"'arg' should be one of “GET”, “POST”")

})

test_that("WFS_getfeature set2", {
  # test that there are no differences for version 1.1.0 and 2.0.0
  #   with GET/POST for simple query  with resultType='hits'

  typename <- "wijkenbuurten2019:cbs_buurten_2019"
  alturl   <- "https://geodata.nationaalgeoregister.nl/wijkenbuurten2019/wfs"

  comb     <- expand.grid(version  = c('1.1.0','2.0.0'),
                      httrType = c("GET","POST"))
  count    <-purrr::map(purrr::array_branch(comb,1), function(vp){
           WFS_getfeature(typename,url=alturl,resultType='hits',
                     version=vp[1],httrType=vp[2])
  })
  count <- unlist(count)
  expect_vector(count,ptype = double(), size = 4)
  expect_true(count[1] > 0)
  expect_true(count[1] < Inf)
  expect_true(all(count==count[1]))
})



test_that("WFS_getfeature set3", {
  # test that there are no differences for version 1.1.0 and 2.0.0
  #   with GET/POST for column selection with propertyname='....'
  # NB the field geometry is always returned but empty if it is not requested
  # with the correct name ('geom' in this case)
  pnames   <- c("buurtcode", "buurtnaam","geom")
  typename <- "cbs_buurten_2019" # "wijkenbuurten2019:cbs_buurten_2019"
  alturl   <- "https://geodata.nationaalgeoregister.nl/wijkenbuurten2019/wfs"

  comb     <- expand.grid(version  = c('1.1.0','2.0.0'),
                      httrType = c("GET","POST"))
  res      <- purrr::map(purrr::array_branch(comb,1), function(vp){
               WFS_getfeature(typename, url=alturl,
                      version=vp[1],httrType=vp[2],
                      propertyname=paste(pnames,collapse=','),
                      startindex=0,maxfeatures=5)

     })

  expect_true(inherits(res[[1]],"sf"))
  expect_equal(dim(res[[1]]),c(5,1+length(pnames)))
  expect_equal(names(res[[1]]),c("id", "buurtcode", "buurtnaam", "geometry" ))
  expect_equal(res[[1]],res[[2]])
  expect_equal(res[[2]],res[[3]])
  expect_equal(res[[3]],res[[4]])
})

test_that("WFS_getfeature set4", {
  # test that there are no differences for version 1.1.0 and 2.0.0
  #   with GET/POST for sorting with sortby='....'
  pnames   <- c("buurtcode", "buurtnaam","geom")
  typename <- "cbs_buurten_2019" # "wijkenbuurten2019:cbs_buurten_2019"
  alturl   <- "https://geodata.nationaalgeoregister.nl/wijkenbuurten2019/wfs"

  comb     <- expand.grid(version  = c('1.1.0','2.0.0'),
                      httrType = c("GET","POST"))
  WFS_set_sep('')
  res      <- purrr::map(purrr::array_branch(comb,1), function(vp){
               WFS_getfeature(typename, url=alturl,
                      version=vp[1],httrType=vp[2],
                      propertyname=paste(pnames,collapse=','),
                      sortby= 'buurtcode D,buurtnaam A',
                      startindex=0,maxfeatures=5)

     })

  expect_true(inherits(res[[1]],"sf"))
  expect_equal(dim(res[[1]]),c(5,1+length(pnames)))
  expect_equal(names(res[[1]]),c("id", "buurtcode", "buurtnaam", "geometry" ))
  expect_equal(res[[1]],res[[2]])
  expect_equal(res[[1]],res[[3]])
  expect_equal(res[[1]],res[[4]])
})

test_that("WFS_getfeature set5", {
  # test that there are no differences for version 1.1.0 and 2.0.0
  #   with GET/POST for SRS/CRS selection with srsname='....'
  typename <- "cbs_buurten_2019" # "wijkenbuurten2019:cbs_buurten_2019"
  alturl   <- "https://geodata.nationaalgeoregister.nl/wijkenbuurten2019/wfs"

  comb     <- expand.grid(version  = c('1.1.0','2.0.0'),
                      httrType = c("GET","POST"))
  res      <- purrr::map(purrr::array_branch(comb,1), function(vp){
               WFS_getfeature(typename, url=alturl,
                      version=vp[1],httrType=vp[2],
                      srsname='EPSG:4326',
                      startindex=0,maxfeatures=5)

     })

  expect_true(inherits(res[[1]],"sf"))
  expect_equal(sf::st_crs(res[[1]],parameters=TRUE)$epsg,4326)
  expect_equal(dim(res[[1]])[1],5)
  expect_equal(res[[1]],res[[2]])
  expect_equal(res[[2]],res[[3]])
  expect_equal(res[[3]],res[[4]])

   res      <- purrr::map(purrr::array_branch(comb,1), function(vp){
               WFS_getfeature(typename, url=alturl,
                      version=vp[1],httrType=vp[2],
                    #  srsname='EPSG:4326', # so default expected
                      startindex=0,maxfeatures=5)

     })

  cap1   <- WFS_getcapabilities(url=alturl)
  defcrs <-tail(strsplit(
             WFS_featuretypes(cap1, filternames = typename)$defaultcrs,
             ':')[[1]],1)

  expect_true(inherits(res[[1]],"sf"))
  expect_equal(sf::st_crs(res[[1]],parameters=TRUE)$epsg,as.numeric(defcrs))
  expect_equal(dim(res[[1]])[1],5)
  expect_equal(res[[1]],res[[2]])
  expect_equal(res[[2]],res[[3]])
  expect_equal(res[[3]],res[[4]])
})


test_that ("WFS_getfeature set6", {
  # test that there are no differences for version 1.1.0 and 2.0.0
  #   with GET/POST for various outputformats
  typename <- "cbs_buurten_2019" # "wijkenbuurten2019:cbs_buurten_2019"
  alturl   <- "https://geodata.nationaalgeoregister.nl/wijkenbuurten2019/wfs"

  cap1     <- WFS_getcapabilities(url=alturl,version='1.1.0')
  cap2     <- WFS_getcapabilities(url=alturl,version='2.0.0')
  ofmt1    <- sort(WFS_util_parameter_values(cap1,'GetFeature','outputFormat'))
  ofmt2    <- sort(WFS_util_parameter_values(cap2,'GetFeature','outputFormat'))
  expect_identical(ofmt1,ofmt2)
  comb     <- expand.grid(version  = c('1.1.0','2.0.0'),
                      httrType = c("GET","POST"),
                      ofmt =sort(ofmt1),
                      KEEP.OUT.ATTRS = F,
                      stringsAsFactors = F)
  res      <- purrr::map(purrr::array_branch(comb,1), function(vp){
               WFS_getfeature(typename, url=alturl,
                      version=vp[1],httrType=vp[2],outputFormat=vp[3],
                      startindex=0,maxfeatures=1)
  })
  classes  <- purrr::map(res,class)

  # json outputs -> sf
  json1    <- grep('json',comb$ofmt,fixed = T)
  sf1      <- grep('sf',classes,fixed=T)
  expect_identical(json1,sf1)
  expect_equal(sort(unique(as.character(comb$ofmt[json1]))),
               c("application/json", "json") )

  eq_geom  <- purrr:: map_lgl(res[sf1],
                              ~identical(sf::st_geometry(.),
                                         sf::st_geometry(res[[sf1[1]]])))
  expect_true(all(eq_geom))
  eq_geom  <- purrr:: map_lgl(res[sf1],
                              ~identical(sf::st_drop_geometry(.),
                                         sf::st_drop_geometry(res[[sf1[1]]])))
  expect_true(all(eq_geom))

  # csv outputs -> char
  csv1     <- grep('csv',comb$ofmt,fixed = T)
  char1    <- grep('char',classes,fixed=T)
  expect_identical(csv1,char1)
  expect_equal(unique(as.character(comb$ofmt[csv1])),
               c("csv") )

  eq_char  <- purrr:: map_lgl(res[csv1],
                              ~identical(.,
                                         res[[csv1[1]]]))
  expect_true(all(eq_char))
  # kml -> xml
  kml1    <- grep('kml',comb$ofmt,ignore.case = T)
  test1   <- sort(unique(comb$ofmt[kml1]))
  test2   <- c("application/vnd.google-earth.kml xml",
                 "application/vnd.google-earth.kml+xml", "KML" )
  test2   <- sort(test2)
  expect_equal(test1,test2)

  rep1    <- purrr::map(res[kml1],xml2::as_list)
  eq_kml  <- purrr:: map_lgl(rep1,
                              ~identical(.,
                                         rep1[[1]]))
  expect_true(all(eq_kml))
  expect_identical(xml2::xml_name(res[[kml1[1]]]),"kml")

  # the other xml (gml)types
  gml1    <- grep('gml',comb$ofmt,ignore.case = T)
  ofmt1   <- as.character(comb$ofmt[gml1])
  # convert from xml_document to list
  rep1    <- purrr::map(res[gml1],xml2::as_list)
  eq_rep1 <- purrr::map_lgl(rep1,
                              ~identical(names(.),"FeatureCollection"))
  expect_true(all(eq_rep1))

  # first test results without attributes and geometry

  rep1n0  <- purrr::map(rep1, function(x) {
    n <- names(purrr::pluck(x, 1,))
    paste0(n, collapse = '_')
  })
  rep1n0 <- unlist(rep1n0)
  comb0  <- cbind(comb[gml1, ], str = rep1n0)
  #  gml2    fields boundedBy and featureMember
  #  gml3.1  fields featureMembers
  #  gml3.2  fields member

  rep1n1  <- purrr::map(rep1, function(x) {
    if (!is.null(x$FeatureCollection$boundedBy)) {
      purrr::pluck(x, 1, 1) <- NULL # remove boundedBy element
    }
    attributes(purrr::pluck(x, 1, 1, 1))[2]
  })
  n     <- names(unlist(rep1n1) )
  comb0 <- cbind(comb[gml1, ], n=n)
  #  gml2    attribute of this typename instance: fid
  #  gml3.1  attribute of this typename instance: id
  #  gml3.2  attribute of this typename instance: id

   rep1na  <- purrr::map(rep1, function(x) {
    if (!is.null(x$FeatureCollection$boundedBy)) {
      purrr::pluck(x,1,1) <- NULL # remove boundedBy element
    }
    geom_ix <- grep('geom',names(purrr::pluck(x,1,1,1)),fixed=T)
    purrr::pluck(x, 1, 1, 1, geom_ix)  <- NULL # remove geom part
    n <- names(purrr::pluck(x, 1, 1, 1))  # save names
    attributes(purrr::pluck(x, 1, 1, 1)) <- NULL # remove fid (gml2) or id (gml3) attribute
    names(purrr::pluck(x, 1, 1, 1)) <- n  # reload names
    attributes(x$FeatureCollection) <- NULL # remove xlmns attributes and names member/featureMember/featureMember
    x
  })

  eq_rep1na <- purrr::map_lgl(rep1na, ~identical(.,rep1na[[1]]))
  expect_true(all(eq_rep1na))

  # then test geometry
  # (we will see differences between gml versions, not GET/POST or WFS version)
  ix_a      <- seq_along(gml1)
  rep1g  <- purrr::map(rep1, function(x) {
    if (is.null(x$FeatureCollection$boundedBy)) {
      geom_ix <- grep('geom',names(purrr::pluck(x,1,1,1)),fixed=T)
      purrr::pluck(x, 1, 1, 1, geom_ix)
    } else {
      geom_ix <- grep('geom',names(purrr::pluck(x,1,2,1)),fixed=T)
      purrr::pluck(x, 1, 2, 1, geom_ix)
    }
  })

  eq_rep1g <- purrr::map_lgl(rep1g, ~identical(.,rep1g[[1]]))

  testa <-  c("application/gml+xml; version=3.2","gml32","text/xml; subtype=gml/3.2" )
  testa <-  sort(testa)
  # rep1g[[match(T,eq_rep1g)]] : MultiSurface poslist srsName "urn:ogc:def:crs:EPSG::28992" srsDimension
  testb <-  c("GML2","text/xml; subtype=gml/2.1.2")
  testb <-  sort(testb)
  # rep1gb[[match(T,eq_rep1gb)]] : MultiPolygon coordinates srsName http ..../epsg.xml#28992 decimal etc
  testc <-  c("gml3","text/xml; subtype=gml/3.1.1")
  testc <-  sort(testc)
  # rep1gc[[match(T,eq_rep1gc)]] : MultiSurface poslist srsName "urn:x-ogc:def:crs:EPSG:28992" srsDimension
  testabc <- list(testa,testb,testc)

  test1 <- sort(unique(ofmt1[eq_rep1g]))
  ix    <- purrr::map_lgl(testabc, ~identical(.,test1))
  expect_equal(sum(ix), 1 )
  testabc <- testabc[!ix]

  ix_b     <- setdiff(ix_a,ix_a[eq_rep1g])
  rep1gb   <- rep1g[ix_b]
  ofmt1b   <- ofmt1[ix_b]
  ix_b      <- seq_along(ofmt1b)
  eq_rep1gb <- purrr::map_lgl(rep1gb, ~identical(., rep1gb[[1]]))
  test1 <- sort(unique(ofmt1b[eq_rep1gb]))
  ix    <- purrr::map_lgl(testabc, ~identical(.,test1))
  expect_equal(sum(ix), 1 )
  testabc <- testabc[!ix]

  ix_c     <- setdiff(ix_b,ix_b[eq_rep1gb])
  rep1gc   <- rep1gb[ix_c]
  ofmt1c   <- ofmt1b[ix_c]
  ix_c      <- seq_along(ofmt1c)
  eq_rep1gc <- purrr::map_lgl(rep1gc, ~identical(.,rep1gc[[1]]))
  test1 <- sort(unique(ofmt1c[eq_rep1gc]))
  ix    <- purrr::map_lgl(testabc, ~identical(.,test1))
  expect_equal(sum(ix), 1 )
  testabc <- testabc[!ix]
  expect_equal(length(testabc),0)

  ix_d <- setdiff(ix_c,ix_c[eq_rep1gc])
  expect_equal(length(ix_d),0)

  # then test attributes
  # (we will see the following sets with differences (but not between GET/POST apart from `next` in 2.0.0 )
  #    gml 3.2  and WFS 1.1.0 versus 2.0.0
  #    gml 2    all WFS versions
  #    gml 3.1 (3) and WFS 1.1.0 versus 2.0.0 )

  # gml 3.2 wfs 1.1.0 POST/GET (name "member" numberMatched unknown )
  # gml 3.2 wfs 2.0.0 POST/GET (name "member" numberMatched is filled)
  # gml 2   wfs 1.1.0 and 2.0.0 POST/GET (name "featuremember" no numberMatched and numberOfFeatures
  #                                         and schemaLocation always 1.0.0 version)
  # gml 3.1 wfs 1.1.0 POST/GET (name "featureMembers" numberOfFeatures no numberMatched
  #                                         and schemaLocation  1.1.0 version)
  # gml 3.1 wfs 2.0.0 POST/GET (name "featureMembers" numberOfFeatures no numberMatched
  #                                         and schemaLocation always 2.0.0 version)

  rep1a  <- purrr::map(rep1, function(x) {
    a <- attributes(x$FeatureCollection)
    a$timeStamp <- NULL
    a$`next` <- NULL
    a
  })


  x = tibble::tibble(v1=list(testa,testa,testb,testb,testc,testc),
                     v2=c('1.1.0','2.0.0','1.1.0','2.0.0','1.1.0','2.0.0'))
  comb2 = comb[gml1,c('ofmt','version')]

  for (count in 1:5) {
    eq_rep1a <- purrr::map_lgl(rep1a, ~ identical(., rep1a[[1]]))
    test1  <- comb2[eq_rep1a, ]
    test1a <- sort(unique(test1[, 'ofmt']))
    #print(test1a)
    test1b <- sort(unique(test1[, 'version']))
    #print(test1b)
    #print(rep1a[[1]])
    ix     <- purrr::map_lgl(x$v1, ~ identical(., test1a)) &
      purrr::map_lgl(x$v2, ~ . %in% test1b)
    expect_true(sum(ix) %in% 1:2)
    x      <- x[!ix, ]
    comb2 <- comb2[!eq_rep1a, ]
    rep1a <- rep1a[!eq_rep1a]
  }

  expect_equal(nrow(comb2),0)
  expect_equal(nrow(x),0)
  expect_equal(length(rep1a),0)

})


test_that("WFS_getfeature set7", {
  typename <-"topp:gidw_groenbomen"
  pnames   <- c("boom_omschrijf", "jaar")
  version1 <- '1.1.0'
  version2 <- '2.0.0'
  # tests for case without queries 1.1.0 and 2.0.0
  fti1     <- WFS_describefeaturetype(typename,version=version1)$name
  fti2     <- WFS_describefeaturetype(typename,version=version2)$name
  expect_identical(fti1,fti2)
  fti1a    <- c("id",stringr::str_replace(fti1,'^geometrie$','geometry'))

  wfs1     <-  WFS_getfeature(typename,version=version1)
  wfs2     <-  WFS_getfeature(typename,version=version2)
  expect_identical(wfs1,wfs2)
  expect_identical(fti1a,names(wfs1))
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
