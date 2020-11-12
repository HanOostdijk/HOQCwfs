
# library(HOQCwfs)
# library(testthat)

test_that("WFS_util_showxml", {
local_edition(3)

my_list <- list(
  root = list(
    a=list(2, b=list(x1='x1',x2='y',x3='z'),c='text1'),
    a=list(3, b=list(x1='a',x2='b2',x3='c'),c='text2'),
    b=list(4, b=list(x1='a',x2='b2',x3='c'),c='text2')
  )
)

my_xml <- xml2::as_xml_document(my_list)
expect_true(inherits(my_xml,"xml_document"))
expect_snapshot_output(WFS_util_showxml(my_xml))

my_xml_node <-xml2::xml_find_first(my_xml,'.//a')
expect_true(inherits(my_xml_node,"xml_node"))
expect_snapshot_output(WFS_util_showxml(my_xml_node))

my_xml_nodeset <-xml2::xml_find_all(my_xml,'.//a')
expect_true(inherits(my_xml_nodeset,"xml_nodeset"))
expect_snapshot_output(WFS_util_showxml(my_xml_nodeset))

expect_true(is.null(WFS_util_showxml('no xml object')))

})

test_that("WFS_util_parameter_values", {
local_edition(3)

cap1 <- WFS_getcapabilities(version='1.1.0')
cap2 <- WFS_getcapabilities(version='2.0.0')

x <- F
suppressWarnings({
  x=WFS_util_parameter_values(cap1)
  })
expect_true(is.null(x))
xx <- "?? is not a valid operation\nchoose one from `GetCapabilities`, `DescribeFeatureType`, `GetFeature`, `GetGmlObject`, `LockFeature`, `GetFeatureWithLock` and `Transaction`"
expect_warning(
  WFS_util_parameter_values(cap1),
  fixed=T,regexp=xx )

x <- F
suppressWarnings({
  x=WFS_util_parameter_values(cap2)
  })
expect_true(is.null(x))
xx <- "?? is not a valid operation\nchoose one from `GetCapabilities`, `DescribeFeatureType`, `GetFeature`, `GetPropertyValue`, `ListStoredQueries`, `DescribeStoredQueries`, `CreateStoredQuery`, `DropStoredQuery`, `LockFeature`, `GetFeatureWithLock` and `Transaction`"
expect_warning(
  WFS_util_parameter_values(cap2),
  fixed=T,regexp=xx )

x <- F
suppressWarnings({
  x=WFS_util_parameter_values(cap1,operation='GetCapabilities')
  })
expect_true(is.null(x))
xx <- "?? is not a valid parameter\nchoose `AcceptVersions` and `AcceptFormats`"
expect_warning(
  WFS_util_parameter_values(cap1,operation='GetCapabilities'),
  fixed=T,regexp=xx )

x <- F
suppressWarnings({
  x=WFS_util_parameter_values(cap2,operation='GetCapabilities')
  })
expect_true(is.null(x))
xx <- "?? is not a valid parameter\nchoose `AcceptVersions` and `AcceptFormats`"
expect_warning(
  WFS_util_parameter_values(cap2,operation='GetCapabilities'),
  fixed=T,regexp=xx )


x<-WFS_util_parameter_values(cap1,operation='GetCapabilities',parameter="AcceptVersions")
expect_identical(x,c('1.0.0','1.1.0'))

x<-WFS_util_parameter_values(cap2,operation='GetCapabilities',parameter="AcceptVersions")
expect_identical(x,c('1.0.0','1.1.0','2.0.0'))
})

test_that("WFS_util_attrs", {

cap1 <- WFS_getcapabilities(version='1.1.0')

x    <- WFS_util_attrs(cap1)
expect_true( inherits(x,"character") )
expect_true( grepl('version="1.1.0"',x,fixed=T) )
expect_true( grepl('xmlns:wfs=',x,fixed=T) )

x    <- WFS_util_attrs(cap1,skip="version")
expect_true( inherits(x,"character") )
expect_false( grepl('version="1.1.0"',x,fixed=T) )
expect_true( grepl('xmlns:wfs=',x,fixed=T) )

x    <- WFS_util_attrs(cap1,skip="known")
expect_true( inherits(x,"character") )
expect_false( grepl('version="1.1.0"',x,fixed=T) )
expect_false( grepl('xmlns:wfs=',x,fixed=T) )

cap2 <- WFS_getcapabilities(version='2.0.0')
x1   <- WFS_util_attrs(cap2,as.text=F)
expect_true( inherits(x1,"list") )
y1   <- names(x1)
expect_true( 'version' %in% y1 )
expect_true( 'xmlns:wfs' %in% y1 )
expect_identical( x1[['version']],'2.0.0')

x2    <- WFS_util_attrs(cap2,skip="version",as.text=F)
expect_true( inherits(x2,"list") )
y2    <- names(x2)
expect_false( 'version' %in% y2 )
expect_true( 'xmlns:wfs' %in% y2 )
expect_identical( y2, setdiff(y1,"version"))

x3    <- WFS_util_attrs(cap2,skip="known",as.text=F)
expect_true( inherits(x3,"list") )
y3    <- names(x3)
expect_false( 'version' %in% y3 )
expect_false( 'xmlns:wfs' %in% y3 )

})


test_that("WFS_util_xmlns_defs", {

x     <- WFS_util_xmlns_defs()
expect_true( inherits(x,"character") )
expect_true( length(x) == 1 )
expect_named( x, NULL )
expect_false( grepl('version="1.1.0"',x,fixed=T) )
expect_true( grepl('xmlns:ogc="http://www.opengis.net/ogc"',x,fixed=T) )
expect_false( grepl('xmlns:fes="http://www.opengis.net/fes/2.0"',x,fixed=T) )

x     <- WFS_util_xmlns_defs(version='2.0.0')
expect_true( inherits(x,"character") )
expect_true( length(x) == 1 )
expect_named( x, NULL )
expect_false( grepl('version="2.0.0"',x,fixed=T) )
expect_false( grepl('xmlns:ogc="http://www.opengis.net/ogc"',x,fixed=T) )
expect_true( grepl('xmlns:fes="http://www.opengis.net/fes/2.0"',x,fixed=T) )

x     <- WFS_util_xmlns_defs(as.txt=F)
expect_true( inherits(x,"character") )
expect_true( length(x) > 1 )
expect_named( x, NULL )
expect_false( any(grepl('version=',x,fixed=T)) )
expect_true( any(grepl('xmlns:ogc="http://www.opengis.net/ogc"',x,fixed=T)) )
expect_false( any(grepl('xmlns:fes="http://www.opengis.net/fes/2.0"',x,fixed=T)) )

x     <- WFS_util_xmlns_defs(version='2.0.0',as.txt=F)
expect_true( inherits(x,"character") )
expect_true( length(x) > 1 )
expect_named( x, NULL )
expect_false( any(grepl('version=',x,fixed=T)) )
expect_false( any(grepl('xmlns:ogc="http://www.opengis.net/ogc"',x,fixed=T)) )
expect_true( any(grepl('xmlns:fes="http://www.opengis.net/fes/2.0"',x,fixed=T)) )

})

test_that("WFS_util_replace_names", {

L <- list( a = 1, A = 2, aA = 3, Aa = 4,
           b = 6, B = 7, bB = 8, Bb = 9)
Lu <- unname(L)
#names(L)  c("a", "A", "aA", "Aa", "b", "B","bB", "Bb"))

x = WFS_util_replace_names(L,'a','X')
expect_identical( names(x), c("X", "X", "aA", "Aa", "b", "B", "bB", "Bb"))
expect_identical( unname(x), Lu)

x = WFS_util_replace_names(L,'A' ,'X')
expect_identical( names(x), c("X", "X", "aA", "Aa", "b", "B", "bB", "Bb"))
expect_identical( unname(x), Lu)

x = WFS_util_replace_names(L,c('a','b'),c('X','Y'))
expect_identical( names(x), c("X", "X", "aA", "Aa", "Y", "Y", "bB", "Bb"))
expect_identical( unname(x), Lu)

x = WFS_util_replace_names(L,c('a','b'),c('b','a'))
expect_identical( names(x), c("b", "b", "aA", "Aa", "a", "a", "bB", "Bb"))
expect_identical( unname(x), Lu)
})

test_that("WFS_util_v12_names", {

vn <- WFS_util_v12_names()
expect_true( inherits(vn,"list") )
expect_equal( length(vn),3)
expect_true( all(purrr::map_dbl(vn,length)==length(vn[[1]])))
expect_true( all(purrr::map_chr(vn,class)=="character"))

})


test_that("WFS_util_unify_names", {

L <- list( a = 1, A = 2, aA = 3, Aa = 4,
           b = 6, B = 7, bB = 8, Bb = 9)
vnames <- list( c('X','Y'), c('a', 'aa'), c('b', 'bb'))

x <- WFS_util_unify_names(L,vnames)
expect_true( inherits(x,"list") )
expect_equal( unname(x), unname(L))
expect_equal( names(x),c("X", "X", "Y", "Y", "X", "X", "Y", "Y"))

})

test_that("WFS_util_keep_unique", {

L <- list( a = 1, A = 2, aA = 3, Aa = 4,
           a = 6, A = 7, aA = 8, Aa = 9)

x <- WFS_util_keep_unique(L,keep_first = T, ignore.case=T)
expect_true( inherits(x,"list") )
expect_equal( x, list(a = 1, aA = 3))
x <- WFS_util_keep_unique(L,keep_first = F, ignore.case=T)
expect_true( inherits(x,"list") )
expect_equal( x, list(A = 7, Aa = 9))
x <- WFS_util_keep_unique(L,keep_first = T, ignore.case=F)
expect_true( inherits(x,"list") )
expect_equal( x, list(a = 1, A = 2, aA = 3, Aa =4))
x <- WFS_util_keep_unique(L,keep_first = F, ignore.case=F)
expect_true( inherits(x,"list") )
expect_equal( x, list(a = 6, A = 7, aA = 8, Aa =9))
})

test_that("WFS_util_check_in_list", {

L <- list( a = "a", A = "b", aA = "c", Aa = "d",
           a = "e", A = "f", aA = "g", Aa = "h")

x <- WFS_util_check_in_list(L, "a" ,"a",
            keep_first = T, name_ignore_case=T,value_ignore_case=T)
expect_true( x)
x <- WFS_util_check_in_list(L, "a" ,"a",
            keep_first = F, name_ignore_case=T,value_ignore_case=T)
expect_false( x)
x <- WFS_util_check_in_list(L, "a" ,"f",
            keep_first = F, name_ignore_case=T,value_ignore_case=T)
expect_true( x)
x <- WFS_util_check_in_list(L, "A" ,"a",
            keep_first = T, name_ignore_case=F,value_ignore_case=T)
expect_false( x)
x <- WFS_util_check_in_list(L, "A" ,"b",
            keep_first = T, name_ignore_case=F,value_ignore_case=T)
expect_true( x)
x <- WFS_util_check_in_list(L, "a" ,"e",
            keep_first = F, name_ignore_case=F,value_ignore_case=T)
expect_true( x)
x <- WFS_util_check_in_list(L, "a" ,"E",
            keep_first = F, name_ignore_case=F,value_ignore_case=T)
expect_true( x)
x <- WFS_util_check_in_list(L, "a" ,"E",
            keep_first = F, name_ignore_case=F,value_ignore_case=F)
expect_false( x)
})



