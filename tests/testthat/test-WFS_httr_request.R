
test_that("WFS_httr_request.R", {
  # library(HOQCwfs)
  # library(testthat)

reqP <- paste0(
 '<?xml version="1.0" encoding="ISO-8859-1"?>',
 '<GetCapabilities service="WFS" version="1.1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ',
 'xmlns="http://www.opengis.net/wfs" xmlns:wfs="http://www.opengis.net/wfs" ',
 'xmlns:ows="http://www.opengis.net/ows" xmlns:gml="http://www.opengis.net/gml" ',
 'xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" ',
 'xsi:schemaLocation="http://www.opengis.net/wfs"/>'
)
cap1 <- httr_POST_request(WFS_get_url(),reqP)
expect_true(inherits(cap1,'xml_document'))
expect_equal(xml2::xml_name(cap1),"WFS_Capabilities")
expect_equal(WFS_util_attrs(cap1,as.text=F)[["version"]],"1.1.0")

reqG <- "https://geoweb.amstelveen.nl/geoserver/topp/wfs?service=WFS&version=2.0.0&request=GetCapabilities"
cap2 <- httr_GET_request(reqG)
expect_true(inherits(cap2,'xml_document'))
expect_equal(xml2::xml_name(cap2),"WFS_Capabilities")
expect_equal(WFS_util_attrs(cap2,as.text=F)[["version"]],"2.0.0")

x=HOQCutil::capture.output.both(
  cap3 <- httr_GET_request(reqG,httrverbose=c(T))
  )
expect_true(inherits(cap3,'xml_document'))
expect_equal(xml2::xml_name(cap3),"WFS_Capabilities")
expect_equal(cap2,cap3)
expect_true(all (
       stringr::str_detect(paste(x,collapse=' '),
            c("-> GET", "<- Content-Type: application/xml"))
         ))
})

