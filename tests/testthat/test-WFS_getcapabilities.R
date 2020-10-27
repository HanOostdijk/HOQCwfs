test_that("WFS_getcapabilities checks", {
  # library(HOQCwfs)
  # library(testthat)
  v_in = 'x.y.z'
  xml_doc = WFS_getcapabilities(version=v_in)
  expect_equal(xml_doc,"only version '1.1.0' and '2.0.0' are allowed")

  v_in = '1.1.0'
  f_out = 'HOQCwfs_temp.xml'
  xml_doc = WFS_getcapabilities(version=v_in,out_path = f_out)
  v_out <- xml_doc %>%
     xml2::xml_find_first("//wfs:WFS_Capabilities") %>%
     xml2::xml_attr("version")
  expect_equal(v_in,v_out)

  xml_doc2 = xml2::read_xml( f_out)
  v_out <- xml_doc2 %>%
     xml2::xml_find_first("//wfs:WFS_Capabilities") %>%
     xml2::xml_attr("version")
  unlink( f_out)
  expect_equal(v_in,v_out)

  xml_doc3 = WFS_getcapabilities(version=v_in,url='*UNKNOWN URL*')
  expect_true(grepl('URL using bad/illegal format or missing URL',
                    as.character(xml_doc3),fixed = T))

  v_in = '2.0.0'
  xml_doc = WFS_getcapabilities(version=v_in,out_path = f_out )
   v_out <- xml_doc %>%
     xml2::xml_find_first("//wfs:WFS_Capabilities") %>%
     xml2::xml_attr("version")
  expect_equal(v_in,v_out)

  xml_doc2 = xml2::read_xml( f_out)
  v_out <- xml_doc2 %>%
     xml2::xml_find_first("//wfs:WFS_Capabilities") %>%
     xml2::xml_attr("version")
  unlink( f_out)
  expect_equal(v_in,v_out)

  xml_doc3 = WFS_getcapabilities(version=v_in,url='*UNKNOWN URL*')
  expect_true(grepl('URL using bad/illegal format or missing URL',
                    as.character(xml_doc3),fixed = T))

  xml_doc4 = WFS_getcapabilities(version='x.1.2')
  expect_equal(xml_doc4,"only version '1.1.0' and '2.0.0' are allowed")

  testfile <- test_path('HOQCwfs_temp.txt')
  suppressWarnings({
    verify_output(testfile,xml_doc5b<-WFS_getcapabilities(version='2.0.0',verbose=T))
  })
  res5a <- c( '> xml_doc5b <- WFS_getcapabilities(version = "2.0.0", verbose = T)',
     'https://geoweb.amstelveen.nl/geoserver/topp/wfs?service=WFS&version=2.0.0&request=GetCapabilities',
     'Success: (200) OK', '')
  res5b <- readLines(testfile,warn = F)
  expect_equal(res5a,res5b[1:4])
  unlink(testfile)

  xml_doc6=WFS_getcapabilities(version=v_in,debug=T)
  expect_s3_class(xml_doc6,"response")
  res6 = glue::glue("{WFS_get_url()}?service=WFS&version={v_in}&request=GetCapabilities")
  expect_equal(xml_doc6$url,as.character(res6))

})
