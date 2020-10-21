test_that("WFS_getcapabilities checks", {
  v_in = '1.1.0'
  f_out = 'HOQCwfs_temp.xml'
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

  v_in = '2.0.0'
  xml_doc = WFS_getcapabilities(version=v_in)
  v_out <- xml_doc %>%
     xml2::xml_find_first("//wfs:WFS_Capabilities") %>%
     xml2::xml_attr("version")
  expect_equal(v_in,v_out)
})
