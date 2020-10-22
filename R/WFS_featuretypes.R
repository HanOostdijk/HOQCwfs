
#' Retrieve featuretypes information
#'
#' Retrieve the names of the features with spatial information from a GetCapabilities document
#' @param xml_doc XML document with the output of a GetCapabilities document
#' @param filternames Character string that filters the returned featurenames
#' @return tibble with name, default CRS, lower and upper corner (in WGS84 coordinates) for each filtered featuretype name
#' @export
#' @examples
#' xml_cap1 <- WFS_getcapabilities()
#' FeatureTypes1 <- WFS_featuretypes(xml_cap1,
#'        filternames=stringr::fixed("bomen", ignore_case = T))

WFS_featuretypes <- function(xml_doc,filternames=NULL){
  `%>%` <- magrittr::`%>%`
   version <- xml_doc %>%
     xml2::xml_find_first("//wfs:WFS_Capabilities") %>%
     xml2::xml_attr("version")
  if (version == '2.0.0') {
    crs_clause1 <- ".//FeatureTypeList//FeatureType"
    crs_clause2 <- ".//DefaultCRS"
  } else {
    crs_clause1 <- ".//FeatureType"
    crs_clause2 <- ".//DefaultSRS"
  }
  features <- xml2::xml_find_all(xml_doc, crs_clause1)
  features <- purrr::map_dfr(features,
      function(ft) {
        bb_wgs84 <- ft %>% xml2::xml_find_first(".//ows:WGS84BoundingBox")
        c(layer=ft %>% xml2::xml_find_first(".//Name") %>% xml2::xml_text(),
        defaultcrs=ft %>% xml2::xml_find_first(crs_clause2) %>% xml2::xml_text(),
        lc_wgs84=bb_wgs84 %>% xml2::xml_find_first(".//ows:LowerCorner")
           %>% xml2::xml_text(),
        uc_wgs84=bb_wgs84 %>% xml2::xml_find_first(".//ows:UpperCorner")
           %>% xml2::xml_text()
        )
      })
   if (!is.null(filternames)) {
    features <-features %>%
      dplyr::filter( stringr::str_detect(layer, filternames) )
   }
  features
}

