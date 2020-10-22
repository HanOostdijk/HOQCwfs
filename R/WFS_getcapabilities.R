
#' GetCapabilities for the WFS service
#'
#' Retrieve the GetCapabilities document with request version `version` for WFS service `url`
#' @param url URL of the WFS service. See [WFS_get_url()] for the default
#' @param version software version for WFS service request. See [WFS_get_version()] for the default
#' @param out_path (optional) path where the xml result is to be saved
#' @return xml document with the `GetCapabilities` information for this WFS service
#' or character string 'UNEXPECTED ERROR' when an error was encountered
#' @export
#' @examples
#' WFS_getcapabilities("https://geoweb.amstelveen.nl/geoserver/topp/wfs","1.1.0","gc.xml")

WFS_getcapabilities <- function(url=WFS_get_url(),version=WFS_get_version(),out_path=NULL){
  url       <- httr::parse_url(url)
  url$query <- list(service = "WFS"
                    ,version = version
                    ,request = "GetCapabilities")
  request   <- httr::build_url(url)
  xml_doc <- 'UNEXPECTED ERROR' ;
  suppressWarnings(xml_doc <- try(xml2::read_xml((request),options="NOWARNING"),silent=TRUE))
  if ( ('try-error' %in% class(xml_doc) || (xml_doc == 'UNEXPECTED ERROR') )) {
    return('UNEXPECTED ERROR')
  }

  xml2::xml_ns_strip(xml_doc)
  if (!is.null(out_path)) {
    unlink(out_path)
    xml2::write_xml(xml_doc,out_path)
  }
  xml_doc
}
