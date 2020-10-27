
#' GetCapabilities for the WFS service
#'
#' Retrieve the GetCapabilities document with request version `version` for WFS service `url`
#' @param url URL of the WFS service. See [WFS_get_url()] for the default
#' @param version software version for WFS service request. See [WFS_get_version()] for the default
#' @param debug Logical indicating the httr response is to be returned
#' @param verbose Logical indicating full request and httr response code will be displayed
#' @param out_path (optional) path where the xml result is to be saved
#' @return xml document with the `GetCapabilities` information for this WFS service
#' or character string 'UNEXPECTED ERROR' when an error was encountered
#' @export
#' @examples
#' \dontrun{
#' WFS_getcapabilities("https://geoweb.amstelveen.nl/geoserver/topp/wfs","1.1.0",out_path="gc.xml")
#' }

WFS_getcapabilities <- function(url=WFS_get_url(),version=WFS_get_version(),debug=F,verbose=F,out_path=NULL){
  if (! (version %in% c('1.1.0','2.0.0') ) )
    return("only version '1.1.0' and '2.0.0' are allowed")
  url       <- httr::parse_url(url)
  url$query <- list(service = "WFS"
                    ,version = version
                    ,request = "GetCapabilities")
  request   <- httr::build_url(url)

  xml_doc <- WFS_GET_request (request,debug=debug,to_sf=F,verbose=verbose)

  if (debug) {
    return(xml_doc)
  } else if (inherits(xml_doc,"try-error"))  {
    return(as.character(xml_doc))
  } else if (inherits(xml_doc,"character"))  {
    return(xml_doc)
  } else if (inherits(xml_doc,"xml_document") && (xml2::xml_name(xml_doc) == 'ExceptionReport')) {
    return(xml2::xml_text(xml2::xml_find_first(xml_doc,'.//ows:ExceptionText')))
  } else if (inherits(xml_doc,"xml_document") && (xml2::xml_name(xml_doc) != 'WFS_Capabilities')) {
    return('UNKNOWN ERROR')
  }

  xml2::xml_ns_strip(xml_doc)
  if (!is.null(out_path)) {
    unlink(out_path)
    xml2::write_xml(xml_doc,out_path)
  }
  xml_doc
}
