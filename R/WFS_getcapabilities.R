
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

WFS_getcapabilities <- function(
      url=WFS_get_url(),
      version=WFS_get_version(),
      debug=F,
      verbose=F,
      out_path=NULL){
  cv <- check_version(version )
  if (! is.null(cv) )  return(cv)

  url       <- httr::parse_url(url)
  url$query <- list(service = "WFS"
                    ,version = version
                    ,request = "GetCapabilities")
  request   <- httr::build_url(url)
  xml_doc <- WFS_GET_request (request, debug=debug,
                              to_sf=F,verbose=verbose)
  handle_cap_output(xml_doc,debug,out_path)
}

handle_cap_output <- function(xml_doc,debug,out_path) {
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


#' Retrieves the GetCapabilities document
#'
#' @param url Character with base url for WFS service
#' @param version  software version for WFS service request. See [WFS_get_version()] for the default
#' @param debug Logical indicating the httr response is to be returned
#' @param verbose Logical indicating full request and httr response code will be displayed
#' @param out_path (optional) path where the xml result is to be saved
#' @return xml document with the `GetCapabilities` information for this WFS service
#' or character string 'UNEXPECTED ERROR' when an error was encountered
#' @export
#' @examples
#' \dontrun{
#' base_url  <- "https://geodata.nationaalgeoregister.nl/wijkenbuurten2019/wfs"
#' cap1 = WFS_getcapabilities_POST(base_url,'1.1.0')
#' WFS_util_showxml(cap1,lines=2:3)
#' cap2 <- WFS_getcapabilities_POST(url=WFS_get_url(),version='1.1.0')
#' WFS_util_showxml(cap2,lines=2:3)
#' }

WFS_getcapabilities_POST <- function(
      url=WFS_get_url(),
      version=WFS_get_version(),
      debug=F,
      verbose=F,
      out_path=NULL){
  xmlns <- paste(WFS_util_xmlns_defs(version),collapse = ' ')
  fgh <- '<?xml version="1.0" encoding="ISO-8859-1"?>'
  fg1 <- fg('GetCapabilities'
          , ta=glue::glue('service="WFS" version="{version}" {xmlns}' )
        )
  fg0  <- glue::glue_collapse(c(fgh,fg1),sep='\n')
  xmldoc  <- HOQCwfs:::WFS_POST_request(fg0,url,debug = debug,
                                        to_sf = F, verbose = verbose)
  handle_cap_output(xml_doc,debug,out_path)
}
