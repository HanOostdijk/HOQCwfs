
#' Retrieve WFS information with the GetFeature request
#'
#' Retrieve the requested information
#' @param typename Character with the name of a feature (such as e.g. found by using [WFS_featuretypes()] )
#' @param ... optional arguments for the `GetFeature` request. See **Details**
#' @param url URL of the WFS service. See [WFS_get_url()] for the default
#' @param version software version for WFS service request. See [WFS_get_version()] for the default
#' @param debug Logical indicating the httr response is to be returned
#' @param verbose Logical indicating full request and httr response code will be displayed
#' @return a `sf` object with the requested information (the 'id' variable and the geometry will always be included)
#' or a character string with an error message
#' @export
#' @details Arguments that are recognized by the GetFeature request are:
#' - `bbox        ` (not in combination with `cql_filter` or `filter`)
#' - `cql_filter  `
#' - `filter      `
#' - `resultType` - default 'results' and alternative 'hits'. In the latter case only the number of matched features (`numbeOfFeatures` in 1.1.0 or `numberMatched` in 2.0.0) is returned
#' - `srsname     ` indicate the crs for the coordinates e.g. `srsname='EPSG:4326'`
#' - `propertyname` the name of the fields to retrieve. The `id` and `geometry` fields will always be included.
#' - `startIndex  ` number of features to skip before retrieving features ( the output with `startindex=1` will start with the second feature )
#' - `maxfeatures ` (only for version `1.1.0`) or `count` (only for version `2.0.0`) indicates the number of features to retrieve
#' The `GetFeature` argument `outputFormat` has value 'application/json'but this can be overwritten
#'
#' @examples
#' \dontrun{
#' typename <- 'topp:gidw_groenbomen'
#' wfs1 = WFS_getfeature(typename)
#' wfs2 = WFS_getfeature(typename,
#'    cql_filter= r"(boom_omschrijf='Prunus serrulata ''Kanzan''')" ) # double internal quotes !
#' wfs3 = WFS_getfeature(typename,
#'    bbox = '119038,479244,119500,479500') # in EPSG:28992  (default see WFS_featuretypes)
#' wfs3a = WFS_getfeature(typename,
#'   cql_filter= r"( bbox(geometrie,119038,479244,119500,479500,'EPSG:28992') and (boom_omschrijf='Acer freemanii ''Elegant'''))")
#' wfs3b = WFS_getfeature(typename,
#'   cql_filter= r"( bbox(geometrie,119038,479244,119500,479500,'EPSG:28992'))")
#' wfs4 = WFS_getfeature(typename,
#'    bbox = '119038,479244,119500,479500,EPSG:28992',  # in EPSG:28992
#'    srsname = 'EPSG:4326') # but request coordinates in  WGS84 (EPSG:4326)
#' f5 = fg("Filter"
#' , fg(
#'   "PropertyIsEqualTo"
#'   , bg('PropertyName', 'topp:boom_omschrijf')
#'   , bg('Literal',  r"(Acer freemanii 'Elegant')" )#'
#'   )
#' )
#' f5
#' wfs5 = WFS_getfeature(typename,
#'    srsname = 'EPSG:4326', # but request coordinates in  WGS84 (EPSG4326)
#'    filter  = f5)
#' }

WFS_getfeature <- function(typename, ..., url=WFS_get_url(),version=WFS_get_version(),debug=F,verbose=F){
  if (! (version %in% c('1.1.0','2.0.0') ) )
    return("only version '1.1.0' and '2.0.0' are allowed")
  url       <- httr::parse_url(url)
  url$query <- list(service = "WFS"
        ,version    = version
        ,request    = "GetFeature"
        ,typename   = typename
        ,outputFormat = "application/json"
        )
  uniq <- !(names(url$query) %in% names(list(...)) ) # enable replace
  url$query = append(url$query[uniq],list(...))
  request <- httr::build_url(url)
  res <- WFS_GET_request (request,debug=debug,to_sf=T,verbose=verbose)
  if ( inherits(res,'data.frame') ) row.names(res) <- NULL
  if ( inherits(res,'xml_document') && (xml2::xml_name(res) == 'FeatureCollection') ){
    if ( version =='1.1.0' ) {
       res1 <- xml2::xml_attr(res,'numberOfFeatures',NULL)
       if (!is.null(res1)) res<- as.numeric(res1)
    }
    else if ( version =='2.0.0' ) {
       res1 <- xml2::xml_attr(res,'numberMatched',NULL)
       if (!is.null(res1)) res<- as.numeric(res1)
    }
  }
  res
}

#' Retrieve information with the GET request
#'
#' Retrieve the requested information
#' @param request Character string with expanded url
#' @param debug Logical indicating the httr response is to be returned
#' @param verbose Logical indicating full request and httr response code will be displayed
#' @return a `json` object when that is returned but converted to an `sf` object when `to_sf==T`.
#' When the GET returns an `xml` object it is returned but unpacked if it is an `ExceptionReport`.
#' @export
#' @examples
#' \dontrun{
#' }

WFS_GET_request <- function (request,debug=F,to_sf=T,verbose=F){
  suppressWarnings(res <- try(httr::GET(request),silent=TRUE))
  if (debug || ('try-error' %in% class(res))) return(res)
  if (verbose)
    cat(URLdecode(res$url),httr::http_status(res)$message,sep='\n')
  if (httr::http_error(res)) return(httr::http_status(res)$message)
	cnt1 = tolower(httr::headers(res)$`content-type`)
  res_data <- httr::content(res,encoding = 'UTF-8',as='text')
  if (grepl('json',cnt1,fixed = T)) {
    if (to_sf) {
      r <- sf::read_sf(res_data,quiet=T,as_tibble = F)
    } else {
      r <- jsonlite::fromJSON(res_data)
    }
  } else if (grepl('xml',cnt1,fixed = T)) {
      r <- xml2::read_xml(res_data, options = "NOWARNING")
      # if (xml2::xml_name(r) == 'ExceptionReport')
      #   r <- xml2::xml_text(xml2::xml_find_first(r,'.//ows:ExceptionText'))
  } else {
      r <- res_data
  }
  r
}
