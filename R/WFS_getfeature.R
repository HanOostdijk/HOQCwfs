
#' Retrieve WFS information with the GetFeature request
#'
#' Retrieve the requested information
#' @param typename Character with the name of a feature (such as e.g. found by using [WFS_featuretypes()] )
#' @param ... optional arguments for the `GetFeature` request. See **Details**
#' @param url URL of the WFS service. See [WFS_get_url()] for the default
#' @param version software version for WFS service request. See [WFS_get_version()] for the default#'
#' @param debug Logical indicating only json output required
#' @return a `sf` object with the requested information (the 'id' variable and the geometry will always be included) or a character string with an error message
#' @export
#' @details Arguments that are recognized by the GetFeature request are:
#' - `bbox        ` (not in combination with `cql_filter` or `filter`)
#' - `cql_filter  `
#' - `filter      `
#' - `srsname     ` indicate the crs for the coordinates e.g. `srsname='EPSG:4326'`
#' - `propertyname` the name of the fields to retrieve. The `id` and `geometry` fields will always be included.
#' - `startIndex  ` number of features to skip before retrieving features ( the output with `startindex=1` will start with the second feature )
#' - `maxfeatures ` (only for version `1.1.0`) or `count` (only for version `2.0.0`) indicates the number of features to retrieve
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

WFS_getfeature <- function(typename, ..., url=WFS_get_url(),version=WFS_get_version(),debug=F){
  url       <- httr::parse_url(url)
  url$query <- list(service = "WFS"
        ,version    = version
        ,request    = "GetFeature"
        ,typename   = typename
        ,outputFormat = "application/json"
        )
  url$query = append(url$query,list(...))
  request <- httr::build_url(url)
  res_data <- NULL ;
  if (debug == F) {
    suppressWarnings(res_data <- try(sf::read_sf(request,as_tibble=F),silent=TRUE))
    # TODO split in request part and convert-to-sf part
    }
  if (debug || ('try-error' %in% class(res_data))) {
    url$query = append(url$query, list(exceptions = "application/json"))
    res = httr::GET(httr::build_url(url))
    if (debug)
      return(res)
    cnt1 = httr::headers(res)$`content-type`
    txt1 = httr::content(res, as = "text", encoding = 'UTF-8')
    if (stringr::str_detect(cnt1,
                            stringr::fixed('json', ignore_case = T))) {
      res_data <- jsonlite::fromJSON(txt1)
      if (!is.null(res_data$exceptions$text))
         res_data <- res_data$exceptions$text
    } else if (stringr::str_detect(cnt1,
                                   stringr::fixed('xml', ignore_case = T))) {
       res_data <- xml2::read_xml(txt1, options = "NOWARNING")
       res_data <- xml2::xml_text(xml2::xml_find_first(res_data,'.//ows:ExceptionText'))
    }

  } else {
    row.names(res_data) <- NULL
  }
  res_data
}

