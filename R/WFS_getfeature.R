
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
#' - `maxfeatures ` (version `1.1.0`) or `count` (version `2.0.0`) indicates the number of features to retrieve. When the wrong argument is specified it will be translated in the other.
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
#'    srsname = 'EPSG:4326', # but request coordinates in  WGS84 (EPSG:4326)
#'    filter  = f5)
#' }

WFS_getfeature <- function(typename, ...,
                           url=WFS_get_url(),
                           version=WFS_get_version(),
                           debug=F,
                           sfverbose=F,
                           httrverbose=rep(F,4)){
  cv <- check_version(version )
  if (! is.null(cv) )  return(cv)

  base_url <- url
  url       <- httr::parse_url(url)
  url$query <- list(service = "WFS"
        ,version    = version
        ,request    = "GetFeature"
        ,typename   = typename
        ,outputFormat = "application/json"
        )
  url$query = append(url$query,list(...))
  url$query <- WFS_util_keep_unique(url$query, keep_first = F)
  url$query <- build_request_GET(url$query)
  request <- httr::build_url(url)

  res <- WFS_GET_request (request, debug=debug,
             to_sf=T, sfverbose=sfverbose, httrverbose=httrverbose)

  if (inherits(res, 'data.frame'))
    row.names(res) <- NULL
  if (inherits(res, 'xml_document')) {
    if ((xml2::xml_name(res) == 'FeatureCollection')
        && WFS_util_check_in_list(url$query,'resulttype','hits')) {
      if (version == '1.1.0') {
        res1 <- xml2::xml_attr(res, 'numberOfFeatures', NULL)
        if (!is.null(res1))
          res <- as.numeric(res1)
      }
      else if (version == '2.0.0') {
        res1 <- xml2::xml_attr(res, 'numberMatched', NULL)
        if (!is.null(res1))
          res <- as.numeric(res1)
      }
    } else if (xml2::xml_name(res) == 'ExceptionReport')
        res <-
          xml2::xml_text(xml2::xml_find_first(res, './/ows:ExceptionText'))
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

WFS_GET_request <- function (request,
           debug = F,
           to_sf = T,
           sfverbose = F,
           httrverbose = rep(F, 4)) {
  suppressWarnings(res <- try({
    if (any(httrverbose == T)) {
      res <- httr::GET(request,
                       do.call(httr::verbose, as.list(httrverbose)))
    } else {
      res <- httr::GET(request)
    }
    # httr::GET(request)
  }, silent = TRUE)
  )
  handle_res(res,debug,to_sf,sfverbose)
}

build_request_GET <- function(reqlist) {
  version <- reqlist[["version"]] # (latest) version
  vnames  <- WFS_util_v12_names()
  replarg <- (ifelse (version == '1.1.0',
                     vnames[2],vnames[3]))[[1]]
  reqlist <- WFS_util_unify_names(reqlist,vnames)
  reqlist <- WFS_util_keep_unique(reqlist, keep_first = F)
  WFS_util_replace_names(reqlist, vnames[[1]], replarg)
}

