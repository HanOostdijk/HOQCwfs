
#' Retrieve WFS information with the GetFeature request
#'
#' Retrieve the requested information.
#' @param typename Character with the name of a feature (such as e.g. found by using [WFS_featuretypes()] )
#' @param ... optional arguments for the `GetFeature` request. See **Details**
#' @param httrType Character with the type of httr request: `GET` or `POST`
#' @param url URL of the WFS service. See [WFS_get_url()] for the default
#' @param version software version for WFS service request. See [WFS_get_version()] for the default
#' @param debug Logical indicating the httr response is to be returned
#' @param sfverbose Logical indicating if [sf::read_sf()] messages will be displayed
#' @param httrverbose Logical vector of up to four entries to be used in [httr::verbose()]
#' @return a `sf` object with the requested information (the 'id' variable and the geometry will always be included)
#' or a character string with an error message
#' @export
#' @details Arguments that are recognized by the GetFeature request are:
#' - `bbox        ` (not in combination with `cql_filter` or `filter`)
#' - `cql_filter  `
#' - `filter      `
#' - `resultType` - default 'results' and alternative 'hits'. In the latter case only the number of matched features (`numbeOfFeatures` in 1.1.0 or `numberMatched` in 2.0.0) is returned
#' - `srsname     ` indicate the crs for the coordinates of the output e.g. `srsname='EPSG:4326'`
#' - `propertyname` the name of the fields to retrieve. The `id` and `geometry` fields will always be included.
#' - `startIndex  ` number of features to skip before retrieving features ( the output with `startindex=1` will start with the second feature )
#' - `maxfeatures ` (version `1.1.0`) or `count` (version `2.0.0`) indicates the number of features to retrieve. When the wrong argument is specified it will be translated in the other.
#' The `GetFeature` argument `outputFormat` has value 'application/json' but this can be overwritten
#'
#' @examples
#' \dontrun{
#'
#' }

WFS_getfeature <- function(typename, ...,
         httrType= c("GET","POST"),
         url=WFS_get_url(),
         version=WFS_get_version(),
         debug=F,
         sfverbose=F,
         httrverbose=rep(F,4)){
    cv <- check_version(version)
    if (!is.null(cv))
      return(cv)
    httrType = match.arg(httrType)

    base_url <- url
    url       <- httr::parse_url(url)
    url$query <- list(
      service = "WFS"
      ,
      version    = version
      ,
      request    = "GetFeature"
      ,
      typename   = typename
      ,
      outputFormat = "application/json"
    )
    url$query = append(url$query, list(...))
    url$query <- WFS_util_keep_unique(url$query, keep_first = F)

    if (httrType == "GET") {
      url$query <- build_request_GET(url$query)
      request <- httr::build_url(url)
      res <- httr_GET_request (
        request,
        debug = debug,
        to_sf = T,
        sfverbose = sfverbose,
        httrverbose = httrverbose
      )
    } else {
      request <- build_request_POST(url$query)
      res <- httr_POST_request (
        base_url,
        request,
        debug = debug,
        to_sf = T,
        sfverbose = sfverbose,
        httrverbose = httrverbose
      )
    }

    if (inherits(res, 'data.frame'))
      row.names(res) <- NULL
    if (inherits(res, 'xml_document')) {
      if ((xml2::xml_name(res) == 'FeatureCollection')
          && WFS_util_check_in_list(url$query, 'resulttype', 'hits')) {
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

build_request_GET <- function(reqlist) {
  version <- reqlist[["version"]] # (latest) version
  vnames  <- WFS_util_v12_names()
  replarg <- (ifelse (version == '1.1.0',
                     vnames[2],vnames[3]))[[1]]
  reqlist <- WFS_util_unify_names(reqlist,vnames)
  reqlist <- WFS_util_keep_unique(reqlist, keep_first = F)
  WFS_util_replace_names(reqlist, vnames[[1]], replarg)
}


build_request_POST <- function(reqlist) {
  g  <- glue::glue
  gc <- glue::glue_collapse

  version <- reqlist[["version"]] # (latest) version
  vnames  <- WFS_util_v12_names()
  replarg <- (ifelse (version == '1.1.0',
                     vnames[2],vnames[3]))[[1]]
  reqlist <- WFS_util_unify_names(reqlist,vnames)
  reqlist <- WFS_util_keep_unique(reqlist, keep_first = F)

  v0names <- c('maxF',        'typeN')
  v1names <- c('maxFeatures', 'typeName')
  v2names <- c('count',       'typeNames')

  names(reqlist) <- tolower(names(reqlist))
  names(replarg) <- tolower(vnames[[1]])

  attribs <- WFS_util_xmlns_defs(version, as.txt = T)
  filter  <- reqlist[["filter"]]
  typen   <- reqlist[["typen"]]
  propertyname  <- reqlist[["propertyname"]]
  if (is.null(propertyname)) {
    propertyname <- NULL
  } else {
    propertyname <-
         paste( purrr::map_chr(strsplit(propertyname,',')[[1]],
                   ~propertyname_xml(.,'1.1.0')), # no ValueReference
                collapse= ''  )
  }
  srsname  <- reqlist[["srsname"]]
  srsname  <- ifelse(is.null(srsname),'',g(' srsName="{srsname}"'))

  fg2     <- fg('wfs:Query'
                , ifelse (is.null(propertyname), '', propertyname)
                , ifelse (is.null(filter), '', filter)
                , ta =g('{v9}="{typen}"{srsname}',v9=replarg['typen'])
             )
  outputFormat <- reqlist[["outputformat"]]
  maxf   <- reqlist[["maxf"]]
  maxfc  <- ifelse (is.null(maxf), '', g(' {v9}="{maxf}"',v9=replarg['maxf']))
  startindex <- reqlist[["startindex"]]
  startindex <- ifelse (is.null(startindex), '', g(' startIndex="{startindex}"'))
  resultType <- reqlist[["resulttype"]]
  resultType  <- if (is.null(resultType))
                   ''
                 else if (tolower(resultType) == 'hits')
                   ' resultType="hits"'
                 else ''

  fg1 <- fg('wfs:GetFeature'
    , fg2
    , ta = g('service="WFS" version="{version}" {attribs} outputFormat="{outputFormat}"',
              '{maxfc}{startindex}{resultType}')
  )
  fgh <- '<?xml version="1.0" encoding="ISO-8859-1"?>'
  gc(c(fgh, fg1), sep = '\n')
}


