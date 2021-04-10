
#' Retrieve WFS information with the GetFeature request
#'
#' Retrieving the requested geospatial data will most often be done by specifying
#' the bounding box of the region of interest or by specifying a filter to restrict
#' the features that will be returned. As described in
#' [https://docs.geoserver.org/latest/en/user/filter/syntax.html](https://docs.geoserver.org/latest/en/user/filter/syntax.html)
#' there are two ways to specify a filter: with the `cql_filter` or the `filter` argument. See  **Details** .
#' @param typename Character with the name of a feature (such as e.g. found by using [WFS_featuretypes()] )
#' @param ... optional arguments for the `GetFeature` request. See **Details**
#' @param httrType Character with the type of httr request: `GET` or `POST`
#' @param url URL of the WFS service. See [WFS_get_url()] for the default
#' @param version software version for WFS service request. See [WFS_get_version()] for the default
#' @param debug Logical indicating the httr response is to be returned
#' @param to_sf Logical indicating if a `json` object should be converted to an `sf` object
#' @param sfverbose Logical indicating if [sf::read_sf()] messages will be displayed
#' @param echo_request Logical indicating if the generated GET or POST request is to be echoed
#' @param httrverbose Logical vector of up to four entries to be used in [httr::verbose()]
#' @return a `sf` object with the requested information (the 'id' variable and the geometry will always be included)
#' or a character string with an error message
#' @export
#' @details Arguments that are recognized by the GetFeature request are:
#' - `bbox        ` (not in combination with `cql_filter` or `filter`)
#' - `cql_filter  ` a filter derived from the plain-text language mechanism for the OGC Catalog specification.
#' See for an introduction the [tutorial](http://docs.geoserver.org/stable/en/user/tutorials/cql/cql_tutorial.html)
#'  in the [GeoServer](https://docs.geoserver.org/stable/en/user/index.html) webpages. Also see the **Examples**
#' - `filter      ` an XML-based specification of a filter.
#' See this [GeoServer page](https://docs.geoserver.org/latest/en/user/filter/function.html) and the **Examples**.\cr
#' And see [wfsfilteraux] for functions to build these XML filters
#' - `resultType` - default 'results' and alternative 'hits'. In the latter case only the number of matched features (`numbeOfFeatures` in 1.1.0 or `numberMatched` in 2.0.0) is returned
#' - `srsname     ` indicate the crs for the coordinates of the output e.g. `srsname='EPSG:4326'`
#' - `propertyname` the name of the fields to retrieve. The `id` and `geometry` fields will always be included
#' - `startIndex  ` number of features to skip before retrieving features ( the output with `startindex=1` will start with the second feature )
#' - `maxfeatures ` (version `1.1.0`) or `count` (version `2.0.0`) indicates the number of features to retrieve. When the wrong argument is specified it will be translated in the other.
#' The `GetFeature` argument `outputFormat` has value 'application/json' but this can be overwritten
#'
#' See vignette for more examples
#'
#' @examples
#' \dontrun{
#' # examples use the default dataset !
#' # retrieve all fields from the first 5 features of "topp:gidw_groenbomen" :
#' wfs1     <-  WFS_getfeature("topp:gidw_groenbomen", maxfeatures=5)
#'
#' # retrieve all fields from all features of "topp:gidw_groenbomen" in the indicated bbox :
#' bbox_wgs84 <- "4.860793, 52.313319, 4.861587, 52.316493,EPSG:4326"
#' wfs2     <-  WFS_getfeature("topp:gidw_groenbomen", bbox=bbox_wgs84)
#'
#' # retrieve all fields from all features of "topp:gidw_groenbomen" in the indicated bbox
#' # coordinates in bbox given in same crs as the data (EPSG:28992) but we want the output coordinates in WGS84 :
#' bbox_28992 <- "119103.4, 480726.0, 119160.1, 481078.7"
#' wfs3     <-  WFS_getfeature("topp:gidw_groenbomen", bbox=bbox_28992,srsname='EPSG:4326' )
#'
#' # filter species with embedded quote (using cql_filter=) :
#' a_species    <- r"(Prunus serrulata 'Kanzan')"    # embedded quote
#' bm=stringr::str_replace_all(a_species,"'","''")   # double that for cql_filter
#' wfs4     <-  WFS_getfeature("topp:gidw_groenbomen",
#'            cql_filter= glue::glue("boom_omschrijf='{bm}'") )
#'
#' # filter species with embedded quote (using filter=) and two of the auxiliary functions :
#' f5   <- build_filter(version='1.1.0' ,
#'            propeq_xml('topp:boom_omschrijf',a_tree)
#'     )
#' wfs5 <- WFS_getfeature("topp:gidw_groenbomen", version='1.1.0',
#'            filter  = f5)
#' }

WFS_getfeature <- function(typename, ...,
         httrType= c("GET","POST"),
         url=WFS_get_url(),
         version=WFS_get_version(),
         debug=F,
         to_sf=T,
         sfverbose=F,
         echo_request=F,
         httrverbose=rep(F,4)){
    cv <- check_version(version)
    if (!is.null(cv))
      return(cv)
    httrType = match.arg(httrType)

    base_url <- url
    url       <- httr::parse_url(url)
    url$query <- list(
      service = "WFS"
      , version    = version
      , request    = "GetFeature"
      , typename   = typename
      , outputFormat = "application/json"
    )
    url$query = append(url$query, list(...))
    url$query <- WFS_util_keep_unique(url$query, keep_first = F)

    if (httrType == "GET") {
      url$query <- build_request_GET(url$query)
      request <- httr::build_url(url)
      if (echo_request == T) {
        cat("\n",request,"\n\n")
      }
      res <- httr_GET_request (
        request,
        debug = debug,
        to_sf = to_sf,
        sfverbose = sfverbose,
        httrverbose = httrverbose
      )
    } else {
      request <- build_request_POST(url$query)
      if (echo_request == T) {
        cat("\n",request,"\n\n")
      }
      res <- httr_POST_request (
        base_url,
        request,
        debug = debug,
        to_sf = to_sf,
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


build_request_POST <- function(reqlist,sep='') {
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
  sortby  <- reqlist[["sortby"]]
  fg2     <- fg('wfs:Query'
                , ifelse (is.null(propertyname), '', propertyname)
                , ifelse (is.null(sortby), '', sortby_xml(sortby,version,sep)) # ? no ValueReference ?
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

sortby_xml <- function (sortby, version, sep = WFS_get_sep()) {
  x = strsplit(stringr::str_squish(strsplit(sortby, ',')[[1]]), ' ')
  fields = purrr::map_chr(x,  ~ purrr::pluck(., 1, .default = ''))
  so = tolower(purrr::map_chr(x,  ~ stringr::str_sub(purrr::pluck(., 2, .default =
                                                                    ''), 1, 1)))
  so[so == 'd'] <- 'DESC'
  so[so != 'DESC'] <- 'ASC'

  if (version == '1.1.0')
    v <- function(s)  paste0('ogc:', s)
  else
    v <- function(s)  paste0('fes:', s)
  sps <- purrr::map2(fields, so,  ~
                       fg(v('SortProperty')
                         , propertyname_xml(.x, version = version,nopref=F)
                         , bg(v('SortOrder'), .y)
                       ))
  fg(v('SortBy'),  paste(sps, collapse = sep))
}


