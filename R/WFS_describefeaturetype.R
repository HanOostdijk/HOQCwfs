
#' DescribeFeatureType for the WFS service
#'
#' Retrieve and unpack the `DescribeFeatureType` document for one or more `FeatureTypes` into a tibble.
#' `typename` is copied to the result tibble to distinguish the various `FeatureTypes` when more than one is specified.
#' Apart from `typename` the tibble contains the fields  `name`, `maxOccurs`, `minOccurs` and `type` .
#' @param typenames Character vector  with the name of features (such as e.g. obtained by using [WFS_featuretypes()] )
#' @param url URL of the WFS service. See [WFS_get_url()] for the default
#' @param version software version for WFS service request. See [WFS_get_version()] for the default
#' @param debug Logical indicating the httr response is to be returned.
#' Only valid when typenames contains one featuretype
#' @param httrverbose Logical vector of up to four entries to be used in [httr::verbose()].
#'  When this vector contains at least one TRUE debugging messages are displayed.
#'  Useful to find out what exactly was sent to and received from the webserver
#' @param out_path (optional) path where the `FeatureType` info is to be saved in xml format.
#' Only valid when `typenames` contains one featuretype
#' @return a `tibble` with `typename` and `name` (the fieldnames)
#' @export
#' @examples
#' \dontrun{
#' WFS_describefeaturetype("topp:gidw_groenbomen")
#' }

WFS_describefeaturetype <-
  function(typenames,
           url      = WFS_get_url(),
           version  = WFS_get_version(),
           debug    = F,
           httrverbose  = F,
           out_path = NULL) {
    if (! (version %in% c('1.1.0','2.0.0') ) )
       return("only version '1.1.0' and '2.0.0' are allowed")

    if (length(typenames) > 1) {
      debug = F
      out_path = NULL
    }
    if (debug ==T )
      return(
        WFS_describefeaturetype1(
        typename = typenames,
        url = url,
        version = version,
        debug = debug,
        httrverbose = httrverbose,
        out_path = out_path)
      )
    else
    purrr::map_dfr(
      typenames,
      ~ WFS_describefeaturetype1(
        typename = .,
        url = url,
        version = version,
        debug = debug,
        httrverbose = httrverbose,
        out_path = out_path
      )
    )
  }

# function for handling one FeatureType :
WFS_describefeaturetype1 <-
  function(typename,
           url      = url,
           version  = version,
           debug    =  debug,
           httrverbose  = httrverbose,
           out_path = NULL) {
    url       <- httr::parse_url(url)
    url$query <- list(
      service = "WFS"
      , version = version
      , request = "DescribeFeatureType"
      , typename = typename
    )
    request   <- httr::build_url(url)
    res       <- httr_GET_request (request,
                       debug = debug,
                       to_sf = F,
                       httrverbose = httrverbose)
    if (inherits(res, 'response'))
      return(res)
    if ((!inherits(res, 'xml_document')) ||
        (xml2::xml_name(res) == "ExceptionReport")) {
      return(tibble::tibble(typename = typename, name = '*NOT_FOUND*'))
    }
    xml2::xml_ns_strip(res)
    if (!is.null(out_path)) {
      unlink(out_path)
      xml2::write_xml(res, out_path)
    }
    `%>%` <- magrittr::`%>%`
    fieldnames <- res %>%
      xml2::xml_find_first(
        paste0(
          ".//xsd:extension[contains(@base,'gml:AbstractFeatureType')]",
          "//xsd:sequence"
        )
      ) %>%
      xml2::xml_children()

    purrr::map_dfr(fieldnames,
                   ~ tibble::tibble(
                     typename = typename,
                     name = xml2::xml_attr(., 'name'),
                     maxOccurs = xml2::xml_attr(., 'maxOccurs'),
                     minOccurs = xml2::xml_attr(., 'minOccurs'),
                     type = xml2::xml_attr(., 'type')
                   ))
  }
