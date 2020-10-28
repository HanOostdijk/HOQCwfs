
#' DescribeFeatureType for the WFS service
#'
#' Retrieve and unpack the `DescribeFeatureType` document for one or more  `FeatureTypes` into a tibble
#'
#' `typenames` is copied to the result tibble to distinguish the various `FeatureTypes` when more than one is specified
#' @param typenames Character vector  with the name of features (such as e.g. obtained by using [WFS_featuretypes()] )
#' @param url URL of the WFS service. See [WFS_get_url()] for the default
#' @param version software version for WFS service request. See [WFS_get_version()] for the default
#' @param debug Logical indicating the httr response is to be returned
#' @param verbose Logical indicating full request and httr response code will be displayed
#' @param out_path (optional) path where the `FeatureType` info is to be saved in xml format. Only valid when `typenames` contains one featuretype
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
           verbose  = F,
           out_path = NULL) {
    if (! (version %in% c('1.1.0','2.0.0') ) )
       return("only version '1.1.0' and '2.0.0' are allowed")
    `%>%` <- magrittr::`%>%`

    # subfunction for handling one FeatureType :
    WFS_describefeaturetype1 <-
      function(typename,
               url      = url,
               version  = version,
               debug    =  debug,
               verbose  = verbose,
               out_path = NULL) {
        url       <- httr::parse_url(url)
        url$query <- list(
          service = "WFS"
          , version = version
          , request = "DescribeFeatureType"
          , typename = typename
        )
        request   <- httr::build_url(url)
        res       <- WFS_GET_request (request,
                           debug = debug,
                           to_sf = F,
                           verbose = verbose)
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
                         name = xml2::xml_attr(., 'name')
                       ))
      }

    if (length(typenames) > 1) {
      debug = F
      out_path = NULL
    }
    purrr::map_dfr(
      typenames,
      ~ WFS_describefeaturetype1(
        typename = .,
        url = url,
        version = version,
        debug = debug,
        verbose = verbose,
        out_path = out_path
      )
    )
  }