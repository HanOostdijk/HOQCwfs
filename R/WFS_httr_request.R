#' @name wfshttrrequest
NULL
#> NULL


#' Retrieve information with a httr GET or POST request
#'
#' The `httr_GET_request` and `httr_POST_request` retrieve information from a webserver.\cr
#' We assume it could provide data that could be converted to an `sf` (simple features object) as for most
#' functions in this package but the functions should also work outside this setting. See **Details** .
#'
#' The function `httr_GET_request` expects a request that will immediately be processed by [httr::GET()] (so complete with `url`)
#' and the `httr_POST_request` expects to receive separately the `url` and `POST body` to be passed to [httr::POST()].
#' The result of this functions will be
#'
#' - the resulting response object when `debug=TRUE`
#' - a converted `json` object (converted with [jsonlite::fromJSON()]) when the response object
#' indicates a `json` object and `to_sf=TRUE` is not specified
#' - an `sf` object (converted by [sf::read_sf()]) when the response object
#' indicates a `json` object and `to_sf=TRUE` is specified
#' - an `xml_document` object (converted by [xml2::read_xml()]) if the response object
#' indicates a `xml` object
#' - a character string with message in case of an `httr` error
#'
#' @param request Character string with url as needed for [httr::GET()] in case of the `httr_GET_request` and a character string with
#' a [httr::POST()] body otherwise
#' @param url Character string with the url of the WFS service
#' @param debug Logical indicating if the httr response object is to be returned
#' @param to_sf Logical indicating if a `json` object should be converted to an `sf` object
#' @param sfverbose Logical indicating if [sf::read_sf()] messages will be displayed
#' @param httrverbose Logical vector of up to four entries to be used in [httr::verbose()]. When this
#' vector contains at least one `TRUE` debugging messages are displayed.
#' Useful to find out what exactly was sent to and received from the webserver
#' @return depending on the request a `json`, `sf` or `xml` object or a character string with a `httr` message
#' in case of an error
#' @export
#' @rdname wfshttrrequest
#' @examples
#' \dontrun{
#' reqG <- "https://geoweb.amstelveen.nl/geoserver/topp/wfs?service=WFS&version=1.1.0&request=GetCapabilities"
#' cap1 <- httr_GET_request(reqG,httrverbose=c(T,F))
#'
#' reqP <- paste0(
#'  '<?xml version="1.0" encoding="ISO-8859-1"?>',
#'  '<GetCapabilities service="WFS" version="1.1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ',
#'  'xmlns="http://www.opengis.net/wfs" xmlns:wfs="http://www.opengis.net/wfs" ',
#'  'xmlns:ows="http://www.opengis.net/ows" xmlns:gml="http://www.opengis.net/gml" ',
#'  'xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" ',
#'  'xsi:schemaLocation="http://www.opengis.net/wfs"/>'
#' )
#' cap1 <- httr_POST_request(WFS_get_url(),reqP,httrverbose=c(T,F))
#' }

httr_GET_request <- function (request,
           debug = F,
           to_sf = F,
           sfverbose = F,
           httrverbose = rep(F, 4)) {
  suppressWarnings(res <-
    try({
      if (any(httrverbose == T)) {
        res <- httr::GET(request,
                         do.call(httr::verbose, as.list(httrverbose)))
      } else {
        res <- httr::GET(request)
      }
    }, silent = TRUE)
  )
  handle_res(res,debug,to_sf,sfverbose)
}
#' @export
#' @rdname wfshttrrequest
httr_POST_request <-   function (url = WFS_get_url(),
            request,
            debug = F,
            to_sf = F,
            sfverbose = F,
            httrverbose = rep(F,4)) {
    suppressWarnings(res <- try({
        if (any(httrverbose==T) ) {
          res <- httr::POST(url, body = request,
                          httr::content_type("text/xml"),
                          do.call(httr::verbose,as.list(httrverbose)) )
        } else {
          res <- httr::POST(url, body = request,
                          httr::content_type("text/xml") )
        }
    }, silent = TRUE))
      handle_res(res,debug,to_sf,sfverbose)
}

handle_res <- function(res,debug,to_sf,sfverbose) {
  if (debug || ('try-error' %in% class(res)))
    return(res)
  if (httr::http_error(res))
    return(httr::http_status(res)$message)
  cnt1 = tolower(httr::headers(res)$`content-type`)
  res_data <- httr::content(res, encoding = 'UTF-8', as  =  'text')
  if (grepl('json', cnt1, fixed = T))
  {
    if (to_sf)
    {
      r <- sf::read_sf(res_data, quiet  = sfverbose==F, as_tibble = F)
    } else {
      r <- jsonlite::fromJSON(res_data)
    }
  } else if (grepl('xml', cnt1, fixed = T)) {
    r <- xml2::read_xml(res_data, options = "NOWARNING")
  } else {
    r <- res_data
  }
  r
}
