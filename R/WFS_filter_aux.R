
#' Auxiliary functions to create filters
#'
#' - The function `build_filter` creates a filter for the indication version by inserting the appropriate 'xmlns' urls
#' in the Filter tag and inserting the XML elements from the `...` argument i.e. all arguments except `version`.
#' While evaluating these elements the version is temporarily set to the argument `version` of `build_filter` unless explicitly
#' overwritten.
#' - The function `propeq_xml` creates a `PropertyIsEqualTo` XML clause
#' - The function `bbox_xml` creates a `BBOX` XML clause
#' - The function `convert_bbox` convert a bbox from one CRS to another
#'
#' @param tag Character with tag that will be used as both start and end tag
#' @param ... XML elements to be added to filter
#' @param version Character string with the WFS request version
#' @param sep NULL for the default separator (set by [WFS_set_sep()]) or required separator otherwise
#' @param propname Character string with the `PropertyName` (1.1.0) or `ValueReference` (2.0.0)
#' @param gemprop Character string with the name of the geometric field
#' @param crs_in Character string with the name of the input crs (e.g. `"EPSG:4326"` )
#' @param crs_out Character string with the name of the output crs (e.g. `"EPSG:28992"` )
#' @param bbox_coords Numeric vector with four elements indicating the bounding box
#' @param propvalue Character string to filter with
#' @return Character vector with the created filter or xml fragment
#' @export
#' @rdname wfsfilteraux
#' @examples
#' bbox_28992 <- c(119103, 480726, 119160, 481078)
#' f1 <- build_filter(version='1.0.0',
#'    fg("And"
#'        , propeq_xml('topp:boom_omschrijf',"Alnus glutinosa 'Laciniata'")
#'        , bbox_xml("geometrie","EPSG:28992",bbox_28992)
#'      )
#'   )
#' bbox_4326 <- convert_bbox(bbox_28992,"EPSG:28992","EPSG:4326")
#' f2 <- build_filter(version='2.0.0',
#'    fg("And"
#'        , propeq_xml('topp:boom_omschrijf',"Alnus glutinosa 'Laciniata'")
#'        , bbox_xml("geometrie","EPSG:4326",bbox_4326)
#'      )
#'   )
#'
#'


build_filter <- function (..., version=WFS_get_version(),sep=WFS_get_sep()) {
  if (! (version %in% c('1.1.0','2.0.0') ) )
    return("only version '1.1.0' and '2.0.0' are allowed")
  oversion <- WFS_get_version()
  osep     <- WFS_get_sep()
  on.exit(d<-WFS_set_version(oversion),add=T)
  on.exit(d<-WFS_set_sep(osep),add=T)
  WFS_set_version(version)
  WFS_set_sep(sep)
  xmlns    <- paste('xmlns:gml="http://www.opengis.net/gml{sufgml}"',
                    'xmlns:{ogcfes}="http://www.opengis.net/{ogcfes}{suffes}"'
                    )
  if (version=='1.1.0'){
      ogcfes ='ogc'; sufwfs=''; suffes=''; sufgml=''
  } else if (version=='2.0.0') {
    ogcfes ='fes';sufwfs='/2.0';suffes='/2.0';sufgml='/3.2'
  }
  fg('Filter',...,ta=glue::glue(xmlns))
}

#' @export
#' @rdname wfsfilteraux

propeq_xml <-
  function(propname, propvalue, version = WFS_get_version()) {
    if (!(version %in% c('1.1.0', '2.0.0')))
      return("only version '1.1.0' and '2.0.0' are allowed")
    if (version == '1.1.0') {
      fg1 = fg("PropertyIsEqualTo"
               , bg('PropertyName', propname)
               , bg('Literal', propvalue))
    } else if (version == '2.0.0') {
      fg1 = fg("PropertyIsEqualTo"
               , bg('ValueReference', propname)
               , bg('Literal', propvalue))
    }
    return(fg1)
  }

#' @export
#' @rdname wfsfilteraux

bbox_xml <- function (gemprop, crs_in, bbox_coords, version = WFS_get_version()) {
  if (!(version %in% c('1.1.0', '2.0.0')))
    return("only version '1.1.0' and '2.0.0' are allowed")
  if (version == '1.1.0') {
    fg1 = fg(
      "BBOX"
      , bg("PropertyName", gemprop)
      , fg("gml:Envelope"
        , fg('gml:coord'
           , bg('gml:X', bbox_coords[1])
           , bg('gml:Y', bbox_coords[2]))
        , fg('gml:coord'
           , bg('gml:X', bbox_coords[3])
           , bg('gml:Y', bbox_coords[4]))
        )
      , ta = glue::glue('srsName = "{crs_in}"')
    )
  }
  else if (version == '2.0.0') {
    fg1 = fg(
      "BBOX"
      , bg("ValueReference", gemprop)
      , fg("gml:Envelope"
        , bg('gml:lowerCorner'
           , glue::glue_collapse(bbox_coords[1:2], sep = ' '))
        , bg('gml:upperCorner'
           , glue::glue_collapse(bbox_coords[3:4], sep = ' '))
        )
      , ta = glue::glue('srsName = "{crs_in}"')
    )
  }
  return(fg1)
}

#' @export
#' @rdname wfsfilteraux

 convert_bbox <- function (bbox_coords,crs_in,crs_out) {
    mp_sfc <- sf::st_sfc(
      sf::st_multipoint(matrix(bbox_coords,ncol=2,byrow=T)),crs=crs_in)
    sf::st_bbox(sf::st_transform(mp_sfc,crs=crs_out))
  }

