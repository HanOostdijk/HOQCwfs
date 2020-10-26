
#' Auxiliary functions to create filters
#'
#' The function `build_filter` creates a filter for the indication version by inserting the appropriate 'xmlns' urls
#' in the Filter tag and inserting the XML elements from the `...` argument i.e. all arguments except `version`.
#' While evaluating these elements the version is temporarily set to the argument `version` of `build_filter` unless explicitly
#' overwritten.\cr\cr
#' The function `propeq_xml` creates a `PropertyIsEqualTo` XML clause\cr\cr
#' The function `bbox_xml` creates a `BBOX` XML clause
#'
#' @param tag Character with tag that will be used as both start and end tag
#' @param ... XML elements to be added to filter
#' @param version Character string with the WFS request version
#' @param propname Character string with the `PropertyName` (1.1.0) or `ValueReference` (2.0.0)
#' @param gemprop Character string with the name of the geometric field
#' @param crs Character string with the name of crs of the bbox (e.g. `"EPSG:28992"` )
#' @param bbox Numeric vector with four elements indicating the bounding box
#' @param propvalue Character string to filter with
#' @return Character vector with the created filter or xml fragment
#' @export
#' @rdname wfsfilteraux
#' @examples
#' bbox_28992 = c(119103, 480726, 119160, 481078)
#' x=build_filter(version='2.0.0',
#'    fg("And"
#'        , propeq_xml('topp:boom_omschrijf',"Alnus glutinosa 'Laciniata'")
#'        , bbox_xml("geometrie","EPSG:28992",bbox_28992)
#'      )
#'   )
#' cat(x)
#'


build_filter <- function (..., version=WFS_get_version()) {
  oversion <- WFS_get_version()
  on.exit(d<-WFS_set_version(oversion),add=T)
  WFS_set_version(version)
  xmlns    <- paste('xmlns:gml="http://www.opengis.net/gml{sufgml}"',
                    'xmlns:{ogcfes}="http://www.opengis.net/{ogcfes}{suffes}"'
                    )
  if (version=='1.1.0'){
      ogcfes ='ogc'; sufwfs=''; suffes=''; sufgml=''
  } else if (version=='2.0.0') {
    ogcfes ='fes';sufwfs='/2.0';suffes='/2.0';sufgml='/3.2'
  } else {
    message('build_filter: not supported version')
    return()
    }
  fg('Filter',...,ta=glue::glue(xmlns))
}

#' @export
#' @rdname wfsfilteraux

propeq_xml <- function(propname, propvalue, version=WFS_get_version()) {
    if (version == '1.1.0') {
      fg1 = fg("PropertyIsEqualTo"
           , bg('PropertyName', propname)
           , bg('Literal', propvalue)
           )
    } else if (version == '2.0.0') {
      fg1 = fg("PropertyIsEqualTo"
          , bg('ValueReference', propname)
          , bg('Literal', propvalue)
          )
    } else {
        message('build_filter: not supported version')
        return()
    }
    return(fg1)
}

#' @export
#' @rdname wfsfilteraux

 bbox_xml <- function (gemprop,crs,bbox,version=WFS_get_version()) {
    if (version=='1.1.0'){
       fg1 = fg("BBOX"
              , bg("PropertyName",gemprop)
              , fg("gml:Envelope"
                 , fg('gml:coord'
                    , bg('gml:X',bbox[1])
                    , bg('gml:Y',bbox[2])
                   )
                 , fg('gml:coord'
                    , bg('gml:X',bbox[3])
                    , bg('gml:Y',bbox[4])
                )
             )
             , ta = glue::glue('srsName = "{crs}"')
       )
    }
    else if (version=='2.0.0'){
       fg1 = fg("BBOX"
              , bg("ValueReference",gemprop)
              , fg("gml:Envelope"
                 , bg('gml:lowerCorner'
                    , glue::glue_collapse(bbox[1:2],sep=' ')
                   )
                 , bg('gml:upperCorner'
                    , glue::glue_collapse(bbox[3:4],sep=' ')
                   )
                )
              , ta = glue::glue('srsName = "{crs}"')
       )
    } else {
        message('build_filter: not supported version')
        return()
    }
    return(fg1)
}

