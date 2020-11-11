
#' Auxiliary functions to create filters
#'
#' - The function `build_filter` creates a filter for the indication version by inserting the appropriate 'xmlns' urls
#' in the Filter tag and inserting the XML elements from the `...` argument i.e. all arguments except `version`.
#' While evaluating these elements the version is temporarily set to the argument `version` of `build_filter` unless explicitly
#' overwritten.
#' - The function `propeq_xml` creates a `PropertyIsEqualTo` XML clause
#' - The function `bbox_xml` creates a `BBOX` XML clause
#' - The function `convert_bbox` convert a bbox from one CRS to another
#' - The function `convert_points` convert a set of points (in vector or matrix form) from one CRS to another
#'
#' @param tag Character with tag that will be used as both start and end tag
#' @param ... XML elements to be added to filter
#' @param version Character string with the WFS request version
#' @param sep NULL for the default separator (set by [WFS_set_sep()]) or required separator otherwise
#' @param propname Character string with the `PropertyName` (1.1.0) or `ValueReference` (2.0.0)
#' @param gemprop Character string with the name of the geometric field
#' @param crs_in Character string with the name of the input crs (e.g. `"EPSG:4326"` )
#' @param crs_out Character string with the name of the output crs (e.g. `"EPSG:28992"` )
#' @param coords Numeric vector with four elements indicating the bounding box (for `convert_bbox` ) or a numeric vector of even length
#' or numeric matrix with two columns (for `convert_points` )
#' @param propvalue Character string to filter with
#' @param out_matrix Logical indicating that the output of  `convert_points` should be matrix
#' @param keep_names Logical indicating that the output of `convert_points` keeps the column names when the input is a matrix
#' @return Character vector with the created filter or xml fragment. However the `convert_bbox` returns a numeric vector and the  `convert_points` returns a numeric matrix with two columns
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
#' points_4326 <- convert_points(bbox_28992,"EPSG:28992","EPSG:4326")
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
        , ta = glue::glue('srsName = "{crs_in}"')
        )
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
        , ta = glue::glue('srsName = "{crs_in}"')
        )
    )
  }
  return(fg1)
}

#' @export
#' @rdname wfsfilteraux

 convert_bbox <- function (coords,crs_in,crs_out) {
   if (!is.matrix(coords)) {
     coords = matrix(coords, ncol = 2, byrow = T)
     }
    mp_sfc <- sf::st_sfc(
      sf::st_multipoint(coords),crs=crs_in)
    sf::st_bbox(sf::st_transform(mp_sfc,crs=crs_out))
 }

#' @export
#' @rdname wfsfilteraux
 convert_points <- function (coords,
                             crs_in,
                             crs_out,
                             out_matrix = T,
                             keep_names = T) {
   if (!is.matrix(coords)) {
     was_vector <- T # apparently vector
     coords <- matrix(coords, ncol = 2, byrow = T)
   } else {
     was_vector <- F
     orgattr <- attributes(coords)
   }
   mp_sfc <- sf::st_sfc(sf::st_multipoint(coords), crs = crs_in)
   mp_sfc <- sf::st_transform(mp_sfc, crs = crs_out)
   coords <- sf::st_coordinates(mp_sfc)[, c('X', 'Y')]
   if (was_vector) {
     if (out_matrix == F)
       return (as.vector(t(coords)))
     else {
       dimnames(coords) <- NULL
       return(coords)
     }
   } else {
     if (keep_names)
       attributes(coords) <- orgattr
     else
       dimnames(coords) <- NULL
   }
   coords
 }

#' Creates the description in XML format of a spatial feature
#'
#'
#' The coordinates have to be specified in the way done in the corresponding `sf` function:
#' [sf::st_point()], [sf::st_linestring()], [sf::st_polygon()],
#' [sf::st_multipoint()], [sf::st_multilinestring()], [sf::st_multipolygon()],
#' or [sf::st_bbox()] (the latter for `envelope`).
#' In places where the corresponding `sf` function requires a two-column matrix, this function also
#' accepts even-length vectors. See Details.
#'
#' Assuming that we always use a two_colum matrix (apart from the 'envelope' that needs a length four vector) we need the following coordinates structure:
#'
#' - point : a one-row  matrix
#' - linestring : a n-row matrix with n > 1
#' - polygon : a list of matrices where each matrix has more than three rows with the first row equal to the last.
#' The first matrix specifies the outer boundary and the optional other matrices specify holes.
#' - multipoint: a n-row matrix with n > 1
#' - multilinestring : a list of linestring specifications
#' - multipolygon : a list of polygon specifications (therefore a list of a list)
#'
#' The resulting XML fragment can be used in [build_filter()] or [spat_xml()]
#'
#' @param sptype Character string with the type of spatial feature.
#' One of `envelope`, `point`, `linestring`, `polygon` or
#' the multi version of the last three options. The argument is case insensitive
#' @param crs_in Character string with the Coordinate Reference System
#' @param coords A numeric vector of even length with the coordinates of the feature or a list (of lists) of those vectors.
#'  Instead of a vector also a two-column matrix can be specified.
#' @param version Character string with the WFS request version
#' @param sep Character string with separator (to be used to split outer from inner polygons). Useful for printouts of query strings.
#' @return Character vector with the created xml fragment
#' @export
#' @examples
#' \dontrun{
#' crs <- 'EPSG:28992'
#' spat_feature('envelope',crs, c(x1,y1,x2,y2) )
#' spat_feature('point',crs, c(x1,y1) )
#' spat_feature('linestring',crs, matrix(c(x1,y1,x2,y2,x3,y3),ncol=2,byrow=T) ) # or
#' spat_feature('linestring',crs, c(x1,y1,x2,y2,x3,y3) )
#' spat_feature('polygon',crs,list(outer_coords,hole1_coords,hole2_coords) )
#' spat_feature('multipolygon',crs,list(polygon1,polygon2))
#' }
 spat_feature <- function (sptype,crs_in,
                           coords, version = WFS_get_version(),
                           sep = WFS_get_sep()) {
   if (!(version %in% c('1.1.0', '2.0.0')))
     return("only version '1.1.0' and '2.0.0' are allowed")

   sptype1 <- tolower(sptype)
   if (sptype1 == 'envelope') {
     fg2 = create_envelope(coords, version)
     fg1 = fg("gml:Envelope"
       , fg2
       , ta = glue::glue('srsName = "{crs_in}"')
     )
   } else if (sptype1 == 'point') {
     fg2 = create_coord(coords, 'pos', version)
     fg1 = fg("gml:Point"
       , fg2
       , ta = glue::glue('srsName = "{crs_in}"')
     )
   } else if (sptype1 == 'linestring') {
     fg2 = create_coord(coords, 'poslist', version)
     fg1 = fg("gml:LineString"
       , fg2
       , ta = glue::glue('srsName = "{crs_in}"')
     )
   } else if (sptype1 == 'polygon') {
     fg2 = create_pol(coords, 'poslist', version, sep = sep)
     fg1 = fg("gml:Polygon"
        , fg2
        , ta = glue::glue('srsName = "{crs_in}"')
     )
   } else if (sptype1 == 'multipoint') {
     fg2 = create_mp(coords, 'pos', version, sep = sep)
     fg1 = fg("gml:MultiPoint"
        , fg2
        , ta = glue::glue('srsName = "{crs_in}"')
     )
   } else if (sptype1 == 'multilinestring') {
     if (version == '1.1.0')
       gmltype = 'gml:MultiLineString'
     else
       gmltype = 'gml:MultiCurve'
     fg2 = create_mls(coords, 'poslist', version, sep = sep)
     fg1 = fg(gmltype
        , fg2
        , ta = glue::glue('srsName = "{crs_in}"')
     )
   } else if (sptype1 == 'multipolygon') {
     if (version == '1.1.0')
       gmltype = 'gml:MultiPolygon'
     else
       gmltype = 'gml:MultiSurface'
     fg2 = create_mpol(coords, 'poslist', version, sep = sep)
     fg1 = fg(gmltype
        , fg2
        , ta = glue::glue('srsName = "{crs_in}"')
     )
   }
   fg1
 }

 create_envelope <- function (coords, version) {
   if (version == '1.1.0') {
     paste(fg('gml:coord'
              , bg('gml:X', coords[1])
              , bg('gml:Y', coords[2]))
           ,
           fg('gml:coord'
              , bg('gml:X', coords[3])
              , bg('gml:Y', coords[4]))
           ,
           collapse = '')
   } else {
     paste(bg('gml:lowerCorner'
              , mat2char(coords[1:2], sep = ' '))
           ,
           bg('gml:upperCorner'
              , mat2char(coords[3:4], sep = ' '))
           ,
           collapse = '')
   }
 }

 create_mp <- function (coords, coord_type, version, sep) {
   pos <- purrr::array_branch(coords, 1)
   x <- purrr::map(pos,  function (x) {
     fg("gml:pointMember"
        , fg("gml:Point"
             , create_coord(x, coord_type, version)))
   })
   paste(x, collapse = sep)
 }

 create_mls <- function (coords, coord_type, version, sep) {
   pos <- purrr::array_branch(coords, 1)
   if (version == '1.1.0') gmltype = 'gml:lineStringMember'
   else gmltype = 'gml:curveMember'
   x <- purrr::map(pos,  function (x) {
     fg(gmltype
        , fg("gml:LineString"
             , create_coord(x, coord_type, version)))
   })
   paste(x, collapse = sep)
 }

 create_pol <- function (coords, coord_type, version, sep = sep) {
   poslist <- create_coord(coords, coord_type, version)
   create_pol2(poslist,sep)
 }

 create_pol2 <- function(poslist,sep) {
   x <- purrr::imap(poslist,  function (x, ix) {
     if (ix == 1)
       tiepe = 'gml:exterior'
     else
       tiepe = 'gml:interior'
     fg(tiepe
        , fg("gml:LinearRing"
             , x))
   })
   paste(x, collapse = sep)
 }

  create_mpol <- function (coords, coord_type, version, sep = sep) {
   poslist <- create_coord(coords, coord_type, version )
   if (version == '1.1.0') gmltype = 'gml:polygonMember'
    else gmltype = 'gml:surfaceMember'
   x <- purrr::map(poslist,  function (x) {
     fg(gmltype
        , fg('gml:Polygon'
             , create_pol2(x,sep)))
   })
   paste(x, collapse = sep)
 }

 mat2char <- function(coords, sep = ',') {
     if (!is.matrix(coords)) {
       coords1 <- matrix(coords, ncol = 2, byrow = T)
     } else {
       coords1 <- coords
     }
     a <- purrr::array_branch(coords1, 1)
     b <- purrr::map(a,  ~ paste(., collapse = sep))
     paste(b, collapse = ' ')
   }

 create_coord <-
   function (coords, coord_type, version = WFS_get_version()) {
     if (!(version %in% c('1.1.0', '2.0.0')))
       return("only version '1.1.0' and '2.0.0' are allowed")
     if (is.list(coords))
       purrr::map(coords,  ~ create_coord(., coord_type, version))
     else {
       if (!is.matrix(coords))
         coords <- matrix(coords, ncol = 2, byrow = T)
       if (version == '1.1.0')
         fg1 <- fg('gml:coordinates'
                   , mat2char(coords, sep = ',')
                   , ta = 'decimal="." cs="," ts=" "')
       else {
         if (tolower(coord_type) == 'pos')
           coord_type <- 'gml:pos'
         else
           coord_type <- 'gml:posList'
         fg1 = fg(coord_type
                  , mat2char(coords, sep = ' ')
                  , ta = 'decimal="." cs="," ts=" "')
       }
       fg1
     }
   }

#' Creates the spatial part of a filter in XML format
#'
#' Can be used to create in XML format the spatial part of spatial operators such as
#' `Disjoint`, `Equals`, `DWithin`, `Beyond`, `Intersects`, `Touches`, `Crosses`, `Within`,
#'  `Contains`, `Overlaps` and `BBOX` .
#'
#' @param gemprop Character string with the name of the geometric field
#' @param feature Character with XML description of feature. Description can be result of [spat_feature()]
#' @param distance Number scalar with distance for `DWithin` operator. Unit given by argument `units`
#' @param version Character string with the WFS request version
#' @param spat_fun  Character string with the name of the spatial operator
#' @param units Character string indicating the units of the argument `distance`
#' @return Character vector with the created xml fragment
#' @export
#' @examples
#' \dontrun{
#' spat_xml('geometrie',
#'       spat_feature('point','EPSG:28992', point_coords),
#'              50) # filters features within 59 meters of point
#' spat_xml('geometrie',
#'        spat_feature('Polygon','EPSG:28992',list(outer_coords,hole_coords)),
#'       spat_fun='Intersects') # filters features intersecting polygon with hole
#' }

 spat_xml <- function (gemprop,
                       feature,
                       distance=NULL,
                       version = WFS_get_version(),
                       spat_fun='DWithin',
                       units='meters') {
  if (!(version %in% c('1.1.0', '2.0.0')))
    return("only version '1.1.0' and '2.0.0' are allowed")
  if (version == '1.1.0')
    v <- function(s) paste0('ogc:',s)
  else
    v <- function(s) paste0('fes:',s)
  if (!is.null(distance))
    fg2 = fg(v('Distance')
        , glue::glue('{distance}')
        , ta = glue::glue('{u}="{units}"',u=v('units')))
  else
    fg2 = ''
  if (version == '1.1.0') {
    fg1 = fg(
      v(spat_fun)
      , bg(v("PropertyName"), gemprop)
      , feature
      , fg2
    )
  }
  else if (version == '2.0.0') {
    fg1 = fg(
      v(spat_fun)
      , bg(v("ValueReference"), gemprop)
      , feature
      , fg2
    )
  }
  return(fg1)
}


