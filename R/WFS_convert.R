#' @name wfsconvert
NULL
#> NULL

#' Convert coordinates
#'
#' - The function `convert_bbox` converts (the coordinates of) a bbox from one CRS to another
#' - The function `convert_points` converts a set of points (in vector or matrix form) from one CRS to another
#'
#' @param coords Numeric vector with four elements indicating the bounding box (for `convert_bbox` ) or a numeric vector of even length
#' or numeric matrix with two columns (for `convert_points` )
#' @param crs_in Character string with the name of the input crs (e.g. `"EPSG:4326"` )
#' @param crs_out Character string with the name of the output crs (e.g. `"EPSG:28992"` )
#' @param out_matrix Logical indicating that the output of  `convert_points` should be matrix
#' @param keep_names Logical indicating that the output of `convert_points` keeps the column names when the input is a matrix
#' @return The `convert_bbox` returns a numeric vector of class `bbox` and the  `convert_points` returns a numeric matrix
#' with two columns (if `outmatrix == FALSE` a vector is returned)
#' @examples
#' bbox_28992 <- c(119103, 480726, 119160, 481078)
#' bbox_4326  <- convert_bbox(bbox_28992,"EPSG:28992","EPSG:4326")
#' points_4326 <- convert_points(bbox_28992,"EPSG:28992","EPSG:4326")

#' @export
#' @rdname wfsconvert

 convert_bbox <- function (coords,crs_in,crs_out) {
   if (!is.matrix(coords)) {
     coords = matrix(coords, ncol = 2, byrow = T)
     }
   mp_sfc <- sf::st_sfc(
      sf::st_multipoint(coords),crs=crs_in)
   sf::st_bbox(sf::st_transform(mp_sfc,crs=crs_out))
 }

#' @export
#' @rdname wfsconvert
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

