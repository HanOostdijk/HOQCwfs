
#' Tag functions fg and bg
#'
#' The `fg` and `bg` functions are auxiliary functions to create xml elements.
#' Using these functions only the start tag and contents have to be provided; \cr
#' e.g.
#' \cr\cr `bg('PropertyName', 'topp:objec_omschrijf')`
#' \cr  will create
#' \cr `<PropertyName>topp:objec_omschrijf</PropertyName>`
#' \cr\cr The function `fg` allows additional arguments for the tag and allows subtags.
#' See **Examples** and the functions in [wfsfilteraux]
#'
#' @param tag Character with tag that will be used as both start and end tag
#' @param ... Character arguments that will be inserted between start and end tag
#' @param value Character argument that will be inserted between start and end tag
#' @param ta Character with additional information for start tag
#' @param sep NULL for the default separator (set by [WFS_set_sep()]) or required separator otherwise
#' @return Character vector with the generated xml element
#' @export
#' @rdname wfstags
#' @examples
#' fg(
#'   'PropertyIsLike'
#'   , bg('PropertyName', 'topp:objec_omschrijf')
#'   , bg('Literal', '.unmoolen')
#'   , ta = 'wildCard="*" singleChar="." escape="!"'
#'   )

fg <- function(tag, ..., ta=NULL, sep=WFS_get_sep()) {
  ta   <- ifelse(is.null(ta),"",glue::glue(" {ta}"))
  dots <- do.call(paste,c(list(...),sep=sep))
  as.character(glue::glue("<{tag}{ta}>{sep}{dots}{sep}</{tag}>"))
}

#' @export
#' @rdname wfstags
bg <- function(tag,value='') {
  #  fg(tag,value='',sep='')
  as.character(glue::glue("<{tag}>{value}</{tag}>"))
}
