#' @name wfstags
NULL
#> NULL

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
#' @param sep NULL for the default separator (set by [WFS_set_sep()]) or required separator otherwise.
#' Only one of consecutive separators will be kept
#' @return Character vector with the generated xml element
#' @export
#' @rdname wfstags
#' @examples
#' xml_clause = fg(
#'   'PropertyIsLike'
#'   , bg('PropertyName', 'topp:objec_omschrijf')
#'   , bg('Literal', '.unmoolen')
#'   , ta = 'wildCard="*" singleChar="." escape="!"'
#'   , sep \ "\n"
#'   )
#' # cat(xml_clause)
#' # <PropertyIsLike wildCard="*" singleChar="." escape="!">
#' # <PropertyName>topp:objec_omschrijf</PropertyName>
#' # <Literal>.unmoolen</Literal>
#' # </PropertyIsLike>

fg <- function(tag, ..., ta=NULL, sep=WFS_get_sep()) {
  ta   <- ifelse(is.null(ta),"",glue::glue(" {ta}"))
  if (length(list(...))>0)
    dots <- do.call(paste,c(list(...),sep=sep))
  else
    dots =''
  x    <- as.character(glue::glue("<{tag}{ta}>{sep}{dots}{sep}</{tag}>"))
  if (stringr::str_length(sep) >0 )
    x <- stringr::str_replace_all(x,glue::glue("{sep}+"),sep)
  x
}

#' @export
#' @rdname wfstags
bg <- function(tag,value='') {
  #  fg(tag,value='',sep='')
  as.character(glue::glue("<{tag}>{value}</{tag}>"))
}

