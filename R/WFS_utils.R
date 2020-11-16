
#' Show XML fragment on screen
#'
#' This is a cover function for the XML  `xml2:::print.xml_document`, `print.xml_nodeset` and `print.xml_node` print functions
#' @param myxml XML document, node_set or node
#' @param width Numeric scalar passed to the xml2 print function indicating how many characters of the xml lines will be shown
#' @param max_n=20 Numeric scalar passed to the xml2 print function indicating how many  xml lines will be printed
#' @param lines Numerical vector indicating which lines of the print output will actually be shown. `NULL` indicates all. This argument is passed to [HOQCutil::cap.out()]
#' @param screenwidth Numeric scalar scalar indicating at which point the print output will be wrapped. This argument is passed to [HOQCutil::cap.out()] as its 'with' argument
#' @param verbose Logical indicating full request and httr response code will be displayed
#' @return NULL (invisible)
#' @export
#' @examples
#' \dontrun{
#' WFS_util_showxml(cap1,lines=2:3)
#' }


WFS_util_showxml <- function(myxml,width=2000,max_n=20,lines=NULL,screenwidth=getOption('width',100)) {
  if (inherits(myxml,"xml_document")) {
    HOQCutil::cap.out(xml2:::print.xml_document(myxml, width, max_n),width=screenwidth, lines=lines)
  } else if (inherits(myxml,"xml_node")) {
    HOQCutil::cap.out(xml2:::print.xml_node(myxml, width, max_n),width=screenwidth, lines=lines)
  } else if (inherits(myxml,"xml_nodeset")) {
    HOQCutil::cap.out(xml2:::print.xml_nodeset(myxml, width, max_n),width=screenwidth, lines=lines)
  }
}

#' Retrieves the allowed values for the parameters of WFS operations
#'
#' @param cap XML document containg the GetCapabilities information
#' @param operation Character string indicating one of the WFS operations
#' @param parameter Character string indicating one of the parameters of the operation
#' @return Character vector with the allowed values for the parameter
#' @export
#' @examples
#' \dontrun{
#' WFS_util_parameter_values(cap,operation='GetFeature',parameter='outputFormat')
#' WFS_util_parameter_values(cap,operation='DescribeFeatureType',parameter='outputFormat')
#' }

WFS_util_parameter_values <-
  function (cap, operation='??', parameter='??') {
    version <- as.list(xml2::xml_attrs(cap))[['version']]
    if (compareVersion(version, '2.0.0') >= 0)
      value_clause <- ".//ows:AllowedValues//ows:Value"
    else
      value_clause <- ".//ows:Value"

    ops  <- xml2::xml_find_all(cap,".//ows:OperationsMetadata//ows:Operation")
    opn  <- purrr::map_chr(ops,~xml2::xml_attr(.,"name"))
    op   <- xml2::xml_find_all(xml2::xml_parent(ops),
              glue::glue(".//ows:Operation[@name='{operation}']"))

    if (length(op) == 0){
      opc <- ifelse(length(opn)>1,'one from ','')
      opn <- glue::glue_collapse(glue::backtick(opn),
                                 sep=', ', last= ' and ',)
      warning(
        glue::glue('{operation} is not a valid operation\nchoose {opc}{opn}'))
      return(c())
    }

    parms <- xml2::xml_find_all(op,".//ows:Parameter")
    parmn <- purrr::map_chr(parms,~xml2::xml_attr(.,"name"))
    parm <- xml2::xml_find_all(xml2::xml_parent(parms),
              glue::glue(".//ows:Parameter[@name='{parameter}']"))

    if (length(parm) == 0){
      parmc <- ifelse(length(parm)>1,'one from ','')
      parmn <- glue::glue_collapse(glue::backtick(parmn),sep=', ', last= ' and ')
      warning(
        glue::glue('{parameter} is not a valid parameter\nchoose {parmc}{parmn}'))
      return(c())
    }

    purrr::map_chr(xml2::xml_find_all(parm, value_clause), xml2::xml_text)
  }

#' Retrieves the (xmlns) attributes from a GetCapabilities document
#'
#' @param cap XML document containg the GetCapabilities information
#' @param skip Character string which of the attributes will be removed. See Details
#' @param as.text Logical scalar. If `TRUE` indicates that the result will be a character strim (ready for inclusion in a XML request).
#' Otherwise the result is a named list.
#' @return Character string or list depending on the argument `as.text`
#' @export
#' @details The user can indicate with the `skip` argument which attributes will be left out:
#'
#' - `none` : all will be returned
#' - `known` : all the known (standard) attributes will be left out:
#' `version`, `schemaLocation`, `updateSequence`, `xmlns:wfs`,
#'             `xmlns:xsi`, `xmlns`, `xmlns:ows`, `xmlns:gml`, `xmlns:ogc`,
#'            `xmlns:fes`, `xmlns:xlink` and `xmlns:xs`
#' - `version`: only the version attribute will be left out
#' @examples
#' \dontrun{
#' base_url  <- "https://geodata.nationaalgeoregister.nl/wijkenbuurten2019/wfs"
#' cap1 = WFS_getcapabilities_POST(base_url,'1.1.0')
#' WFS_util_attrs(cap1,skip='version',as.text=T)
#' }

WFS_util_attrs <- function(cap, skip=c('none','known','version'), as.text=T) {
  skip <- match.arg(skip)
  attribs  <- as.list(xml2::xml_attrs(cap))
  known <- c("version", "schemaLocation", "updateSequence", "xmlns:wfs",
             "xmlns:xsi", "xmlns", "xmlns:ows", "xmlns:gml", "xmlns:ogc",
             "xmlns:fes", "xmlns:xlink", "xmlns:xs")
  if (skip == 'version')
    attribs['version']  <- NULL
  else if (skip == 'known')
    attribs[names(attribs) %in% known] <-NULL
  if (as.text)
     as.character(glue::glue_collapse(
                purrr::imap_chr(attribs,  ~ glue::glue('{.y}="{.x}"'))
                , sep = ' '))
  else attribs
}

#' Provides the standard xmlns definitions
#'
#' @param version  software version for WFS service request. See [WFS_get_version()] for the default
#' @param as.txt Logical indicating is character string is to be returned or a list
#' @return character string or list with the xmlns definitions
#' @export
#' @examples
#' \dontrun{
#' WFS_util_xmlns_defs(as.txt=T)
#' }
WFS_util_xmlns_defs <- function(version=WFS_get_version(),as.txt=T) {
  if (version == '2.0.0') {
    defs <-c(
      'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'
      , 'xmlns="http://www.opengis.net/wfs/2.0"'
      , 'xmlns:wfs="http://www.opengis.net/wfs/2.0"'
      , 'xmlns:ows="http://www.opengis.net/ows/1.1"'
      , 'xmlns:gml="http://www.opengis.net/gml/3.2"'
      , 'xmlns:fes="http://www.opengis.net/fes/2.0"'
      , 'xmlns:xlink="http://www.w3.org/1999/xlink"'
      , 'xmlns:xs="http://www.w3.org/2001/XMLSchema"'
      , 'xsi:schemaLocation="http://www.opengis.net/wfs/2.0"'
    )
  } else {
    defs <-c(
      'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'
      , 'xmlns="http://www.opengis.net/wfs"'
      , 'xmlns:wfs="http://www.opengis.net/wfs"'
      , 'xmlns:ows="http://www.opengis.net/ows"'
      , 'xmlns:gml="http://www.opengis.net/gml"'
      , 'xmlns:ogc="http://www.opengis.net/ogc"'
      , 'xmlns:xlink="http://www.w3.org/1999/xlink"'
      , 'xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd"'
    )
  }
  if (as.txt) defs <- paste(defs,collapse = ' ')
  defs
}
#' Replace names in a list
#'
#'
#'
#' @param mylist A named list
#' @param onames Character vector with names to be replaced (case insensitive)
#' @param nnames Character vector with corresponding replacements
#' @return A copy of `mylist` with names in `onames` replaced by the corresponding ones in `nnames`
#' @export
#' @examples
#' \dontrun{
#' WFS_util_replace_names(mylist,
#'   c('maxFeatures', 'typeName'),
#'   c('count',       'typeNames') )
#' }
WFS_util_replace_names <- function(mylist, onames, nnames) {
  n = names(mylist)
  m = match (tolower(n) , tolower(onames))
  names(mylist)[!is.na(m)] <- nnames[m[!is.na(m)]]
  mylist
}

#' incompatible argument names in versions `1.1.0` and `2.0.0`
#'
#' @return A list of three character vectors:
#'
#' - uniform names of temporary use
#' - corresponding version `1.1.0` names
#' - corresponding version `2.0.0` names
WFS_util_v12_names <- function() {
  list(
    v0 = c('maxF',        'typeN'),
    v1 = c('maxFeatures', 'typeName'),
    v2 = c('count',       'typeNames')
  )
}

#' replace argument names by their uniform counterparts
#'
#' @param mylist A named list
#' @param vnames A list of three character vectors:
#'
#' - uniform names
#' - first set of names
#' - second set of names
#'
#' @return A copy of `mylist` with names in both sets replaced by the corresponding uniform one
WFS_util_unify_names <- function (mylist,vnames) {
  WFS_util_replace_names(mylist,
         c(vnames[[2]],vnames[[3]]),
         c(vnames[[1]],vnames[[1]]))
}

#' keep first or last entry of list entries with duplicated names
#'
#' @param mylist A named list
#' @param keep.first Logical scalar indicating if the first (when `TRUE`) or last entry (when `FALSE`) with duplicate names will kept
#' @param ignore.case Logical scalar indicating if case is ignored when comparing for duplicate names
#' @return A copy of `mylist` with duplicate names removed (apart from the first when `keep_first=TRUE` or last otherwise )

WFS_util_keep_unique <- function(mylist, keep_first = T, ignore.case=T) {
  n  <- names(mylist)
  if (ignore.case)
    n<-tolower(n)
  if (keep_first) {
    un <- unique(match(n, n))
    return (mylist[un])
  } else {
    un <- unique(match(n, rev(n)))
    return (mylist[1+length(n)-un])
  }
}

#' Check a character value in a named list with character contents
#'
#' With the default parameters the function is checking without caring for lower and upper caps
#' if a certain entry contains a certain value and when a name occurs more than once
#' the first occurrence will be used. By specifying the 'ignore_case' arguments to `FALSE` the user
#' can indicate that the difference in lower and upper caps is important when comparing 'names'
#' (in case of `name_ignore_case` ) and/or  'values' (in case of `value_ignore_case` ).
#' By specifying `keep.first=FALSE` the last entry will be used when more than one is selected.
#'
#' @param mylist A named list with character contents
#' @param name Character string with the name of the entry that is searched
#' @param value A character scalar to check for in the `name` entry
#' @param keep.first Logical scalar indicating if the first (when `TRUE`) or last entry (when `FALSE`) with duplicate names will be used
#' @param name_ignore_case Logical scalar indicating if case is ignored when searching for the name
#' @param value_ignore_case Logical scalar indicating if case is ignored when checking the value
#' @return Logical value indicating if the `name` entry contains the value `value`

WFS_util_check_in_list <- function(mylist,  name, value, keep_first = T,
                      name_ignore_case = T, value_ignore_case = T)   {
  n  <- names(mylist)
  if (name_ignore_case) {
    n <- tolower(n)
    name <- tolower(name)
  }
  if (keep_first) {
    ix <- match(name, n, nomatch = 0)
    if (ix == 0)
      return(F)
  } else {
    ix <- match(name, rev(n), nomatch = 0)
    if (ix == 0)
      return(F)
    ix <- 1 + length(n) - ix
  }
  if (value_ignore_case) {
    c1 <- tolower(mylist[[ix]])
    c2 <- tolower(value)
  } else {
    c1 <- mylist[[ix]]
    c2 <- value
  }
  if (c1 == c2)
    return(T)
  else
    return(F)
}



