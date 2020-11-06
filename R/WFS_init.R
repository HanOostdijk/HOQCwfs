.WFS_options <- new.env()

.onLoad <- function(libname, pkgname) {
  invisible()
}

WFS_default_url     <- "https://geoweb.amstelveen.nl/geoserver/topp/wfs"
WFS_default_version <- "1.1.0"
WFS_default_sep     <- "\n"

#' Get/Set default parameters url, request version and separator
#'
#' The `url`  determines the location of the WFS service. If the `url` is not specified before a `HOQCwfs` function is used,
#' the default url (`r WFS_default_url`) will be used. \cr
#' The `version` determines the version of the WFS requests to be used. If no `version` is specified before a `HOQCwfs` function is used,
#' the default version (`r WFS_default_version`) is be used. Currently only '1.1.0' and '2.0.0' are supported.
#' The `sep` determines the separator to include in the output of the [HOQCwfs::fg()] function. If no `sep` is specified before a `HOQCwfs` function is used,
#' the new line character (`\n`) is be used.
#' \cr\cr
#' Note that these values are used as the default values for the various functions. They can always be overwritten there.
#'
#' @param url NULL for the default url ( **`r WFS_default_url`** ) or the url of the WFS service otherwise
#' @return Character vector with for the Get the current url, version or separator.
#' In case of the Set the old version is returned.
#' @export
#' @rdname wfsinit
#' @examples
#' \dontrun{
#' WFS_set_url("https://geoweb.amstelveen.nl/geoserver/topp/wfs")
#' }

WFS_set_url <- function (url=NULL) {
  org <- .WFS_options$url
  if (is.null(url)) {
    url = WFS_default_url
  }
  .WFS_options$url = url
  invisible(org)
}

#' @export
#' @rdname wfsinit
#' @examples
#' WFS_get_url()

WFS_get_url <- function () {
  url <- .WFS_options$url
  if (is.null(url)) {
   .WFS_options$url <- WFS_default_url
		url <- .WFS_options$url
  }
  url
}

#' @param version NULL for the default ( **`r WFS_default_version`** ) request version or an alternative version (e.g. `r '2.0.0'` ) otherwise
#' @export
#' @rdname wfsinit
#' @examples
#' WFS_set_version()
#' WFS_set_version('2.0.0')


WFS_set_version <- function (version=NULL) {
	if ( (!is.null(version)) && (! (version %in% c('1.1.0','2.0.0') ) ) ) {
	  warning("version ",version," is not supported: ",WFS_default_version," will be used")
		version <- WFS_default_version
	}
  org <- .WFS_options$version
	if (is.null(version)  ) {
		version <- WFS_default_version
	}
	.WFS_options$version <- version
	invisible(org)
}

#' @export
#' @rdname wfsinit
#' @examples
#' WFS_get_version()

WFS_get_version <- function () {
	version <- .WFS_options$version
	if (is.null(version)) {
		.WFS_options$version <- WFS_default_version
		version <- .WFS_options$version
	}
	version
}

#' @param sep NULL for the default ( **`\n`** ) separator or an alternative sep (e.g. `''`  ) otherwise
#' @export
#' @rdname wfsinit
#' @examples
#' WFS_set_sep()
#' WFS_set_sep('\n')


WFS_set_sep <- function (sep=NULL) {
  org = .WFS_options$sep
	if (is.null(sep)) {
		sep <- WFS_default_sep
	}
	.WFS_options$sep <- sep
	invisible(org)
}

#' @export
#' @rdname wfsinit
#' @examples
#' WFS_get_sep()

WFS_get_sep <- function () {
	sep <- .WFS_options$sep
	if (is.null(sep)) {
		.WFS_options$sep <- WFS_default_sep
		sep <- .WFS_options$sep
	}
	sep
}
