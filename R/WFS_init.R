.WFS_options <- new.env()

.onLoad <- function(libname, pkgname) {
  invisible()
}

WFS_default_url = "https://geoweb.amstelveen.nl/geoserver/topp/wfs"
WFS_default_version = "1.1.0"

#' Get/Set url and request version for the WFS service
#'
#' Determines from which WFS service information will be extracted and the version of the WFS requests to use. If the url is not specified before
#' the default url (`r WFS_default_url`) will be used. If no version is specified the default version (`r WFS_default_version`) is be used.
#' \cr\cr
#' Note that these values are used as the default values in the various request functions. They can always be overwritten in these functions.
#' @param url NULL for the default url ( **`r WFS_default_url`** ) or the url of the WFS service otherwise
#' @return Character vector (invisible for the set version) with the url or version to be used
#' @export
#' @rdname wfsinit
#' @examples
#' WFS_set_url("https://geoweb.amstelveen.nl/geoserver/topp/wfs")

WFS_set_url <- function (url=NULL) {
  if (is.null(url)) {
    url = WFS_default_url
  }
  .WFS_options$url = url
  invisible(url)
}

#' @export
#' @rdname wfsinit
#' @examples
#' WFS_get_url()

WFS_get_url <- function () {
  url = .WFS_options$url
  if (is.null(url)) {
    url = WFS_set_url()
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
	if (is.null(version)) {
		version = WFS_default_version
	}
	.WFS_options$version = version
	invisible(version)
}

#' @export
#' @rdname wfsinit
#' @examples
#' WFS_get_version()

WFS_get_version <- function () {
	version = .WFS_options$version
	if (is.null(version)) {
		version = WFS_set_version()
	}
	version
}
