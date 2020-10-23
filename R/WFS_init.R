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
#' the default version (`r WFS_default_version`) is be used.
#' The `sep` determines the separator to include in the output of the [HOQCwfs::fg()] function. If no `sep` is specified before a `HOQCwfs` function is used,
#' the new line character (`\n`) is be used.
#' \cr\cr
#' Note that these values are used as the default values for the various functions. They can always be overwritten there.
#'
#' @param url NULL for the default url ( **`r WFS_default_url`** ) or the url of the WFS service otherwise
#' @return Character vector (invisible for the set version) with the url or version to be used
#' @export
#' @rdname wfsinit
#' @examples
#' \dontrun{
#' WFS_set_url("https://geoweb.amstelveen.nl/geoserver/topp/wfs")
#' }

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

#' @param sep NULL for the default ( **`\n`** ) separator or an alternative sep (e.g. `''`  ) otherwise
#' @export
#' @rdname wfsinit
#' @examples
#' WFS_set_sep()
#' WFS_set_sep('\n')


WFS_set_sep <- function (sep=NULL) {
	if (is.null(sep)) {
		sep = WFS_default_sep
	}
	.WFS_options$sep = sep
	invisible(sep)
}

#' @export
#' @rdname wfsinit
#' @examples
#' WFS_get_sep()

WFS_get_sep <- function () {
	sep = .WFS_options$sep
	if (is.null(sep)) {
		sep = WFS_set_sep()
	}
	sep
}
