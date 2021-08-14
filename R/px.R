##' @title The PXDataset to find and download proteomics data
##'
##' @aliases class:PXDataset PXDataset
##'     pxfiles,PXDataset-method pxfiles pxget,PXDataset-method pxget
##'     pxid,PXDataset-method pxid pxref,PXDataset-method pxref
##'     pxtax,PXDataset-method pxtax pxurl,PXDataset-method pxurl
##'     show,PXDataset-method
##'
##' @name PXDataset
##'
##' @description
##'
##' The `rpx` package provides the infrastructure to access, store and
##' retrieve information for ProteomeXchange (PX) data sets. This can
##' be achieved with `PXDataset` objects can be created with the
##' `PXDataset()` constructor that takes the unique ProteomeXchange
##' project identifier as input.
##'
##' @details
##'
##' Since version 1.99.1, `rpx` uses the Bioconductor `BiocFileCache`
##' package to automatically cache all downloaded ProteomeXchange
##' files. When a file is downloaded for the first time, it is added
##' to the cache. When already available, the file path to the cached
##' file is directly returned. The central `rpx` package chache,
##' object of class `BiocFileCache`, is returned by
##' [rpxCache()]. Users can also provide their own cache object
##' instead of using the default central cache to `pxget()`.
##'
##' Since 2.1.1, `PXDataset` instances are also cached using the same
##' mechanism as project files. Each `PXDataset` instance also stored
##' the project file names, the reference, taxonomy of the sample and
##' the project URL (see slot `cache`) instead of accessing these
##' every time they are needed to reduce remote access and reliance on
##' a stable internet connection. As for files, the default cache is
##' as returned by [rpxCache()], but users can pass their own
##' `BiocFileCache` objects.
##'
##' For more details on how to manage the cache (for example if some
##' files need to be deleted), please refer to the `BiocFileCache`
##' package vignette and documentation. See also [rpxCache()] for
##' additional details.
##'
##' @slot id `character(1)` containing the dataset's unique
##'     ProteomeXchange identifier, as used to create the object.
##'
##' @slot formatVersion `character(1)` storing the version of the
##'     ProteomeXchange schema. Schema versions 1.0, 1.1 and 1.2 are
##'     supported (see
##'     [https://code.google.com/p/proteomexchange/source/browse/schema/](https://code.google.com/p/proteomexchange/source/browse/schema/)).
##'
##' @slot cache `list()` storing the available files (element
##'     `pxfiles`), the reference associated with the data set
##'     (`pxref`), the taxonomy of the sample (`pxtax`) and the
##'     datasets' ProteomeXchange URL (`pxurl`). These are returned by
##'     the respective accessors. It also stores the path to the cache
##'     it is stored in (element `cachepath`).
##'
##' @slot Data `XMLNode` storing the ProteomeXchange description as
##'     XML node tree.
##'
##' @section Accessors:
##'
##' - `pxfiles(object)` returns the project file names.
##'
##' - `pxget(object, list, cache)`: if the file(s) in `list` have
##'    never been requested, `pxget()` downloads the files from the
##'    ProteomeXchange repository, caches them in `cache` and returns
##'    their path. If the files have previously been downloaded and
##'    are available in `cache`, their path is directly returned.
##'
##'    If `list` is missing, the file to be downloaded can be selected
##'    from a menu. If `list = "all"`, all files are downloaded. The
##'    file names, as returned by `pxfiles()` can also be
##'    used. Alternatively, a `logical` or `numeric` index can be
##'    used.
##'
##'    The argument `cache` can be passed to define the path to the
##'    cache. The default cache is the packages' default as returned
##'    by `rpxCache()`.
##'
##' - `pxtax(object)`: returns the taxonomic name of `object`.
##'
##' - `pxurl(object)`: returns the base url on the ProteomeXchange
##'    server where the project files reside.
##'
##' - `pxCacheInfo(object, cache): prints and invisibly returns
##'    `object`'s caching information from `cache` (default is
##'    `rpxCache()`). The return value is a named vector of length two
##'    containing the resourne identifier and the cache location.
##'
##' @author Laurent Gatto
##'
##' @references Vizcaino J.A. et al. 'ProteomeXchange: globally co-ordinated
##' proteomics data submission and dissemination', Nature Biotechnology 2014,
##' 32, 223 -- 226, doi:10.1038/nbt.2839.
##'
##' Source repository for the ProteomeXchange project:
##' https://code.google.com/p/proteomexchange/
##'
##' @examples
##'
##' px <- PXDataset("PXD000001")
##' px
##' pxtax(px)
##' pxurl(px)
##' pxref(px)
##' pxfiles(px)
##' pxCacheInfo(px)
##'
##' fas <- pxget(px, "erwinia_carotovora.fasta")
##' fas
##' library("Biostrings")
##' readAAStringSet(fas)
NULL

##  Wrong ftp URL in xml of data, as documented in issue #5
rpx_env <- new.env(parent = emptyenv())
rpx_env$rpx_fix_issue_5 <- TRUE
apply_fix_issue_5 <- function(x = TRUE)
    rpx_env$rpx_fix_issue_5 <- x

## setOldClass(c("xml_document", "xml_node"))

.valid_ftp_url <- function(url) {
    if (length(url) == 0) return(FALSE)
    valid <- try(RCurl::getURL(paste0(url, "/"), dirlistonly = TRUE),
                 silent = TRUE)
    ifelse(inherits(valid, "try-error"), FALSE, TRUE)
}

##' @importFrom methods new
.PXDataset <- setClass("PXDataset",
                       slots = list(
                           ## attributes
                           id = "character",
                           formatVersion = "character",
                           ## Cache
                           cache = "list",
                           ## Nodes
                           Data = "xml_document"))


##' @importFrom methods show
##'
##' @exportMethod show
setMethod("show", "PXDataset",
          function(object) {
              cat("Object of class \"", class(object), "\"\n", sep = "")
              fls <- pxfiles(object)
              fls <- paste0("'", fls, "'")
              n <- length(fls)
              cat(" Id:", object@id, "with ")
              cat(n, "files\n ")
              pxCacheInfo(object)
              cat(" ")
              if (n < 3) {
                  cat(paste(fls, collapse = ", "), "\n")
              } else {
                  cat("[1]", paste(fls[1], collapse = ", "))
                  cat(" ... ")
                  cat("[", n, "] ", paste(fls[n], collapse = ", "),
                      "\n", sep = "")
                  cat(" Use 'pxfiles(.)' to see all files.\n")
              }
          })

##' @param object An instance of class `PXDataset`, as created by
##'     `PXDataset()`.
##'
##' @rdname PXDataset
##'
##' @export
pxid <- function(object) object@id

##' @rdname PXDataset
##'
##' @export
pxurl <- function(object) {
    stopifnot(inherits(object, "PXDataset"))
    if (is.null(object@cache$pxurl)) {
        p <- "//cvParam[@accession = 'PRIDE:0000411']"
        url <- xml_attr(xml_find_all(object@Data, p), "value")
        if (!.valid_ftp_url(url)) {
            url <- sub("ac\\.uk/", "ac\\.uk/pride/data/archive/", url)
        }
        if (!.valid_ftp_url(url)) {
            p <- "//cvParam[@accession = 'MS:1002852']"
            url <- xml_attr(xml_find_all(object@Data, p), "value")
        }
        if (!.valid_ftp_url(url)) {
            stop("No URL detected: please open an issue at https://github.com/lgatto/rpx/issues")
        }
        names(url) <- NULL
        object@cache$pxurl <- url
    }
    object@cache$pxurl
}

##' @rdname PXDataset
##'
##' @export
pxtax <- function(object) {
    stopifnot(inherits(object, "PXDataset"))
    if (is.null(object@cache$pxtax)) {
        p <- "//cvParam[@accession = 'MS:1001469']"
        tax <- xml_attr(xml_find_all(object@Data, p), "value")
        names(tax) <- NULL
        object@cache$pxtax <- tax
    }
    object@cache$pxtax
}


##' @rdname PXDataset
##'
##' @export
pxref <- function(object) {
    stopifnot(inherits(object, "PXDataset"))
    if (is.null(object@cache$pxref)) {
        p <- "//cvParam[@accession = 'PRIDE:0000400']"
        q <- "//cvParam[@accession = 'PRIDE:0000432']"
        ref <- xml_attr(xml_find_all(object@Data, p), "value")
        pendingref <- xml_attr(xml_find_all(object@Data, q), "value")
        object@cache$pxref <- c(ref, pendingref)
    }
    object@cache$pxref
}

##' @rdname PXDataset
##'
##' @importFrom RCurl getURL
##'
##' @export
pxfiles <- function(object) {
    stopifnot(inherits(object, "PXDataset"))
    if (is.null(object@cache$pxfiles)) {
        ftpdir <- paste0(pxurl(object), "/")
        ans <- strsplit(getURL(ftpdir, dirlistonly = TRUE), "\n")[[1]]
        if (Sys.info()['sysname'] == "Windows")
            ans <- sub("\r$", "", ans)
        ## Don't display the 'generated' directory (contains files
        ## generated by ProteomeXchange).
        object@cache$pxfiles <- ans[!grepl("generated", ans)]
    }
    object@cache$pxfiles
}

##' @rdname PXDataset
##'
##' @param list `character()`, `numeric()` or `logical()` defining the
##'     project files to be downloaded. This list of files can
##'     retrieved with `pxfiles()`.
##'
##' @param cache Object of class `BiocFileCache`. Default is to use
##'     the central `rpx` cache returned by `rpxCache()`, but users
##'     can use their own cache. See [rpxCache()] for details.
##'
##' @importFrom utils menu
##'
##' @export
pxget <- function(object, list, cache = rpxCache()) {
    fls <- pxfiles(object)
    url <- pxurl(object)
    if (missing(list))
        list <- menu(fls, FALSE, paste0("Files for ", object@id))
    if (length(list) == 1 && list == "all") {
        toget <- fls
    } else {
        if (is.character(list)) {
            toget <- fls[fls %in% list]
        } else toget <- fls[list]
    }
    if (length(toget) < 1)
        stop("No files to download.")
    toget <- urls <- gsub(" ", "\ ", paste0(url, "/", toget))
    for (i in 1:length(urls)) {
            toget[i] <- pxget1(urls[i], cache)
    }
    toget
}


##' @rdname PXDataset
##'
##' @export
##'
pxCacheInfo <- function(object, cache = rpxCache()) {
    rid <- ridFromCache(object)
    if (is.na(rid)) msg <- "No caching information found."
    else msg <- paste0("Resource ID ", rid, " in cache in ", object@cache$cachepath, ".")
    message(msg)
    invisible(c(rid = rid, cachepath = object@cache$cachepath))
}

## ns10 <- "https://raw.githubusercontent.com/proteomexchange/proteomecentral/master/lib/schemas/proteomeXchange-1.0.xsd"
## ns11 <- "https://raw.githubusercontent.com/proteomexchange/proteomecentral/master/lib/schemas/proteomeXchange-1.1.0.xsd"
## ns12 <- "https://raw.githubusercontent.com/proteomexchange/proteomecentral/master/lib/schemas/proteomeXchange-1.2.0.xsd"
## ns13 <- "https://raw.githubusercontent.com/proteomexchange/proteomecentral/master/lib/schemas/proteomeXchange-1.3.0.xsd"

##' @name PXDataset
##'
##' @param id `character(1)` containing a valid ProteomeXchange
##'     identifier.
##'
##' @import xml2
##'
##' @importFrom methods validObject
##'
##' @export
##'
##' @return The `PXDataset()` constructor returns a cached `PXDataset`
##'     object. It thus also modifies the cache used to projet
##'     caching, as defined by the `cache` argument.
PXDataset <- function(id, cache = rpxCache()) {
    ## Check if that PX id is already available in BiocFileCache
    rpxId <- paste0(".rpx", id)
    rpath <- bfcquery(cache, rpxId, "rname", exact = TRUE)$rpath
    if (!length(rpath)) {
        ## Query PX identifier
        message("Querying ProteomeXchange for ", id, "...")
        url <- paste0(
            "http://proteomecentral.proteomexchange.org/cgi/GetDataset?ID=",
            id, "&outputMode=XML&test=no")
        x <- readLines(url)
        if (length(grep("ERROR", x)) > 0) {
            x <- x[grep("message=", x)]
            x <- sub("message=", "", x)
            stop(x)
        }
        x <- x[x != ""]
        v <- sub("\".+$", "",  sub("^.+formatVersion=\"", "", x[2]))
        x <- read_xml(url)
        .formatVersion <- xml_attr(x, "formatVersion")
        .id <- xml_attr(x, "id")
        if (length(.id) != 1)
            stop("Got ", length(.id), " identifiers: ",
                 paste(.id, collapse = ", "), ".")
        if (id != .id)
            warning("Identifier '", id, "' not found. Retrieved '",
                    .id, "' instead.")
        if (v != .formatVersion)
            warning("Format version does not match. Got '",
                    .formatVersion, "' instead of '", v, "'.")
        ## Create PX object
        ans <- .PXDataset(id = .id,
                          formatVersion = .formatVersion,
                          Data = x)
        ## Populate object datag
        message("Retrieving project data...")
        ans@cache <- list(pxurl = pxurl(ans),
                          pxref = pxref(ans),
                          pxfiles = pxfiles(ans),
                          pxtax = pxtax(ans),
                          cachepath = bfccache(cache))
        ## Add the object to cache
        savepath <- bfcnew(cache, rpxId, ext=".rds")
        saveRDS(ans, savepath)
        return(ans)
    }
    ## Retrieve from cache and return
    message("Loading ", id, " from cache.")
    px <- readRDS(rpath)
    if (!inherits(px, "PXDataset") | !methods::validObject(px))
        stop("Project ", id, " isn't a valid PXDataset object.\n",
             "  Please delete it from cache and regenerate it.\n",
             "  See ?rpxCached() for details.")
    return(px)
}
