##' @title The PXDataset to find and download proteomics data
##'
##' @aliases PXDataset-class class:PXDataset PXDataset
##'     pxfiles,PXDataset-method pxfiles pxget,PXDataset-method pxget
##'     pxid,PXDataset-method pxid pxref,PXDataset-method pxref
##'     pxtax,PXDataset-method pxtax pxurl,PXDataset-method pxurl
##'     show,PXDataset-method
##'
##' @name PXDataset
##'
##' @description
##'
##' An S4 class to access, store and retrieve information for
##' ProteomeXchange (PX) data sets. Objects can be created with the
##' `PXDataset()` constructor.
##'
##' @details
##'
##' Since version 1.99.1, `rpx` uses the Bioconductor `BiocFileCache`
##' package to automatically cache all downloaded ProteomeXchange
##' files. When a file is downloaded for the first time, it is added
##' to the cache. When already available, the file path to the cached
##' file is returned. The rpx chache is returned by
##' `rpxCache()`. The user is asked to confirm its creation on
##' first usage of the package. Users can also provide their own cache
##' directories instead of using the default central cache to
##' `pxget()`.
##'
##' Since 2.1.1, `PXDataset` instances are also cached using the same
##' mechanism as project files. Each `PXDataset` instance also stored
##' the project file names, the reference, taxonomy of the sample and
##' the project URL (see slot `cache`) instead of accessing these
##' every time they are needed to reduce remote access and reliance on
##' a stable internet connection.
##'
##' For more details on how to manage the cache (for example if some
##' files need to be deleted), please refer to the BiocFileCache
##' vignette.
##'
##' @slot id `character(1)` containing the dataset's unique
##'     ProteomeXchange identifier, as used to create the object.
##'
##' @slot formatVersion `character(1)` storing the version of the
##'     ProteomeXchange schema. Schema versions 1.0, 1.1 and 1.2 are
##'     supported (see
##'     https://code.google.com/p/proteomexchange/source/browse/schema/).
##'
##' @slot cache `list()` storing the available files (element
##'     `pxfiles`), the reference associated with the data set
##'     (`pxref`), the taxonomy of the sample (`pxtax`) and the
##'     datasets' ProteomeXchange URL (`pxurl`). These are returned by
##'     the respective accessors.
##'
##' @slot Data `XMLNode` storing the ProteomeXchange description as
##'     XML node tree.
##'
##' @section Accessors:
##'
##' - `pxfiles(object)` returns the project file names.
##'
##' - `pxget(object, list, cache)`: downloads the files from the
##'    ProteomeXchange repository. If `list` is missing, the file to
##'    be downloaded can be selected from a menu. If `list = "all"`,
##'    all files are downloaded. The file names, as returned by
##'    `pxfiles()` can also be used. Alternatively, a `logical` or
##'    `numeric` indices can be used.
##'
##'    If not already cached, the files are downloaded and added to
##'    the package cache. The function then returns the names of the
##'    files in the cache directory.
##'
##'    The argument `cache` can be passed to define the path to the
##'    cache directory. The default cache is the packages' default as
##'    returned by `rpxCache()`.
##'
##' - `pxtax(object)`: returns the taxonomic name of `object`.
##'
##' - `pxurl(object)`: returns the base url on the ProteomeXchange
##'    server where `pxfiles` reside.
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
              cat(n, "files\n")
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

## ##' Returns the node names of the underliyng XML content of an
## ##' \code{PXDataset} object, available in the \code{Data} slot. This
## ##' function is meant to be used if additional parsing of the XML
## ##' structure is needed.
## ##'
## ##' @title Return the nodes of a \code{PXDataset}
## ##' @param pxdata An instance of class \code{PXDataset}.
## ##' @param name The name of a node.
## ##' @param all Should node from all levels be returned. Default is
## ##' \code{FALSE}.
## ##' @return A \code{character} with XML node names.
## ##' @author Laurent Gatto
## pxnodes <- function(pxdata, name, all = FALSE) {
##     stopifnot(inherits(pxdata, "PXDataset"))
##     stop("Not available for new version")
##     if (all) {
##         ans <- names(unlist(pxdata@Data))
##         ans <- ans[grep("children", ans)]
##         ans <- gsub("\\.", "/", ans)
##         ans <- gsub("children", "", ans)
##         return(ans)
##     }
##     if (missing(name)) ans <- names(names(pxdata@Data))
##     else ans <- names(xmlChildren(pxdata@Data[[name]]))
##     ans
## }


##' @param object An instance of class `PXDataset`.
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
##'     `pxfiles()` to be downloaded.
##'
##' @param cache `character(1)` with the path to the cache
##'     directory. Default is to use the central `rpx` cache returned
##'     by `rpxCache()`.
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
##' @export
##'
##' @return An cached object of class `PXDataset`.
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
        ## Populate object data
        message("Populating object...")
        ans@cache <- list(pxurl = pxurl(ans),
                          pxref = pxref(ans),
                          pxfile = pxfiles(ans),
                          pxtax = pxtax(ans))
        ## Add the object to cache
        savepath <- bfcnew(cache, rpxId, ext=".rds")
        saveRDS(ans, savepath)
        return(ans)
    }
    if (length(rpath) != 1)
        stop(paste0("Non-unique internal ", rpxId, " found!\n",
                    "Please check and clear your cache."))
    ## Retrieve from cache and return
    message("Loading ", id, " from cache.")
    readRDS(rpath)
}
