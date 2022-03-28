##' @title Package cache
##'
##' @name cache
##'
##' @description
##'
##' Function to access and manage the cache. `rxpCache()` returns the
##' central `rpx` cache. `pxCachedrojects()` prints the names of the
##' cached projects and invisibly returns the cache table.
##'
##' @details
##'
##' The cache is an object of class `BiocFileCache`, and created with
##' `BiocFileCache::BiocFileCache()`. It can be either the
##' package-wide cache as defined by `rpxCache()` or an instaned
##' provided by the user.
##'
##' When projects are cached, they are given a resource name (`rname`)
##' composed of the `.rpx` prefix followed by the ProteomeXchange
##' identifier. For example, project `PXD000001` is named
##' `.rpxPXD000001` (`.rpx2PXD000001` for the `PXDataset2` class) to
##' avoid any conflicts with other resources that user-created
##' resources.
##'
##' @return The `rpxCache()` function returns an instance of class
##'     `BiocFileCache`. `pxCachedProjects()` invisbly returns a
##'     `tibble` of cached ProteomeXchange projects.
##'
##' @rdname cache
##'
##' @author Laurent Gatto
##'
##' @importFrom tools R_user_dir
##'
##' @examples
##'
##' ## Default rpx cache
##' rpxCache()
##'
##' \dontrun{
##'
##' ## Set up your own cache by providing a file or a directory to
##' ## BiocFileCache::BiocFileCache()
##' my_cache <- BiocFileCache::BiocFileCache(tempfile())
##' my_cache
##' px <- PXDataset("PXD000001", cache = my_cache)
##' pxget(px, "erwinia_carotovora.fasta", cache = my_cache)
##'
##'
##' ## List of cached projects
##' pxCachedProjects() ## default rpx cache
##' pxCachedProjects(my_cache)
##'
##' ## To delete project a project from the default cache, first find
##' ## its resource id (rid) in the cache
##' px1_cache_info <- pxCacheInfo(px)
##' (rid <- px1_cache_info["rid"])
##'
##' ## Then remove it with BiocFileCache:: bfcremove()
##' BiocFileCache:::bfcremove(my_cache, rid)
##' pxCachedProjects(my_cache)
##' }
NULL

##' @rdname cache
##'
##' @export
rpxCache <- function() {
    cache <- tools::R_user_dir(package = "rpx", which = "cache")
    BiocFileCache::BiocFileCache(cache, ask = interactive())
}


##' @rdname cache
##'
##' @param cache Object of class `BiocFileCache`.
##'
##' @param rpxprefix `character(1)` defining the resourne name prefix
##'     in `cache`. Default is `"^\\.rpx(2?)"` to match objects of
##'     class `PXDataset` and `PXDataset2`.
##'
##' @export
pxCachedProjects <- function(cache = rpxCache(), rpxprefix = "^\\.rpx(2?)") {
    res <- bfcquery(cache, rpxprefix, "rname")
    ids <- grep(rpxprefix, bfcinfo(cache)$rname, value = TRUE)
    ids <- sub("^\\.rpx(2?)", "", ids)
    msg <- paste(strwrap(paste0("Cached projects (", length(ids), "): ",
                               paste(ids, collapse = ", "))), sep = "\n")
    message(paste(msg, "\n"), appendLF = FALSE)
    invisible(res)
}

##' @import BiocFileCache
pxget1 <- function(url, cache) {
    url <- get_url(url)
    ## Query the local rpx cache
    rid <- bfcquery(cache, url, "fpath", exact = TRUE)$rid
    ## Add the new file to the cache
    if (!length(rid)) {
        message("Downloading ", basename(url), " file." )
        rid <- names(bfcadd(cache, url, url))
    } else {
        message("Loading ", basename(url), " from cache." )
    }
    ## Update the file in the cache if needed. In theory, this should never be
    ## necessary, as the files are never changed in ProteomeXchange. Could
    ## explore <ChangeLogEntry date="2013-06-04"> tag in object@Data to
    ## explicitly check this.
    ## if (!isFALSE(bfcneedsupdate(cache, rid)))
    ##     bfcdownload(rpx_cache, rid)
    bfcrpath(cache, rids = rid)
}

ridFromCache2 <- function(object) {
    stopifnot(inherits(object, "PXDataset2"))
    rid <- bfcquery(BiocFileCache(object@cachepath),
                    object@px_rid, "rname", exact = TRUE)$rid
    if (!length(rid)) {
        warning("Project not found in cache.")
        rid <- NA
    }
    if (length(rid) > 1)
        stop("Multiple resource ids found.")
    rid
}



ridFromCache1 <- function(object) {
    stopifnot(inherits(object, "PXDataset"))
    if (is.null(object@cache$cachepath))
        return(NA)
    rid <- bfcquery(BiocFileCache(object@cache$cachepath),
                    paste0(".rpx", pxid(object)),
                    "rname", exact = TRUE)$rid
    if (!length(rid)) {
        warning("Project not found in cache.")
        rid <- NA
    }
    if (length(rid) > 1)
        stop("Multiple resource ids found.")
    rid
}

##' @importFrom curl has_internet
allPXD <- function(cache = rpxCache()) {
    .read_and_parse_sitemap <- function(rpath) {
        x <- readLines(rpath)
        sub("^.+projects/", "", grep("PXD", x, value = TRUE))
    }
    url <- "https://www.ebi.ac.uk/pride/ws/archive/v2/misc/sitemap"
    rid <- bfcquery(cache, url)
    ## initial download
    if (nrow(rid) == 0L) {
        if (!curl::has_internet())
            stop("Need an internet connection to download sitemap.")
        bfcadd(cache, "PrideSitemap", fpath = url)
        rid <- bfcquery(cache, url)
        return(.read_and_parse_sitemap(rid$rpath))
    }
    if (nrow(rid) > 1L) {
        ## clean cache and call again
        bfcremove(cache, rid$rid)
        return(allPXD(cache = cache))
    } ## nrow(rid) == 1
    if (!curl::has_internet()) {
        ## return from cache
        return(.read_and_parse_sitemap(rid$rpath))
    } else {
        ## update and return
        bfcdownload(cache, rid$rid, ask = FALSE)
        rid <- bfcquery(cache, url)
        return(.read_and_parse_sitemap(rid$rpath))
    }
}

localPxRepo <- function(cache = rpxCache()) {
    ids <- allPXD()
    x <- data.frame(id = ids, status = "remote")
    ## See GitHub issue
    x[x$id == "PXD009968", "status"] <- "error"
    x[x$id == "PXD012095", "status"] <- "error"
    x[x$id == "PXD012922", "status"] <- "error"
    x[x$id == "PXD012577", "status"] <- "error"
    x[x$id == "PXD010692", "status"] <- "error"
    x[x$id == "PXD012097", "status"] <- "error"
    rnames <- suppressMessages(pxCachedProjects(cache, rpxprefix = "^\\.rpx2")$rname)
    already_local <- sub("^\\.rpx2", "", rnames)
    x[x$id %in% already_local, "status"] <- "local"
    x
}
