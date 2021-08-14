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
##' `.rpxPXD000001` to avoid any conflicts with other resources that
##' user-created resources.
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
##' ## Set up your own cache by providing a file or a directory to
##' ## BiocFileCache::BiocFileCache()
##' my_cache <- BiocFileCache::BiocFileCache(tempfile())
##' my_cache
##' \dontrun{
##' px <- PXDataset("PXD000001", cache = my_cache)
##' pxget(px, "README.txt", cache = my_cache)
##' }
##'
##' ## List of cached projects
##' pxCachedProjects()
##'
##' ## To delete project a project from the default cache, first find
##' ## its resource id (rid) in the cache
##' (cache_tbl <- pxCachedProjects())
##' (rid <- cache_tbl[cache_tbl$rname == ".rpxPXD000001", "rid"][[1]])
##'
##' ## Alternatively, extact the information from the project
##' px <- PXDataset("PXD000001")
##' px1_cache_info <- pxCacheInfo(px)
##' (rid <- px1_cache_info["rid"])
##'
##' ## Then remove it with BiocFileCache:: bfcremove()
##' \dontrun{
##' BiocFileCache:::bfcremove(rpxCache(), rid)
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
##' @export
pxCachedProjects <- function(cache = rpxCache()) {
    res <- bfcquery(cache, "^.rpx")
    ids <- sub("^\\.rpx", "", res$rname)
    message("Cached projects: ", paste(ids, collapse = ", "))
    invisible(res)
}

##' @import BiocFileCache
pxget1 <- function(url, cache) {
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

ridFromCache <- function(object) {
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
