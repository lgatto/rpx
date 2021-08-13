##' @title Package cache
##'
##' @description
##'
##' Function to access the cache directory. `rxpCache()` returns the
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
##' @return `character(1)` with the path to the cache directory for
##'     `rpxCache()` and a `tibble` for `pxCachedProjects()`.
##'
##' @rdname cache
##'
##' @author Laurent Gatto
##'
##' @importFrom tools R_user_dir
##'
##' @export
##'
##' @examples
##'
##' rpxCache()
##'
##' pxCachedProjects()
rpxCache <- function() {
    cache <- tools::R_user_dir(package = "rpx", which = "cache")
    BiocFileCache::BiocFileCache(cache, ask = interactive())
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
