##' This function returns the central `rpx` cache directory.
##'
##' @title Package cache
##'
##' @return `character(1)` with the path to the cache directory.
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
