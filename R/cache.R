.get_cache <- function() {
    cache <- rappdirs::user_cache_dir(appname = "rpx")
    BiocFileCache::BiocFileCache(cache, ask = interactive())
}

pxget1 <- function(url, cache = .get_cache()) {
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
