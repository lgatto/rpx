##' Queries the PX rss feed file for the latest PX dataset
##' announcements.
##'
##' @title Return recent PX announcements 
##' @return A \code{data.frame} with announcements data set
##' identifiers, publication dates and annoucement messages.
##' @author Laurent Gatto
##' @examples
##' pxannounced()
pxannouced <- function() {
    rss <- "https://groups.google.com/forum/feed/proteomexchange/msgs/rss_v2_0.xml"
    dest <- tempfile()
    download.file(rss, dest, method = "wget", quiet = TRUE)
    doc <- xmlParse(dest)

    ## parse title
    ttls <- getNodeSet(doc, "//title")    
    ttls <- xmlSApply(ttls, function(xx) xmlSApply(xx, xmlValue))[-1]
    msg <- sub(" ProteomeXchange dataset.+$", "", ttls)
    msg <- sub(" for", "", msg)
    dat <- sub("^.+dataset ", "", ttls)
    n <- length(ttls)
    message(n, " new ProteomeXchange annoucements")
    names(ttls) <- NULL

    ## parse pubDate
    pubs <- getNodeSet(doc, "//pubDate")
    pubs <- xmlSApply(pubs, function(xx) xmlSApply(xx, xmlValue))
    names(pubs) <- NULL
    pubs <- strptime(pubs,  "%a, %d %b %Y %H:%M:%S", tz = "GMT")

    ann <- data.frame(Data.Set = dat,
                      Publication.Data = pubs,
                      Message = msg)
    return(ann)
    
}
