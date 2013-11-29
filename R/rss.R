## pxlatest <- function(n = -1) {
##     rss <- "https://groups.google.com/forum/feed/proteomexchange/msgs/rss_v2_0.xml"
##     rss <- sub("https", "http", rss)
##     dest <- tempfile()
##     download.file(rss, dest, quiet = TRUE)
##     doc <- xmlTreeParse(dest)
## }
