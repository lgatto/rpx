## See https://github.com/lgatto/rpx/issues/17
fix_issue_17 <- function(x) {
    make_http_url <- function(x)
        sub("^ftp://", "http://", x)
    http_x <- make_http_url(x)
    if (!RCurl::url.exists(http_x))
        x2 <- sub("/data/", "-", x)
    http_x2 <- make_http_url(x2)
    if (!RCurl::url.exists(http_x2))
        stop("Can't fix URL")
    x2
}


get_url <- function(x)
    fix_issue_17(x)
