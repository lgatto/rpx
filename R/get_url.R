## See https://github.com/lgatto/rpx/issues/17
fix_issue_17 <- function(x) {
    make_http_url <- function(x)
        sub("^ftp://", "http://", x)
    failed_url <- c()
    if (RCurl::url.exists(make_http_url(x))) {
        return(x)
    }
    failed_url <- append(failed_url, x)
    x <- sub("/data/", "-", x)
    if (RCurl::url.exists(make_http_url(x))) {
        return(x)
    }
    failed_url <- append(failed_url, x)
    stop("Can't access URL(s):\n ",
         paste(failed_url, collapse = "\n "))
}

get_url <- function(x)
    fix_issue_17(x)
