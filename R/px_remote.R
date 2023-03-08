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

get_url <- function(x) {
    ## if there's no internet, don't check/fix URL
    if (curl::has_internet())
        fix_issue_17(x)
    else x
}


##' @importFrom curl new_handle handle_setopt curl
list_files <- function(ftp_url) {
    ## Adapted from https://gist.github.com/adamhsparks/18f7702906f33dd66788e0078979ff9a
    list_files <- curl::new_handle()
    curl::handle_setopt(list_files, ftp_use_epsv = TRUE, dirlistonly = TRUE)
    con <- curl::curl(url = ftp_url, "r", handle = list_files)
    on.exit(close(con))
    readLines(con)
}

pride_files_dataframe <- function(files, ftp_url) {
    files <- as.character(files)
    if (!length(files))
        stop("No files provided")
    ans <- data.frame(ID = seq_along(files),
                      NAME = files,
                      URI = paste0(ftp_url, files),
                      TYPE = NA_character_,
                      MAPPINGS = "-")
    ## TODO: set TYPES
    ans
}
