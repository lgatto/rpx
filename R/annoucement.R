annoucement <- function(pxid) {
    ns10 <- "http://proteomexchange.googlecode.com/svn/schema/proteomeXchange-1.0.xsd"
    ns <- ns11 <- "http://proteomexchange.googlecode.com/svn/schema/proteomeXchange-1.1.0.xsd"
    v <- sub("\" .+$", "",  sub("^.+formatVersion=\"", "", readLines(url)[2]))
    if (length(grep("1.0.0", v)) == 1) ns <- ns10

    
    url <- paste0("http://proteomecentral.proteomexchange.org/cgi/GetDataset?ID=",
                  pxid, "&outputMode=XML&test=no")
    doc <- xmlInternalTreeParse(url)


    
}


x <- xpathApply(doc, "/descendant::*")

