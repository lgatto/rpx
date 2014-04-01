.PXDataset <- setClass("PXDataset",
                       slots = list(
                           ## attributes
                           id = "character",
                           formatVersion = "character",
                           ## Nodes
                           Data = "XMLNode"))

setMethod("show", "PXDataset",
          function(object) {
              cat("Object of class \"",class(object),"\"\n",sep="")
              fls <- pxfiles(object)
              fls <- paste0("'", fls, "'")
              n <- length(fls)
              cat(" Id:", object@id, "with ")
              cat(n, "files\n")
              cat(" ")
              if (n < 3) {
                  cat(paste(fls, collapse = ", "), "\n")
              } else {
                  cat("[1]", paste(fls[1], collapse = ", "))
                  cat(" ... ")
                  cat("[", n, "] ", paste(fls[n], collapse = ", "),
                      "\n", sep = "")
                  cat(" Use 'pxfiles(.)' to see all files.\n")
              }
          })

pxnodes <- function(pxdata, name, all = FALSE) {
    if (all) {
        ans <- names(unlist(pxdata@Data))
        ans <- ans[grep("children", ans)]
        ans <- gsub("\\.", "/", ans)
        ans <- gsub("children", "", ans)
        return(ans)
    }
    if (missing(name)) ans <- names(names(pxdata@Data))        
    else ans <- names(xmlChildren(pxdata@Data[[name]]))
    ans
}


setMethod("pxid", "PXDataset",
          function(object) {
              ans <- object@id
              names(ans) <- NULL
              ans
          })

setMethod("pxurl", "PXDataset",
          function(object) {
              p <- "//cvParam[@accession = 'PRIDE:0000411']"
              x <- xmlAttrs(getNodeSet(doc = object@Data, path = p)[[1]])["value"]
              names(x) <- NULL
              x
          })


setMethod("pxtax", "PXDataset",
          function(object) {
              p <- "//cvParam[@accession = 'MS:1001469']"
              x <- xmlAttrs(getNodeSet(doc = object@Data, path = p)[[1]])["value"]
              names(x) <- NULL
              x
          })


setMethod("pxref", "PXDataset",
          function(object) {              
              p <- "//cvParam[@accession = 'PRIDE:0000400']"
              q <- "//cvParam[@accession = 'PRIDE:0000432']"    
              refnode <- getNodeSet(doc = object@Data, path = p)
              pendingrefnode <- getNodeSet(doc = object@Data, path = q)
              ans <- NA
              if (length(refnode) > 0) {
                  ## there is a publication
                  ans <- xmlAttrs(refnode[[1]])["value"]
                  names(ans) <- NULL
              } else if (length(pendingrefnode) > 0) { 
                  ## maybe a pending publication?                  
                  ans <- xmlAttrs(pendingrefnode[[1]])["name"]
                  names(ans) <- NULL                  
              } else {
                  message("No (pending) publication found")
              }
              return(ans)
          })


setMethod("pxfiles", "PXDataset",
          function(object) {
              ftpdir <- paste0(pxurl(object), "/")
              ans <- strsplit(getURL(ftpdir, dirlistonly = TRUE), "\n")[[1]]
              if (Sys.info()['sysname'] == "Windows")
                  ans <- sub("\r$", "", ans)
              ans
          })


setMethod("pxget", "PXDataset",
          function(object, list, force=FALSE, ...) {
              fls <- pxfiles(object)
              url <- pxurl(object)
              if (missing(list)) 
                  list <- menu(fls, FALSE, paste0("Files for ", object@id))
              if (length(list) == 1 && list == "all") {
                  toget <- fls
              } else {
                  if (is.character(list)) {
                      toget <- fls[fls %in% list]
                  } else toget <- fls[list]
              }
              if (length(toget) < 1)
                  stop("No files to download.")
              urls <- gsub(" ", "\ ", paste0(url, "/", toget))
              message("Downloading ", length(urls), " file",
                      ifelse(length(urls) > 1, "s", ""))
              for (i in 1:length(urls)) {
                  if (file.exists(toget[i]) && !force)
                      message(toget[i], " already present.")
                  else download.file(urls[i], toget[i], ...)
              }
              invisible(toget)
          })

## constructor
PXDataset <- function(id) {
    ## Supported formats: 1.0, 1.1 and 1.2
    url <- paste0(
        "http://proteomecentral.proteomexchange.org/cgi/GetDataset?ID=",
        id, "&outputMode=XML&test=no")
    ns10 <- "http://proteomexchange.googlecode.com/svn/schema/proteomeXchange-1.0.xsd"
    ## 1.1 and 1.2 only differ by a few minor changes
    ns11 <- "http://proteomexchange.googlecode.com/svn/schema/proteomeXchange-1.1.0.xsd"
    ns <- ns12 <- "http://proteomexchange.googlecode.com/svn/schema/proteomeXchange-1.2.0.xsd"
    x <- readLines(url)
    if (length(grep("ERROR", x)) > 0) {
        x <- x[grep("message=", x)]
        x <- sub("message=", "", x)
        stop(x)
    }       
    x <- x[x != ""]   
    v <- sub("\".+$", "",  sub("^.+formatVersion=\"", "", x[2]))
    if (length(grep("1.0.0", v)) == 1) ns <- ns10    
    doc <- xmlTreeParse(url)
    pxdata <- doc[["doc"]]$children$ProteomeXchangeDataset
    .formatVersion <- xmlAttrs(pxdata)["formatVersion"]
    .id <- xmlAttrs(pxdata)["id"]
    if (length(.id) != 1)
        stop("Got ", length(.id), " identifiers: ",
             paste(.id, collapse = ", "), ".")
    if (id != .id)
        warning("Identifier '", id, "' not found. Retrieved '",
                .id, "' instead.")
    if (v != .formatVersion)
        warning("Format version does not match. Got '",
                .formatVersion, "' instead of '", v, "'.")
    .PXDataset(id = .id,
               formatVersion = .formatVersion,
               Data = pxdata)
}


