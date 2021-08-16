##' @title New PXDataset (v2) to find and download proteomics data
##'
##' @aliases class:PXDataset2 PXDataset2 PXDataset pxtitle
##'     pxfiles,PXDataset2-method pxfiles pxget,PXDataset2-method pxget
##'     pxid,PXDataset2-method pxid pxref,PXDataset2-method pxref
##'     pxtax,PXDataset2-method pxtax pxurl,PXDataset2-method pxurl
##'     show,PXDataset2-method pxCacheInfo,PXdataset-method pxCacheInfo
##'
##' @name PXDataset2
##'
##' @description
##'
##' The `rpx` package provides the infrastructure to access, store and
##' retrieve information for ProteomeXchange (PX) data sets. This can
##' be achieved with `PXDataset2` objects can be created with the
##' `PXDataset2()` constructor that takes the unique ProteomeXchange
##' project identifier as input.
##'
##' The new `PXDataset2` class superseeds the previous and now
##' deprecated `PXDataset` version.
##'
##' @details
##'
##' The `rpx` packages uses caching to store ProteomeXchange projects
##' and project files. When creating an object with `PXDataset2()`,
##' the cache is first queried for the projects identifier. If a
##' unique hit is found, the project is retrieved and returned. If no
##' matching project identifier is found, then the remote resource is
##' accessed to first create the new `PXDataset2()` project, then
##' cache it before returning it to the user. The same mechanism is
##' applied when project files are requested.
##'
##' Caching is supported by BiocFileCache package. The `PXDataset2()`
##' constructor and the `px_get()` function can be passed a instance
##' of class `BiocFileCache` that defines the cache. The default is to
##' use the package-wide cache defined in `rpxCache()`. For more
##' details on how to manage the cache (for example if some files need
##' to be deleted), please refer to the `BiocFileCache` package
##' vignette and documentation. See also [rpxCache()] for additional
##' details.
##'
##'
##' @slot px_id `character(1)` containing the dataset's unique
##'     ProteomeXchange identifier, as used to create the object.
##'
##' @slot px_rid `character(1)` storing the cached resource name in
##'     the BiocFileCache instance stored in `cachepath`.
##'
##' @slot px_title `character(1)` with the project's title.
##'
##' @slot px_url `character(1) with the project's URL.
##'
##' @slot px_doi `character(1)` with the project's DOI.
##'
##' @slot px_ref `character` containing the project's reference(s).
##'
##' @slot px_ref_doi `character` containing the project's reference DOIs.
##'
##' @slot px_pubmed `character` containing the project's reference
##'     PubMed identifier.
##'
##' @slot px_files `data.frame` containing information about the
##'     project files, including file names, URIs and types. The files
##'     are retrieved from the project's README.txt file.
##'
##' @slot px_tax `charcter` (typically of length 1) containing the
##'     taxonomy of the sample.
##'
##' @slot px_metadata `list` containing the project's metadata, as
##'     downloaded from the ProteomeXchange site. All slots but
##'     `px_files` are populated from this one.
##'
##' @slot cachepath `character(1)` storing the path to the cache the
##'     project object is stored in.
##'
##' @section Accessors:
##'
##' - `pxfiles(object, n = 10)` invisibly returns all the project file
##'    names. The function prints the first `n` files specifying
##'    whether they are local of remote (based on the cache the object
##'    is stored in). The printing can be ignored by wrapping the call
##'    in `suppressMessages()`.
##'
##' - `pxget(object, list, cache)`: `list` is a vector defining the
##'    files to be downloaded. If `list = "all"`, all files are
##'    downloaded. The file names, as returned by `pxfiles()` can also
##'    be used. Alternatively, a `logical` or `numeric` index can be
##'    used. If missing, the file to be downloaded can be selected
##'    from a menu.
##'
##'    The argument `cache` can be passed to define the path to the
##'    cache. The default cache is the packages' default as returned
##'    by `rpxCache()`.
##'
##' - `pxtax(object)`: returns the taxonomic name of `object`.
##'
##' - `pxurl(object)`: returns the base url on the ProteomeXchange
##'    server where the project files reside.
##'
##' - `pxCacheInfo(object, cache): prints and invisibly returns
##'    `object`'s caching information from `cache` (default is
##'    `rpxCache()`). The return value is a named vector of length two
##'    containing the resourne identifier and the cache location.
##'
##' - `pxtitle(object): returns the project's title.
##'
##' - `pxref(object)`: returns the project's bibliographic
##'   reference(s).
##'
##' @author Laurent Gatto
##'
##' @references Vizcaino J.A. et al. 'ProteomeXchange: globally co-ordinated
##' proteomics data submission and dissemination', Nature Biotechnology 2014,
##' 32, 223 -- 226, doi:10.1038/nbt.2839.
##'
##' Source repository for the ProteomeXchange project:
##' https://code.google.com/p/proteomexchange/
##'
##' @examples
##'
##' px <- PXDataset2("PXD000001")
##' px
##' pxtax(px)
##' pxurl(px)
##' pxref(px)
##' pxfiles(px)
##' pxCacheInfo(px)
##'
##' fas <- pxget(px, "erwinia_carotovora.fasta")
##' fas
##' library("Biostrings")
##' readAAStringSet(fas)
NULL

.PXDataset2 <- setClass("PXDataset2",
                        slots = list(
                            px_id = "character",
                            px_rid = "character",
                            px_title = "character",
                            px_url = "character",
                            px_doi = "character",
                            px_ref = "character",
                            px_ref_doi = "character",
                            px_pubmed = "character",
                            px_files = "data.frame",
                            px_tax = "character",
                            px_metadata = "list",
                            cachepath = "character"))

##' @name PXDataset2
##'
##' @param id `character(1)` containing a valid ProteomeXchange
##'     identifier.
##'
##' @importFrom methods validObject
##'
##' @importFrom jsonlite fromJSON
##'
##' @importFrom utils read.delim
##'
##' @export
##'
##' @return The `PXDataset2()` returns a cached `PXDataset2`
##'     object. It thus also modifies the cache used to projet
##'     caching, as defined by the `cache` argument.
PXDataset2 <- function(id, cache = rpxCache()) {
    ## Check if that PX id is already available in BiocFileCache
    rpxId <- paste0(".rpx2", id)
    rpath <- BiocFileCache::bfcquery(cache, rpxId, "rname", exact = TRUE)$rpath
    if (!length(rpath)) {
        ## Generate new object
        message("Querying ProteomeXchange for ", id, ".")
        ws_url <- "https://www.ebi.ac.uk/pride/ws/archive/v2/projects/"
        project_url <- paste0(ws_url, id)
        px_metadata <- jsonlite::fromJSON(project_url)
        px_url  <- px_metadata[["_links"]]$datasetFtpUrl$href
        px_files <- read.delim(paste0(px_url, "/README.txt"))
        px_id <- px_metadata$accession
        if (id != px_id)
            message("Replacing ", id, " with ", px_id, ".")
        px_ref <- px_ref_doi <- px_pubmed <- NA_character_
        if (length(px_metadata$references)) {
            if (!is.null(px_metadata$references$referenceLine))
                px_ref <- px_metadata$references$referenceLine
            if (!is.null(px_metadata$references$doi))
                px_ref_doi <- px_metadata$references$doi
            if (!is.null(px_metadata$references$pubmedId))
                px_pubmed <- as.character(px_metadata$references$pubmedId)
        }
        ans <- .PXDataset2(px_id = px_id,
                           px_rid = paste0(".rpx2", px_id),
                           px_title = px_metadata$title,
                           px_url = px_url,
                           px_doi = px_metadata$doi,
                           px_ref = px_ref,
                           px_ref_doi = px_ref_doi,
                           px_pubmed = px_pubmed,
                           px_files = px_files,
                           px_tax = px_metadata$organisms$name,
                           px_metadata = px_metadata,
                           cachepath = BiocFileCache::bfccache(cache))
        savepath <- BiocFileCache::bfcnew(cache, rpxId, ext=".rds")
        saveRDS(ans, savepath)
        return(ans)
    }
    ## Retrieve from cache
    message("Loading ", id, " from cache.")
    px <- readRDS(rpath)
    if (!inherits(px, "PXDataset2") | !methods::validObject(px))
        stop("Project ", id, " isn't a valid PXDataset2 object.\n",
             "  Please delete it from cache and regenerate it.",
             "  See ?rpxCached() for details.")
    return(px)
}

##' @export
##'
##' @rdname PXDataset2
PXDataset <- PXDataset2

##' @importFrom methods show
##'
##' @exportMethod show
setMethod("show", "PXDataset2",
          function(object) {
              fls <- object@px_files$NAME
              fls <- paste0("'", fls, "'")
              n <- length(fls)
              cat("Project", object@px_id, "with ")
              cat(n, "files\n ")
              pxCacheInfo(object)
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

##' @param object An instance of class `PXDataset2`.
##'
##' @rdname PXDataset2
##'
##' @exportMethod pxid
setMethod("pxid", "PXDataset2", function(object) object@px_id)

##' @rdname PXDataset2
##'
##' @exportMethod pxurl
setMethod("pxurl", "PXDataset2", function(object) object@px_url)

##' @rdname PXDataset2
##'
##' @exportMethod pxtax
setMethod("pxtax", "PXDataset2", function(object) object@px_tax)

##' @rdname PXDataset2
##'
##' @exportMethod pxref
setMethod("pxref", "PXDataset2",
          function(object)
              paste0(sub(" +$", "", object@px_ref),
                     " doi:", object@px_ref_doi,
                     " PMID:", object@px_pubmed))

##' @rdname PXDataset2
##'
##' @export
pxtitle <-  function(object) object@px_title

##' @rdname PXDataset2
##'
##' @param n `integer(1)` indicating the number of files to be printed.
##'
##' @exportMethod pxfiles
setMethod("pxfiles", "PXDataset2",
          function(object, n = 10) {
              ans <- fls <- object@px_files$NAME
              uris <- object@px_files$URI
              tbl <- bfcinfo(BiocFileCache(object@cachepath))
              n_fls <- length(fls)
              is_local <- match(uris, tbl$fpath)
              is_local <- ifelse(is.na(is_local), "[remote] ", "[local]  ")
              message("Project ", object@px_id, " files (", n_fls, "):")
              for (k in seq_len(min(n, n_fls)))
                  message(" ", is_local[k], fls[k])
              if (n_fls > n)
                  message(" ...")
              invisible(ans)
          })

##' @rdname PXDataset2
##'
##' @exportMethod pxCacheInfo
setMethod("pxCacheInfo", "PXDataset2",
          function(object) {
              rid <- ridFromCache2(object)
              if (is.na(rid)) msg <- "No caching information found."
              else msg <- paste0("Resource ID ", rid, " in cache in ", object@cachepath, ".")
              message(msg)
              invisible(c(rid = rid, cachepath = object@cachepath))
          })

##' @rdname PXDataset2
##'
##' @param list `character()`, `numeric()` or `logical()` defining the
##'     project files to be downloaded. This list of files can
##'     retrieved with `pxfiles()`.
##'
##' @param cache Object of class `BiocFileCache`. Default is to use
##'     the central `rpx` cache returned by `rpxCache()`, but users
##'     can use their own cache. See [rpxCache()] for details.
##'
##' @importFrom utils menu
##'
##' @exportMethod pxget
setMethod("pxget", "PXDataset2",
          function(object, list, cache = rpxCache()) {
              suppressMessages(fls <- pxfiles(object))
              if (missing(list))
                  list <- menu(fls, FALSE, paste0("Files for ", object@px_id))
              if (length(list) == 1 && list == "all") {
                  toget <- fls
              } else {
                  if (is.character(list)) {
                      toget <- fls[fls %in% list]
                  } else toget <- fls[list]
              }
              if (length(toget) < 1)
                  stop("No files to download.")
              k <- match(toget, fls)
              uris <- object@px_files$URI[k]
              for (i in seq_along(uris)) {
                  toget[i] <- pxget1(uris[i], cache)
              }
              toget
          })
