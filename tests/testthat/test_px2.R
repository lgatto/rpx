## Delete entries in default cache. This assures that the instances
## matche any updates in the class definition.
px1 <- PXDataset2("PXD000001")
BiocFileCache::bfcremove(rpxCache(), pxCacheInfo(px1)["rid"])

PXD022816 <- PXDataset2("PXD022816")
BiocFileCache::bfcremove(rpxCache(), pxCacheInfo(PXD022816)["rid"])

## Create new object
px1 <- PXDataset2("PXD000001")

test_that("Object content is valid (v2)", {
    id <- "PXD000001"
    expect_null(show(px1))
    expect_identical(pxid(px1), id)
    ## Remove this as it can be either given the changes in URL
    ## related to the issue with PRIDE and whether or when the object
    ## was chached.
    ## url <- "ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2012/03/PXD000001"
    ## url <- "ftp://ftp.pride.ebi.ac.uk/pride-archive/2012/03/PXD000001"
    ## expect_identical(pxurl(px1), url)
    pxf <- pxfiles(px1)
    fls <- sort(c("erwinia_carotovora.fasta", "F063721.dat",
                  "F063721.dat-mztab.txt",
                  "PRIDE_Exp_Complete_Ac_22134.xml.gz",
                  "PRIDE_Exp_mzData_Ac_22134.xml.gz",
                  "PXD000001_mztab.txt", "README.txt",
                  "TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01-20141210.mzML",
                  "TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01-20141210.mzXML",
                  "TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01.mzXML",
                  "TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01.raw"))

    expect_identical(sort(pxf), fls)
    expect_identical(length(pxf), 11L)
    expect_identical(pxtax(px1), "Erwinia carotovora")
    ## ref <- "Gatto L, Christoforou A; Using R and Bioconductor for proteomics data analysis., Biochim Biophys Acta, 2013 May 18, doi:10.1016/j.bbapap.2013.04.032 PMID:23692960"
    ## Changed on [2025-02-07 Fri]
    ref <- "Gatto L, Christoforou A; Using R and Bioconductor for proteomics data analysis., Biochim Biophys Acta, 2013 May 18, doi:10.1016/j.bbapap.2013.04.032 PMID:NA"
    expect_identical(pxref(px1), ref)
    fa <- pxget(px1, "erwinia_carotovora.fasta")
    expect_equal(length(Biostrings::readAAStringSet(fa)), 4499)
})

test_that("PX announcements (v2)", {
    pa <- pxannounced()
    ## expect_is(pa, "data.frame")
    ## expect_identical(names(pa),
    ##                  c("Data.Set", "Publication.Data", "Message"))
})

## test_that("PX identifiers (v2)", {
##     expect_error(PXDataset2("P1"))
##     expect_error(PXDataset2("1"))
##     expect_error(PXDataset2("PXD1"))
## })

test_that("PXD022816: valid URLs and files (v2)", {
    nfiles <- 32L
    PXD022816 <- PXDataset2("PXD022816")
    expect_identical(length(pxurl(PXD022816)), 1L)
    expect_identical(length(pxfiles(PXD022816)), nfiles)
})

test_that("Object content is valid.", {
    ## Add a resource with rname .rpxPXD000001 to a tmp cache
    tmp_cache <- BiocFileCache::BiocFileCache(tempfile(), ask = FALSE)
    path <- BiocFileCache::bfcnew(tmp_cache, rname = ".rpx2PXD000001")
    saveRDS(1, path)
    ## Try to load if from cache
    expect_error(p <- PXDataset2("PXD000001", cache = tmp_cache))
})
