px1 <- PXDataset1("PXD000001")

test_that("Object content is valid.", {
    id <- "PXD000001"
    expect_null(show(px1))
    expect_identical(pxid(px1), id)
    ## Assertions manually looked up at
    url <- "ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2012/03/PXD000001"
    pxf <- pxfiles(px1)
    fls <- sort(c("README.txt",
                  "F063721.dat",
                  "F063721.dat-mztab.txt",
                  "PRIDE_Exp_Complete_Ac_22134.xml.gz",
                  "PRIDE_Exp_mzData_Ac_22134.xml.gz",
                  "PXD000001_mztab.txt",
                  "TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01.mzXML",
                  "TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01.raw",
                  "TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01-20141210.mzML",
                  "TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01-20141210.mzXML",
                  "erwinia_carotovora.fasta"))
    expect_identical(length(pxf), 11L)
    expect_identical(sort(pxf), fls)
    expect_identical(pxtax(px1), "Erwinia carotovora")
    expect_identical(pxurl(px1), url)
    ref <- "Gatto L, Christoforou A. Using R and Bioconductor for proteomics data analysis. Biochim Biophys Acta. 2013 May 18. doi:pii: S1570-9639(13)00186-6. 10.1016/j.bbapap.2013.04.032"
    expect_identical(pxref(px1), ref)
    fa <- pxget(px1, "erwinia_carotovora.fasta")
    expect_equal(length(Biostrings::readAAStringSet(fa)), 4499)
})

test_that("PXD version", {
    p <- px1
    ver_1 <- "1.3.0"
    if (grepl("archive", pxurl(p)))
        ver_1 <- "1.0.0"
    expect_identical(p@formatVersion, ver_1)
    p <- PXDataset1("PXD000561")
    expect_identical(p@formatVersion, "1.2.0")
    p <- PXDataset1("PXD004938")
    expect_identical(p@formatVersion, "1.3.0")
})

test_that("PX announcements", {
    pa <- pxannounced()
    ## expect_is(pa, "data.frame")
    ## expect_identical(names(pa),
    ##                  c("Data.Set", "Publication.Data", "Message"))
})

test_that("PX identifiers", {
    expect_error(PXDataset1("P1"))
    px01 <- PXDataset1("1")
    expect_equal(px01, px1)
    px02 <- PXDataset1("PXD1")
    expect_equal(px01, px02)
})

test_that("PXD022816: valid URLs and files", {
    nfiles <- 32L
    PXD022816 <- PXDataset1("PXD022816")
    rpx:::apply_fix_issue_5(TRUE)
    expect_identical(length(pxurl(PXD022816)), 1L)
    expect_identical(length(pxfiles(PXD022816)), nfiles)
    rpx:::apply_fix_issue_5(FALSE)
    expect_identical(length(pxurl(PXD022816)), 1L)
    expect_identical(length(pxfiles(PXD022816)), nfiles)
})

test_that("Object content is valid.", {
    ## Add a resource with rname .rpxPXD000001 to a tmp cache
    tmp_cache <- BiocFileCache::BiocFileCache(tempfile(), ask = FALSE)
    path <- BiocFileCache::bfcnew(tmp_cache, rname = ".rpxPXD000001")
    saveRDS(1, path)
    ## Try to load if from cache
    expect_error(p <- PXDataset1("PXD000001", cache = tmp_cache))
})
