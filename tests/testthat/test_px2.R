px1 <- PXDataset2("PXD000001")

test_that("Object content is valid (v2)", {
    id <- "PXD000001"
    expect_null(show(px1))
    expect_identical(px_id(px1), id)
    ## Assertions manually looked up at
    url <- "ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2012/03/PXD000001"
    pxf <- px_files(px1)
    fls <- sort(c("F063721.dat",
                  "F063721.dat-mztab.txt",
                  "PRIDE_Exp_Complete_Ac_22134.pride.mgf.gz",
                  "PRIDE_Exp_Complete_Ac_22134.pride.mztab.gz",
                  "PRIDE_Exp_Complete_Ac_22134.xml.gz",
                  "TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01.mzXML",
                  "TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01.raw",
                  "TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01-20141210.mzML",
                  "TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01-20141210.mzXML",
                  "erwinia_carotovora.fasta"))
    expect_identical(sort(pxf), fls)

    expect_identical(length(pxf), 10L)
    expect_identical(px_tax(px1), "Erwinia carotovora")
    expect_identical(px_url(px1), url)
    ref <- "Gatto L, Christoforou A; Using R and Bioconductor for proteomics data analysis., Biochim Biophys Acta, 2013 May 18, "
    expect_identical(px_ref(px1), ref)
    fa <- px_get(px1, "erwinia_carotovora.fasta")
    expect_equal(length(Biostrings::readAAStringSet(fa)), 4499)
})

test_that("PX announcements (v2)", {
    pa <- pxannounced()
    ## expect_is(pa, "data.frame")
    ## expect_identical(names(pa),
    ##                  c("Data.Set", "Publication.Data", "Message"))
})

test_that("PX identifiers (v2)", {
    expect_error(PXDataset2("P1"))
    expect_error(PXDataset2("1"))
    expect_error(PXDataset2("PXD1"))
})

test_that("PXD022816: valid URLs and files (v2)", {
    nfiles <- 31L
    PXD022816 <- PXDataset2("PXD022816")
    expect_identical(length(px_url(PXD022816)), 1L)
    expect_identical(length(px_files(PXD022816)), nfiles)
})

test_that("Object content is valid.", {
    ## Add a resource with rname .rpxPXD000001 to a tmp cache
    tmp_cache <- BiocFileCache::BiocFileCache(tempfile(), ask = FALSE)
    path <- BiocFileCache::bfcnew(tmp_cache, rname = ".rpx2PXD000001")
    saveRDS(1, path)
    ## Try to load if from cache
    expect_error(p <- PXDataset2("PXD000001", cache = tmp_cache))
})
