test_that("Test file types.", {
    x <- tibble::tribble(
        ~file,                ~expected_type,
        "foo",                NA_character_,
        "foo.gz",             "archive",
        "foo.mzML",           "raw",
        "foo.mzML.gz",        "raw",
        "foo.txt",            "doc",
        "foo.mztab",          "mztab",
        "foo.mztab.txt",      "mztab",
        "foo.data-mztab.txt", "mztab",
        "foo.pkl",            "pkl",
        "foo.pkl.gz",         "pkl",
        "foo.mgf.gz",         "pkl",
        "foo.mzid",           "id",
        "foo.mzid.gz",        "id",
        "foo.mzid.zip",       "id",
        "foo.fasta",          "fas",
        "foo.fasta.gz",       "fas"
    )
    ans <- px_file_types(x$file)
    expect_identical(x$expected_type, ans$type)
})
