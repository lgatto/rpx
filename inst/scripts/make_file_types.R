library(tidyverse)

## Useful sources/references:
## - https://www.ebi.ac.uk/pride/markdownpage/pridefileformats
## - https://www.mcponline.org/article/S1535-9476(20)33457-5/fulltext
## - https://analyticalsciencejournals.onlinelibrary.wiley.com/doi/10.1002/rcm.1603

## The following people have helped in compiling this list:
## Dr. Samuel Wein
## - https://fediscience.org/@samweingamgee/110004273656629947
## - https://fediscience.org/@samweingamgee/110004344762810013
## - https://fediscience.org/@samweingamgee/110004359133956180
## Marc Vaudel
## - https://sciencemastodon.com/@mvaudel/110006600573181489
## Michael MacCoss
## - https://genomic.social/@maccoss/110005441931739135

exts <- list(
    ## put archive and doc first so that more specific filetypes (such
    ## as "xml.gz" or "dat-mztab.txt") or get re-annotated later.
    archive = c("zip", "tar.gz", "rar", "RAR",
                "7z", "tgz",
                "ZIP", "tar", "tgz", "RAR", "bz2",
                "webarchive", "gz", "xy"),
    doc = c("doc", "pdf", "PDF", "ppt", "odt", "docx", "pptx", "rtf",
            "html","html", "htm", "shtml", "readme", "txt", "md", "css"),
    rawbin = c("raw", "Raw", "RAW", "d", "d.zip", "raw.zip", "raw.gz",
               "wiff", "wiff2", "wiff.scan", "wiff.1.~idx2", "wiff.mtd",
               "t2d"),
    raw = c("mzML", "mzML.gz", "mzML.zip",
            "mzXML", "mzXML.gz", "mzXML.zip", "mzxml", "MZXML",
            "TraML", "traML", "traml",
            "netCDF", "CDF",
            "mzData", "mzdata",
            "mz5", "imzML"),
    pkl = c("mgf", "MGF", "mgf.gz", "MGF.gz",
            "pkl", "pkl.gz", "PKL"),
    maxquant = c("res", "apl"), ## MaxQuant peaklist file
    fas = c("fas", "fasta", "fa", "faa", "FASTA", "fasts",
            "FALSE.gz", "FALSTA.zip",
            "fasta.gz", "fasta.zip",
            "fa.gz", "fa.zip",
            "faa.gz", "faa.zip"),
    reflib = c("blib", "elib", "dlib", "msp"),
    id = c("mzIdentML", "mzidentml", "mzidentML",
           "mzID", "mzID.gz",
           "mzid", "mzid.gz", "mzid.zip",
           "dat", "dat.gz", "dat.zip", "idXML", "omx",
           "IdXML", "idxml", "pepnovo",
           "pcml", ## Proteoform markup language file
           "dta", "dta.tgz", "dta.tar.bz2"),
    tbl = c("csv", "tsv", "xls", "xlsx", "XLSX", "xlsb", "ssv",
            "csv.gz", "tsv.gz", "psmtsv", "delim", "tabular"),
    mztab = c("mztab", "mztab.gz", "mzTab", "mzTab.gz",
              "mzTabNA", "-mztab.txt", "_mztab.txt",
              "mztab.txt"),
    fig = c("png", "jpg", "jpeg", "tiff", "TIF", "tif",
            "gif", "PNG", "JPG", "svg"),
    xml = c("xml", "xml.gz"),
    prophet = c("pepXML", "protXML", "pepxml", "protxml"),
    bruker = c("yep", "baf"),
    scaffold = c("sf3", "sptm", "sfdb", "sdia", "metdb"),
    pd = c("pdResult", "msf", "pdResultView", "msfView",
           "pdAnalysis", "pdProcessingWF", "pdConsensusWF",
           "pdStudy", "pdStudy.bak"), ## ProteomeDiscoverer
    sequest = c("ms1", "ms2", "srf", "sqt",
                "out", "out.tgz", "out.tar.bz2"),
    proteinpilot = "group",
    progenesis = c("ProgenesisQIPExperiment", "ProgenesisQIPArchive",
                   "ProgenesisLcmsExperiment",
                   "ProgenesisQIPMultiFractionExperiment"),
    skyline = c("sky", "sky.view", "view", "skyd", "skyl"),
    spectronaut = c("sne", "htrms"),
    peptideshaker = "cpsx",
    params = c("PARAMS", "params", "param", "par",
               "config", "apar", "knwf", ##  KNIME workflow
               "json", "toml", "yaml",
               "ini", "mtd", "index",
               "method", "Method", "FAmethod", "properties"),
    code = c("R", "py", "r", "pl", "js", "jar", "Rmd", "sh", "ipynb"),
    exe = c("exe", "bin", "dll"),
    data = c("RData", "RDS", "sqlite", "h5"),
    chk = c("md5", "cksum", "chksum"),
    tmp = c("bak", "download", "crdownload", "sgdownload", "temp", "tmp"),
    gen = c("gtf", "gff", "fastq", "vcf", "plink"))

file_types <- data.frame(type = rep(names(lengths(exts)), lengths(exts)),
                         ext = unname(unlist(exts))) |>
    mutate(pattern = gsub("\\.", "\\\\.", paste0(".", ext, "$")))

## Update some patterns
file_types[file_types$ext == "-mztab.txt", "pattern"] <- "-mztab\\.txt$"
file_types[file_types$ext == "_mztab.txt", "pattern"] <- "_mztab\\.txt$"

file_types |>
    filter(ext %in% c("-mztab.txt", "_mztab.txt"))

saveRDS(file_types, file = "../extdata/file_types.rds")
