test_px1 <- function() {
    px1 <- PXDataset("PXD000001")
    ## Assertions manually looked up at
    url <- "ftp://ftp.pride.ebi.ac.uk/2012/03/PXD000001"
    checkEquals(length(pxfiles(px1)), 8)
    checkEquals(sort(pxfiles(px1)),
                sort(c("F063721.dat",
                       "F063721.dat-mztab.txt",
                       "PRIDE_Exp_Complete_Ac_22134.xml.gz",
                       "PRIDE_Exp_mzData_Ac_22134.xml.gz",
                       "PXD000001_mztab.txt",
                       "TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01.mzXML",
                       "TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01.raw",
                       "erwinia_carotovora.fasta")))
    checkEquals(pxtax(px1), "Erwinia carotovora")
    checkEquals(pxurl(px1), url)    
}

test_pxv <- function() {
    p <- PXDataset("PXD000001") ## v 1.0.0
    checkEquals(p@formatVersion, "1.0.0")
    q <- PXDataset("PXD000507")
    checkEquals(q@formatVersion, "1.2.0")
    ## can't find a version 1.1.0
    xx <- as.character(pxannounced()[, "Data.Set"])
    ## take 1/2 to reduce time
    i <- seq(1, length(xx), 2)
    vv <- sapply(xx[i], function(x) PXDataset(x)@formatVersion)
    ## This will fail as soon as a new schema version is out
    checkTrue(all(unique(vv) %in% c("1.0.0", "1.1.0", "1.2.0")))
}
