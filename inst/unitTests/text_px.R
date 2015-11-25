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
