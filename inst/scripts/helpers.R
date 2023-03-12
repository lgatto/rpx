## ==============================================
##    Updates  all local PXDataset objets
## ==============================================

library(rpx)

pxprojs <- rpx:::localPxRepo()

table(pxprojs$status)

for (id in pxprojs$id[pxprojs$status == "local"]) {
    px <- PXDataset(id)
    if (!"PX" %in% names(pxfiles(px, as.vector = FALSE))) {
        message("... update file types")
        updatePxFileTypes(px)
    }
}
