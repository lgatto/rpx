## R Interface to the ProteomeXchange Repository

Thre `rpx` package is a simple programmatic interface to proteomics
data available in the ProteomeXchange repository. It uses a
ProteomeXchange identifier such as `PXD000001` to find and download
files associated with that project. The official Bioconductor is
[here](http://bioconductor.org/packages/release/bioc/html/rpx.html). The
GitHub page allows to browse the documentation and the vignette.

### Installation

```
## try http:// if https:// URLs are not supported
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("rpx")
```

For a similar python-based software, see
[`ppx`](https://github.com/wfondrie/ppx/) by Will Fondrie.
