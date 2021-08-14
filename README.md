## R Interface to the ProteomeXchange Repository

Thre `rpx` package is a simple programmatic interface to proteomics
data available in the ProteomeXchange repository. It uses a
ProteomeXchange identifier such as `PXD000001` to find and download
files associated with that project. The official Bioconductor is
[here](http://bioconductor.org/packages/release/bioc/html/rpx.html). The
GitHub page allows to browse the documentation and the vignette.

### Installation

To install the version matching your local R installation from the
Bioconductor repository:

```
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("rpx")
```

And to install the very latest version from GitHub:

```
BiocManager::install("lgatto/rpx")
```


For a similar python-based software, see
[`ppx`](https://github.com/wfondrie/ppx/) by Will Fondrie.
