\name{PXDataset-class}
\Rdversion{1.1}
\docType{class}
\alias{PXDataset-class}
\alias{class:PXDataset}
\alias{PXDataset}
\alias{pxfiles,PXDataset-method}
\alias{pxfiles}
\alias{pxget,PXDataset-method}
\alias{pxget}
\alias{pxid,PXDataset-method}
\alias{pxid}
\alias{pxref,PXDataset-method}
\alias{pxref}
\alias{pxtax,PXDataset-method}
\alias{pxtax}
\alias{pxurl,PXDataset-method}
\alias{pxurl}
\alias{show,PXDataset-method}

\title{Class \code{"PXDataset"}}

\description{
  An S4 class to store and access information about ProteomeXchange (PX)
  data sets. Schema versions 1.0, 1.1 and 1.2 are supported and are
  documented on the PX code repository:
  https://code.google.com/p/proteomexchange/source/browse/schema/
}

\section{Objects from the Class}{
  Objects can be created with the constructor \code{PXDataset}.
}

\section{Slots}{
  \describe{
    \item{\code{id}:}{Object of class \code{"character"} storing the
      datasets unique identifier. Can be accessed with \code{pxid()}.}
    \item{\code{formatVersion}:}{Object of class \code{"character"}
      storing the version of the ProteomeXchange schema.  }
    \item{\code{Data}:}{Object of class \code{"XMLNode"} storing the
      ProteomeXchange description as XML node tree. }
  }
}


\section{Details}{

  From version 1.99.1 on, rpx uses the Bioconductor BiocFileCache
  package to automatically cache all downloaded ProteomeXchange
  files. When a file is downloaded for the first time, it is added to
  the cache. When already available, the file path to the cached file is
  returned. The rpx chache is returned by \code{rpx:::.get_cache()}. The
  user is asked to confirm its creation when data is first downloaded. 

  For more details on how to manage the cache (for example if some files
  need to be deleted), please refer to the BiocFileCache vignette.
  
}

\section{Methods}{
  \describe{

    \item{pxfiles}{\code{signature(object = "PXDataset")}: return a
      \code{character} of all available files. }

    \item{pxget}{\code{signature(object = "PXDataset", list, ...)}:
      downloads the files from the ProteomeXchange repository. If
      \code{list} is missing, the file to be downloaded can be selected
      from a menu. If \code{list = "all"}, all files are downloaded. The
      file names, as returned by \code{pxfiles} can also be
      used. Alternatively, a \code{logical} or \code{numeric} indices
      can be used.

      If not already cached, the files are downloaded and added to the
      package cache. The function then returns the names of the files in
      the cache directory.
    }

    \item{pxid}{\code{signature(object = "PXDataset")}: returns the
      unique ProteomeXchange identifier. }

    \item{pxref}{\code{signature(object = "PXDataset")}: returns the
      reference associcated with the \code{object}. }

    \item{pxtax}{\code{signature(object = "PXDataset")}: returns the
      scientic taxonomic name of \code{object}. }

    \item{pxurl}{\code{signature(object = "PXDataset"}: returns the base
      url on the ProteomeXchange server where \code{pxfiles(object)}
      reside. }

    \item{show}{\code{signature(object = "PXDataset")}: textual
      representation of \code{object}. }
  }

}

\references{
  Vizcaino J.A. et al. 'ProteomeXchange: globally co-ordinated
  proteomics data submission and dissemination', Nature Biotechnology
  2014, 32, 223 -- 226, doi:10.1038/nbt.2839.

  Source repository for the ProteomeXchange project:
  https://code.google.com/p/proteomexchange/
}

\author{
  Laurent Gatto
}


\examples{
px <- PXDataset("PXD000001")
px
pxtax(px)
pxurl(px)
pxref(px)
pxfiles(px)
fnm <- pxget(px, "PXD000001_mztab.txt")
library("MSnbase")
readMzTabData(fnm, "PEP")
unlink("PXD000001_mztab.txt")
}

\keyword{classes}
