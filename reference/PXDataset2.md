# New PXDataset (v2) to find and download proteomics data

The `rpx` package provides the infrastructure to access, store and
retrieve information for ProteomeXchange (PX) data sets. This can be
achieved with `PXDataset2` objects can be created with the
`PXDataset2()` constructor that takes the unique ProteomeXchange project
identifier as input.

The new `PXDataset2` class superseeds the previous and now deprecated
`PXDataset` version.

## Usage

``` r
PXDataset2(id, cache = rpxCache())

PXDataset(id, cache = rpxCache())

# S4 method for class 'PXDataset2'
pxid(object)

# S4 method for class 'PXDataset2'
pxurl(object)

# S4 method for class 'PXDataset2'
pxtax(object)

# S4 method for class 'PXDataset2'
pxref(object)

pxtitle(object)

pxinstruments(object)

pxSubmissionDate(object)

pxPublicationDate(object)

pxptms(object)

pxprotocols(object, which = c("project", "samples", "data"))

# S4 method for class 'PXDataset2'
pxfiles(object, n = 10, as.vector = TRUE)

# S4 method for class 'PXDataset2'
pxCacheInfo(object)

# S4 method for class 'PXDataset2'
pxget(object, list, cache = rpxCache())
```

## Arguments

- id:

  `character(1)` containing a valid ProteomeXchange identifier.

- cache:

  Object of class `BiocFileCache`. Default is to use the central `rpx`
  cache returned by
  [`rpxCache()`](https://lgatto.github.io/rpx/reference/cache.md), but
  users can use their own cache. See
  [`rpxCache()`](https://lgatto.github.io/rpx/reference/cache.md) for
  details.

- object:

  An instance of class `PXDataset2`.

- which:

  [`character()`](https://rdrr.io/r/base/character.html) with one or
  multiple protocols defined as `"project"`, `"samples"` and `"data"`.

- n:

  `integer(1)` indicating the number of files to be printed.

- as.vector:

  `logical(1)` defining if the output should be a vector of character
  with filenames (default) or a data.frame with additional details about
  each file.

- list:

  [`character()`](https://rdrr.io/r/base/character.html),
  [`numeric()`](https://rdrr.io/r/base/numeric.html) or
  [`logical()`](https://rdrr.io/r/base/logical.html) defining the
  project files to be downloaded. This list of files can retrieved with
  `pxfiles()`.

## Value

The `PXDataset2()` returns a cached `PXDataset2` object. It thus also
modifies the cache used to projet caching, as defined by the `cache`
argument.

## Details

The `rpx` packages uses caching to store ProteomeXchange projects and
project files. When creating an object with `PXDataset2()`, the cache is
first queried for the projects identifier. If a unique hit is found, the
project is retrieved and returned. If no matching project identifier is
found, then the remote resource is accessed to first create the new
`PXDataset2()` project, then cache it before returning it to the user.
The same mechanism is applied when project files are requested.

Caching is supported by BiocFileCache package. The `PXDataset2()`
constructor and the `px_get()` function can be passed a instance of
class `BiocFileCache` that defines the cache. The default is to use the
package-wide cache defined in
[`rpxCache()`](https://lgatto.github.io/rpx/reference/cache.md). For
more details on how to manage the cache (for example if some files need
to be deleted), please refer to the `BiocFileCache` package vignette and
documentation. See also
[`rpxCache()`](https://lgatto.github.io/rpx/reference/cache.md) for
additional details.

## Slots

- `px_id`:

  `character(1)` containing the dataset's unique ProteomeXchange
  identifier, as used to create the object.

- `px_rid`:

  `character(1)` storing the cached resource name in the BiocFileCache
  instance stored in `cachepath`.

- `px_title`:

  `character(1)` with the project's title.

- `px_url`:

  \`character(1) with the project's URL.

- `px_doi`:

  `character(1)` with the project's DOI.

- `px_ref`:

  `character` containing the project's reference(s).

- `px_ref_doi`:

  `character` containing the project's reference DOIs.

- `px_pubmed`:

  `character` containing the project's reference PubMed identifier.

- `px_files`:

  `data.frame` containing information about the project files, including
  file names, URIs and types. The files are retrieved from the project's
  README.txt file.

- `px_tax`:

  `charcter` (typically of length 1) containing the taxonomy of the
  sample.

- `px_metadata`:

  `list` containing the project's metadata, as downloaded from the
  ProteomeXchange site. All slots but `px_files` are populated from this
  one.

- `cachepath`:

  `character(1)` storing the path to the cache the project object is
  stored in.

## Accessors

- `pxfiles(object, n = 10, as.vector = TRUE)` by default, invisibly
  returns all the project file names. The function prints the first `n`
  files specifying whether they are local of remote (based on the cache
  the object is stored in). The printing can be ignored by wrapping the
  call in [`suppressMessages()`](https://rdrr.io/r/base/message.html).
  If `as.vector` is set to `FALSE`, it returns a `data.frame` with
  variables ID, NAME, URI, TYPE, MAPPINGS and PXID. Note that the
  variables and their content will depend on the `rpx` version that was
  installed when these objects were created and cached.

- `pxget(object, list, cache)`: `list` is a vector defining the files to
  be downloaded. If `list = "all"`, all files are downloaded. The file
  names, as returned by `pxfiles()` can also be used. Alternatively, a
  `logical` or `numeric` index can be used. If missing, the file to be
  downloaded can be selected from a menu.

  The argument `cache` can be passed to define the path to the cache.
  The default cache is the packages' default as returned by
  [`rpxCache()`](https://lgatto.github.io/rpx/reference/cache.md).

- `pxtax(object)`: returns the taxonomic name of `object`.

- `pxurl(object)`: returns the base url on the ProteomeXchange server
  where the project files reside.

- `pxCacheInfo(object, cache): prints and invisibly returns `object`'s caching information from `cache`(default is`rpxCache()\`).
  The return value is a named vector of length two containing the
  resourne identifier and the cache location.

- \`pxtitle(object): returns the project's title.

- `pxref(object)`: returns the project's bibliographic reference(s).

- `pxinstruments(object)`: returns the instrument(s) used to acquire the
  data.

- `pxptms(object)`: returns the PTMs searched for in the experiment.

- `pxprotocols(object, which)`: returns a list with the project
  description, sample processing and/or data processing protocols.

## References

Vizcaino J.A. et al. 'ProteomeXchange: globally co-ordinated proteomics
data submission and dissemination', Nature Biotechnology 2014, 32, 223 –
226, doi:10.1038/nbt.2839.

Source repository for the ProteomeXchange project:
https://code.google.com/p/proteomexchange/

## Author

Laurent Gatto

## Examples

``` r

px <- PXDataset("PXD000001")
#> Loading PXD000001 from cache.
px
#> Project PXD000001 with 11 files
#>  
#> Resource ID BFC7 in cache in /github/home/.cache/R/rpx.
#>  [1] 'F063721.dat' ... [11] 'erwinia_carotovora.fasta'
#>  Use 'pxfiles(.)' to see all files.
pxtax(px)
#> [1] "Erwinia carotovora"
pxurl(px)
#> [1] "ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2012/03/PXD000001"
pxref(px)
#> [1] "Gatto L, Christoforou A; Using R and Bioconductor for proteomics data analysis., Biochim Biophys Acta, 2013 May 18, doi:10.1016/j.bbapap.2013.04.032 PMID:NA"
pxfiles(px)
#> Project PXD000001 files (11):
#>  [remote] F063721.dat
#>  [local]  F063721.dat-mztab.txt
#>  [remote] PRIDE_Exp_Complete_Ac_22134.xml.gz
#>  [remote] PRIDE_Exp_mzData_Ac_22134.xml.gz
#>  [remote] PXD000001_mztab.txt
#>  [remote] README.txt
#>  [remote] TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01-20141210.mzML
#>  [remote] TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01-20141210.mzXML
#>  [remote] TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01.mzXML
#>  [remote] TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01.raw
#>  ...
pxfiles(px, as.vector = FALSE)
#>    ID                                                                 NAME
#> 1   1                                                          F063721.dat
#> 2   2                                                F063721.dat-mztab.txt
#> 3   3                                   PRIDE_Exp_Complete_Ac_22134.xml.gz
#> 4   4                                     PRIDE_Exp_mzData_Ac_22134.xml.gz
#> 5   5                                                  PXD000001_mztab.txt
#> 6   6                                                           README.txt
#> 7   7  TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01-20141210.mzML
#> 8   8 TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01-20141210.mzXML
#> 9   9          TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01.mzXML
#> 10 10            TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01.raw
#> 11 11                                             erwinia_carotovora.fasta
#>                                                                                                                                    URI
#> 1                                                           ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2012/03/PXD000001/F063721.dat
#> 2                                                 ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2012/03/PXD000001/F063721.dat-mztab.txt
#> 3                                    ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2012/03/PXD000001/PRIDE_Exp_Complete_Ac_22134.xml.gz
#> 4                                      ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2012/03/PXD000001/PRIDE_Exp_mzData_Ac_22134.xml.gz
#> 5                                                   ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2012/03/PXD000001/PXD000001_mztab.txt
#> 6                                                            ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2012/03/PXD000001/README.txt
#> 7   ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2012/03/PXD000001/TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01-20141210.mzML
#> 8  ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2012/03/PXD000001/TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01-20141210.mzXML
#> 9           ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2012/03/PXD000001/TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01.mzXML
#> 10            ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2012/03/PXD000001/TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01.raw
#> 11                                             ftp://ftp.pride.ebi.ac.uk/pride/data/archive/2012/03/PXD000001/erwinia_carotovora.fasta
#>      TYPE MAPPINGS        PX
#> 1      id        - PXD000001
#> 2   mztab        - PXD000001
#> 3     xml        - PXD000001
#> 4     xml        - PXD000001
#> 5   mztab        - PXD000001
#> 6     doc        - PXD000001
#> 7     raw        - PXD000001
#> 8     raw        - PXD000001
#> 9     raw        - PXD000001
#> 10 rawbin        - PXD000001
#> 11    fas        - PXD000001

pxCacheInfo(px)
#> Resource ID BFC7 in cache in /github/home/.cache/R/rpx.

fas <- pxget(px, "erwinia_carotovora.fasta")
#> Loading erwinia_carotovora.fasta from cache.
fas
#> [1] "/github/home/.cache/R/rpx/97474dfd637_erwinia_carotovora.fasta"
library("Biostrings")
#> Loading required package: BiocGenerics
#> Loading required package: generics
#> 
#> Attaching package: ‘generics’
#> The following objects are masked from ‘package:base’:
#> 
#>     as.difftime, as.factor, as.ordered, intersect, is.element, setdiff,
#>     setequal, union
#> 
#> Attaching package: ‘BiocGenerics’
#> The following objects are masked from ‘package:stats’:
#> 
#>     IQR, mad, sd, var, xtabs
#> The following object is masked from ‘package:utils’:
#> 
#>     data
#> The following objects are masked from ‘package:base’:
#> 
#>     Filter, Find, Map, Position, Reduce, anyDuplicated, aperm, append,
#>     as.data.frame, basename, cbind, colnames, dirname, do.call,
#>     duplicated, eval, evalq, get, grep, grepl, is.unsorted, lapply,
#>     mapply, match, mget, order, paste, pmax, pmax.int, pmin, pmin.int,
#>     rank, rbind, rownames, sapply, saveRDS, scale, sequence, table,
#>     tapply, transform, unique, unsplit, which.max, which.min
#> Loading required package: S4Vectors
#> Loading required package: stats4
#> 
#> Attaching package: ‘S4Vectors’
#> The following object is masked from ‘package:utils’:
#> 
#>     findMatches
#> The following objects are masked from ‘package:base’:
#> 
#>     I, expand.grid, unname
#> Loading required package: IRanges
#> Loading required package: XVector
#> Loading required package: Seqinfo
#> 
#> Attaching package: ‘Biostrings’
#> The following object is masked from ‘package:base’:
#> 
#>     strsplit
readAAStringSet(fas)
#> AAStringSet object of length 4499:
#>        width sequence                                       names               
#>    [1]   147 MADITLISGSTLGSAEYVAEHL...QHQIPEDPAEEWLGSWVNLLK ECA0001 putative ...
#>    [2]   153 VAEIYQIDNLDRGILSALMENA...EIQSTETLISLQNPIMRTIAP ECA0002 AsnC-fami...
#>    [3]   330 MKKQYIEKQQQISFVKSFFSSQ...IGQVQCGVWPQPLRESVSGLL ECA0003 putative ...
#>    [4]   492 MITLESLEMLLSIDENELLDDL...WRFDTGLKSRLMRRWQHGKAY ECA0004 conserved...
#>    [5]   499 MRQTAALAERISRLSHALEHGL...AKIEASLQQVAEQIQQSEQQD ECA0005 conserved...
#>    ...   ... ...
#> [4495]   634 MSDKIIHLTDDSFDTDVLKADG...RRKVDPLRVFASDMARRLELL trx-rv3790 trx-rv...
#> [4496]    93 MTKMNNKARRTARELKHLGASI...RELRDEFPMGYLGDYKDDDDK TimBlower TimBlower
#> [4497]   309 MFSNLSKRWAQRTLSKSFYSTA...KFKWAGIKTRKFVFNPPKPRK sp|P07143|CY1_YEA...
#> [4498]   231 FPTDDDDKIVGGYTCAANSIPY...PGVYTKVCNYVNWIQQTIAAN sp|P00761|TRYP_PI...
#> [4499]   269 GVSGSCNIDVVCPEGNGHRDVI...DAAGTGAQFIDGLDSTGTPPV sp|Q7M135|LYSC_LY...
```
