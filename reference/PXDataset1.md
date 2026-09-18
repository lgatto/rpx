# The PXDataset to find and download proteomics data

The `rpx` package provides the infrastructure to access, store and
retrieve information for ProteomeXchange (PX) data sets. This can be
achieved with `PXDataset` objects can be created with the
[`PXDataset()`](https://lgatto.github.io/rpx/reference/PXDataset2.md)
constructor that takes the unique ProteomeXchange project identifier as
input.

The `PXDataset` class is replaced by `PXDataset2` and is now deprecated.
It will be defunct in the next release.

## Usage

``` r
# S4 method for class 'PXDataset'
pxid(object)

# S4 method for class 'PXDataset'
pxurl(object)

# S4 method for class 'PXDataset'
pxtax(object)

# S4 method for class 'PXDataset'
pxref(object)

# S4 method for class 'PXDataset'
pxfiles(object)

# S4 method for class 'PXDataset'
pxget(object, list, cache = rpxCache())

# S4 method for class 'PXDataset'
pxCacheInfo(object, cache = rpxCache())

PXDataset1(id, cache = rpxCache())
```

## Arguments

- object:

  An instance of class `PXDataset`, as created by
  [`PXDataset()`](https://lgatto.github.io/rpx/reference/PXDataset2.md).

- list:

  [`character()`](https://rdrr.io/r/base/character.html),
  [`numeric()`](https://rdrr.io/r/base/numeric.html) or
  [`logical()`](https://rdrr.io/r/base/logical.html) defining the
  project files to be downloaded. This list of files can retrieved with
  [`pxfiles()`](https://lgatto.github.io/rpx/reference/PXDataset2.md).

- cache:

  Object of class `BiocFileCache`. Default is to use the central `rpx`
  cache returned by
  [`rpxCache()`](https://lgatto.github.io/rpx/reference/cache.md), but
  users can use their own cache. See
  [`rpxCache()`](https://lgatto.github.io/rpx/reference/cache.md) for
  details.

- id:

  `character(1)` containing a valid ProteomeXchange identifier.

## Value

The
[`PXDataset()`](https://lgatto.github.io/rpx/reference/PXDataset2.md)
constructor returns a cached `PXDataset` object. It thus also modifies
the cache used to projet caching, as defined by the `cache` argument.

## Details

Since version 1.99.1, `rpx` uses the Bioconductor `BiocFileCache`
package to automatically cache all downloaded ProteomeXchange files.
When a file is downloaded for the first time, it is added to the cache.
When already available, the file path to the cached file is directly
returned. The central `rpx` package chache, object of class
`BiocFileCache`, is returned by
[`rpxCache()`](https://lgatto.github.io/rpx/reference/cache.md). Users
can also provide their own cache object instead of using the default
central cache to
[`pxget()`](https://lgatto.github.io/rpx/reference/PXDataset2.md).

Since 2.1.1, `PXDataset` instances are also cached using the same
mechanism as project files. Each `PXDataset` instance also stored the
project file names, the reference, taxonomy of the sample and the
project URL (see slot `cache`) instead of accessing these every time
they are needed to reduce remote access and reliance on a stable
internet connection. As for files, the default cache is as returned by
[`rpxCache()`](https://lgatto.github.io/rpx/reference/cache.md), but
users can pass their own `BiocFileCache` objects.

For more details on how to manage the cache (for example if some files
need to be deleted), please refer to the `BiocFileCache` package
vignette and documentation. See also
[`rpxCache()`](https://lgatto.github.io/rpx/reference/cache.md) for
additional details.

## Slots

- `id`:

  `character(1)` containing the dataset's unique ProteomeXchange
  identifier, as used to create the object.

- `formatVersion`:

  `character(1)` storing the version of the ProteomeXchange schema.
  Schema versions 1.0, 1.1 and 1.2 are supported (see
  <https://code.google.com/p/proteomexchange/source/browse/schema/>).

- `cache`:

  [`list()`](https://rdrr.io/r/base/list.html) storing the available
  files (element `pxfiles`), the reference associated with the data set
  (`pxref`), the taxonomy of the sample (`pxtax`) and the datasets'
  ProteomeXchange URL (`pxurl`). These are returned by the respective
  accessors. It also stores the path to the cache it is stored in
  (element `cachepath`).

- `Data`:

  `XMLNode` storing the ProteomeXchange description as XML node tree.

## Accessors

- `pxfiles(object)` returns the project file names.

- `pxget(object, list, cache)`: if the file(s) in `list` have never been
  requested,
  [`pxget()`](https://lgatto.github.io/rpx/reference/PXDataset2.md)
  downloads the files from the ProteomeXchange repository, caches them
  in `cache` and returns their path. If the files have previously been
  downloaded and are available in `cache`, their path is directly
  returned.

  If `list` is missing, the file to be downloaded can be selected from a
  menu. If `list = "all"`, all files are downloaded. The file names, as
  returned by
  [`pxfiles()`](https://lgatto.github.io/rpx/reference/PXDataset2.md)
  can also be used. Alternatively, a `logical` or `numeric` index can be
  used.

  The argument `cache` can be passed to define the path to the cache.
  The default cache is the packages' default as returned by
  [`rpxCache()`](https://lgatto.github.io/rpx/reference/cache.md).

- `pxtax(object)`: returns the taxonomic name of `object`.

- `pxurl(object)`: returns the base url on the ProteomeXchange server
  where the project files reside.

- `pxCacheInfo(object, cache): prints and invisibly returns `object`'s caching information from `cache`(default is`rpxCache()\`).
  The return value is a named vector of length two containing the
  resourne identifier and the cache location.

## References

Vizcaino J.A. et al. 'ProteomeXchange: globally co-ordinated proteomics
data submission and dissemination', Nature Biotechnology 2014, 32, 223 –
226, doi:10.1038/nbt.2839.

Source repository for the ProteomeXchange project:
https://code.google.com/p/proteomexchange/

## Author

Laurent Gatto
