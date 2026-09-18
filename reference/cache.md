# Package cache

Function to access and manage the cache. `rxpCache()` returns the
central `rpx` cache. `pxCachedrojects()` prints the names of the cached
projects and invisibly returns the cache table.

## Usage

``` r
rpxCache()

pxCachedProjects(cache = rpxCache(), rpxprefix = "^\\.rpx(2?)")
```

## Arguments

- cache:

  Object of class `BiocFileCache`.

- rpxprefix:

  `character(1)` defining the resourne name prefix in `cache`. Default
  is `"^\\.rpx(2?)"` to match objects of class `PXDataset` and
  `PXDataset2`.

## Value

The `rpxCache()` function returns an instance of class `BiocFileCache`.
`pxCachedProjects()` invisbly returns a `tibble` of cached
ProteomeXchange projects.

## Details

The cache is an object of class `BiocFileCache`, and created with
[`BiocFileCache::BiocFileCache()`](https://rdrr.io/pkg/BiocFileCache/man/BiocFileCache-class.html).
It can be either the package-wide cache as defined by `rpxCache()` or an
instaned provided by the user.

When projects are cached, they are given a resource name (`rname`)
composed of the `.rpx` prefix followed by the ProteomeXchange
identifier. For example, project `PXD000001` is named `.rpxPXD000001`
(`.rpx2PXD000001` for the `PXDataset2` class) to avoid any conflicts
with other resources that user-created resources.

## Author

Laurent Gatto

## Examples

``` r

## Default rpx cache
rpxCache()
#> class: BiocFileCache
#> bfccache: /github/home/.cache/R/rpx
#> bfccount: 4
#> For more information see: bfcinfo() or bfcquery()

if (FALSE) { # \dontrun{

## Set up your own cache by providing a file or a directory to
## BiocFileCache::BiocFileCache()
my_cache <- BiocFileCache::BiocFileCache(tempfile())
my_cache
px <- PXDataset("PXD000001", cache = my_cache)
pxget(px, "erwinia_carotovora.fasta", cache = my_cache)


## List of cached projects
pxCachedProjects() ## default rpx cache
pxCachedProjects(my_cache)

## To delete project a project from the default cache, first find
## its resource id (rid) in the cache
px1_cache_info <- pxCacheInfo(px)
(rid <- px1_cache_info["rid"])

## Then remove it with BiocFileCache:: bfcremove()
BiocFileCache:::bfcremove(my_cache, rid)
pxCachedProjects(my_cache)
} # }
```
