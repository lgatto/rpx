# rpx 2.5

## rpx 2.5.1

- Address failing unit test since PRIDE URL has changed back.

## rpx 2.5.0

- New bioconductor devel version

# rpx 2.4

## rpx 2.4.0

- New bioconductor release version

# rpx 2.3

## rpx 2.3.3

- Don't run caching example as it replies on PRIDE which is failing
  too often for the tests to pass on all systems.

## rpx 2.3.2

- Provide fix for PRIDE migration and annotation discrepancies (see
  issue #17). This fix will however lead to re-downloading some cached
  files due to different URLs. The fix might only be temporary, based
  on if/how PRIDE will address their inconsistencies.

## rpx 2.3.1

- Use `BiocFileCache::bfcrpath` to get correct rpath, irrespective of
  absolute or relative.

## rpx 2.3.0

- New bioconductor devel branch

# rpx 2.1

## rpx 2.1.12

- Annotate additional experiments as returning errors (see issues for
  full list) <2021-10-07 Thu>

## rpx 2.1.11

- Annotate PXD012095 as returning an error (see #12) <2021-10-04 Mon>

## rpx 2.1.10

- Caching PRIDE sitemap with all PXD projects.
- New `pxinstruments()`, `pxptms()` and `pxprotocols()` accessors.

## rpx 2.1.9

- Internal function to tally local project vs full PX.

## rpx 2.1.8

- New `PXDataset2` class with richer interface and more stable data
  downloading functions. `PXDataset` and `PXDataset2` work
  transparently and `PXDataset2` is now default.

- Add deprecation notice in `PXDataset()` constructor.

## rpx 2.1.7

- Improve object creating printout.
- Improve `pxCachedProjects()` output.

## rpx 2.1.6

- Update installation instruction in `README.md`.
- Cache location is now also stored inside the `PXDataset` object.
- New `pxCacheInfo()` function and use it to show caching info in
  `show,PXDataset`.

## rpx 2.1.5

- Improve documentation.

- Check for cached PXDataset object validity.

## rpx 2.1.4

- Fix bug in PXDdataset internal data storage.

- New `pxCachedProjects()` function that return the cached projects.

- Fixes and improvements in the documentation.

## rpx 2.1.3

- PXDatasets are also cached upon creation and retrieved from cache
  next time they are generated.

- cache is now returned by `rpxCache()`.

## rpx 2.1.2

- New feature: PXDataset objects now query and store data (ref, tax,
  files, url) when generated instead of fetching these on the fly
  every time. This new feature has been added to circumvent the issues
  with data access (see #5).

## rpx 2.1.1

- `pxannouced()` paused (see #7).

# rpx 1.99.0

## rpx 1.99.8
- Remove `rappdirs` from Suggests.

## rpx 1.99.7
- Add `markdown` to Suggests.

## rpx 1.99.6
- Switching to `tools::R_user_dir()` from `rappdirs::user_cache_dir()`
  to set package cache directory, following changes in `BiocFileCache`
  ([see](https://stat.ethz.ch/pipermail/bioc-devel/2021-April/017928.html)
  for details).

## rpx 1.99.5
- User can pass an own cache when downloading files <2021-04-03 Sat>

## rpx 1.99.4
- Automatically detect/fix correct url <2021-03-22 Mon>

## rpx 1.99.3
- Use `MS:1002852` to get URL when `PRIDE:0000411` returns empty
  string.

## rpx 1.99.2
- Don't ask for cache location in non-interactive mode. This forces
  the usage of the default cache location (rather than usage of a Rtmp
  directory) and enables reusage of a persistent cache over different
  build/check cycles.

## rpx 1.99.1
- Using BiocFileCache to cache PXD files that are downloaded.

## Changes in version 1.27.0
- New devel version

# rpx 1.23

## Changes in version 1.23.1
- Define modifiable `rpx_fix_issue_5` <2019-11-26 Tue>

## Changes in version 1.23.0
- New devel version

# rpx 1.21

## Changes in version 1.21.3
- Update NEWS titles

## Changes in version 1.21.2
- Temporary fix for issue #5 (wrong URL from PRIDE server)
  <2019-10-02 Wed>

## Changes in version 1.21.1
- Don't set old class <2019-08-09 Fri>

# rpx 1.19

## Changes in version 1.19.4
- Delete plain NEWS now that R supports NEWS.md

## Changes in version 1.19.3
- Fix NEWS

## Changes in version 1.19.2
- Mention setting `method` on Windows <2019-04-16 Tue>

## Changes in version 1.19.1
- Add ImmunoOncology biocView

## Changes in version 1.19.0
- New release for Bioc 3.9

# rpx 1.18

## Changes in version 1.18.0
- New release for Bioc 3.8

# rpx 1.17

## Changes in version 1.17.2
- Update NEWS and pkgdown site

## Changes in version 1.17.1
- replace BiocInstaller biocLite mentions with BiocManager

## Changes in version 1.17.0
- New version for Bioconductor devel

# rpx 1.16

## Changes in version 1.16.0
- New version for Bioconductor release

# rpx 1.13

## Changes in version 1.13.5

- Nothing yet

## Changes in version 1.13.4

- Restore reference unit test

## Changes in version 1.13.3

- No commit

## Changes in version 1.13.2

- Fix errors due to changes on PX side. PXD000001 reference remains
  untested - see https://twitter.com/lgatt0/status/885091284142239744
  for details. <2017-07-12 Wed>

## Changes in version 1.13.1

- Using xml2 <2017-06-13 Tue>
- Use functions instead of methods <2017-06-13 Tue>

## Changes in version 1.13.0

- Bioc devel 3.6

# rpx 1.12

## Changes in version 1.12.1

- Using xml2 (backported from devel) <2017-06-13 Tue>
- Use functions instead of methods (backported from devel) <2017-06-13 Tue>

## Changes in version 1.12.0

- Bioc release 3.5

# rpx 1.11

## Changes in version 1.11.0

- Bioc devel 3.4

## Changes in version 1.11.4

- Update unit test to reflect upstream changes <2017-02-26 Sun>

## Changes in version 1.11.3

- `pxget` gains a `destdir` argument to specify the destination to which files
    should be downloaded <2017-01-27 Fri>

## Changes in version 1.11.2

- Update unit test to reflect upstream changes <2017-01-25 Wed>

## Changes in version 1.11.1

- Migrate vignette to BiocStyle::html_document2 <2016-12-22 Thu>
- Use NEWS.md <2016-12-28 Wed>

## Changes in version 1.11.0

- Bioconductor devel 3.5

# rpx 1.10

## Changes in version 1.10.0

- Bioconductor release 3.4

# rpx 1.9

## Changes in version 1.9.4

- Updating unit tests <2016-10-04 Tue>

## Changes in version 1.9.3

- Update vignette to use readMzTabData v0.9 <2016-07-24 Sun>

# rpx 1.5

## Changes in version 1.5.1

- unit tests <2015-06-30 Tue>
- export and document pxnodes <2015-06-30 Tue>

## Changes in version 1.5.0

- Bioc version 3.2 (devel)

# rpx 1.4

## Changes in version 1.4.0

- Bioc version 3.1 (release)

# rpx 1.3

## Changes in version 1.3.1

- update vignette to reflect new files in px example, and avoid
  downloaded the raw data [2015-03-31 Tue]

# rpx 0.99

## Changes in version 0.99.9

- In pxref, dealing with cases without any-r pending publications
   [2014-04-01 Tue]

## Changes in version 0.99.8

- fix leftover issues from renaming package (d.tenenbaum)
   [2014-03-30 Sun]

## Changes in version 0.99.7

- renamed to rpx due to CRAN name clash [2014-03-28]

## Changes in version 0.99.6

- more typos in vignette [2014-03-12 Wed]

## Changes in version 0.99.5

- Suggestions, biocView and typos reported by Nate Hayden
   [2014-03-12 Wed]
- added Runit test [2014-03-12 Wed]
- use ae fonts in vignette [2014-03-12 Wed]

## Changes in version 0.99.4

- Update reference in man and vignette [2014-03-11 Tue]
- Typos in vignette [2014-03-11 Tue]

## Changes in version 0.99.3

- reverting pxannounced to empty signature to fix warning
 [2014-03-05 Wed]

## Changes in version 0.99.2

- replacing download.file by getURL to download annoucement rss-ver
   https <2014-03-05 Wed>

## Changes in version 0.99.1

- fixing pxfiles ending by \r-n Windows [2014-03-05 Wed]

## Changes in version 0.99.0

- added a vignette [2014-03-05 Wed]
- handle invalid identifiers [2014-03-05 Wed]
