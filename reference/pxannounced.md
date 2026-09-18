# Return recent PX announcements

Queries the PX rss feed file for the latest PX dataset announcements.

## Usage

``` r
pxannounced()
```

## Value

A `data.frame` with announcements data set identifiers, publication
dates and annoucement messages.

## Author

Laurent Gatto

## Examples

``` r

pxannounced()
#> The google group rss feeds are currenlty down (see
#> https://github.com/lgatto/rpx/issues/7 for details). Please visit
#> https://groups.google.com/g/proteomexchange for the latest annoucements
```
