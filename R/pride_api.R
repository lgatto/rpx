################################################################################
## PRIDE Archive Restful WS version 3.0
##
## https://www.ebi.ac.uk/pride/ws/archive/v3/v3/api-docs
##
## The PRIDE PRoteomics IDEntifications (PRIDE) database is a centralized,
## standards compliant, public data repository for proteomics data, including
## protein and peptide identifications, post-translational modifications and
## supporting molecules evidence.
##
## https://www.ebi.ac.uk/pride/ws/archive/v3/webjars/swagger-ui/index.html
##
################################################################################


################################################################################
## projects
## -----------------------------------------------------------------------------

## /status/{accession}: Check if the dataset is public/private in PRIDE
status <- function(accession) {
    url <- paste0("https://www.ebi.ac.uk/pride/ws/archive/v3/status/", accession)
    RCurl::getURL(url)
}

## /search/projects
## /search/autocomplete
## /projects/{projectAccession}
## /projects/{projectAccession}/files/count
## /projects/{projectAccession}/files/all
## /projects/{projectAccession}/files
## /projects/{accession}/similarProjects
## /projects/reanalysis/{projectAccession}
## /projects/metadata

## /projects/files-path/{projectAccession}: Return the path of the dataset's files
projects_filespath <- function(accession) {
    url <- paste0("https://www.ebi.ac.uk/pride/ws/archive/v3/projects/files-path/",
                  accession)
    jsonlite::fromJSON(url)
}

## /projects/download
## /projects/download/by/keyword
## /projects/count
## /projects/all
## /projects
## /files/checksum/{projectAccession}
## /facet/projects





################################################################################
## stats
## -----------------------------------------------------------------------------

## /stats/{name}
## /stats/submitted-data
## /stats/submissions-monthly-tsv
## /stats/submissions-monthly


################################################################################
## affinity-projects
## -----------------------------------------------------------------------------

## /pride-ap/search/projects
## /pride-ap/search/autocomplete
## /pride-ap/projects/{accession}/similarProjects
## /pride-ap/projects
## /pride-ap/facet/projects


################################################################################
## files
## -----------------------------------------------------------------------------

## /files/{fileAccession}
## /files/sdrf/{projectAccession}
## /files/sdrf/{projectAccession}

## /files/count: Count of all PRIDE Archive Files
files_count <- function() {
    res <- RCurl::getURL("https://www.ebi.ac.uk/pride/ws/archive/v3/files/count")
    as.integer(res)
}

## /files/all