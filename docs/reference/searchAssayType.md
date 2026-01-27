# searchAssayType

Search for omics assay types and its availability status.

## Usage

``` r
searchAssayType(assay_type = NULL)
```

## Arguments

- assay_type:

  A list of assay types to be looked up. Default is NULL which will
  return all of the assay types in the database.

## Value

A data frame with assay types and their availability status.

There are two availability status: `"Available"` means the assay type
currently exists in the database, and users can upload signatures or
collections that are specifically associated with this assay type.
`"Unavailable"` means the assay type isn't existed in the database yet,
and users cannot upload signatures or collections that are specifically
associated with this assay type.

## Examples

``` r
if (FALSE) { # \dontrun{

# Search a list of assay types in the database 
SigRepo::searchAssayType(
  assay_type = "transcriptomics"
)

} # }

```
