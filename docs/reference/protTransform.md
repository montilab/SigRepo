# protTransform

Function to dynamically transform the ftp uniprot data into the SigRepo
dictionary structure

## Usage

``` r
protTransform(
  organism_code = "HUMAN",
  organism_name = "Homo sapiens",
  tax_id = "9606",
  version = base::format(base::Sys.Date(), "%m%d%Y"),
  is_current = 1
)
```

## Arguments

- organism_code:

  organism name you want to retrieve data from

- tax_id:

  Taxonomic id of organism

- version:

  this is set the date of downloading the ftp file

- is_current:

  boolean for if the feature_names are current or not.
