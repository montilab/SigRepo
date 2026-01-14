# addOrganism

Add organisms to database

## Usage

``` r
addOrganism(conn_handler, organism_tbl, verbose = TRUE)
```

## Arguments

- conn_handler:

  An R object obtained from SigRepo::newConnhandler() (required)

- organism_tbl:

  A Data Frame; Must contain the following column names: organism,
  biomart_db, biomart_dataset, biomart_description, biomart_version,
  biomart_updated_date, prot_organism_code, prot_organism_taxid,
  prot_updated_date (required)

- verbose:

  Logical; whether to print diagnostic messages. Defaults to 'TRUE'

## Examples
