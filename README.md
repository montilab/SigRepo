
<br>

# <img src="man/figures/logo.png" align="left" width="190" /> SigRepo: An R package for storing and processing omic signatures

The `SigRepo` package includes a suite of functions for easily storing
and managing biological signatures and its constituents. Currently,
`Sigrepo` is capable of storing, searching, and retrieving signatures
and its signature collections from a MySQL database of choice. Interest
in setting-up your own `SigRepo` database? See <a href="">here</a> on
how to initiate a MySQL database with the appropriate schema.

In order to interact with a suite of functions in `SigRepo` package, the
input data must represent an R6 object for the representation of
signatures and signature collections, and they can be created using our
proprietary package,
<a href="https://github.com/montilab/OmicSignature">OmicSignature</a>.

Click on each link below for more information:

- [Overview of the object
  structure](https://montilab.github.io/OmicSignature/articles/ObjectStructure.html)
- [Create an OmicSignature
  (OmS)](https://montilab.github.io/OmicSignature/articles/CreateOmS.html)
- [Create an OmicSignatureCollection
  (OmSC)](https://montilab.github.io/OmicSignature/articles/CreateOmSC.html)

For demonstrations, we will walk through the steps of how to use
`SigRepo` package to store, retrieve, and interact with a list of
signatures stored in our MySQL SigRepo Database.

# Installation

- Using `devtools` package

<!-- -->

    # Load devtools package
    library(devtools)

    # Install SigRepo
    devtools::install_github(repo = 'montilab/SigRepo')

    # Install OmicSignature
    devtools::install_github(repo = 'montilab/OmicSignature')

    # Load tidyverse package
    library(tidyverse)

    # Load SigRepo package
    library(SigRepo)

    # Load OmicSignature package
    library(OmicSignature)

# Connect to SigRepo Database

We adopt a MySQL database structure for efficiently storing, searching,
and retrieving the biological signatures and its constituents. To access
the signatures stored in our database,
<a href="https://sigrepo.org/">VISIT OUR WEBSITE</a> to create an
account or <a href="mailto:sigrepo@bu.edu">CONTACT US</a> to be added.

There are three types of user accounts:<br> - `admin` has <b>READ</b>
and <b>WRITE</b> access to all signatures in the database.<br> -
`editor` has <b>READ</b> and <b>WRITE</b> access to ONLY their own
uploaded signatures in the database.<br> - `viewer` has <b>ONLY READ</b>
access to see a list of signatures that are publicly available in the
database but <b>DO NOT HAVE WRITE</b> access to the database.<br>

Once you have a valid account, to connect to our SigRepo database, one
can use the `newConnHandler()` function to create a handler which
contains user credentials to establish connection to our database.

    # Create a connection handler
    conn_handler <- SigRepo::newConnHandler(
      dbname = "sigrepo", 
      host = "sigrepo.org", 
      port = 3306, 
      user = <your_username>, 
      password = <your_password>
    )

# Load Signatures

Here, we provide two signature objects that comes with the package for
demonstrations:

1.  LLFS_Aging_Gene_2023
2.  Myc_reduce_mice_liver_24m

# Upload a signature

The `addSignature()` function allows users to upload a signature to the
database.

**IMPORTANT NOTE:**

- User **MUST HAVE** an `editor` or `admin` account to use this
  function.
- A signature **MUST BE** an R6 object obtained from
  **OmicSignature::OmicSignature()**

## **Example 1**: Create an omic signature using **OmicSignature** package and upload to the database

``` r
# Create an OmicSignature metadata
metadata <- OmicSignature::createMetadata(
  # required attributes:
  signature_name = "Myc_reduce_mice_liver_24m_readme",
  organism = "Mus musculus",
  direction_type = "bi-directional",
  assay_type = "transcriptomics",
  phenotype = "Myc_reduce",

  # optional and recommended:
  covariates = "none",
  description = "mice Myc haploinsufficient (Myc(+/-))",
  platform = "transcriptomics by array",
  sample_type = "liver", # use BRENDA ontology

  # optional cut-off attributes.
  # specifying them can facilitate the extraction of signatures.
  logfc_cutoff = NULL,
  p_value_cutoff = NULL,
  adj_p_cutoff = 0.05,
  score_cutoff = 5,

  # other optional built-in attributes:
  keywords = c("Myc", "KO", "longevity"),
  cutoff_description = NULL,
  author = NULL,
  PMID = 25619689,
  year = 2015,

  # example of customized attributes:
  others = list("animal_strain" = "C57BL/6")
)

# Create difexp object
difexp <- base::readRDS(base::file.path(base::system.file("extdata", package = "OmicSignature"), "difmatrix_Myc_mice_liver_24m.rds")) 
base::colnames(difexp) <- OmicSignature::replaceDifexpCol(base::colnames(difexp))
#> Warning in OmicSignature::replaceDifexpCol(base::colnames(difexp)): Required
#> column for OmicSignature object difexp: feature_name, is not found in your
#> input. This may cause problem when creating your OmicSignature object.

# Rename ensembl with feature name and add group label to difexp
difexp <- difexp |>  
  dplyr::rename(feature_name = ensembl) |> 
  dplyr::mutate(group_label = base::as.factor(base::ifelse(.data$score > 0, "MYC Reduce", "WT")))

# Create signature object
signature <- difexp |>
  dplyr::filter(base::abs(.data$score) > metadata$score_cutoff & .data$adj_p < metadata$adj_p_cutoff) |>
  dplyr::select(c("probe_id", "feature_name", "score")) |>
  dplyr::mutate(group_label = base::as.factor(base::ifelse(.data$score > 0, "MYC Reduce", "WT")))

# Create signature object 
omic_signature <- OmicSignature::OmicSignature$new(
  metadata = metadata,
  signature = signature,
  difexp = difexp
)
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_readme created.

# Add signature to database
SigRepo::addSignature(
  conn_handler = conn_handler,        # A handler contains user credentials to establish connection to a remote database
  omic_signature = omic_signature,    # An R6 object obtained from OmicSignature::OmicSignature()
  visibility = FALSE,                 # Whether to make signature public or private. Default is FALSE.
  return_signature_id = FALSE,        # Whether to return the uploaded signature id. Default is FALSE.
  verbose = TRUE                      # Whether to print diagnostic messages. Default is TRUE.
)
#> Uploading signature metadata to the database...
#> Saving difexp to the database...
#> now dyn.load("/Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/library/curl/libs/curl.so") ...
#> Adding signature owner to the signature access table of the database...
#> Adding signature feature set to the database...
#> Finished uploading.
#> ID of the uploaded signature: 11
```

## **Example 2**: Upload `LLFS_Aging_Gene_2023` signature

``` r
SigRepo::addSignature(
  conn_handler = conn_handler, 
  omic_signature = LLFS_Aging_Gene_2023
)
#> Uploading signature metadata to the database...
#> Saving difexp to the database...
#> Adding signature owner to the signature access table of the database...
#> Adding signature feature set to the database...
#> Finished uploading.
#> ID of the uploaded signature: 12
```

## **Example 3**: Upload `Myc_reduce_mice_liver_24m` signature

``` r
missing_features <- SigRepo::addSignature(
  conn_handler = conn_handler, 
  omic_signature = Myc_reduce_mice_liver_24m,
  return_missing_features = TRUE       # Whether to return a list of missing features during upload.
)
#> Uploading signature metadata to the database...
#> Saving difexp to the database...
#> Adding signature owner to the signature access table of the database...
#> Adding signature feature set to the database...
#> Warning in SigRepo::showTranscriptomicsErrorMessage(db_table_name = ref_table, : 
#> The following features do not existed in the 'transcriptomics_features' table of the database:
#> 'ENSG00000213949'
#> 'ENSG00000127946'
#> 'ENSG00000159167'
#> 'ENSG00000111335'
#> 'ENSG00000033327'
#> 'ENSG00000266472'
#> 'ENSG00000088280'
#> 'ENSG00000074935'
#> 'ENSG00000205318'
#> 'ENSG00000145781'
#> 'ENSG00000182158'
#> 'ENSG00000275183'
#> 'ENSG00000137876'
#> 'ENSG00000135069'
#> 'ENSG00000108582'
#> 'ENSG00000165312'
#> 'ENSG00000135205'
#> 'ENSG00000151726'
#> 'ENSG00000197879'
#> 'ENSG00000154310'
#> 'ENSG00000116016'
#> 'ENSG00000082781'
#> 'ENSG00000141258'
#> 'ENSG00000107833'
#> 'ENSG00000102858'
#> 'ENSG00000182054'
#> 'ENSG00000167106'
#> 'ENSG00000100196'
#> 'ENSG00000150347'
#> 'ENSG00000123977'
#> 'ENSG00000143553'
#> 'ENSG00000182704'
#> 'ENSG00000134909'
#> 'ENSG00000139725'
#> 'ENSG00000170542'
#> 'ENSG00000041982'
#> 'ENSG00000162511'
#> 'ENSG00000134243'
#> 'ENSG00000095383'
#> 'ENSG00000198925'
#> 'ENSG00000163872'
#> 'ENSG00000180891'
#> 'ENSG00000126368'
#> 'ENSG00000014914'
#> 'ENSG00000186104'
#> 'ENSG00000109472'
#> 'ENSG00000196924'
#> 'ENSG00000100605'
#> 'ENSG00000113070'
#> 'ENSG00000145431'
#> 'ENSG00000167272'
#> 'ENSG00000100280'
#> 'ENSG00000182518'
#> 'ENSG00000155363'
#> 'ENSG00000213445'
#> 'ENSG00000272620'
#> 'ENSG00000179941'
#> 'ENSG00000108561'
#> 'ENSG00000005100'
#> 'ENSG00000117616'
#> 'ENSG00000161642'
#> 'ENSG00000196981'
#> 'ENSG00000125434'
#> 'ENSG00000140937'
#> 'ENSG00000105287'
#> 'ENSG00000080561'
#> 'ENSG00000163932'
#> 'ENSG00000106399'
#> 'ENSG00000085185'
#> 'ENSG00000171298'
#> 'ENSG00000120333'
#> 'ENSG00000111875'
#> 'ENSG00000165507'
#> 'ENSG00000166797'
#> 'ENSG00000103404'
#> 'ENSG00000268043'
#> 'ENSG00000265972'
#> 'ENSG00000137494'
#> 'ENSG00000107485'
#> 'ENSG00000106397'
#> 'ENSG00000252623'
#> 'ENSG00000177084'
#> 'ENSG00000155189'
#> 'ENSG00000205189'
#> 'ENSG00000000419'
#> 'ENSG00000168275'
#> 'ENSG00000144674'
#> 'ENSG00000107263'
#> 'ENSG00000113083'
#> 'ENSG00000198873'
#> 'ENSG00000164347'
#> 'ENSG00000065526'
#> 'ENSG00000153294'
#> 'ENSG00000226742'
#> 'ENSG00000163617'
#> 'ENSG00000070047'
#> 'ENSG00000134330'
#> 'ENSG00000201962'
#> 'ENSG00000168538'
#> 'ENSG00000158042'
#> 'ENSG00000160072'
#> 'ENSG00000181634'
#> 'ENSG00000145022'
#> 'ENSG00000128510'
#> 'ENSG00000125037'
#> 'ENSG00000238357'
#> 'ENSG00000077157'
#> 'ENSG00000185989'
#> 
#> You can use 'searchTranscriptomicsFeatureSet()' to see a list of available features.
#> 
#> To add these features to our database, please contact our admin for support.
```

# Search for a list of signatures

The `searchSignature()` function allows users to search for all or a
specific set of signatures that are available in the database.

## Example 1: Search for all signatures

``` r
signature_tbl <- SigRepo::searchSignature(conn_handler = conn_handler)

if(nrow(signature_tbl) > 0){
  knitr::kable(
    signature_tbl, 
    row.names = FALSE
  )
}
```

| signature_id | signature_name                   | organism     | direction_type | assay_type      | phenotype  | platform_name            | sample_type | covariates                                    | description                           | score_cutoff | logfc_cutoff | p_value_cutoff | adj_p_cutoff | cutoff_description | keywords         |     PMID | year | others                     | has_difexp | num_of_difexp | num_up_regulated | num_down_regulated | user_name | date_created        | visibility | signature_hashkey                |
|-------------:|:---------------------------------|:-------------|:---------------|:----------------|:-----------|:-------------------------|:------------|:----------------------------------------------|:--------------------------------------|-------------:|-------------:|---------------:|-------------:|:-------------------|:-----------------|---------:|-----:|:---------------------------|-----------:|--------------:|-----------------:|-------------------:|:----------|:--------------------|-----------:|:---------------------------------|
|            7 | Myc_reduce_mice_liver_24m_v1     | Mus musculus | bi-directional | transcriptomics | Myc_reduce | transcriptomics by array | liver       | none                                          | mice Myc haploinsufficient (Myc(+/-)) |            5 |           NA |             NA |         0.05 | NA                 | Myc,KO,longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> |          1 |           884 |                5 |                 10 | root      | 2025-09-19 19:50:04 |          0 | eacd0935d97847d56cb717455b0e48a3 |
|            8 | Myc_reduce_mice_liver_24m_v2     | Mus musculus | bi-directional | transcriptomics | Myc_reduce | transcriptomics by array | liver       | none                                          | mice Myc haploinsufficient (Myc(+/-)) |            5 |           NA |             NA |         0.05 | NA                 | Myc,KO,longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> |          1 |           884 |                5 |                 10 | root      | 2025-09-19 19:50:04 |          0 | ec356cabc33fcc22f8a0b77b24db91ab |
|            9 | Myc_reduce_mice_liver_24m_v3     | Mus musculus | bi-directional | transcriptomics | Myc_reduce | transcriptomics by array | liver       | none                                          | mice Myc haploinsufficient (Myc(+/-)) |            5 |           NA |             NA |         0.05 | NA                 | Myc,KO,longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> |          1 |           884 |                5 |                 10 | root      | 2025-09-19 19:50:07 |          0 | fa416468cc91f872f073e15efdaf1ece |
|           10 | Myc_reduce_mice_liver_24m_v4     | Mus musculus | bi-directional | transcriptomics | Myc_reduce | transcriptomics by array | liver       | none                                          | mice Myc haploinsufficient (Myc(+/-)) |            5 |           NA |             NA |         0.05 | NA                 | Myc,KO,longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> |          1 |           884 |                5 |                 10 | root      | 2025-09-19 19:50:07 |          0 | 5bc419a0fbd2c012e669a19c67f2b434 |
|           11 | Myc_reduce_mice_liver_24m_readme | Mus musculus | bi-directional | transcriptomics | Myc_reduce | transcriptomics by array | liver       | none                                          | mice Myc haploinsufficient (Myc(+/-)) |            5 |           NA |             NA |         0.05 | NA                 | Myc,KO,longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> |          1 |           884 |                5 |                 10 | root      | 2025-09-24 13:06:55 |          0 | 7e24a413c1287f840d492b2963c263a5 |
|           12 | LLFS_Aging_Gene_2023             | Homo sapiens | bi-directional | transcriptomics | Aging      | transcriptomics by array | blood       | sex,fc,education,percent_intergenic,PC1-4,GRM | NA                                    |            6 |           NA |             NA |         0.01 | NA                 | human,aging,LLFS |       NA | 2023 | NA                         |          1 |          1000 |               82 |                 87 | root      | 2025-09-24 13:06:56 |          0 | 7fdecac66691beed1703efc25487768c |

## Example 2: Search for a specific signature, e.g., **signature_name = “LLFS_Aging_Gene_2023”**

``` r
signature_tbl <- SigRepo::searchSignature(
  conn_handler = conn_handler, 
  signature_name = "LLFS_Aging_Gene_2023"
)

if(nrow(signature_tbl) > 0){
  knitr::kable(
    signature_tbl, 
    row.names = FALSE
  )
}
```

| signature_id | signature_name       | organism     | direction_type | assay_type      | phenotype | platform_name            | sample_type | covariates                                    | description | score_cutoff | logfc_cutoff | p_value_cutoff | adj_p_cutoff | cutoff_description | keywords         | PMID | year | others | has_difexp | num_of_difexp | num_up_regulated | num_down_regulated | user_name | date_created        | visibility | signature_hashkey                |
|-------------:|:---------------------|:-------------|:---------------|:----------------|:----------|:-------------------------|:------------|:----------------------------------------------|:------------|-------------:|-------------:|---------------:|-------------:|:-------------------|:-----------------|-----:|-----:|:-------|-----------:|--------------:|-----------------:|-------------------:|:----------|:--------------------|-----------:|:---------------------------------|
|           12 | LLFS_Aging_Gene_2023 | Homo sapiens | bi-directional | transcriptomics | Aging     | transcriptomics by array | blood       | sex,fc,education,percent_intergenic,PC1-4,GRM | NA          |            6 |           NA |             NA |         0.01 | NA                 | human,aging,LLFS |   NA | 2023 | NA     |          1 |          1000 |               82 |                 87 | root      | 2025-09-24 13:06:56 |          0 | 7fdecac66691beed1703efc25487768c |

# Retrieve a list of omic signatures

The `getSignature()` function allows users to retrieve a list of omic
signature objects that they are **PUBLICLY** available in the database.

**IMPORTANT NOTE:**

- Users can **ONLY RETRIEVE** a list of signatures that are publicly
  available in the database including their own uploaded signatures.
- If a signature is `PRIVATE` and belongs to other user in the database,
  users will need to be given an `editor` permission from its owner in
  order to access, retrieve, and edit their signatures.

## Example 1: Retrieve all signatures that are publicly available or owned by the user in the database

``` r
signature_list <- SigRepo::getSignature(conn_handler = conn_handler)
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_v1 created.
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_v2 created.
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_v3 created.
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_v4 created.
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_readme created.
#>   [Success] OmicSignature object LLFS_Aging_Gene_2023 created.
```

## Example 2: Retrieve a specific signature that is publicly available or owned by the user in the database, e.g., **signature_name = “LLFS_Aging_Gene_2023”**

``` r
LLFS_oms <- SigRepo::getSignature(
  conn_handler = conn_handler, 
  signature_name = "LLFS_Aging_Gene_2023"
)
#>   [Success] OmicSignature object LLFS_Aging_Gene_2023 created.
```

# Delete a signature

The `deleteSignature()` function allows users to delete a signature from
the database.

**IMPORTANT NOTE:**

- Users **MUST HAVE** an `editor` or `admin` account to use this
  function.
- Users can **ONLY DELETE** their own uploaded signatures or were given
  an `editor` permission from its owner to access, retrieve, and edit
  their signatures.
- Users can **ONLY DELETE** a signature one at a time.

**For example:** You want to remove **signature_name =
“LLFS_Aging_Gene_2023”** from the database.

``` r
# Let's search for signature_name = "LLFS_Aging_Gene_2023" in the database
signature_tbl <- SigRepo::searchSignature(
  conn_handler = conn_handler, 
  signature_name = "LLFS_Aging_Gene_2023"
)

# If the signature exists, remove it from the database
if(nrow(signature_tbl) > 0){
  SigRepo::deleteSignature(
    conn_handler = conn_handler, 
    signature_id = signature_tbl$signature_id  
  )
}
#> Remove difexp belongs to signature_id = '12' from the database.
#> Remove signature_id = '12' from 'signatures' table of the database.
#> Remove features belongs to signature_id = '12' from 'signature_feature_set' table of the database.
#> Remove user access to signature_id = '12' from 'signature_access' table of the database.
#> Remove signature_id = '12' from 'signature_collection_access' table of the database.
#> signature_id = '12' has been removed.
```

# Update a signature

The `updateSignature()` function allows users to update a specific
signature in the SigRepo database.

**IMPORTANT NOTE:**

- Users **MUST HAVE** an `editor` or `admin` account to use this
  function.
- Users can **ONLY UPDATE** their own uploaded signatures or were given
  an `editor` permission from its owner to access, retrieve, and edit
  their signatures.
- Users can **ONLY UPDATE** a signature one at a time.

**For example:** If the `platform` information in the previous uploaded
signature, **“Myc_reduce_mice_liver_24m_readme”**, is incorrect, and you
wish to update the `platform` information with the correct value, e.g.,
**platform = “transcriptomics by single-cell RNA-seq”**. You can use the
`updateSignature()` function as follows:

``` r
# 1. Revise the metadata object with new platform = transcriptomics by single-cell RNA-seq
metadata_revised <- OmicSignature::createMetadata(
  # required attributes:
  signature_name = "Myc_reduce_mice_liver_24m_readme",
  organism = "Mus musculus",
  direction_type = "bi-directional",
  assay_type = "transcriptomics",
  phenotype = "Myc_reduce",

  # optional and recommended:
  covariates = "none",
  description = "mice Myc haploinsufficient (Myc(+/-))",
  platform = "transcriptomics by single-cell RNA-seq",
  sample_type = "liver", # use BRENDA ontology

  # optional cut-off attributes.
  # specifying them can facilitate the extraction of signatures.
  logfc_cutoff = NULL,
  p_value_cutoff = NULL,
  adj_p_cutoff = 0.05,
  score_cutoff = 5,

  # other optional built-in attributes:
  keywords = c("Myc", "KO", "longevity"),
  cutoff_description = NULL,
  author = NULL,
  PMID = 25619689,
  year = 2015,

  # example of customized attributes:
  others = list("animal_strain" = "C57BL/6")
)

# Create difexp object
difexp <- base::readRDS(base::file.path(base::system.file("extdata", package = "OmicSignature"), "difmatrix_Myc_mice_liver_24m.rds")) 
base::colnames(difexp) <- OmicSignature::replaceDifexpCol(base::colnames(difexp))
#> Warning in OmicSignature::replaceDifexpCol(base::colnames(difexp)): Required
#> column for OmicSignature object difexp: feature_name, is not found in your
#> input. This may cause problem when creating your OmicSignature object.

# Rename ensembl with feature name and add group label to difexp
difexp <- difexp |>  
  dplyr::rename(feature_name = ensembl) |> 
  dplyr::mutate(group_label = base::as.factor(base::ifelse(.data$score > 0, "MYC Reduce", "WT")))

# Create signature object
signature <- difexp |>
  dplyr::filter(base::abs(.data$score) > metadata$score_cutoff & .data$adj_p < metadata$adj_p_cutoff) |>
  dplyr::select(c("probe_id", "feature_name", "score")) |>
  dplyr::mutate(group_label = base::as.factor(base::ifelse(.data$score > 0, "MYC Reduce", "WT")))

# Create signature object 
updated_omic_signature <- OmicSignature::OmicSignature$new(
  metadata = metadata_revised,
  signature = signature,
  difexp = difexp
)
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_readme created.
```

``` r
# Now, let's search for Myc_reduce_mice_liver_24m_readme in the database
# in which we would like to revise the value of platform to 'transcriptomics by single-cell RNA-seq'
signature_tbl <- SigRepo::searchSignature(
  conn_handler = conn_handler, 
  signature_name = metadata_revised$signature_name
)

# If signature exists, update the signature with the revised omic_signature object
if(base::nrow(signature_tbl) > 0){
  SigRepo::updateSignature(
    conn_handler = conn_handler, 
    signature_id = signature_tbl$signature_id, 
    omic_signature = updated_omic_signature
  )
}
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_readme created.
#>  signature_id = '11' has been updated.
```

Let’s look up **signature_name = “Myc_reduce_mice_liver_24m_readme”**
and see if the value of `platform` has been changed.

``` r
signature_tbl <- SigRepo::searchSignature(
  conn_handler = conn_handler, 
  signature_name = "Myc_reduce_mice_liver_24m_readme"
)

if(base::nrow(signature_tbl) > 0){
  knitr::kable(
    signature_tbl,
    row.names = FALSE
  )
}
```

| signature_id | signature_name                   | organism     | direction_type | assay_type      | phenotype  | platform_name                          | sample_type | covariates | description                           | score_cutoff | logfc_cutoff | p_value_cutoff | adj_p_cutoff | cutoff_description | keywords         |     PMID | year | others                     | has_difexp | num_of_difexp | num_up_regulated | num_down_regulated | user_name | date_created        | visibility | signature_hashkey                |
|-------------:|:---------------------------------|:-------------|:---------------|:----------------|:-----------|:---------------------------------------|:------------|:-----------|:--------------------------------------|-------------:|-------------:|---------------:|-------------:|:-------------------|:-----------------|---------:|-----:|:---------------------------|-----------:|--------------:|-----------------:|-------------------:|:----------|:--------------------|-----------:|:---------------------------------|
|           11 | Myc_reduce_mice_liver_24m_readme | Mus musculus | bi-directional | transcriptomics | Myc_reduce | transcriptomics by single-cell RNA-seq | liver       | none       | mice Myc haploinsufficient (Myc(+/-)) |            5 |           NA |             NA |         0.05 | NA                 | Myc,KO,longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> |          1 |           884 |                5 |                 10 | root      | 2025-09-24 13:07:00 |          0 | 7e24a413c1287f840d492b2963c263a5 |

Finally, remove **signature_name = “Myc_reduce_mice_liver_24m_readme”**
from the database

``` r
# Let's search for signature_name = "Myc_reduce_mice_liver_24m_readme" in the database
signature_tbl <- SigRepo::searchSignature(
  conn_handler = conn_handler, 
  signature_name = "Myc_reduce_mice_liver_24m_readme"
)

# If the signature exists, remove it from the database
if(nrow(signature_tbl) > 0){
  SigRepo::deleteSignature(
    conn_handler = conn_handler, 
    signature_id = signature_tbl$signature_id
  )
}
#> Remove difexp belongs to signature_id = '11' from the database.
#> Remove signature_id = '11' from 'signatures' table of the database.
#> Remove features belongs to signature_id = '11' from 'signature_feature_set' table of the database.
#> Remove user access to signature_id = '11' from 'signature_access' table of the database.
#> Remove signature_id = '11' from 'signature_collection_access' table of the database.
#> signature_id = '11' has been removed.
```

# Additional Guides

- [Upload a signature collection to the SigRepo
  database](https://montilab.github.io/SigRepo/articles/collection-tutorials.html)
