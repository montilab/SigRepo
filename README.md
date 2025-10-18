
<br>

# <img src="man/figures/logo.png" align="left" width="190" /> SigRepo: An R package for storing and processing omic signatures

The `SigRepo` package provides a comprehensive set of functions for easy
storage and management of biological signatures and their components.
SigRepo (the **client**) works alongside `SigRepo_Server`, its
**server** counterpart. While SigRepo enables you to store, search, and
retrieve signatures and signature collections, these operations rely on
a running SigRepo_Server instance.

Interested in setting up your own SigRepo_Server? Check out the
installation instructions
<a target="_blank" href="https://montilab.github.io/SigRepo_Server/articles/install_sigrepo.html" >here</a>.

To upload and download signatures — and to fully utilize the
functionalities offered by the SigRepo package — signatures and
signature collections must be represented as specific R6 objects. You
can create these objects using our proprietary package,
<a target="_blank" href="https://github.com/montilab/OmicSignature">OmicSignature</a>.

Click on each link below for more information:

- <a
  href="https://montilab.github.io/OmicSignature/articles/ObjectStructure.html"
  target="_blank">Overview of the object structure</a>
- <a
  href="https://montilab.github.io/OmicSignature/articles/CreateOmS.html"
  target="_blank">Create an OmicSignature (OmS)</a>
- <a
  href="https://montilab.github.io/OmicSignature/articles/CreateOmSC.html"
  target="_blank">Create an OmicSignatureCollection (OmSC)</a>

Below, we walk you through few essential steps to install the `SigRepo`
package, and to store, retrieve, and interact with a list of signatures
stored in an <a target="_blank" href="https://sigrepo.org">already
deployed SigRepo server</a>.

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

## Before you begin

Please navigate to our
<a href="https://sigrepo.org" target="_blank">sigrepo.org</a> portal to
create your account. On the login page, click `"Register here!"` and
fill out the registration form to create an account. You will receive an
email when your account has been activated. Due to SQL constraints,
having multiple users on the same testing account, like running the
tutorial in the readme, will fail to connect. Each user using their own
account is ideal.

# Connect to SigRepo Database

We adopt a MySQL database structure for efficiently storing, searching,
and retrieving the biological signatures and its constituents. To access
the signatures stored in our database,
<a target="_blank" href="https://sigrepo.org/">VISIT OUR WEBSITE</a> to
create an account or <a href="mailto:sigrepo@bu.edu">CONTACT US</a> to
be added.

There are three types of user accounts:<br> - `admin` has <b>READ</b>
and <b>WRITE</b> access to all signatures in the database.<br> -
`editor` has <b>READ</b> and <b>WRITE</b> access to ONLY their own
uploaded signatures in the database.<br> - `viewer` has <b>ONLY READ</b>
access to see a list of signatures that are publicly available in the
database but <b>DO NOT HAVE WRITE</b> access to the database.<br>

Once you have a valid account, to connect to our SigRepo database, one
can use the `SigRepo::newConnHandler()` function to create a handler
which contains user credentials to establish connection to our database.

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

The `SigRepo::addSignature()` function allows users to upload a
signature to the database.

**IMPORTANT NOTE:**

- User `MUST HAVE` an `editor` or `admin` account to use this function.
- A signature `MUST BE` an R6 object obtained from
  `OmicSignature::OmicSignature()`

## **Example 1**: Upload `LLFS_Aging_Gene_2023` signature

``` r
## Show the OmicSignature summary
print(LLFS_Aging_Gene_2023)
#> Signature Object: 
#>   Metadata: 
#>     adj_p_cutoff = 0.01 
#>     assay_type = transcriptomics 
#>     covariates = sex,fc,education,percent_intergenic,PC1-4,GRM 
#>     direction_type = bi-directional 
#>     keywords = human, aging, LLFS 
#>     organism = Homo Sapiens 
#>     phenotype = Aging 
#>     platform = transcriptomics by array 
#>     sample_type = blood 
#>     score_cutoff = 6 
#>     signature_name = LLFS_Aging_Gene_2023 
#>     year = 2023 
#>   Signature: 
#>     Group1 (82)
#>     Group2 (87)
#>   Differential Expression Data: 
#>     1000 x 8

## Add the signature to the repository
SigRepo::addSignature(
  conn_handler = conn_handler, 
  omic_signature = LLFS_Aging_Gene_2023
)
#> Uploading signature metadata to the database...
#> Saving difexp to the database...
#> now dyn.load("/Library/Frameworks/R.framework/Versions/4.5-x86_64/Resources/library/curl/libs/curl.so") ...
#> Adding signature owner to the signature access table of the database...
#> Adding signature feature set to the database...
#> Finished uploading.
#> ID of the uploaded signature: 452
```

## **Example 2**: Upload `Myc_reduce_mice_liver_24m` signature

In this example, we show what happens when the OmicSignature contains
`feature_name`’s that are not part of “dictionary” for the corresponding
omics layer.

``` r
## Show the OmicSignature summary
print(Myc_reduce_mice_liver_24m)
#> Signature Object: 
#>   Metadata: 
#>     adj_p_cutoff = 0.05 
#>     assay_type = transcriptomics 
#>     covariates = none 
#>     description = mice Myc haploinsufficient (Myc(+/-)) 
#>     direction_type = bi-directional 
#>     keywords = Myc, KO, longevity 
#>     organism = Mus musculus 
#>     others = C57BL/6 
#>     phenotype = Myc_reduce 
#>     platform = transcriptomics by array 
#>     PMID = 25619689 
#>     sample_type = liver 
#>     score_cutoff = 5 
#>     signature_name = Myc_reduce_mice_liver_24m 
#>     year = 2015 
#>   Metadata user defined fields: 
#>     animal_strain = C57BL/6 
#>   Signature: 
#>     Group1 (30)
#>     Group2 (78)
#>   Differential Expression Data: 
#>     1000 x 5

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

## **Example 3**: Create an omic signature using `OmicSignature` package and upload to the database

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
  keywords = "Myc, KO, longevity",
  cutoff_description = NULL,
  author = NULL,
  PMID = 25619689,
  year = 2015,

  # example of customized attributes:
  others = base::list("animal_strain" = "C57BL/6")
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
#> Adding signature owner to the signature access table of the database...
#> Adding signature feature set to the database...
#> Finished uploading.
#> ID of the uploaded signature: 454
```

# Search for a list of signatures

The `SigRepo::searchSignature()` function allows users to search for all
or a specific set of signatures that are available in the database.

## Example 1: Search for all signatures.

``` r
# Get all signatures
signature_tbl <- SigRepo::searchSignature(conn_handler = conn_handler)

# WARNINGS: THE SIGNATURE TABLE CAN BE LARGE, SO ONLY THE FIRST SIX OBSERVATIONS ARE SHOWN.
if(base::nrow(signature_tbl) > 0){
  knitr::kable(
    utils::head(signature_tbl), 
    row.names = FALSE
  )
}
```

| signature_id | signature_name | organism | direction_type | assay_type | phenotype | platform_name | sample_type | covariates | description | score_cutoff | logfc_cutoff | p_value_cutoff | adj_p_cutoff | cutoff_description | keywords | PMID | year | others | has_difexp | num_of_difexp | num_up_regulated | num_down_regulated | user_name | date_created | visibility | signature_hashkey |
|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|:---|:---|---:|---:|:---|---:|---:|---:|---:|:---|:---|---:|:---|
| 213 | Aging_Hs_HNSC_RNASeq_TCGA_ACC_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative ACC aging signature | 0 | NA | NA | 1 | NA | TCGA,ACC,Aging | NA | 2025 | NA | 1 | 32402 | 18031 | 14371 | H_Nikoueian | 2025-10-03 15:01:24 | 0 | 37b8bb25907349dfaa1b38fecc9e872b |
| 214 | Aging_Hs_HNSC_RNASeq_TCGA_BLCA_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative BLCA aging signature | 0 | NA | NA | 1 | NA | TCGA,BLCA,Aging | NA | 2025 | NA | 1 | 39689 | 22346 | 17343 | H_Nikoueian | 2025-10-03 15:01:35 | 0 | 28d8ba3d012ecbc6a080996fc2dcd66e |
| 215 | Aging_Hs_HNSC_RNASeq_TCGA_BRCA_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative BRCA aging signature | 0 | NA | NA | 1 | NA | TCGA,BRCA,Aging | NA | 2025 | NA | 1 | 42011 | 26478 | 15533 | H_Nikoueian | 2025-10-03 15:01:49 | 0 | 9ae65c31c82b6edc15819fb7f8cd0640 |
| 216 | Aging_Hs_HNSC_RNASeq_TCGA_CESC_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative CESC aging signature | 0 | NA | NA | 1 | NA | TCGA,CESC,Aging | NA | 2025 | NA | 1 | 38521 | 21746 | 16775 | H_Nikoueian | 2025-10-03 15:02:05 | 0 | e487cfd606e3d4e2214a8e4435b424b8 |
| 217 | Aging_Hs_HNSC_RNASeq_TCGA_COAD_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative COAD aging signature | 0 | NA | NA | 1 | NA | TCGA,COAD,Aging | NA | 2025 | NA | 1 | 38857 | 22186 | 16671 | H_Nikoueian | 2025-10-03 15:02:20 | 0 | afb09a078a63c9342530d2a119698085 |
| 218 | Aging_Hs_HNSC_RNASeq_TCGA_ESCA_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative ESCA aging signature | 0 | NA | NA | 1 | NA | TCGA,ESCA,Aging | NA | 2025 | NA | 1 | 42053 | 15816 | 26237 | H_Nikoueian | 2025-10-03 15:02:33 | 0 | 3275eeda945640a32d963a55d22143a9 |

## Example 2: Search for a specific signature, e.g., `signature_name = "LLFS_Aging_Gene_2023"`

``` r
signature_tbl <- SigRepo::searchSignature(
  conn_handler = conn_handler, 
  signature_name = "LLFS_Aging_Gene_2023"
)

if(base::nrow(signature_tbl) > 0){
  knitr::kable(
    signature_tbl, 
    row.names = FALSE
  )
}
```

| signature_id | signature_name | organism | direction_type | assay_type | phenotype | platform_name | sample_type | covariates | description | score_cutoff | logfc_cutoff | p_value_cutoff | adj_p_cutoff | cutoff_description | keywords | PMID | year | others | has_difexp | num_of_difexp | num_up_regulated | num_down_regulated | user_name | date_created | visibility | signature_hashkey |
|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|:---|:---|---:|---:|:---|---:|---:|---:|---:|:---|:---|---:|:---|
| 452 | LLFS_Aging_Gene_2023 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by array | blood | sex,fc,education,percent_intergenic,PC1-4,GRM | NA | 6 | NA | NA | 0.01 | NA | human,aging,LLFS | NA | 2023 | NA | 1 | 1000 | 82 | 87 | montilab | 2025-10-18 00:21:05 | 0 | b35b7c1d387440d474bfcb3cb162c9a6 |

# Retrieve a list of omic signatures

The `SigRepo::getSignature()` function allows users to retrieve a list
of omic signature objects that they are `PUBLICLY` available in the
database.

**IMPORTANT NOTE:**

- Users can `ONLY RETRIEVE` a list of signatures that are publicly
  available in the database including their own uploaded signatures.
- If a signature is `PRIVATE` and belongs to other user in the database,
  users will need to be given an `editor` permission from its owner in
  order to access, retrieve, and edit their signatures.

## Example 1: Retrieve all signatures that are publicly available or owned by the user in the database

    # WARNINGS: THE SIGNATURE LIST CAN BE LARGE TO DOWNLOAD (NOT RUN)
    signature_list <- SigRepo::getSignature(conn_handler = conn_handler)

## Example 2: Retrieve a specific signature that is publicly available or owned by the user in the database, e.g., `signature_name = "LLFS_Aging_Gene_2023"`

``` r
LLFS_oms <- SigRepo::getSignature(
  conn_handler = conn_handler, 
  signature_name = "LLFS_Aging_Gene_2023"
)
#>   [Success] OmicSignature object LLFS_Aging_Gene_2023 created.
```

# Delete a signature

The `SigRepo::deleteSignature()` function allows users to delete a
signature from the database.

**IMPORTANT NOTE:**

- Users `MUST HAVE` an `editor` or `admin` account to use this function.
- Users can `ONLY DELETE` their own uploaded signatures or were given an
  `editor` permission from its owner to access, retrieve, and edit their
  signatures.
- Users can `ONLY DELETE` a signature one at a time.

**For example:** You want to remove
`signature_name = "LLFS_Aging_Gene_2023"` from the database.

``` r
# Let's search for signature_name = "LLFS_Aging_Gene_2023" in the database
signature_tbl <- SigRepo::searchSignature(
  conn_handler = conn_handler, 
  signature_name = "LLFS_Aging_Gene_2023"
)

# If the signature exists, remove it from the database
if(base::nrow(signature_tbl) > 0){
  SigRepo::deleteSignature(
    conn_handler = conn_handler, 
    signature_id = signature_tbl$signature_id  
  )
}
#> Remove difexp belongs to signature_id = '452' from the database.
#> Remove signature_id = '452' from 'signatures' table of the database.
#> Remove features belongs to signature_id = '452' from 'signature_feature_set' table of the database.
#> Remove user access to signature_id = '452' from 'signature_access' table of the database.
#> Remove signature_id = '452' from 'signature_collection_access' table of the database.
#> signature_id = '452' has been removed.
```

# Update a signature

The `SigRepo::updateSignature()` function allows users to update a
specific signature in the SigRepo database.

**IMPORTANT NOTE:**

- Users `MUST HAVE` an `editor` or `admin` account to use this function.
- Users can `ONLY UPDATE` their own uploaded signatures or were given an
  `editor` permission from its owner to access, retrieve, and edit their
  signatures.
- Users can `ONLY UPDATE` a signature one at a time.

**For example:** If the `platform` information in the previous uploaded
signature, `"Myc_reduce_mice_liver_24m_readme"`, is incorrect, and you
wish to update the `platform` information with a correct value, e.g.,
`platform = "transcriptomics by single-cell RNA-seq"`. You can use the
`SigRepo::updateSignature()` function as follows:

``` r
# Revise the metadata object with new platform = "transcriptomics by single-cell RNA-seq"
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
  keywords = "Myc, KO, longevity",
  cutoff_description = NULL,
  author = NULL,
  PMID = 25619689,
  year = 2015,

  # example of customized attributes:
  others = base::list("animal_strain" = "C57BL/6")
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
  dplyr::filter(base::abs(.data$score) > metadata_revised$score_cutoff & .data$adj_p < metadata_revised$adj_p_cutoff) |>
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
# in which we would like to revise the value of platform to "transcriptomics by single-cell RNA-seq"
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
#>  signature_id = '454' has been updated.
```

Let’s look up `signature_name = "Myc_reduce_mice_liver_24m_readme"` and
see if the value of `platform` has been changed.

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

| signature_id | signature_name | organism | direction_type | assay_type | phenotype | platform_name | sample_type | covariates | description | score_cutoff | logfc_cutoff | p_value_cutoff | adj_p_cutoff | cutoff_description | keywords | PMID | year | others | has_difexp | num_of_difexp | num_up_regulated | num_down_regulated | user_name | date_created | visibility | signature_hashkey |
|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|:---|:---|---:|---:|:---|---:|---:|---:|---:|:---|:---|---:|:---|
| 454 | Myc_reduce_mice_liver_24m_readme | Mus musculus | bi-directional | transcriptomics | Myc_reduce | transcriptomics by single-cell RNA-seq | liver | none | mice Myc haploinsufficient (Myc(+/-)) | 5 | NA | NA | 0.05 | NA | Myc, KO, longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> | 1 | 884 | 5 | 10 | montilab | 2025-10-18 00:21:34 | 0 | a10176ce6e727366cf740e2bfb56e2bc |

Finally, remove `signature_name = "Myc_reduce_mice_liver_24m_readme"`
from the database

``` r
# Let's search for signature_name = "Myc_reduce_mice_liver_24m_readme" in the database
signature_tbl <- SigRepo::searchSignature(
  conn_handler = conn_handler, 
  signature_name = "Myc_reduce_mice_liver_24m_readme"
)

# If the signature exists, remove it from the database
if(base::nrow(signature_tbl) > 0){
  SigRepo::deleteSignature(
    conn_handler = conn_handler, 
    signature_id = signature_tbl$signature_id
  )
}
#> Remove difexp belongs to signature_id = '454' from the database.
#> Remove signature_id = '454' from 'signatures' table of the database.
#> Remove features belongs to signature_id = '454' from 'signature_feature_set' table of the database.
#> Remove user access to signature_id = '454' from 'signature_access' table of the database.
#> Remove signature_id = '454' from 'signature_collection_access' table of the database.
#> signature_id = '454' has been removed.
```

# Additional Guides

- [Upload a signature collection to the SigRepo
  database](https://montilab.github.io/SigRepo/articles/collection-tutorials.html)
