# Signature Tutorial

This tutorial will go over how to interact with the core functions of
the **SigRepo** package. Please navigate to the
[README](https://github.com/montilab/SigRepo) on our GitHub repo to
setup an account and create your connection handler in R-Studio.

### Installation

- Using `devtools` package

&nbsp;

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

### Load packages

    # Load tidyverse package
    library(tidyverse)

    # Load SigRepo package
    library(SigRepo)

    # Load OmicSignature package
    library(OmicSignature)

### Create connection handler

    # Create a connection handler
    conn_handler <- SigRepo::newConnHandler(
      dbname = "sigrepo", 
      host = "sigrepo.org", 
      port = 3306, 
      user = <your_username>, 
      password = <your_password>
    )

### Load Signatures

Here, we provide two signature objects that comes with the package for
demonstrations:

1.  LLFS_Aging_Gene_2023
2.  Myc_reduce_mice_liver_24m

### Upload a signature

The
[`SigRepo::addSignature()`](https://montilab.github.io/SigRepo/reference/addSignature.md)
function allows users to upload a signature to the database.

**IMPORTANT NOTE:**

- User `MUST HAVE` an `editor` or `admin` account to use this function.
- A signature `MUST BE` an R6 object obtained from
  [`OmicSignature::OmicSignature()`](https://rdrr.io/pkg/OmicSignature/man/OmicSignature.html)

### **Example 1**: Upload `LLFS_Aging_Gene_2023` signature

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
#> Warning in private$checkMetadata(metadata, signatureType =
#> metadata$direction_type, : Organism is not in the predefined list. Ignore this
#> message if intentional.
#>   [Success] OmicSignature object LLFS_Aging_Gene_2023 created.
#> Uploading signature metadata to the database...
#> Saving difexp to the database...
#> Adding signature owner to the signature access table of the database...
#> Adding signature feature set to the database...
#> Finished uploading.
#> ID of the uploaded signature: 1772
```

### **Example 2**: Upload `Myc_reduce_mice_liver_24m` signature

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
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m created.
#>  You already uploaded a signature with the name = 'Myc_reduce_mice_liver_24m' to the database.
#>      ID of the uploaded signature: 1274
```

### **Example 3**: Create an omic signature using **OmicSignature** package and upload to the database

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
  PMID = "25619689",
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
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_readme created.
#> Uploading signature metadata to the database...
#> Saving difexp to the database...
#> Adding signature owner to the signature access table of the database...
#> Adding signature feature set to the database...
#> Finished uploading.
#> ID of the uploaded signature: 1773
```

## Search for a list of signatures

The
[`SigRepo::searchSignature()`](https://montilab.github.io/SigRepo/reference/searchSignature.md)
function allows users to search for all or a specific set of signatures
that are available in the database. This function is a bit more nuanced
than it might seem. When you search for the signature, you will be given
the metadata of the signature.This does not mean you have access to
download the Signature. The best way to think of this is like a library.
You can browse the catalog of books, but some you might need certain
permissions to access.

### Example 1: Search for all signatures.

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
| 275 | Aging_4mosc1_Old_vs_Young_MontiLab2024 | Mus musculus | bi-directional | transcriptomics | old vs. young | transcriptomics by bulk RNA-seq | oral squamous cell carcinoma cell line | none | Profiles of the transcriptional differences between young and old OSCC 4MOSC1 mouse models | 1.0 | 2.0 | NA | 0.05 | NA | age,tumor,OSCC | NA | 2024 | NA | 1 | 15896 | 211 | 527 | H_Nikoueian | 2025-10-06 00:44:53 | 1 | 89bf83c2db5af0236a28a19fbb328ce9 |
| 277 | Aging_4mosc1_arecoline_vs_PBS_MontiLab2024 | Mus musculus | bi-directional | transcriptomics | arecoline vs. PBS | transcriptomics by bulk RNA-seq | oral squamous cell carcinoma cell line | none | Profiles of the transcriptional response to arecoline in young OSCC 4MOSC1 mouse models | 1.5 | 1.5 | NA | 0.05 | NA | oralcancer,OSCC | NA | 2024 | NA | 1 | 14563 | 80 | 13 | H_Nikoueian | 2025-10-06 00:45:48 | 1 | e3cd8b22dfd04d2d960e34b99886085d |
| 278 | Aging_4mosc1_YAP_KD_vs_Control_MontiLab2024 | Mus musculus | bi-directional | transcriptomics | YAP KD vs Control | transcriptomics by bulk RNA-seq | oral squamous cell carcinoma cell line | none | Bulk signature comparing 4MOSC1 cells treated with siYap1 vs. siControl | 1.5 | 1.5 | NA | 0.01 | NA | oralcancer,aging,tumor,OSCC | NA | 2024 | NA | 1 | 12945 | 52 | 10 | H_Nikoueian | 2025-10-06 00:45:58 | 1 | 10454591737c219a1b19eed676e8186d |
| 279 | Aging_4mosc1_TAZ_KD_vs_Control_MontiLab2024 | Mus musculus | bi-directional | transcriptomics | TAZ KD vs Control | transcriptomics by bulk RNA-seq | oral squamous cell carcinoma cell line | none | Bulk signature comparing 4MOSC1 cells treated with siWwtr1 vs. siControl | 1.5 | 1.5 | NA | 0.01 | NA | oralcancer,aging,tumor,OSCC | NA | 2024 | NA | 1 | 13220 | 32 | 6 | H_Nikoueian | 2025-10-06 00:46:03 | 1 | 035e86580ef7597bedc81ed929c2e2f7 |
| 280 | Aging_4mosc1_YAPTAZ_KD_vs_Control_MontiLab2024 | Mus musculus | bi-directional | transcriptomics | YAPTAZ KD vs Control | transcriptomics by bulk RNA-seq | oral squamous cell carcinoma cell line | none | Bulk signature comparing 4MOSC1 cells treated with siYap1+siWwtr1 vs. siControl | 1.5 | 1.5 | NA | 0.01 | NA | oralcancer,aging,tumor,OSCC | NA | 2024 | NA | 1 | 13771 | 612 | 97 | H_Nikoueian | 2025-10-06 00:46:07 | 1 | 014d8603cd11d8f470981fe33327ec83 |
| 281 | Aging_4mosc1_vt104_vs_DMSO_Invivo_MontiLab2024 | Mus musculus | bi-directional | transcriptomics | vt104 vs. DMSO InVivo | transcriptomics by bulk RNA-seq | oral squamous cell carcinoma cell line | none | Profiles of the transcriptional response to vt104 in old OSCC 4MOSC1 mouse models | 1.5 | 1.5 | NA | 0.05 | NA | oralcancer,OSCC | NA | 2024 | NA | 1 | 15047 | 13 | 67 | H_Nikoueian | 2025-10-06 00:48:09 | 1 | 54f331c7fbca1c3a6b349dfa9b40f0db |

### Example 2: Search for a specific signature, e.g., `signature_name = "LLFS_Aging_Gene_2023"`

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
| 1772 | LLFS_Aging_Gene_2023 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by array | blood | sex,fc,education,percent_intergenic,PC1-4,GRM | NA | 6 | NA | NA | 0.01 | NA | human,aging,LLFS | NA | 2023 | NA | 1 | 1000 | 82 | 87 | montilab | 2026-01-27 16:04:14 | 1 | b35b7c1d387440d474bfcb3cb162c9a6 |

## Retrieve a list of omic signatures

The
[`SigRepo::getSignature()`](https://montilab.github.io/SigRepo/reference/getSignature.md)
function allows users to retrieve a list of omic signature objects that
they are `PUBLICLY` available in the database.

**IMPORTANT NOTE:**

- Users can `ONLY RETRIEVE` a list of signatures that are publicly
  available in the database including their own uploaded signatures.
- If a signature is `PRIVATE` and belongs to other user in the database,
  users will need to be given an `editor` permission from its owner in
  order to access, retrieve, and edit their signatures.

### Example 1: Retrieve all signatures that are publicly available or owned by the user in the database

    # WARNING: THE SIGNATURE LIST CAN BE LARGE TO DOWNLOAD (NOT RUN)
    #signature_list <- SigRepo::getSignature(conn_handler = conn_handler)

### Example 2: Retrieve a specific signature that is publicly available or owned by the user in the database, e.g., `signature_name = "LLFS_Aging_Gene_2023"`

``` r
LLFS_oms <- SigRepo::getSignature(
  conn_handler = conn_handler, 
  signature_name = "LLFS_Aging_Gene_2023"
)
#>   [Success] OmicSignature object LLFS_Aging_Gene_2023 created.
```

## Delete a signature

The
[`SigRepo::deleteSignature()`](https://montilab.github.io/SigRepo/reference/deleteSignature.md)
function allows users to delete a signature from the database.

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
#> Remove difexp belongs to signature_id = '1772' from the database.
#> Remove signature_id = '1772' from 'signatures' table of the database.
#> Remove features belongs to signature_id = '1772' from 'signature_feature_set' table of the database.
#> Remove user access to signature_id = '1772' from 'signature_access' table of the database.
#> Remove signature_id = '1772' from 'signature_collection_access' table of the database.
#> signature_id = '1772' has been removed.
```

## Update a signature

The
[`SigRepo::updateSignature()`](https://montilab.github.io/SigRepo/reference/updateSignature.md)
function allows users to update a specific signature in the SigRepo
database.

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
[`SigRepo::updateSignature()`](https://montilab.github.io/SigRepo/reference/updateSignature.md)
function as follows:

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
  PMID = "25619689",
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
#> signature_id = '1773' has been updated.
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
| 1773 | Myc_reduce_mice_liver_24m_readme | Mus musculus | bi-directional | transcriptomics | Myc_reduce | transcriptomics by single-cell RNA-seq | liver | none | mice Myc haploinsufficient (Myc(+/-)) | 5 | NA | NA | 0.05 | NA | Myc, KO, longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> | 1 | 884 | 5 | 10 | montilab | 2026-01-27 16:04:43 | 0 | a10176ce6e727366cf740e2bfb56e2bc |

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
#> Remove difexp belongs to signature_id = '1773' from the database.
#> Remove signature_id = '1773' from 'signatures' table of the database.
#> Remove features belongs to signature_id = '1773' from 'signature_feature_set' table of the database.
#> Remove user access to signature_id = '1773' from 'signature_access' table of the database.
#> Remove signature_id = '1773' from 'signature_collection_access' table of the database.
#> signature_id = '1773' has been removed.
```
