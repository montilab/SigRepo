# Signature Collection Tutorial

## Installation

- Using `devtools` package

&nbsp;

    # Load devtools package
    library(devtools)

    # Install SigRepo
    devtools::install_github(repo = 'montilab/SigRepo')

    # Install OmicSignature
    devtools::install_github(repo = 'montilab/OmicSignature')

## Load packages

    # Load tidyverse package
    library(tidyverse)

    # Load SigRepo package
    library(SigRepo)

    # Load OmicSignature package
    library(OmicSignature)

## Create a handler to connect to SigRepo Database

    # Create a connection handler
    conn_handler <- SigRepo::newConnHandler(
      dbname = "sigrepo", 
      host = "sigrepo.org", 
      port = 3306, 
      user = <your_username>, 
      password = <your_password>
    )

## Load signatures

Here, we provided 4 signature objects that came with the package for
demonstrations:

1.  omic_signature_1 (`Myc_reduce_mice_liver_24m_v1`)
2.  omic_signature_2 (`Myc_reduce_mice_liver_24m_v2`)
3.  omic_signature_3 (`Myc_reduce_mice_liver_24m_v3`)
4.  omic_signature_4 (`Myc_reduce_mice_liver_24m_v4`)

``` r
# Read in the signature objects
utils::data("omic_signature_1", package = "SigRepo")
utils::data("omic_signature_2", package = "SigRepo")
utils::data("omic_signature_3", package = "SigRepo")
utils::data("omic_signature_4", package = "SigRepo")
```

## Create an omic collection

Here, we will create a collection with two signatures,
`omic_signature_1` and `omic_signature_1`, provided above. See
`?OmicSignatureCollection()` from `OmicSignature` package for more
details on how to create an omic collection object.

``` r
# Create a metadata object for the collection
metadata <- base::list(
  "collection_name" = "my_collection",
  "description" = "An example of signature collection"
)

# Create an omic collection using OmicSignatureCollection() from OmicSignature package
omic_collection <- OmicSignature::OmicSignatureCollection$new(
  OmicSigList = base::list(omic_signature_1, omic_signature_2),
  metadata = metadata
)
#>   [Success] OmicSignature Collection my_collection created.
```

## Upload a collection to the database

The
[`SigRepo::addCollection()`](https://montilab.github.io/SigRepo/reference/addCollection.md)
function allows users to upload a collection to the database.

**IMPORTANT NOTE:**

- The user `MUST HAVE` an `editor` or `admin` account to use this
  function.
- A collection `MUST BE` an R6 object obtained from
  [`OmicSignature::OmicSignatureCollection()`](https://rdrr.io/pkg/OmicSignature/man/OmicSignatureCollection.html)

``` r
SigRepo::addCollection(
  conn_handler = conn_handler,        # A handler contains user credentials to establish connection to a remote database
  omic_collection = omic_collection,  # An R6 object obtained from OmicSignature::OmicSignatureCollection()
  visibility = FALSE,                 # Whether to make the collection public or private. Default is FALSE.
  return_collection_id = FALSE,       # Whether to return the uploaded collection id. Default is FALSE.
  verbose = TRUE                      # Whether to print diagnostic messages. Default is TRUE.
)
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_v1 created.
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_v2 created.
#>   [Success] OmicSignature Collection my_collection created.
#> Uploading each signature in the collection to the database...
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_v1 created.
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_v2 created.
#> Uploading collection metadata to the database...
#> Adding user to collection access table in the database...
#> Adding signature to the collection access table of the database...
#> Finished uploading.
#> ID of the uploaded collection: 399
```

## Search for a collection in the database

The
[`SigRepo::searchCollection()`](https://montilab.github.io/SigRepo/reference/searchCollection.md)
function allows users to retrieve all collections that are available in
the database.

### Example 1: Search for all collections

``` r
# Get all collections
collection_tbl_1 <- SigRepo::searchCollection(conn_handler = conn_handler)

# WARNINGS: THE COLLECTION TABLE CAN BE LARGE, SO ONLY THE FIRST SIX OBSERVATIONS ARE SHOWN.
if(base::nrow(collection_tbl_1) > 0){
  knitr::kable(
    utils::head(collection_tbl_1), 
    row.names = FALSE
  )
}
```

| collection_id | collection_name | description | user_name | date_created | visibility | collection_hashkey | signature_id | signature_collection_hashkey | signature_name |
|---:|:---|:---|:---|:---|---:|:---|---:|:---|:---|
| 63 | Aging_4mosc1_YAPTAZ_MontiLab2024 | either siYap1 (YAP KD), siWwtr1 (TAZ KD), siYap1 + siWwtr1 (YAP/TAZ KD) or siControl (Control) in 4MOSC1 cells | H_Nikoueian | 2025-10-06 00:46:12 | 0 | 9ccbe9e91eebb32743633d64eed5a37d | 278 | 7edc2de170157fa2d1ea167ae15f1734 | Aging_4mosc1_YAP_KD_vs_Control_MontiLab2024 |
| 63 | Aging_4mosc1_YAPTAZ_MontiLab2024 | either siYap1 (YAP KD), siWwtr1 (TAZ KD), siYap1 + siWwtr1 (YAP/TAZ KD) or siControl (Control) in 4MOSC1 cells | H_Nikoueian | 2025-10-06 00:46:12 | 0 | 9ccbe9e91eebb32743633d64eed5a37d | 279 | c48f5a2d585c74f35c5abee9a8182559 | Aging_4mosc1_TAZ_KD_vs_Control_MontiLab2024 |
| 63 | Aging_4mosc1_YAPTAZ_MontiLab2024 | either siYap1 (YAP KD), siWwtr1 (TAZ KD), siYap1 + siWwtr1 (YAP/TAZ KD) or siControl (Control) in 4MOSC1 cells | H_Nikoueian | 2025-10-06 00:46:12 | 0 | 9ccbe9e91eebb32743633d64eed5a37d | 280 | 9d5cc282ed8d39ab1f15b2338af14cb2 | Aging_4mosc1_YAPTAZ_KD_vs_Control_MontiLab2024 |
| 64 | Aging_4mosc1_vt104_vs_DMSO_Invivo&InVitro_MontiLab2024 | VT104 treatment versus DMSO in tdTomato-4MOSC1 tumors from aged mice and in vitro 4MOSC1 cells | H_Nikoueian | 2025-10-06 00:48:19 | 0 | 1a6f64e2bc303cf9a5d56d6e553e3332 | 281 | 9a44100c280747961774218de69d688c | Aging_4mosc1_vt104_vs_DMSO_Invivo_MontiLab2024 |
| 64 | Aging_4mosc1_vt104_vs_DMSO_Invivo&InVitro_MontiLab2024 | VT104 treatment versus DMSO in tdTomato-4MOSC1 tumors from aged mice and in vitro 4MOSC1 cells | H_Nikoueian | 2025-10-06 00:48:19 | 0 | 1a6f64e2bc303cf9a5d56d6e553e3332 | 282 | 21058d1ceace59afaafa77683f7ad4c0 | Aging_4mosc1_vt104_vs_DMSO_InVitro_MontiLab2024 |
| 65 | Oralcancer_Hs_HSC3_YAPTAZ_DPAGT1_MontiLab2022 | TAZ, YAP, T+Y and DPAGT1 KD experiments in HSC3 HNSCC cancer cells | H_Nikoueian | 2025-10-06 15:15:44 | 0 | 52b08fcc2980cb6a8212e24a6e2e05a2 | 283 | 41e9c697b36c6b038a95c7139e59fde3 | Oralcancer_Hs_HSC3_TAZ_vs_control_MontiLab2022 |

### Example 2: Search for a specific collection by its name, e.g., `collection_name = "my_collection"`.

``` r
collection_tbl_2 <- SigRepo::searchCollection(
  conn_handler = conn_handler, 
  collection_name = "my_collection"
)

if(base::nrow(collection_tbl_2) > 0){
  knitr::kable(
    collection_tbl_2, 
    row.names = FALSE
  )
}
```

| collection_id | collection_name | description | user_name | date_created | visibility | collection_hashkey | signature_id | signature_collection_hashkey | signature_name |
|---:|:---|:---|:---|:---|---:|:---|---:|:---|:---|
| 399 | my_collection | An example of signature collection | montilab | 2026-01-14 23:07:53 | 0 | a7213731b133effec506851ef7cb443f | 414 | d8051852c1835a59049b9a1944882b6e | Myc_reduce_mice_liver_24m_v2 |
| 399 | my_collection | An example of signature collection | montilab | 2026-01-14 23:07:53 | 0 | a7213731b133effec506851ef7cb443f | 615 | f915a8f3e454e0ea25bab98e936a9399 | Myc_reduce_mice_liver_24m_v1 |

## Retrieve a set of collections in the database

The
[`SigRepo::getCollection()`](https://montilab.github.io/SigRepo/reference/getCollection.md)
function allows users to retrieve a set of collections that are
`PUBLICLY` available in the database.

**IMPORTANT NOTE:**

- Users can `ONLY RETRIEVE` a list of collections that are publicly
  available in the database including their own uploaded collections.
- If a collection is `PRIVATE` and belongs to other user in the
  database, users will need to be given an `editor` permission from its
  owner to access, retrieve, and edit their collections.

### Example 1: Get all collections that are publicly available in the database

    # WARNINGS: THE COLLECTION LIST CAN BE LARGE TO DOWNLOAD (NOT RUN)
    collection_list_1 <- SigRepo::getCollection(conn_handler = conn_handler)

### Example 2: Get a specific collection that is publicly available or owned by user in the database, e.g., `collection_name = "my_collection"`

``` r
collection_list_2 <- SigRepo::getCollection(
  conn_handler = conn_handler,
  collection_name = "my_collection"
)
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_v2 created.
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_v1 created.
#>   [Success] OmicSignature Collection my_collection created.
```

## Update a collection metadata

The
[`SigRepo::updateCollectionMetadata()`](https://montilab.github.io/SigRepo/reference/updateCollectionMetadata.md)
function allows users to update the metadata information of a collection
in the database.

**IMPORTANT NOTE:**

- Users `MUST HAVE` an `editor` or `admin` account to use this function.
- Furthermore, users can `ONLY UPDATE` their own uploaded signatures or
  were given an `editor` permission from its owner to access, retrieve,
  and edit their collections.

For example, if you want to change the description of `"my_collection"`
in the database to `"This is the updated description."` You can use the
[`SigRepo::updateCollectionMetadata()`](https://montilab.github.io/SigRepo/reference/updateCollectionMetadata.md)
function as follows:

``` r
# Let's search for my_collection in the database
collection_tbl <- SigRepo::searchCollection(
  conn_handler = conn_handler, 
  collection_name = "my_collection"
)

# If the collection exists, update it with the new description
if(base::nrow(collection_tbl) > 0){
  SigRepo::updateCollectionMetadata(
    conn_handler = conn_handler, 
    collection_id = collection_tbl$collection_id,
    description =  "This is the updated description." 
  )
}
#> collection_id = '399' has been updated.
```

Now, let’s search for `"my_collection"` in the database and see if its
description has been updated.

``` r
# Search for my_collection in the database
collection_tbl <- SigRepo::searchCollection(
  conn_handler = conn_handler, 
  collection_name = "my_collection"
)

# If the collection exists, display the table
if(base::nrow(collection_tbl) > 0){
  knitr::kable(
    collection_tbl,
    row.names = FALSE
  )
}
```

| collection_id | collection_name | description | user_name | date_created | visibility | collection_hashkey | signature_id | signature_collection_hashkey | signature_name |
|---:|:---|:---|:---|:---|---:|:---|---:|:---|:---|
| 399 | my_collection | This is the updated description. | montilab | 2026-01-14 23:07:53 | 0 | a7213731b133effec506851ef7cb443f | 414 | d8051852c1835a59049b9a1944882b6e | Myc_reduce_mice_liver_24m_v2 |
| 399 | my_collection | This is the updated description. | montilab | 2026-01-14 23:07:53 | 0 | a7213731b133effec506851ef7cb443f | 615 | f915a8f3e454e0ea25bab98e936a9399 | Myc_reduce_mice_liver_24m_v1 |

## Add signatures to an existing collection in the database

The
[`SigRepo::addSignatureToCollection()`](https://montilab.github.io/SigRepo/reference/addSignatureToCollection.md)
function allows users to add a set of signatures to an existing
collection in the database.

**IMPORTANT NOTE:**

- Users `MUST HAVE`an `editor` or `admin` account to use this function.
- Furthermore, users can `ONLY UPDATE` their own uploaded signatures or
  were given an `editor` permission from its owner to access, retrieve,
  and edit their collections.

For example, if you wish to create two new signatures
(`"omic_signature_3"`, `"omic_signature_4"`) and add them to
`"my_collection"`in the database.

``` r
# Add omic_signature_3 to the database
signature_id_3 <- SigRepo::addSignature(
  conn_handler = conn_handler, 
  omic_signature = omic_signature_3,
  return_signature_id = TRUE,
  verbose = FALSE
)
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_v3 created.

# Add omic_signature_4 to the database
signature_id_4 <- SigRepo::addSignature(
  conn_handler = conn_handler, 
  omic_signature = omic_signature_4,
  return_signature_id = TRUE,
  verbose = FALSE
)
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_v4 created.

# Search for my_collection in the database
collection_tbl <- SigRepo::searchCollection(
  conn_handler = conn_handler, 
  collection_name = "my_collection",
  verbose = FALSE
)

# If both collection and signatures exist, add its signature ids to collection.
if(base::nrow(collection_tbl) > 0 && base::length(signature_id_3) > 0 && base::length(signature_id_4) > 0){
  SigRepo::addSignatureToCollection(
    conn_handler = conn_handler, 
    collection_id = collection_tbl$collection_id,
    signature_id =  c(signature_id_3, signature_id_4)
  )
}
```

Now, let’s search for `"my_collection"` in the database and see if the
signatures are added to the collection.

``` r
# Search for my_collection in the database
collection_tbl <- SigRepo::searchCollection(
  conn_handler = conn_handler, 
  collection_name = "my_collection"
)

# If collection exists, display the table
if(base::nrow(collection_tbl) > 0){
  knitr::kable(
    collection_tbl,
    row.names = FALSE
  )
}
```

| collection_id | collection_name | description | user_name | date_created | visibility | collection_hashkey | signature_id | signature_collection_hashkey | signature_name |
|---:|:---|:---|:---|:---|---:|:---|---:|:---|:---|
| 399 | my_collection | This is the updated description. | montilab | 2026-01-14 23:07:53 | 0 | a7213731b133effec506851ef7cb443f | 414 | d8051852c1835a59049b9a1944882b6e | Myc_reduce_mice_liver_24m_v2 |
| 399 | my_collection | This is the updated description. | montilab | 2026-01-14 23:07:53 | 0 | a7213731b133effec506851ef7cb443f | 415 | 6aad4f057a4a26af4f1d9515e9460cef | Myc_reduce_mice_liver_24m_v3 |
| 399 | my_collection | This is the updated description. | montilab | 2026-01-14 23:07:53 | 0 | a7213731b133effec506851ef7cb443f | 416 | 205e27ae880cfe0f07dbff3256de2244 | Myc_reduce_mice_liver_24m_v4 |
| 399 | my_collection | This is the updated description. | montilab | 2026-01-14 23:07:53 | 0 | a7213731b133effec506851ef7cb443f | 615 | f915a8f3e454e0ea25bab98e936a9399 | Myc_reduce_mice_liver_24m_v1 |

## Remove a list of signatures from a collection in the database

The
[`SigRepo::removeSignatureFromCollection()`](https://montilab.github.io/SigRepo/reference/removeSignatureFromCollection.md)
function allows users to remove a list of signatures from a collection
in the database.

**IMPORTANT NOTE:**

- Users `MUST HAVE`an `editor` or `admin` access to use this function.
- Furthermore, users can `ONLY UPDATE`their own uploaded signatures or
  were given an `editor` permission from its owner to access, retrieve,
  and edit their collections.

For example, if you wish to remove `"Myc_reduce_mice_liver_24m_v1"` from
`"my_collection"`in the database

``` r
# Lets' check if signature = Myc_reduce_mice_liver_24m_v1 is in my_collection
collection_tbl <- SigRepo::searchCollection(
  conn_handler = conn_handler, 
  collection_name = "my_collection",
  signature_name = "Myc_reduce_mice_liver_24m_v1"
)

# If signature collection combo exists, remove the signature.
if(base::nrow(collection_tbl) > 0){
  SigRepo::removeSignatureFromCollection(
    conn_handler = conn_handler, 
    collection_id = collection_tbl$collection_id,
    signature_id = collection_tbl$signature_id
  )
}
#> Removing signature_id = '615' from collection_id = '399' completed.
```

Now, let’s search for `"my_collection"` in the database and see if
signatures have been removed from the collection.

``` r
# Search for my_collection in the database
collection_tbl <- SigRepo::searchCollection(
  conn_handler = conn_handler, 
  collection_name = "my_collection"
)

# If collection exists, display the table
if(base::nrow(collection_tbl) > 0){
  knitr::kable(
    collection_tbl,
    row.names = FALSE
  )
}
```

| collection_id | collection_name | description | user_name | date_created | visibility | collection_hashkey | signature_id | signature_collection_hashkey | signature_name |
|---:|:---|:---|:---|:---|---:|:---|---:|:---|:---|
| 399 | my_collection | This is the updated description. | montilab | 2026-01-14 23:07:53 | 0 | a7213731b133effec506851ef7cb443f | 414 | d8051852c1835a59049b9a1944882b6e | Myc_reduce_mice_liver_24m_v2 |
| 399 | my_collection | This is the updated description. | montilab | 2026-01-14 23:07:53 | 0 | a7213731b133effec506851ef7cb443f | 415 | 6aad4f057a4a26af4f1d9515e9460cef | Myc_reduce_mice_liver_24m_v3 |
| 399 | my_collection | This is the updated description. | montilab | 2026-01-14 23:07:53 | 0 | a7213731b133effec506851ef7cb443f | 416 | 205e27ae880cfe0f07dbff3256de2244 | Myc_reduce_mice_liver_24m_v4 |

## Delete the whole collection from the database

The
[`SigRepo::deleteCollection()`](https://montilab.github.io/SigRepo/reference/deleteCollection.md)
function allows users to delete a collection from the database.

**IMPORTANT NOTE:**

- Users `MUST HAVE` an `editor` or `admin` account to use this function.
- Furthermore, users can `ONLY DELETE` their own uploaded collections or
  were given an `editor` permission from other users in the database to
  access and delete their collection.
- A collection can `ONLY BE REMOVED OR DELETED` one at a time.

``` r
# Search for my_collection in the database 
collection_tbl <- SigRepo::searchCollection(
  conn_handler = conn_handler, 
  collection_name = "my_collection"
)

# If the collection exists, remove it from the database
if(base::nrow(collection_tbl) > 0){
  SigRepo::deleteCollection(
    conn_handler = conn_handler, 
    collection_id = collection_tbl$collection_id
  )
}
#> Remove collection_id = '399' from 'collection' table of the database.
#> Remove collection_id = '399' from 'collection_access' table of the database.
#> Remove signatures belongs to collection_id = '399' from 'signature_collection_access' table of the database.
#> collection_id = '399' has been removed.
```

Finally, let’s search for `"my_collection"` in the database and see if
the collection has been removed.

``` r
collection_tbl <- SigRepo::searchCollection(
  conn_handler = conn_handler, 
  collection_name = "my_collection"
)
#> There is no collection returned from the search parameters.
```
