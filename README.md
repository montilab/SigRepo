
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
<a href="https://github.com/montilab/SigRepo_Server">here</a>.

To upload and download signatures — and to fully utilize the
functionalities offered by the SigRepo package — signatures and
signature collections must be represented as specific R6 objects. You
can create these objects using our proprietary package,
<a href="https://github.com/montilab/OmicSignature">OmicSignature</a>.

Click on each link below for more information:

- [Overview of the object
  structure](https://montilab.github.io/OmicSignature/articles/ObjectStructure.html)
- [Create an OmicSignature
  (OmS)](https://montilab.github.io/OmicSignature/articles/CreateOmS.html)
- [Create an OmicSignatureCollection
  (OmSC)](https://montilab.github.io/OmicSignature/articles/CreateOmSC.html)

Below, we walk you through few essential steps to install the `SigRepo`
package, and to store, retrieve, and interact with a list of signatures
stored in an <a href="https://sigrepo.org">already deployed SigRepo
server</a>.

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
#> now dyn.load("/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/curl/libs/curl.so") ...
#> Adding signature owner to the signature access table of the database...
#> Adding signature feature set to the database...
#> Finished uploading.
#> ID of the uploaded signature: 309
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

| signature_id | signature_name | organism | direction_type | assay_type | phenotype | platform_name | sample_type | covariates | description | score_cutoff | logfc_cutoff | p_value_cutoff | adj_p_cutoff | cutoff_description | keywords | PMID | year | others | has_difexp | num_of_difexp | num_up_regulated | num_down_regulated | user_name | date_created | visibility | signature_hashkey |
|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|:---|:---|---:|---:|:---|---:|---:|---:|---:|:---|:---|---:|:---|
| 179 | Myc_reduce_mice_liver_24m_readme | Mus musculus | bi-directional | transcriptomics | Myc_reduce | transcriptomics by single-cell RNA-seq | liver | none | mice Myc haploinsufficient (Myc(+/-)) | 5.0 | NA | NA | 0.05 | NA | Myc,KO,longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> | 1 | 884 | 5 | 10 | root | 2025-09-29 20:26:38 | 0 | 7e24a413c1287f840d492b2963c263a5 |
| 213 | Aging_Hs_HNSC_RNASeq_TCGA_ACC_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative ACC aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,ACC,Aging | NA | 2025 | NA | 1 | 32402 | 18031 | 14371 | H_Nikoueian | 2025-10-03 15:01:24 | 0 | 37b8bb25907349dfaa1b38fecc9e872b |
| 214 | Aging_Hs_HNSC_RNASeq_TCGA_BLCA_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative BLCA aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,BLCA,Aging | NA | 2025 | NA | 1 | 39689 | 22346 | 17343 | H_Nikoueian | 2025-10-03 15:01:35 | 0 | 28d8ba3d012ecbc6a080996fc2dcd66e |
| 215 | Aging_Hs_HNSC_RNASeq_TCGA_BRCA_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative BRCA aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,BRCA,Aging | NA | 2025 | NA | 1 | 42011 | 26478 | 15533 | H_Nikoueian | 2025-10-03 15:01:49 | 0 | 9ae65c31c82b6edc15819fb7f8cd0640 |
| 216 | Aging_Hs_HNSC_RNASeq_TCGA_CESC_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative CESC aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,CESC,Aging | NA | 2025 | NA | 1 | 38521 | 21746 | 16775 | H_Nikoueian | 2025-10-03 15:02:05 | 0 | e487cfd606e3d4e2214a8e4435b424b8 |
| 217 | Aging_Hs_HNSC_RNASeq_TCGA_COAD_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative COAD aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,COAD,Aging | NA | 2025 | NA | 1 | 38857 | 22186 | 16671 | H_Nikoueian | 2025-10-03 15:02:20 | 0 | afb09a078a63c9342530d2a119698085 |
| 218 | Aging_Hs_HNSC_RNASeq_TCGA_ESCA_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative ESCA aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,ESCA,Aging | NA | 2025 | NA | 1 | 42053 | 15816 | 26237 | H_Nikoueian | 2025-10-03 15:02:33 | 0 | 3275eeda945640a32d963a55d22143a9 |
| 219 | Aging_Hs_HNSC_RNASeq_TCGA_GBM_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative GBM aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,GBM,Aging | NA | 2025 | NA | 1 | 37144 | 18552 | 18592 | H_Nikoueian | 2025-10-03 15:02:48 | 0 | 77557b8625c74a41044e590288c8ef69 |
| 220 | Aging_Hs_HNSC_RNASeq_TCGA_HNSC_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative HNSC aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,HNSC,Aging | NA | 2025 | NA | 1 | 41179 | 24381 | 16798 | H_Nikoueian | 2025-10-03 15:03:02 | 0 | 9da76d9edd2ca3bf46c1fa3c493ea3b6 |
| 221 | Aging_Hs_HNSC_RNASeq_TCGA_KICH_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative KICH aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,KICH,Aging | NA | 2025 | NA | 1 | 32223 | 18780 | 13443 | H_Nikoueian | 2025-10-03 15:03:16 | 0 | c94bd90e154ca69222e7da61d3daff76 |
| 222 | Aging_Hs_HNSC_RNASeq_TCGA_KIRC_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative KIRC aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,KIRC,Aging | NA | 2025 | NA | 1 | 42175 | 22205 | 19970 | H_Nikoueian | 2025-10-03 15:03:29 | 0 | b55646a13f540cb8cecd82b821bc7701 |
| 223 | Aging_Hs_HNSC_RNASeq_TCGA_KIRP_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative KIRP aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,KIRP,Aging | NA | 2025 | NA | 1 | 39104 | 15642 | 23462 | H_Nikoueian | 2025-10-03 15:03:44 | 0 | bf74c7cdb32792fe61259005ac842fe8 |
| 224 | Aging_Hs_HNSC_RNASeq_TCGA_LAML_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative LAML aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,LAML,Aging | NA | 2025 | NA | 1 | 39908 | 22420 | 17488 | H_Nikoueian | 2025-10-03 15:04:00 | 0 | 0674f5de3614bb09d2326ab333af321d |
| 225 | Aging_Hs_HNSC_RNASeq_TCGA_LGG_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative LGG aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,LGG,Aging | NA | 2025 | NA | 1 | 40344 | 21479 | 18865 | H_Nikoueian | 2025-10-03 15:04:15 | 0 | d472b2d95287c3a82d3c02c859533231 |
| 226 | Aging_Hs_HNSC_RNASeq_TCGA_LIHC_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative LIHC aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,LIHC,Aging | NA | 2025 | NA | 1 | 37583 | 20737 | 16846 | H_Nikoueian | 2025-10-03 15:04:31 | 0 | 8d466fb2e346c5b4878737281db882eb |
| 227 | Aging_Hs_HNSC_RNASeq_TCGA_LUAD_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative LUAD aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,LUAD,Aging | NA | 2025 | NA | 1 | 40628 | 18797 | 21831 | H_Nikoueian | 2025-10-03 15:04:45 | 0 | 2e59618581a3cbe33b709b10f45084ac |
| 228 | Aging_Hs_HNSC_RNASeq_TCGA_LUSC_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative LUSC aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,LUSC,Aging | NA | 2025 | NA | 1 | 40658 | 16861 | 23797 | H_Nikoueian | 2025-10-03 15:05:03 | 0 | f0cca4fce80834108893af0e1edcbfd8 |
| 229 | Aging_Hs_HNSC_RNASeq_TCGA_MESO_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative MESO aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,MESO,Aging | NA | 2025 | NA | 1 | 33997 | 19120 | 14877 | H_Nikoueian | 2025-10-03 15:05:19 | 0 | 5d9375db596b7b2cac9e53c08e172daa |
| 230 | Aging_Hs_HNSC_RNASeq_TCGA_OV_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative OV aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,OV,Aging | NA | 2025 | NA | 1 | 41240 | 24527 | 16713 | H_Nikoueian | 2025-10-03 15:05:34 | 0 | 1573638c0e007d848f7b41945f0df93d |
| 231 | Aging_Hs_HNSC_RNASeq_TCGA_PAAD_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative PAAD aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,PAAD,Aging | NA | 2025 | NA | 1 | 36728 | 19693 | 17035 | H_Nikoueian | 2025-10-03 15:05:51 | 0 | 470699e3913b41b862b37623efb07d65 |
| 232 | Aging_Hs_HNSC_RNASeq_TCGA_PCPG_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative PCPG aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,PCPG,Aging | NA | 2025 | NA | 1 | 36048 | 20368 | 15680 | H_Nikoueian | 2025-10-03 15:06:07 | 0 | efc5fc45799b0725b46c041ee57f46ee |
| 233 | Aging_Hs_HNSC_RNASeq_TCGA_PRAD_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative PRAD aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,PRAD,Aging | NA | 2025 | NA | 1 | 39996 | 24223 | 15773 | H_Nikoueian | 2025-10-03 15:06:22 | 0 | 9969b7e17f36fb5023c03f9351d7765b |
| 234 | Aging_Hs_HNSC_RNASeq_TCGA_READ_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative READ aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,READ,Aging | NA | 2025 | NA | 1 | 35263 | 14245 | 21018 | H_Nikoueian | 2025-10-03 15:06:39 | 0 | 90e7d21ef8ca650e441ebc42a35298ab |
| 235 | Aging_Hs_HNSC_RNASeq_TCGA_SARC_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative SARC aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,SARC,Aging | NA | 2025 | NA | 1 | 38311 | 21114 | 17197 | H_Nikoueian | 2025-10-03 15:06:55 | 0 | 17465b10ecfd31a23b439ce999aac019 |
| 236 | Aging_Hs_HNSC_RNASeq_TCGA_SKCM_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative SKCM aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,SKCM,Aging | NA | 2025 | NA | 1 | 34896 | 18534 | 16362 | H_Nikoueian | 2025-10-03 15:07:12 | 0 | 3baa8b3ab4c693bc3dabb11b6a09ff43 |
| 237 | Aging_Hs_HNSC_RNASeq_TCGA_STAD_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative STAD aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,STAD,Aging | NA | 2025 | NA | 1 | 43380 | 18386 | 24994 | H_Nikoueian | 2025-10-03 15:07:29 | 0 | d88954b724158ac9ffee151dcf130cb9 |
| 238 | Aging_Hs_HNSC_RNASeq_TCGA_TGCT_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative TGCT aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,TGCT,Aging | NA | 2025 | NA | 1 | 37623 | 19604 | 18019 | H_Nikoueian | 2025-10-03 15:07:48 | 0 | ee29ff94b3f5921bba8322d12c97a9c4 |
| 239 | Aging_Hs_HNSC_RNASeq_TCGA_THCA_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative THCA aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,THCA,Aging | NA | 2025 | NA | 1 | 39686 | 21642 | 18044 | H_Nikoueian | 2025-10-03 15:08:04 | 0 | 315966b4024e8b490d670a1eb69248c5 |
| 240 | Aging_Hs_HNSC_RNASeq_TCGA_THYM_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative THYM aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,THYM,Aging | NA | 2025 | NA | 1 | 35788 | 18114 | 17674 | H_Nikoueian | 2025-10-03 15:08:23 | 0 | 43de69742bba6cb75562cd6aa5e05334 |
| 241 | Aging_Hs_HNSC_RNASeq_TCGA_UCEC_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative UCEC aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,UCEC,Aging | NA | 2025 | NA | 1 | 41238 | 24387 | 16851 | H_Nikoueian | 2025-10-03 15:08:40 | 0 | 71a597965c67752902b55f1213568f3b |
| 242 | Aging_Hs_HNSC_RNASeq_TCGA_UCS_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative UCS aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,UCS,Aging | NA | 2025 | NA | 1 | 33686 | 15236 | 18450 | H_Nikoueian | 2025-10-03 15:09:00 | 0 | 6fce3dba51330f1a75d489faeff25cf7 |
| 243 | Aging_Hs_HNSC_RNASeq_TCGA_UVM_MontiLab2025 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by bulk RNA-seq | unknown | tumor purity, race, gender | TCGA HPV-negative UVM aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,UVM,Aging | NA | 2025 | NA | 1 | 30412 | 16959 | 13453 | H_Nikoueian | 2025-10-03 15:09:18 | 0 | 436e2bf46f923d18a2e5af42ea65f230 |
| 245 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_BLCA_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA BLCA RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,BLCA,RPPA,Aging | NA | 2025 | NA | 1 | 149 | 79 | 70 | H_Nikoueian | 2025-10-03 18:16:41 | 0 | 093c629fe09b8f82f0fb88c877c4c3be |
| 246 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_BRCA_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA BRCA RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,BRCA,RPPA,Aging | NA | 2025 | NA | 1 | 155 | 78 | 77 | H_Nikoueian | 2025-10-03 18:16:46 | 0 | d75e8ffde24c43d147633c2a4db951b7 |
| 247 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_CESC_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA CESC RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,CESC,RPPA,Aging | NA | 2025 | NA | 1 | 151 | 86 | 65 | H_Nikoueian | 2025-10-03 18:16:51 | 0 | c7a23e1c09e30e00e354d313a17bd05f |
| 248 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_COAD_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA COAD RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,COAD,RPPA,Aging | NA | 2025 | NA | 1 | 154 | 84 | 70 | H_Nikoueian | 2025-10-03 18:16:55 | 0 | 623afaa1964d74d4b08956af67453505 |
| 249 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_ESCA_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA ESCA RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,ESCA,RPPA,Aging | NA | 2025 | NA | 1 | 151 | 70 | 81 | H_Nikoueian | 2025-10-03 18:16:59 | 0 | 7fe1bf94318435fdad234d0b004f5eef |
| 250 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_GBM_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA GBM RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,GBM,RPPA,Aging | NA | 2025 | NA | 1 | 154 | 65 | 89 | H_Nikoueian | 2025-10-03 18:17:04 | 0 | b63199956b6a988fbed554290e809d2c |
| 251 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_HNSC_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA HNSC RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,HNSC,RPPA,Aging | NA | 2025 | NA | 1 | 150 | 78 | 72 | H_Nikoueian | 2025-10-03 18:17:09 | 0 | 0011a8cde61b1b738421f75677492976 |
| 252 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_KICH_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA KICH RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,KICH,RPPA,Aging | NA | 2025 | NA | 1 | 151 | 81 | 70 | H_Nikoueian | 2025-10-03 18:17:14 | 0 | 459222619897c45a6649db54f919f944 |
| 253 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_KIRC_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA KIRC RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,KIRC,RPPA,Aging | NA | 2025 | NA | 1 | 76 | 39 | 37 | H_Nikoueian | 2025-10-03 18:17:18 | 0 | 01ee40787e5ecf7fac03fc559901e3f9 |
| 254 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_KIRP_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA KIRP RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,KIRP,RPPA,Aging | NA | 2025 | NA | 1 | 151 | 72 | 79 | H_Nikoueian | 2025-10-03 18:17:23 | 0 | 643a2fcf06201a4e1718a2be3d71bae7 |
| 255 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_LGG_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA LGG RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,LGG,RPPA,Aging | NA | 2025 | NA | 1 | 150 | 77 | 73 | H_Nikoueian | 2025-10-03 18:17:27 | 0 | 3cd91a0685713f3084fc0b0e080a8cab |
| 256 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_LIHC_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA LIHC RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,LIHC,RPPA,Aging | NA | 2025 | NA | 1 | 152 | 71 | 81 | H_Nikoueian | 2025-10-03 18:17:32 | 0 | 408e8ee694681cf4d98fdcbd70362c89 |
| 257 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_LUAD_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA LUAD RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,LUAD,RPPA,Aging | NA | 2025 | NA | 1 | 149 | 60 | 89 | H_Nikoueian | 2025-10-03 18:17:37 | 0 | 960a75f61496b01ecb453c3415c530d6 |
| 258 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_LUSC_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA LUSC RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,LUSC,RPPA,Aging | NA | 2025 | NA | 1 | 149 | 53 | 96 | H_Nikoueian | 2025-10-03 18:17:41 | 0 | ad70e5e4bf75d8d3c8510d8eb1f19391 |
| 259 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_MESO_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA MESO RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,MESO,RPPA,Aging | NA | 2025 | NA | 1 | 152 | 103 | 49 | H_Nikoueian | 2025-10-03 18:17:46 | 0 | b2f2f45a196a83272007490ca6921860 |
| 260 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_OV_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA OV RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,OV,RPPA,Aging | NA | 2025 | NA | 1 | 156 | 72 | 84 | H_Nikoueian | 2025-10-03 18:17:50 | 0 | d1ae25b432058ada3292c0ed2487c1ac |
| 261 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_PAAD_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA PAAD RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,PAAD,RPPA,Aging | NA | 2025 | NA | 1 | 152 | 82 | 70 | H_Nikoueian | 2025-10-03 18:17:55 | 0 | 765bbaa7fd265fd9dfb99f0ccc8c7e1d |
| 262 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_PCPG_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA PCPG RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,PCPG,RPPA,Aging | NA | 2025 | NA | 1 | 151 | 69 | 82 | H_Nikoueian | 2025-10-03 18:18:00 | 0 | 2e9c02974adce02fd57e85a387b88529 |
| 263 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_PRAD_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA PRAD RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,PRAD,RPPA,Aging | NA | 2025 | NA | 1 | 150 | 62 | 88 | H_Nikoueian | 2025-10-03 18:18:04 | 0 | 669c4a71e729c037aeb5c2ba331ba6ab |
| 264 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_READ_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA READ RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,READ,RPPA,Aging | NA | 2025 | NA | 1 | 154 | 68 | 86 | H_Nikoueian | 2025-10-03 18:18:09 | 0 | e07b4409c2eea3511f7a896326750d58 |
| 265 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_SARC_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA SARC RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,SARC,RPPA,Aging | NA | 2025 | NA | 1 | 151 | 78 | 73 | H_Nikoueian | 2025-10-03 18:18:13 | 0 | b81eb49fb0e9357c362f9034a00ac8d5 |
| 266 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_SKCM_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA SKCM RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,SKCM,RPPA,Aging | NA | 2025 | NA | 1 | 150 | 86 | 64 | H_Nikoueian | 2025-10-03 18:18:18 | 0 | ce3703e9abfa911e58b83ec47c9d5206 |
| 267 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_STAD_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA STAD RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,STAD,RPPA,Aging | NA | 2025 | NA | 1 | 151 | 66 | 85 | H_Nikoueian | 2025-10-03 18:18:22 | 0 | a0da3016e6a1715955ed18b1b4789bba |
| 268 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_TGCT_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA TGCT RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,TGCT,RPPA,Aging | NA | 2025 | NA | 1 | 150 | 90 | 60 | H_Nikoueian | 2025-10-03 18:18:27 | 0 | f270cdf689d7392102566ff5f63503bc |
| 269 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_THCA_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA THCA RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,THCA,RPPA,Aging | NA | 2025 | NA | 1 | 151 | 77 | 74 | H_Nikoueian | 2025-10-03 18:18:31 | 0 | d0aed0c5db7f4686b7f852e85a41f22c |
| 270 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_THYM_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA THYM RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,THYM,RPPA,Aging | NA | 2025 | NA | 1 | 150 | 78 | 72 | H_Nikoueian | 2025-10-03 18:18:36 | 0 | de3118dc26a0b55bce39fe7a8251a4e6 |
| 271 | Aging_Hs_HNSC_RPPA_Proteins_TCGA_UCEC_MontiLab2025 | Homo sapiens | bi-directional | proteomics | Aging | proteomics by antibody or aptamer | unknown | see original limma model | TCGA UCEC RPPA (limma) aging signature | 0.0 | NA | NA | 1.00 | NA | TCGA,UCEC,RPPA,Aging | NA | 2025 | NA | 1 | 155 | 66 | 89 | H_Nikoueian | 2025-10-03 18:18:40 | 0 | e22d7445efbf3025ad54bc240cc100b8 |
| 275 | Aging_4mosc1_Old_vs_Young_MontiLab2024 | Mus musculus | bi-directional | transcriptomics | old vs. young | transcriptomics by bulk RNA-seq | oral squamous cell carcinoma cell line | none | Profiles of the transcriptional differences between young and old OSCC 4MOSC1 mouse models | 1.0 | 2.0 | NA | 0.05 | NA | age,tumor,OSCC | NA | 2024 | NA | 1 | 15896 | 211 | 527 | H_Nikoueian | 2025-10-06 00:44:53 | 0 | 89bf83c2db5af0236a28a19fbb328ce9 |
| 277 | Aging_4mosc1_arecoline_vs_PBS_MontiLab2024 | Mus musculus | bi-directional | transcriptomics | arecoline vs. PBS | transcriptomics by bulk RNA-seq | oral squamous cell carcinoma cell line | none | Profiles of the transcriptional response to arecoline in young OSCC 4MOSC1 mouse models | 1.5 | 1.5 | NA | 0.05 | NA | oralcancer,OSCC | NA | 2024 | NA | 1 | 14563 | 80 | 13 | H_Nikoueian | 2025-10-06 00:45:48 | 0 | e3cd8b22dfd04d2d960e34b99886085d |
| 278 | Aging_4mosc1_YAP_KD_vs_Control_MontiLab2024 | Mus musculus | bi-directional | transcriptomics | YAP KD vs Control | transcriptomics by bulk RNA-seq | oral squamous cell carcinoma cell line | none | Bulk signature comparing 4MOSC1 cells treated with siYap1 vs. siControl | 1.5 | 1.5 | NA | 0.01 | NA | oralcancer,aging,tumor,OSCC | NA | 2024 | NA | 1 | 12945 | 52 | 10 | H_Nikoueian | 2025-10-06 00:45:58 | 0 | 10454591737c219a1b19eed676e8186d |
| 279 | Aging_4mosc1_TAZ_KD_vs_Control_MontiLab2024 | Mus musculus | bi-directional | transcriptomics | TAZ KD vs Control | transcriptomics by bulk RNA-seq | oral squamous cell carcinoma cell line | none | Bulk signature comparing 4MOSC1 cells treated with siWwtr1 vs. siControl | 1.5 | 1.5 | NA | 0.01 | NA | oralcancer,aging,tumor,OSCC | NA | 2024 | NA | 1 | 13220 | 32 | 6 | H_Nikoueian | 2025-10-06 00:46:03 | 0 | 035e86580ef7597bedc81ed929c2e2f7 |
| 280 | Aging_4mosc1_YAPTAZ_KD_vs_Control_MontiLab2024 | Mus musculus | bi-directional | transcriptomics | YAPTAZ KD vs Control | transcriptomics by bulk RNA-seq | oral squamous cell carcinoma cell line | none | Bulk signature comparing 4MOSC1 cells treated with siYap1+siWwtr1 vs. siControl | 1.5 | 1.5 | NA | 0.01 | NA | oralcancer,aging,tumor,OSCC | NA | 2024 | NA | 1 | 13771 | 612 | 97 | H_Nikoueian | 2025-10-06 00:46:07 | 0 | 014d8603cd11d8f470981fe33327ec83 |
| 281 | Aging_4mosc1_vt104_vs_DMSO_Invivo_MontiLab2024 | Mus musculus | bi-directional | transcriptomics | vt104 vs. DMSO InVivo | transcriptomics by bulk RNA-seq | oral squamous cell carcinoma cell line | none | Profiles of the transcriptional response to vt104 in old OSCC 4MOSC1 mouse models | 1.5 | 1.5 | NA | 0.05 | NA | oralcancer,OSCC | NA | 2024 | NA | 1 | 15047 | 13 | 67 | H_Nikoueian | 2025-10-06 00:48:09 | 0 | 54f331c7fbca1c3a6b349dfa9b40f0db |
| 282 | Aging_4mosc1_vt104_vs_DMSO_InVitro_MontiLab2024 | Mus musculus | bi-directional | transcriptomics | vt104 vs. DMSO InVitro | transcriptomics by bulk RNA-seq | oral squamous cell carcinoma cell line | none | Profiles of the transcriptional response to vt104 in 4MOSC1 cells | 1.5 | 1.5 | NA | 0.05 | NA | oralcancer,aging,tumor | NA | 2024 | NA | 1 | 13596 | 411 | 561 | H_Nikoueian | 2025-10-06 00:48:14 | 0 | 7f6245cd2e90ae5858c15046474bc68a |
| 283 | Oralcancer_Hs_HSC3_TAZ_vs_control_MontiLab2022 | Homo sapiens | bi-directional | transcriptomics | TAZ KD | transcriptomics by array | HSC-3 cell | none | Profiles of the transcriptional response of TAZ in Head and Neck squamous cell carcinoma | 2.0 | 2.0 | NA | 0.05 | NA | oralcancer,hnscc | NA | NA | NA | 1 | 10711 | 2 | 2 | H_Nikoueian | 2025-10-06 15:15:25 | 0 | a6345a07874fd64b6e0105b3beeb096a |
| 284 | Oralcancer_Hs_HSC3_YAP_vs_control_MontiLab2022 | Homo sapiens | bi-directional | transcriptomics | YAP KD | transcriptomics by array | HSC-3 cell | none | Profiles of the transcriptional response of YAP in Head and Neck squamous cell carcinoma | 2.0 | 2.0 | NA | 0.05 | NA | oralcancer,hnscc | NA | NA | NA | 1 | 10711 | 58 | 43 | H_Nikoueian | 2025-10-06 15:15:30 | 0 | 37e5fd38cf37ae29580406454da80007 |
| 285 | Oralcancer_Hs_HSC3_TAZ+YAP_vs_control_MontiLab2022 | Homo sapiens | bi-directional | transcriptomics | TAZ+YAP KD | transcriptomics by array | HSC-3 cell | none | Profiles of the transcriptional response of TAZ+YAP in Head and Neck squamous cell carcinoma | 2.0 | 2.0 | NA | 0.05 | NA | oralcancer,hnscc | NA | NA | NA | 1 | 10711 | 12 | 31 | H_Nikoueian | 2025-10-06 15:15:35 | 0 | 0dd7052e044c0ec993d3d36866139eb6 |
| 286 | Oralcancer_Hs_HSC3_DPAGT1_vs_control_MontiLab2022 | Homo sapiens | bi-directional | transcriptomics | DPAGT1 KD | transcriptomics by array | HSC-3 cell | none | Profiles of the transcriptional response of DPAGT1 knock-down in Head and Neck squamous cell carcinoma | 2.0 | 2.0 | NA | 0.05 | NA | oralcancer,hnscc | NA | NA | NA | 1 | 10711 | 8 | 10 | H_Nikoueian | 2025-10-06 15:15:40 | 0 | 6e09bc03e40bf04b6e0baf5992486953 |
| 287 | Myc_reduce_mice_liver_24m_v1 | Mus musculus | bi-directional | transcriptomics | Myc_reduce | transcriptomics by array | liver | none | mice Myc haploinsufficient (Myc(+/-)) | 5.0 | NA | NA | 0.05 | NA | Myc,KO,longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> | 1 | 884 | 5 | 10 | root | 2025-10-06 15:29:43 | 0 | eacd0935d97847d56cb717455b0e48a3 |
| 303 | HNSCC_Hs_OralMucosa_OSCC_vs_Control_MontiLab2023 | Homo sapiens | bi-directional | transcriptomics | OSCC vs healthy control mucosa | transcriptomics by bulk RNA-seq | oral mucosa | none | Transcriptomic signature of oral squamous cell carcinoma (OSCC) compared with normal oral mucosa. | 1.5 | 1.5 | NA | 0.05 | NA | oral,RNA-seq | 37542347 | 2023 | NA | 1 | 4040 | 408 | 658 | H_Nikoueian | 2025-10-06 18:07:18 | 0 | 9cce23aa76b7ba8f3c105fbc3949bb97 |
| 304 | HNSCC_Hs_OralMucosa_Dysplasia_vs_Control_MontiLab2023 | Homo sapiens | bi-directional | transcriptomics | Dysplasia vs healthy control mucosa | transcriptomics by bulk RNA-seq | oral mucosa | none | Differential expression profile of dysplastic premalignant lesions versus normal oral mucosa. | 1.5 | 1.5 | NA | 0.05 | NA | oral,RNA-seq | 37542347 | 2023 | NA | 1 | 3173 | 293 | 270 | H_Nikoueian | 2025-10-06 18:07:24 | 0 | 5b363d448e20bcab33cebebace878acf |
| 305 | HNSCC_Hs_OralMucosa_HkNR_vs_Control_MontiLab2023 | Homo sapiens | bi-directional | transcriptomics | Hyperkeratosis vs healthy control mucosa | transcriptomics by bulk RNA-seq | oral mucosa | none | Gene expression changes in hyperkeratosis not reactive (HkNR) compared with normal mucosa. | 1.5 | 1.5 | NA | 0.05 | NA | oral,RNA-seq | 37542347 | 2023 | NA | 1 | 268 | 65 | 106 | H_Nikoueian | 2025-10-06 18:07:30 | 0 | 45a7bc8620f99556f22ce325173a3d6e |
| 306 | HNSCC_Hs_OralMucosa_Dysplasia_vs_OSCC_MontiLab2023 | Homo sapiens | bi-directional | transcriptomics | Dysplasia vs OSCC | transcriptomics by bulk RNA-seq | oral mucosa | none | Transcriptional differences between dysplastic lesions and oral squamous cell carcinoma (OSCC). | 1.5 | 1.5 | NA | 0.05 | NA | oral,RNA-seq | 37542347 | 2023 | NA | 1 | 735 | 41 | 131 | H_Nikoueian | 2025-10-06 18:07:35 | 0 | 80cdd7cb15637d6462e33cd9eb1079ec |
| 307 | HNSCC_Hs_OralMucosa_HkNR_vs_OSCC_MontiLab2023 | Homo sapiens | bi-directional | transcriptomics | Hyperkeratosis vs OSCC | transcriptomics by bulk RNA-seq | oral mucosa | none | Gene expression contrasts between hyperkeratosis not reactive (HkNR) lesions and oral squamous cell carcinoma (OSCC). | 1.5 | 1.5 | NA | 0.05 | NA | oral,RNA-seq | 37542347 | 2023 | NA | 1 | 1903 | 327 | 529 | H_Nikoueian | 2025-10-06 18:07:40 | 0 | ebf16c3cf873f25a9facc82f1a56170c |
| 308 | HNSCC_Hs_OralMucosa_Dysplasia_vs_HkNR_MontiLab2023 | Homo sapiens | bi-directional | transcriptomics | Dysplasia vs Hyperkeratosis | transcriptomics by bulk RNA-seq | oral mucosa | none | Signature distinguishing dysplasia from hyperkeratosis not reactive (HkNR) among premalignant lesions. | 1.5 | 1.5 | NA | 0.05 | NA | oral,RNA-seq | 37542347 | 2023 | NA | 1 | 405 | 205 | 31 | H_Nikoueian | 2025-10-06 18:07:46 | 0 | 24a81f081f5e5591de914f3f114a85df |
| 309 | LLFS_Aging_Gene_2023 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by array | blood | sex,fc,education,percent_intergenic,PC1-4,GRM | NA | 6.0 | NA | NA | 0.01 | NA | human,aging,LLFS | NA | 2023 | NA | 1 | 1000 | 82 | 87 | smonti | 2025-10-06 20:02:49 | 0 | 0db777c61277d03476bf125c23315fa5 |

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

| signature_id | signature_name | organism | direction_type | assay_type | phenotype | platform_name | sample_type | covariates | description | score_cutoff | logfc_cutoff | p_value_cutoff | adj_p_cutoff | cutoff_description | keywords | PMID | year | others | has_difexp | num_of_difexp | num_up_regulated | num_down_regulated | user_name | date_created | visibility | signature_hashkey |
|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|:---|:---|---:|---:|:---|---:|---:|---:|---:|:---|:---|---:|:---|
| 309 | LLFS_Aging_Gene_2023 | Homo sapiens | bi-directional | transcriptomics | Aging | transcriptomics by array | blood | sex,fc,education,percent_intergenic,PC1-4,GRM | NA | 6 | NA | NA | 0.01 | NA | human,aging,LLFS | NA | 2023 | NA | 1 | 1000 | 82 | 87 | smonti | 2025-10-06 20:02:49 | 0 | 0db777c61277d03476bf125c23315fa5 |

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
#> Error in value[[3L]](cond): Error in private$checkSignature(signature, signatureType = metadata$direction_type, : Signature must contain the following columns: probe_id, feature_name, direction
```

## Example 2: Retrieve a specific signature that is publicly available or owned by the user in the database, e.g., **signature_name = “LLFS_Aging_Gene_2023”**

``` r
LLFS_oms <- SigRepo::getSignature(
  conn_handler = conn_handler, 
  signature_name = "LLFS_Aging_Gene_2023"
)
#> Error in value[[3L]](cond): Error in private$checkSignature(signature, signatureType = metadata$direction_type, : Signature must contain the following columns: probe_id, feature_name, direction
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
#> Remove difexp belongs to signature_id = '309' from the database.
#> Remove signature_id = '309' from 'signatures' table of the database.
#> Remove features belongs to signature_id = '309' from 'signature_feature_set' table of the database.
#> Remove user access to signature_id = '309' from 'signature_access' table of the database.
#> Remove signature_id = '309' from 'signature_collection_access' table of the database.
#> signature_id = '309' has been removed.
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
#> Error in `dplyr::filter()`:
#> ℹ In argument: `&...`.
#> Caused by error:
#> ! object 'metadata' not found

# Create signature object 
updated_omic_signature <- OmicSignature::OmicSignature$new(
  metadata = metadata_revised,
  signature = signature,
  difexp = difexp
)
#> Error in signature$probe_id <- NULL: object of type 'closure' is not subsettable
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
#> Error in SigRepo::updateSignature(conn_handler = conn_handler, signature_id = signature_tbl$signature_id, : 
#> User = 'smonti' does not have the permission to update signature_id = '179' in the SigRepo database.
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

| signature_id | signature_name | organism | direction_type | assay_type | phenotype | platform_name | sample_type | covariates | description | score_cutoff | logfc_cutoff | p_value_cutoff | adj_p_cutoff | cutoff_description | keywords | PMID | year | others | has_difexp | num_of_difexp | num_up_regulated | num_down_regulated | user_name | date_created | visibility | signature_hashkey |
|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|:---|:---|---:|---:|:---|---:|---:|---:|---:|:---|:---|---:|:---|
| 179 | Myc_reduce_mice_liver_24m_readme | Mus musculus | bi-directional | transcriptomics | Myc_reduce | transcriptomics by single-cell RNA-seq | liver | none | mice Myc haploinsufficient (Myc(+/-)) | 5 | NA | NA | 0.05 | NA | Myc,KO,longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> | 1 | 884 | 5 | 10 | root | 2025-09-29 20:26:38 | 0 | 7e24a413c1287f840d492b2963c263a5 |

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
#> Error in SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = signature_tbl$signature_id): 
#> User = 'smonti' does not have permission to delete signature_id = '179' from the SigRepo database.
```

# Additional Guides

- [Upload a signature collection to the SigRepo
  database](https://montilab.github.io/SigRepo/articles/collection-tutorials.html)
