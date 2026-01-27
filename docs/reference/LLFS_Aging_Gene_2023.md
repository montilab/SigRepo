# Transcriptomic signature: Human aging (LLFS cohort, blood, 2023)

This dataset contains a transcriptomic signature derived from peripheral
blood samples of human participants in the Long Life Family Study
(LLFS). The signature captures gene expression differences associated
with biological aging and was generated using an array-based
transcriptomic platform.

## Usage

``` r
LLFS_Aging_Gene_2023
```

## Format

A list (or Signature object) with the following components:

- Metadata:

  A named list containing experimental and analytical metadata:

  - `adj_p_cutoff`: Adjusted p-value cutoff used for differential
    expression (0.01).

  - `assay_type`: Type of assay used: `"transcriptomics"`.

  - `direction_type`: Indicates that both up- and down-regulated genes
    are included (`"bi-directional"`).

  - `phenotype`: Phenotype: `"Aging"`.

  - `organism`: Species: *Homo sapiens*.

  - `sample_type`: Tissue: blood.

  - `platform`: Transcriptomics by array.

  - `year`: Publication or dataset year: 2023.

  - `keywords`: Keywords: human, aging, LLFS.

  - `score_cutoff`: Signature score cutoff: 6.

  - `signature_name`: Signature name: `"LLFS_Aging_Gene_2023"`.

  - `covariates`: Covariates used in the differential expression model:
    sex, fold change (fc), education, percent intergenic, principal
    components 1–4 (PC1–4), and GRM.

- Signature:

  A summary of sample groups in the study:

  - `Group1`: 82 samples.

  - `Group2`: 87 samples.

- DifferentialExpressionData:

  A numeric data frame or matrix of dimension `1000 x 8` containing
  differential expression statistics for 1,000 genes across 8 variables
  (e.g., logFC, SE, p-value, adj.P.Val, etc.).

## Source

Long Life Family Study (LLFS), transcriptomic profiling 2023.

## Details

This signature reflects transcriptomic changes in human peripheral blood
associated with biological aging, derived from the LLFS cohort.
