# Transcriptomic signature: Myc haploinsufficient mouse liver (Myc(+/-), version 3)

This dataset contains a transcriptomic signature derived from liver
tissue of *Myc* haploinsufficient (Myc(+/-)) mice compared to wild-type
(WT) controls. The data represent differential gene expression results
obtained using an array-based transcriptomic platform. This dataset
corresponds to version 3 of the `Myc_reduce_mice_liver_24m` signature
series.

## Usage

``` r
omic_signature_3
```

## Format

A list (or Signature object) with the following components:

- Metadata:

  A named list containing experimental and analytical metadata:

  - `adj_p_cutoff`: Adjusted p-value cutoff used for differential
    expression (0.05).

  - `assay_type`: Type of assay used: `"transcriptomics"`.

  - `description`: Study description: Myc haploinsufficient (Myc(+/-))
    mice.

  - `direction_type`: Indicates that both up- and down-regulated genes
    are included (`"bi-directional"`).

  - `phenotype`: Phenotype: `"Myc_reduce"`.

  - `sample_type`: Tissue: liver.

  - `organism`: Species: *Mus musculus*.

  - `platform`: Transcriptomics by array.

  - `PMID`: Publication identifier: 25619689.

  - `year`: Publication year: 2015.

  - `keywords`: Keywords: Myc, KO, longevity.

  - `score_cutoff`: Signature score cutoff: 5.

  - `signature_name`: Signature name: `"Myc_reduce_mice_liver_24m_v3"`.

  - `covariates`: Covariates used: none.

  - `animal_strain`: Animal strain: C57BL/6.

- Signature:

  A summary of sample groups in the study:

  - `MYC Reduce`: 5 samples (Myc(+/-) mice).

  - `WT`: 10 samples (wild-type mice).

- DifferentialExpressionData:

  A numeric data frame or matrix of dimension `884 x 10` containing
  differential expression statistics for 884 genes across 10 variables
  (e.g., logFC, AveExpr, t, P.Value, adj.P.Val, etc.).

## Source

Derived from [PMID:
25619689](https://pubmed.ncbi.nlm.nih.gov/25619689/).

## Details

The **Myc_reduce_mice_liver_24m_v3** signature represents age-associated
differential gene expression in liver tissue from Myc haploinsufficient
mice compared to wild-type controls. Reduced Myc dosage has been linked
to extended lifespan and altered metabolic pathways in murine models.
