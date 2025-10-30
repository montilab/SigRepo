#' Transcriptomic signature: Myc haploinsufficient mouse liver (Myc(+/-), version 3)
#'
#' This dataset contains a transcriptomic signature derived from liver tissue of
#' \emph{Myc} haploinsufficient (Myc(+/-)) mice compared to wild-type (WT) controls.
#' The data represent differential gene expression results obtained using an
#' array-based transcriptomic platform. This dataset corresponds to version 3 of the
#' \code{Myc_reduce_mice_liver_24m} signature series.
#'
#' @format A list (or Signature object) with the following components:
#' \describe{
#'   \item{Metadata}{A named list containing experimental and analytical metadata:
#'     \itemize{
#'       \item \code{adj_p_cutoff}: Adjusted p-value cutoff used for differential expression (0.05).
#'       \item \code{assay_type}: Type of assay used: \code{"transcriptomics"}.
#'       \item \code{description}: Study description: Myc haploinsufficient (Myc(+/-)) mice.
#'       \item \code{direction_type}: Indicates that both up- and down-regulated genes are included (\code{"bi-directional"}).
#'       \item \code{phenotype}: Phenotype: \code{"Myc_reduce"}.
#'       \item \code{sample_type}: Tissue: liver.
#'       \item \code{organism}: Species: \emph{Mus musculus}.
#'       \item \code{platform}: Transcriptomics by array.
#'       \item \code{PMID}: Publication identifier: 25619689.
#'       \item \code{year}: Publication year: 2015.
#'       \item \code{keywords}: Keywords: Myc, KO, longevity.
#'       \item \code{score_cutoff}: Signature score cutoff: 5.
#'       \item \code{signature_name}: Signature name: \code{"Myc_reduce_mice_liver_24m_v3"}.
#'       \item \code{covariates}: Covariates used: none.
#'       \item \code{animal_strain}: Animal strain: C57BL/6.
#'     }
#'   }
#'   \item{Signature}{A summary of sample groups in the study:
#'     \itemize{
#'       \item \code{MYC Reduce}: 5 samples (Myc(+/-) mice).
#'       \item \code{WT}: 10 samples (wild-type mice).
#'     }
#'   }
#'   \item{DifferentialExpressionData}{A numeric data frame or matrix of dimension
#'   \code{884 x 10} containing differential expression statistics for 884 genes
#'   across 10 variables (e.g., logFC, AveExpr, t, P.Value, adj.P.Val, etc.).}
#' }
#'
#' @details
#' The \strong{Myc_reduce_mice_liver_24m_v3} signature represents age-associated
#' differential gene expression in liver tissue from Myc haploinsufficient mice
#' compared to wild-type controls. Reduced Myc dosage has been linked to extended
#' lifespan and altered metabolic pathways in murine models.
#'
#' @source Derived from \href{https://pubmed.ncbi.nlm.nih.gov/25619689/}{PMID: 25619689}.
#'
#' @keywords dataset transcriptomics Myc mouse liver longevity aging
#'
"omic_signature_3"
