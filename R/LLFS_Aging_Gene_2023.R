#' Transcriptomic signature: Human aging (LLFS cohort, blood, 2023)
#'
#' This dataset contains a transcriptomic signature derived from peripheral blood
#' samples of human participants in the Long Life Family Study (LLFS). The signature
#' captures gene expression differences associated with biological aging and was
#' generated using an array-based transcriptomic platform.
#'
#' @format A list (or Signature object) with the following components:
#' \describe{
#'   \item{Metadata}{A named list containing experimental and analytical metadata:
#'     \itemize{
#'       \item \code{adj_p_cutoff}: Adjusted p-value cutoff used for differential expression (0.01).
#'       \item \code{assay_type}: Type of assay used: \code{"transcriptomics"}.
#'       \item \code{direction_type}: Indicates that both up- and down-regulated genes are included (\code{"bi-directional"}).
#'       \item \code{phenotype}: Phenotype: \code{"Aging"}.
#'       \item \code{organism}: Species: \emph{Homo sapiens}.
#'       \item \code{sample_type}: Tissue: blood.
#'       \item \code{platform}: Transcriptomics by array.
#'       \item \code{year}: Publication or dataset year: 2023.
#'       \item \code{keywords}: Keywords: human, aging, LLFS.
#'       \item \code{score_cutoff}: Signature score cutoff: 6.
#'       \item \code{signature_name}: Signature name: \code{"LLFS_Aging_Gene_2023"}.
#'       \item \code{covariates}: Covariates used in the differential expression model: 
#'         sex, fold change (fc), education, percent intergenic, principal components 1–4 (PC1–4), and GRM.
#'     }
#'   }
#'   \item{Signature}{A summary of sample groups in the study:
#'     \itemize{
#'       \item \code{Group1}: 82 samples.
#'       \item \code{Group2}: 87 samples.
#'     }
#'   }
#'   \item{DifferentialExpressionData}{A numeric data frame or matrix of dimension
#'     \code{1000 x 8} containing differential expression statistics for 1,000 genes
#'     across 8 variables (e.g., logFC, SE, p-value, adj.P.Val, etc.).}
#' }
#'
#' @details
#' This signature reflects transcriptomic changes in human peripheral blood
#' associated with biological aging, derived from the LLFS cohort.
#'
#' @source Long Life Family Study (LLFS), transcriptomic profiling 2023.
#'
#' @keywords dataset transcriptomics aging human LLFS blood
#'
"LLFS_Aging_Gene_2023"
