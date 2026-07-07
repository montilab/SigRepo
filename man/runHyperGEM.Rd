% Generated manually for runHyperGEM
\name{runHyperGEM}
\alias{runHyperGEM}
\title{Run hypeR.GEM enrichment directly from SigRepo metabolomics signatures}
\usage{
runHyperGEM(
  conn_handler = NULL,
  signature_id = NULL,
  signature_name = NULL,
  omic_signature = NULL,
  genesets,
  reference_key = NULL,
  species = "human",
  directional = TRUE,
  merge = TRUE,
  promiscuous_threshold = 10,
  ensemble_id = FALSE,
  background = NULL,
  method = c("weighted", "unweighted"),
  weighted_by = "one_minus_fdr",
  min_metabolite = 1,
  score_col = "score",
  split_by_group = TRUE,
  split_by_direction = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{conn_handler}{An R object obtained from \code{SigRepo::newConnHandler()}. Required unless \code{omic_signature} is supplied.}

\item{signature_id}{One or more SigRepo signature IDs.}

\item{signature_name}{One or more SigRepo signature names.}

\item{omic_signature}{A single \code{OmicSignature} object or a list of \code{OmicSignature} objects.}

\item{genesets}{A named list of genesets, a \code{hypeR} \code{gsets} object, or a \code{hypeR} \code{rgsets} object.}

\item{reference_key}{Optional metabolite identifier column to use for GEM mapping.}

\item{species}{Species passed to \code{hypeR.GEM::signature2gene()}. Defaults to \code{"human"}.}

\item{directional}{Logical; whether to use directional mapping in \code{hypeR.GEM::signature2gene()}.}

\item{merge}{Logical; whether to merge mapped metabolites in \code{hypeR.GEM::signature2gene()}.}

\item{promiscuous_threshold}{Numeric threshold passed to \code{hypeR.GEM::signature2gene()}.}

\item{ensemble_id}{Logical; passed to \code{hypeR.GEM::signature2gene()}.}

\item{background}{Optional background passed to \code{hypeR.GEM}.}

\item{method}{One of \code{"weighted"} or \code{"unweighted"}.}

\item{weighted_by}{Weighting field passed to \code{hypeR.GEM::enrichment()} for weighted GEM analyses.}

\item{min_metabolite}{Minimum metabolite count passed to GEM enrichment.}

\item{score_col}{Column containing directional scores in the metabolomics signature table.}

\item{split_by_group}{Logical; whether to split GEM signatures by \code{group_label}.}

\item{split_by_direction}{Logical; whether to split GEM signatures into up and down subsets when a numeric score column is available.}

\item{verbose}{Logical; whether or not to print diagnostic messages while retrieving signatures.}
}
\value{
A list with \code{result}, \code{signatures}, \code{metadata}, \code{gem_object}, and \code{reference_key}.
}
\description{
Resolves one or more metabolomics signatures from SigRepo and runs \code{hypeR.GEM} without requiring the user to call \code{getSignature()} manually first.
}
\examples{
\dontrun{
gem_res <- SigRepo::runHyperGEM(
  conn_handler = conn_handler,
  signature_name = "example_metabolomics_signature",
  genesets = genesets,
  method = "weighted"
)
}
}
