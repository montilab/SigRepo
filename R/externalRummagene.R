############################################################
# Rummagene: ~1M gene sets mined from PMC supplementary
# tables (Communications Biology 2024, PMID 38643247).
# Public GraphQL endpoint, no auth. Same document the SigRepo
# server's Annotate tab uses.
############################################################

RUMMAGENE_ENRICH_QUERY <- "query enrich($genes:[String]!, $first:Int, $overlapGe:Int, $pvalueLe:Float){
  currentBackground {
    enrich(genes:$genes, first:$first, overlapGe:$overlapGe, pvalueLe:$pvalueLe){
      totalCount
      nodes {
        pvalue adjPvalue oddsRatio nOverlap
        geneSets { nodes { term description nGeneIds
          geneSetPmcsById { nodes { pmcInfoByPmcid { pmcid title yr doi } } } } }
      }
    }
  }
}"

#' @title externalRummagene
#' @description Enrich a translated query against Rummagene. Symbols are sent
#' upper-cased because Rummagene is keyed on human-style symbols.
#' @keywords internal
#' @noRd
externalRummagene <- function(query, limit = 25, timeout = 60) {
  variables <- base::list(
    genes = base::as.list(base::toupper(query$genes)),
    first = base::as.integer(limit),
    overlapGe = 2L,
    pvalueLe = 0.05
  )
  payload <- externalGraphql(externalUrl("rummagene"), RUMMAGENE_ENRICH_QUERY, variables, timeout = timeout)
  parseRummagene(payload, query)
}

#' @title parseRummagene
#' @description Flatten a Rummagene enrich payload into the common frame. A
#' gene-set hash can be shared by several papers; the first is representative
#' and `n_papers` says how many share it.
#' @keywords internal
#' @noRd
parseRummagene <- function(payload, query) {
  enrich <- base::tryCatch(payload$data$currentBackground$enrich, error = function(e) NULL)
  rows <- base::lapply(enrich$nodes %||% base::list(), function(node) {
    gs_nodes <- base::tryCatch(node$geneSets$nodes, error = function(e) NULL) %||% base::list()
    if (base::length(gs_nodes) == 0) {
      return(NULL)
    }
    gs <- gs_nodes[[1]]
    pmc <- NULL
    for (n in gs$geneSetPmcsById$nodes %||% base::list()) {
      if (!base::is.null(n$pmcInfoByPmcid)) { pmc <- n$pmcInfoByPmcid; break }
    }
    term <- gs$term %||% NA_character_
    pmcid <- pmc$pmcid
    if (base::is.null(pmcid) && !base::is.na(term) && base::grepl("^PMC[0-9]+", term)) {
      pmcid <- base::regmatches(term, base::regexpr("^PMC[0-9]+", term))
    }
    pmcid <- pmcid %||% NA_character_
    base::list(
      id = term,
      title = pmc$title %||% term,
      url = if (base::is.na(pmcid)) NA_character_ else base::sprintf("https://www.ncbi.nlm.nih.gov/pmc/articles/%s/", pmcid),
      score = node$oddsRatio,
      score_label = "odds ratio",
      pvalue = node$pvalue,
      adj_pvalue = node$adjPvalue,
      n_overlap = node$nOverlap,
      n_set = gs$nGeneIds,
      pmcid = pmcid,
      year = pmc$yr,
      doi = pmc$doi,
      description = gs$description,
      n_papers = base::length(gs_nodes)
    )
  })
  rows <- base::Filter(Negate(base::is.null), rows)
  out <- bindExternalHits("rummagene", query$direction, rows)
  base::attr(out, "total_count") <- enrich$totalCount %||% base::nrow(out)
  base::attr(out, "query") <- query
  out
}
