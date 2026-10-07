############################################################
# RummaGEO: up/down gene sets auto-computed from GEO RNA-seq
# (ARCHS4). Human and mouse live in separate "backgrounds";
# we look the right one up by species, then enrich against it.
############################################################

RUMMAGEO_BACKGROUNDS_QUERY <- "{ backgrounds { nodes { id species } } }"

RUMMAGEO_ENRICH_QUERY <- "query enrich($id:UUID!, $genes:[String]!, $first:Int, $overlapGe:Int){
  background(id:$id) {
    species
    enrich(genes:$genes, first:$first, overlapGe:$overlapGe){
      totalCount
      nodes {
        pvalue adjPvalue oddsRatio nOverlap
        geneSet { term species nGeneIds geneSetGsesById { nodes { gse } } }
      }
    }
  }
}"

#' @title rummageoBackgroundId
#' @description UUID of the RummaGEO background for "hsa" (human) or "mmu" (mouse).
#' @keywords internal
#' @noRd
rummageoBackgroundId <- function(organism_code, timeout = 60) {
  species <- base::switch(organism_code, hsa = "human", mmu = "mouse",
                          base::stop("RummaGEO only supports Homo sapiens and Mus musculus."))
  payload <- externalGraphql(externalUrl("rummageo"), RUMMAGEO_BACKGROUNDS_QUERY, base::list(), timeout = timeout)
  for (n in payload$data$backgrounds$nodes %||% base::list()) {
    if (base::identical(n$species, species)) {
      return(n$id)
    }
  }
  base::stop(base::sprintf("RummaGEO has no %s background.", species))
}

#' @title externalRummageo
#' @description Enrich a translated query against the RummaGEO background that
#' matches its organism. Symbols are sent as-is (mouse symbols are mixed case).
#' @keywords internal
#' @noRd
externalRummageo <- function(query, limit = 25, timeout = 60) {
  if (base::is.na(query$organism_code)) {
    base::stop(base::sprintf(
      "RummaGEO only supports Homo sapiens and Mus musculus; '%s' is '%s'.",
      query$signature_name, query$organism
    ))
  }
  background_id <- rummageoBackgroundId(query$organism_code, timeout = timeout)
  variables <- base::list(
    id = background_id,
    genes = base::as.list(query$genes),
    first = base::as.integer(limit),
    overlapGe = 2L
  )
  payload <- externalGraphql(externalUrl("rummageo"), RUMMAGEO_ENRICH_QUERY, variables, timeout = timeout)
  parseRummageo(payload, query)
}

#' @title parseRummageo
#' @description Flatten a RummaGEO enrich payload into the common frame. Terms
#' look like "GSE123,GSE124-0-vs-1-human up" or "GSE1-0-vs-6-mouse.tsv up".
#' @keywords internal
#' @noRd
parseRummageo <- function(payload, query) {
  bg <- base::tryCatch(payload$data$background, error = function(e) NULL) %||%
    base::tryCatch(payload$data$currentBackground, error = function(e) NULL)
  enrich <- bg$enrich
  rows <- base::lapply(enrich$nodes %||% base::list(), function(node) {
    gs <- node$geneSet
    if (base::is.null(gs)) {
      return(NULL)
    }
    term <- gs$term %||% NA_character_
    gses <- base::vapply(gs$geneSetGsesById$nodes %||% base::list(),
                         function(n) base::as.character(n$gse %||% NA_character_), character(1))
    gse <- if (base::length(gses) > 0 && !base::is.na(gses[1])) gses[1] else base::sub("-.*$", "", term)
    first_gse <- base::strsplit(gse, ",", fixed = TRUE)[[1]][1]
    rest <- base::sub("^[^-]*-", "", term)
    regulation <- if (base::grepl(" up$", rest)) "up" else if (base::grepl(" (dn|down)$", rest)) "down" else NA_character_
    contrast <- base::sub("-(human|mouse)(\\.tsv)? (up|dn|down)$", "", rest)
    base::list(
      id = term,
      title = term,
      url = base::sprintf("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=%s", first_gse),
      score = node$oddsRatio,
      score_label = "odds ratio",
      pvalue = node$pvalue,
      adj_pvalue = node$adjPvalue,
      n_overlap = node$nOverlap,
      n_set = gs$nGeneIds,
      gse = gse,
      species = gs$species,
      contrast = contrast,
      regulation = regulation
    )
  })
  rows <- base::Filter(Negate(base::is.null), rows)
  out <- bindExternalHits("rummageo", query$direction, rows)
  base::attr(out, "total_count") <- enrich$totalCount %||% base::nrow(out)
  base::attr(out, "query") <- query
  out
}
