############################################################
# Translate a SigRepo signature into the one query shape the
# external engines share: gene symbols + an organism code.
############################################################

#' @title organismCode
#' @description "hsa" for Homo sapiens, "mmu" for Mus musculus, NA otherwise.
#' @keywords internal
#' @noRd
organismCode <- function(organism) {
  key <- base::tolower(base::trimws(base::as.character(organism %||% "")))
  if (base::length(key) == 0 || base::is.na(key[1])) {
    return(NA_character_)
  }
  if (key[1] %in% c("homo sapiens", "human", "hsa")) return("hsa")
  if (key[1] %in% c("mus musculus", "mouse", "mmu")) return("mmu")
  NA_character_
}

#' @title looksLikeGeneSymbol
#' @description TRUE for names that are not Ensembl, Entrez or RefSeq ids.
#' @keywords internal
#' @noRd
looksLikeGeneSymbol <- function(x) {
  x <- base::as.character(x)
  is_ensembl <- base::grepl("^ENS[A-Z]*[GTP][0-9]{6,}(\\.[0-9]+)?$", x)
  is_entrez  <- base::grepl("^[0-9]+$", x)
  is_refseq  <- base::grepl("^(NM|NR|XM|XR|NP|XP)_[0-9]+", x)
  !(is_ensembl | is_entrez | is_refseq)
}

#' @title failedLookup
#' @description An empty mapping that remembers why the lookup produced
#' nothing, so the caller can tell the user instead of blaming the signature.
#' @keywords internal
#' @noRd
failedLookup <- function(message) {
  base::structure(base::character(), error = message)
}

#' @title lookupSymbolsInSigRepo
#' @description feature_name -> gene_symbol from the transcriptomics
#' reference table. Any failure (no connection, viewer without access,
#' missing table) yields an empty mapping so the caller can fall back.
#' @keywords internal
#' @noRd
lookupSymbolsInSigRepo <- function(conn_handler, feature_names, organism) {
  if (base::is.null(conn_handler) || base::length(feature_names) == 0) {
    return(base::character())
  }
  tbl <- base::tryCatch(
    searchTranscriptomicsFeatureSet(
      conn_handler = conn_handler, feature_name = feature_names, organism = NULL, verbose = FALSE
    ),
    error = function(e) e
  )
  if (base::inherits(tbl, "error")) {
    return(failedLookup(base::sprintf("SigRepo reference-table lookup failed: %s", base::conditionMessage(tbl))))
  }
  if (base::is.null(tbl) || !base::all(c("feature_name", "gene_symbol") %in% base::colnames(tbl))) {
    return(base::character())
  }
  sym <- base::trimws(base::as.character(tbl$gene_symbol))
  keep <- !base::is.na(sym) & base::nzchar(sym)
  tbl <- tbl[keep, , drop = FALSE]
  tbl <- tbl[!base::duplicated(tbl$feature_name), , drop = FALSE]
  stats::setNames(base::trimws(base::as.character(tbl$gene_symbol)), base::as.character(tbl$feature_name))
}

#' @title biomartSymbolTable
#' @description The raw biomaRt call, isolated so tests can mock it.
#' @keywords internal
#' @noRd
biomartSymbolTable <- function(ensembl_ids, organism) {
  dataset <- base::switch(
    organismCode(organism),
    hsa = "hsapiens_gene_ensembl",
    mmu = "mmusculus_gene_ensembl",
    NA_character_
  )
  if (base::is.na(dataset)) {
    return(NULL)
  }
  attribute <- symbolAttributeForOrganism(organism)
  mart <- biomaRt::useEnsembl(biomart = "genes", dataset = dataset)
  biomaRt::getBM(
    attributes = c("ensembl_gene_id", attribute),
    filters = "ensembl_gene_id",
    values = base::unique(ensembl_ids),
    mart = mart
  )
}

#' @title lookupSymbolsInBiomart
#' @description Ensembl gene ids (versioned or not) -> symbols via biomaRt.
#' @keywords internal
#' @noRd
lookupSymbolsInBiomart <- function(feature_names, organism) {
  ensembl_names <- feature_names[base::grepl("^ENS[A-Z]*G[0-9]+", feature_names)]
  if (base::length(ensembl_names) == 0) {
    return(base::character())
  }
  ids <- base::sub("\\.[0-9]+$", "", ensembl_names)
  bm <- base::tryCatch(biomartSymbolTable(ids, organism), error = function(e) e)
  if (base::inherits(bm, "error")) {
    return(failedLookup(base::sprintf("biomaRt lookup failed: %s", base::conditionMessage(bm))))
  }
  if (base::is.null(bm) || base::nrow(bm) == 0) {
    return(base::character())
  }
  attribute <- symbolAttributeForOrganism(organism)
  sym <- base::trimws(base::as.character(bm[[attribute]]))
  bm <- bm[!base::is.na(sym) & base::nzchar(sym), , drop = FALSE]
  bm <- bm[!base::duplicated(bm$ensembl_gene_id), , drop = FALSE]
  by_id <- stats::setNames(base::trimws(base::as.character(bm[[attribute]])), bm$ensembl_gene_id)
  out <- by_id[ids]
  base::names(out) <- ensembl_names
  out[!base::is.na(out)]
}

#' @title mapFeaturesToSymbols
#' @description Named character vector feature_name -> symbol; NA where no
#' source could map the name. Symbols pass through untouched.
#' @keywords internal
#' @noRd
mapFeaturesToSymbols <- function(feature_names, organism, conn_handler = NULL) {
  feature_names <- base::unique(base::trimws(base::as.character(feature_names)))
  feature_names <- feature_names[!base::is.na(feature_names) & base::nzchar(feature_names)]
  symbol <- stats::setNames(base::rep(NA_character_, base::length(feature_names)), feature_names)
  is_symbol <- looksLikeGeneSymbol(feature_names)
  symbol[is_symbol] <- feature_names[is_symbol]

  errors <- base::character()
  pending <- base::names(symbol)[base::is.na(symbol)]
  if (base::length(pending) > 0) {
    hits <- lookupSymbolsInSigRepo(conn_handler, pending, organism)
    errors <- c(errors, base::attr(hits, "error"))
    hits <- hits[base::names(hits) %in% pending]
    if (base::length(hits) > 0) symbol[base::names(hits)] <- base::unname(hits)
  }
  pending <- base::names(symbol)[base::is.na(symbol)]
  if (base::length(pending) > 0) {
    hits <- lookupSymbolsInBiomart(pending, organism)
    errors <- c(errors, base::attr(hits, "error"))
    hits <- hits[base::names(hits) %in% pending]
    if (base::length(hits) > 0) symbol[base::names(hits)] <- base::unname(hits)
  }
  if (base::length(errors) > 0) base::attr(symbol, "lookup_errors") <- errors
  symbol
}

#' @title buildExternalQuery
#' @description Turn an OmicSignature into the query all three engines accept.
#' @param omic_signature An OmicSignature (or anything with `$signature` and `$metadata`).
#' @param direction "combined", "up" or "down". Up/down use the sign of `score`.
#' @param conn_handler Optional; enables the reference-table symbol lookup.
#' @param max_genes Cap on genes sent (CORESH refuses more than 500).
#' @param verbose Print a one-line mapping summary.
#' @return list(signature_name, organism, organism_code, direction, genes,
#'   n_input, n_mapped, n_unmapped, truncated, unmapped)
#' @keywords internal
#' @noRd
buildExternalQuery <- function(omic_signature, direction = "combined", conn_handler = NULL,
                               max_genes = 500, verbose = TRUE) {
  sig <- omic_signature$signature
  meta <- omic_signature$metadata
  name <- base::as.character(meta$signature_name %||% "signature")[1]
  organism <- base::as.character(meta$organism %||% NA_character_)[1]
  assay <- base::tolower(base::as.character(meta$assay_type %||% "transcriptomics")[1])

  if (!base::identical(assay, "transcriptomics")) {
    base::stop(base::sprintf(
      "searchExternal() supports transcriptomics signatures only; '%s' has assay_type '%s'.", name, assay
    ))
  }
  if (base::is.null(sig) || base::nrow(sig) == 0 || !"feature_name" %in% base::colnames(sig)) {
    base::stop(base::sprintf("Signature '%s' has no features to search with.", name))
  }

  has_score <- "score" %in% base::colnames(sig) && base::any(!base::is.na(sig$score))
  if (!base::identical(direction, "combined")) {
    if (!has_score) {
      base::stop(base::sprintf("Direction splitting needs a 'score' column; '%s' has none.", name))
    }
    keep <- if (base::identical(direction, "up")) sig$score > 0 else sig$score < 0
    sig <- sig[keep %in% TRUE, , drop = FALSE]
    if (base::nrow(sig) == 0) {
      sign_word <- if (base::identical(direction, "up")) "positive" else "negative"
      other <- if (base::identical(direction, "up")) "down" else "up"
      base::stop(base::sprintf(
        "'%s' has no features with a %s score; use direction = 'combined' or '%s'.", name, sign_word, other
      ))
    }
  }
  if (has_score) {
    sig <- sig[base::order(-base::abs(sig$score), na.last = TRUE), , drop = FALSE]
  }

  feature_names <- base::as.character(sig$feature_name)
  mapping <- mapFeaturesToSymbols(feature_names, organism, conn_handler)
  symbols <- base::unname(mapping[base::trimws(feature_names)])
  genes <- base::unique(symbols[!base::is.na(symbols) & base::nzchar(symbols)])

  n_input <- base::length(base::unique(feature_names))
  n_mapped <- base::length(genes)
  unmapped <- base::names(mapping)[base::is.na(mapping)]
  truncated <- n_mapped > max_genes
  if (truncated) {
    genes <- genes[base::seq_len(max_genes)]
  }
  lookup_errors <- base::attr(mapping, "lookup_errors") %||% base::character()
  if (base::length(genes) < 2) {
    base::stop(base::sprintf(
      "Could not map enough of the %d features of '%s' to gene symbols (organism '%s'): %d mapped, need at least 2.%s",
      n_input, name, organism, n_mapped,
      if (base::length(lookup_errors) > 0) base::paste0(" ", base::paste(lookup_errors, collapse = " ")) else ""
    ))
  }
  if (base::isTRUE(verbose)) {
    for (err in lookup_errors) base::message(base::sprintf("[%s] %s: %s", direction, name, err))
    base::message(base::sprintf(
      "[%s] %s: %d of %d features mapped to gene symbols%s; the query is %s.",
      direction, name, n_mapped, n_input,
      if (base::length(unmapped) > 0) base::sprintf(" (%d unmapped)", base::length(unmapped)) else "",
      if (truncated) {
        base::sprintf("the %d strongest of the %d mapped genes by |score| (the engines accept at most %d)", max_genes, n_mapped, max_genes)
      } else {
        base::sprintf("all %d genes", n_mapped)
      }
    ))
  }

  base::list(
    signature_name = name,
    organism = organism,
    organism_code = organismCode(organism),
    direction = direction,
    genes = genes,
    n_input = n_input,
    n_mapped = n_mapped,
    n_unmapped = base::length(unmapped),
    truncated = truncated,
    unmapped = unmapped,
    lookup_errors = lookup_errors
  )
}
