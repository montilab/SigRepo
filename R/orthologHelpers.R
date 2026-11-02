############################################################
# Helpers behind mapOrthologs() and updateOrthologFeatureSet()
############################################################

#' @title ORTHOLOG_COLUMNS
#' @description The columns a table of ortholog pairs carries on its way into
#' the `gene_orthologs` reference table: the non-human gene (symbol, Ensembl,
#' Entrez), its human ortholog (the same three), babelgene's evidence
#' (`support`, `support_n`) and the babelgene version that produced the pair.
#' `organism` names the non-human side; human is implicit because babelgene
#' only maps to and from human.
#' @keywords internal
#' @noRd
ORTHOLOG_COLUMNS <- c(
  "organism", "gene_symbol", "ensembl_gene_id", "entrez_id",
  "human_symbol", "human_ensembl_gene_id", "human_entrez_id",
  "support", "support_n", "version"
)

#' @title classifyFeatureIdType
#' @description Decide, element by element, which identifier space a feature
#' id belongs to, so a mixed vector can be resolved through the right
#' reference table. Ensembl gene ids start with `ENS`, an optional species
#' code and `G` followed by digits (case does not matter); UniProt accessions
#' follow the UniProt pattern, optionally with an isoform suffix; a string of
#' digits is an Entrez id; anything else is a gene symbol. Blank or NA input
#' gets NA.
#' @param ids Character vector of feature ids.
#' @return Character vector of the same length with values `"ensembl"`,
#'   `"uniprot"`, `"entrez"`, `"symbol"` or `NA`.
#' @keywords internal
#' @noRd
classifyFeatureIdType <- function(ids) {
  x <- base::trimws(base::as.character(ids))
  out <- base::rep(NA_character_, base::length(x))
  known <- !base::is.na(x) & base::nzchar(x)
  is_ensembl <- known & base::grepl("^ENS[A-Z]*G[0-9]{6,}", x, ignore.case = TRUE)
  is_uniprot <- known & !is_ensembl &
    base::grepl("^([OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2})(-[0-9]+)?$", x)
  is_entrez <- known & !is_ensembl & !is_uniprot & base::grepl("^[0-9]+$", x)
  out[is_ensembl] <- "ensembl"
  out[is_uniprot] <- "uniprot"
  out[is_entrez] <- "entrez"
  out[known & base::is.na(out)] <- "symbol"
  out
}

#' @title emptyBabelgeneFrame
#' @description A zero-row frame with the nine columns `babelgene::orthologs()`
#' returns, so a lookup that finds nothing has the same shape as one that does.
#' @keywords internal
#' @noRd
emptyBabelgeneFrame <- function() {
  base::data.frame(
    human_symbol = base::character(), human_entrez = base::integer(), human_ensembl = base::character(),
    taxon_id = base::integer(), symbol = base::character(), entrez = base::integer(),
    ensembl = base::character(), support = base::character(), support_n = base::numeric(),
    stringsAsFactors = FALSE
  )
}

#' @title fetchBabelgeneOrthologs
#' @description The one call into babelgene, kept thin so tests can replace it.
#' babelgene throws "no orthologs found or the genes are not valid" instead of
#' returning zero rows; that case becomes an empty frame here. Every pair is
#' requested (`min_support = 1`, `top = FALSE`): the support count is stored
#' with the pair and the caller decides what to trust.
#' @param genes Character vector of gene symbols to look up.
#' @param species The non-human organism as babelgene names it (see
#'   [babelgeneSpeciesName()]); babelgene does not accept a taxon id here.
#' @param human `TRUE` when `genes` are human symbols, `FALSE` when they belong
#'   to the non-human organism.
#' @return The data frame `babelgene::orthologs()` returns, possibly empty.
#' @keywords internal
#' @noRd
fetchBabelgeneOrthologs <- function(genes, species, human) {
  genes <- base::unique(base::trimws(base::as.character(genes)))
  genes <- genes[!base::is.na(genes) & base::nzchar(genes)]
  # babelgene guesses the id type from the genes it is given: a digit-only
  # value switches the whole query to Entrez ids and an Ensembl-shaped one to
  # Ensembl ids, and the real symbols then return nothing. The reference
  # tables do hold a few such strings in gene_symbol, so drop them here.
  genes <- genes[!base::grepl("^[0-9]+$", genes) & !base::grepl("^ENS[A-Z]*G[0-9]{6,}", genes, ignore.case = TRUE)]
  if (base::length(genes) == 0) {
    return(emptyBabelgeneFrame())
  }
  base::tryCatch({
    base::as.data.frame(babelgene::orthologs(
      genes = genes, species = species, human = human, min_support = 1, top = FALSE
    ), stringsAsFactors = FALSE)
  }, error = function(e) {
    if (base::grepl("no orthologs found", base::conditionMessage(e), fixed = TRUE)) {
      emptyBabelgeneFrame()
    } else {
      base::stop(e)
    }
  })
}

#' @title babelgeneOrthologTable
#' @description Fetch every babelgene pair touching the given non-human
#' symbols or the given human symbols and shape them into [ORTHOLOG_COLUMNS].
#' Pairs missing a symbol on either side are dropped (the symbol is the join
#' key into the reference tables), the union of both directions is
#' de-duplicated on the pair, ids are stored as character with blanks for
#' missing values, and every row carries the organism and the babelgene
#' version.
#' @param symbols Symbols of the non-human organism to look up.
#' @param human_symbols Human symbols to look up.
#' @param organism Organism name as stored in the `organisms` table.
#' @param species The organism as babelgene names it, from [babelgeneSpeciesName()].
#' @return Data frame with [ORTHOLOG_COLUMNS], one row per pair.
#' @keywords internal
#' @noRd
babelgeneOrthologTable <- function(symbols, human_symbols, organism, species) {
  version <- base::as.character(utils::packageVersion("babelgene"))
  fetched <- base::rbind(
    fetchBabelgeneOrthologs(symbols, species, human = FALSE),
    fetchBabelgeneOrthologs(human_symbols, species, human = TRUE)
  )
  is_blank <- function(x) base::is.na(x) | !base::nzchar(base::trimws(base::as.character(x)))
  fetched <- fetched[!is_blank(fetched$symbol) & !is_blank(fetched$human_symbol), , drop = FALSE]
  as_text <- function(x) {
    x <- base::trimws(base::as.character(x))
    x[base::is.na(x)] <- ""
    x
  }
  n <- base::nrow(fetched)
  out <- base::data.frame(
    organism = base::rep(base::as.character(organism), n),
    gene_symbol = as_text(fetched$symbol),
    ensembl_gene_id = as_text(fetched$ensembl),
    entrez_id = as_text(fetched$entrez),
    human_symbol = as_text(fetched$human_symbol),
    human_ensembl_gene_id = as_text(fetched$human_ensembl),
    human_entrez_id = as_text(fetched$human_entrez),
    support = as_text(fetched$support),
    support_n = as_text(fetched$support_n),
    version = base::rep(version, n),
    stringsAsFactors = FALSE
  )
  key <- base::tolower(base::paste(out$gene_symbol, out$human_symbol, sep = "\r"))
  out <- out[!base::duplicated(key), , drop = FALSE]
  base::rownames(out) <- NULL
  out
}

#' @title partitionOrthologUpdates
#' @description Decide what an ortholog refresh should do with each stored
#' pair, given the freshly fetched pairs. Pairs are keyed on
#' (gene_symbol, human_symbol), compared case-insensitively, the same way
#' `partitionFeatureUpdates()` keys the feature tables on feature_name.
#' @param db_tbl Data frame of stored pairs: `gene_symbol`, `human_symbol`.
#' @param fetched_tbl Data frame of fetched pairs: `gene_symbol`, `human_symbol`.
#' @return A list of data frames, each with `gene_symbol` and `human_symbol`:
#'   `refresh` (stored pairs still present, to be stamped with the new
#'   version), `add` (fetched pairs not stored yet) and `archive` (stored
#'   pairs absent from the fetch). Every stored pair lands in exactly one of
#'   `refresh` or `archive`.
#' @keywords internal
#' @noRd
partitionOrthologUpdates <- function(db_tbl, fetched_tbl) {
  pair_key <- function(a, b) {
    base::paste(
      base::tolower(base::trimws(base::as.character(a))),
      base::tolower(base::trimws(base::as.character(b))),
      sep = "\r"
    )
  }
  db <- base::data.frame(
    gene_symbol = base::as.character(db_tbl$gene_symbol),
    human_symbol = base::as.character(db_tbl$human_symbol),
    stringsAsFactors = FALSE
  )
  fetched <- base::data.frame(
    gene_symbol = base::as.character(fetched_tbl$gene_symbol),
    human_symbol = base::as.character(fetched_tbl$human_symbol),
    stringsAsFactors = FALSE
  )
  db_key <- pair_key(db$gene_symbol, db$human_symbol)
  fetched_key <- pair_key(fetched$gene_symbol, fetched$human_symbol)
  fetched <- fetched[!base::duplicated(fetched_key), , drop = FALSE]
  fetched_key <- fetched_key[!base::duplicated(fetched_key)]

  in_fetch <- db_key %in% fetched_key
  pick <- function(tbl, idx) {
    out <- tbl[idx, , drop = FALSE]
    base::rownames(out) <- NULL
    out
  }
  base::list(
    refresh = pick(db, in_fetch),
    add = pick(fetched, !(fetched_key %in% db_key)),
    archive = pick(db, !in_fetch)
  )
}

#' @title babelgeneSpeciesName
#' @description The name babelgene knows an organism by. `babelgene::orthologs()`
#' accepts only the names in `babelgene::species()`, not a taxon id, so the
#' organism name from the `organisms` table is matched against the scientific
#' names there (ignoring case and surrounding whitespace); when the name is
#' spelled differently, the stored `prot_organism_taxid` finds the row instead.
#' @param organism Organism name as stored in the `organisms` table.
#' @param prot_organism_taxid The `organisms.prot_organism_taxid` value, used
#'   as a fallback.
#' @return A single scientific name, `NA` when babelgene knows neither.
#' @keywords internal
#' @noRd
babelgeneSpeciesName <- function(organism, prot_organism_taxid) {
  species <- babelgene::species()
  names_lower <- base::tolower(base::trimws(species$scientific_name))
  key <- base::tolower(base::trimws(base::as.character(organism)))
  hit <- species$scientific_name[names_lower %in% key]
  if (base::length(hit) == 0) {
    taxid <- base::suppressWarnings(base::as.integer(base::as.character(prot_organism_taxid)))
    if (base::length(taxid) > 0 && !base::is.na(taxid[1])) {
      hit <- species$scientific_name[species$taxon_id %in% taxid[1]]
    }
  }
  hit <- hit[!base::is.na(hit)]
  if (base::length(hit) == 0) NA_character_ else base::as.character(hit[1])
}
