############################################################
# MetaboAnalyst compound mapping: converts metabolite names
# or ids between HMDB, PubChem, ChEBI, KEGG and METLIN through
# the public endpoint documented at
# https://www.metaboanalyst.ca/docs/APIs.xhtml. No auth. HTTP
# goes through externalPostJson() so tests run offline.
############################################################

METABOANALYST_COLUMNS <- c(
  "query", "matched", "match", "hmdb", "pubchem", "chebi", "kegg", "metlin", "smiles"
)

METABOANALYST_INPUT_TYPES <- c("name", "hmdb", "pubchem", "chebi", "kegg", "metlin")

#' @title mapMetabolites
#' @description Convert metabolite names or identifiers between namespaces
#' through MetaboAnalyst's public compound-mapping service.
#' @param ids Character vector of compound names or identifiers, all in the
#' namespace named by \code{from}. Values are trimmed, empty values and
#' duplicates are dropped before the request, and a value containing a
#' semicolon (the service's list separator) is an error.
#' @param from Namespace of \code{ids}. One of \code{"name"}, \code{"hmdb"},
#' \code{"pubchem"}, \code{"chebi"}, \code{"kegg"} or \code{"metlin"}, as
#' documented by MetaboAnalyst. Defaults to \code{"name"}.
#' @param timeout Seconds to wait for each request. The service takes about
#' 20 to 30 seconds per request plus a fraction of a second per compound, so
#' long lists are sent in chunks of 200 and the timeout applies to each
#' chunk. Defaults to 120.
#' @return A data frame with one row per distinct input, in input order:
#' \code{query} (the value sent), \code{matched} (logical, whether the
#' service recognised it), \code{match} (MetaboAnalyst's canonical compound
#' name), and the \code{hmdb}, \code{pubchem}, \code{chebi}, \code{kegg},
#' \code{metlin} and \code{smiles} identifiers. Unmatched rows are \code{NA}
#' in every column but \code{query} and \code{matched}.
#' @details
#' SigRepo's metabolite reference is RefMet-centred with HMDB, SMILES and
#' InChIKey cross-references, so this is the step to run when a signature
#' arrives with compound names or KEGG, PubChem or ChEBI ids: map them,
#' then deposit the HMDB ids with \code{metabolomics_nomenclature = "hmdb"}.
#'
#' The endpoint is \code{POST https://rest.xialab.ca/api/mapcompounds},
#' overridable through \code{options(sigrepo.metaboanalyst_url = ...)}.
#' When tested on 2026-10-15 only \code{from = "name"} returned matches;
#' the identifier input types are documented by MetaboAnalyst and are passed
#' through as documented, but every id query came back unmatched. Check the
#' \code{matched} column before relying on an id-based lookup.
#' @examples
#' \dontrun{
#' mapMetabolites(c("D-Glucose", "Citric acid", "1,3-Diaminopropane"))
#' }
#' @export
mapMetabolites <- function(ids, from = "name", timeout = 120) {
  from <- base::match.arg(from, METABOANALYST_INPUT_TYPES)
  ids <- cleanMetaboliteIds(ids)
  if (base::length(ids) == 0) {
    return(emptyMetaboanalystFrame())
  }
  chunk_size <- base::getOption("sigrepo.metaboanalyst_chunk_size", 200)
  chunks <- base::split(ids, base::ceiling(base::seq_along(ids) / chunk_size))
  frames <- base::lapply(chunks, function(chunk) {
    payload <- metaboanalystMapCompounds(chunk, from, timeout = timeout)
    parseMetaboanalystMapping(payload, chunk)
  })
  out <- base::do.call(base::rbind, base::unname(frames))
  base::rownames(out) <- NULL
  out
}

#' @title cleanMetaboliteIds
#' @description Trim, drop empty and NA-like values, de-duplicate, and refuse
#' values that would be split by the service's semicolon separator.
#' @keywords internal
#' @noRd
cleanMetaboliteIds <- function(ids) {
  ids <- base::trimws(base::as.character(ids %||% base::character()))
  ids <- ids[!base::is.na(ids) & !ids %in% c("", "NA", "NULL")]
  ids <- base::unique(ids)
  bad <- ids[base::grepl(";", ids, fixed = TRUE)]
  if (base::length(bad) > 0) {
    base::stop(base::sprintf(
      "MetaboAnalyst separates compounds with a semicolon, so a value may not contain one: %s",
      base::paste(utils::head(bad, 3), collapse = ", ")
    ))
  }
  ids
}

#' @title metaboanalystMapCompounds
#' @description One POST to the mapcompounds endpoint; the fetch half.
#' @keywords internal
#' @noRd
metaboanalystMapCompounds <- function(ids, from, timeout = 120) {
  externalPostJson(
    externalUrl("metaboanalyst"),
    base::list(queryList = base::paste0(base::paste(ids, collapse = ";"), ";"), inputType = from),
    timeout = timeout
  )
}

#' @title emptyMetaboanalystFrame
#' @description A zero-row frame with the mapMetabolites columns.
#' @keywords internal
#' @noRd
emptyMetaboanalystFrame <- function() {
  base::data.frame(
    query = base::character(), matched = base::logical(), match = base::character(),
    hmdb = base::character(), pubchem = base::character(), chebi = base::character(),
    kegg = base::character(), metlin = base::character(), smiles = base::character(),
    stringsAsFactors = FALSE
  )
}

#' @title parseMetaboanalystMapping
#' @description Column-oriented reply -> one row per query, in the order of
#' `queries`; queries absent from the reply become unmatched rows. Missing
#' values arrive as the string "NA" or as JSON null.
#' @keywords internal
#' @noRd
parseMetaboanalystMapping <- function(payload, queries) {
  column <- function(name) {
    values <- payload[[name]] %||% base::list()
    out <- base::vapply(values, function(v) {
      if (base::is.null(v) || base::length(v) == 0) NA_character_ else base::as.character(v[[1]])
    }, base::character(1))
    out[!base::is.na(out) & out %in% c("NA", "")] <- NA_character_
    out
  }
  reply <- base::data.frame(
    query = column("Query"), match = column("Match"), hmdb = column("HMDB"),
    pubchem = column("PubChem"), chebi = column("ChEBI"), kegg = column("KEGG"),
    metlin = column("METLIN"), smiles = column("SMILES"), comment = column("Comment"),
    stringsAsFactors = FALSE
  )
  reply <- reply[!base::is.na(reply$query) & !base::duplicated(reply$query), , drop = FALSE]
  idx <- base::match(queries, reply$query)
  out <- base::data.frame(
    query = base::as.character(queries),
    matched = reply$comment[idx] %in% "1",
    match = reply$match[idx], hmdb = reply$hmdb[idx], pubchem = reply$pubchem[idx],
    chebi = reply$chebi[idx], kegg = reply$kegg[idx], metlin = reply$metlin[idx],
    smiles = reply$smiles[idx],
    stringsAsFactors = FALSE
  )
  out <- out[, METABOANALYST_COLUMNS, drop = FALSE]
  base::rownames(out) <- NULL
  out
}
