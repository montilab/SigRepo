#' MSigDB database for a species/collection pair
#'
#' Mouse-native collections (MH, M1-M8) live in the mouse database. Every other
#' request -- human, mouse against a human collection such as C2 or C5, or any
#' other species -- uses the human database, which msigdbr maps to orthologs.
#' hypeR::msigdb_download() gets both cases wrong: it always uses "MM" for mouse
#' and has a `db_species == "HS"` typo for other species.
#'
#' @noRd
msigdbDbSpecies <- function(species, collection) {
  species <- base::tolower(base::trimws(base::as.character(species[[1]])))
  collection <- base::toupper(base::trimws(base::as.character(collection[[1]])))

  if (base::identical(species, "mus musculus") && base::grepl("^M(H|[0-9]+)$", collection)) {
    return("MM")
  }

  "HS"
}

#' Thin wrapper so tests can replace the msigdbr call
#' @noRd
callMsigdbr <- function(args) {
  base::do.call(msigdbr::msigdbr, args)
}

#' Fetch an MSigDB collection as a hypeR gsets object
#' @noRd
fetchMsigdbGsets <- function(species, collection, subcollection = NULL, clean = FALSE) {
  for (pkg in c("msigdbr", "hypeR")) {
    if (!base::requireNamespace(pkg, quietly = TRUE)) {
      base::stop(base::sprintf("\nPackage '%s' is required when genesets = \"msigdb\". Please install it first.\n", pkg))
    }
  }

  args <- base::list(
    species = species,
    db_species = msigdbDbSpecies(species, collection),
    collection = collection
  )
  if (!base::is.null(subcollection)) {
    args$subcollection <- subcollection
  }

  response <- callMsigdbr(args)

  if (base::nrow(response) == 0L) {
    base::stop(base::sprintf(
      "\nMSigDB returned no genesets for species = '%s', collection = '%s'%s. See msigdbr::msigdbr_collections() for valid combinations.\n",
      species,
      collection,
      if (base::is.null(subcollection)) "" else base::sprintf(", subcollection = '%s'", subcollection)
    ))
  }

  mdf <- base::unique(base::as.data.frame(response)[, c("gs_name", "gene_symbol")])
  mdf <- mdf[!base::is.na(mdf$gene_symbol) & base::nzchar(mdf$gene_symbol), , drop = FALSE]
  genesets <- base::split(mdf$gene_symbol, mdf$gs_name)

  hypeR::gsets$new(
    genesets,
    name = if (base::is.null(subcollection)) collection else base::paste(collection, subcollection, sep = "."),
    version = base::paste0("v", base::as.character(utils::packageVersion("msigdbr"))),
    clean = clean,
    quiet = TRUE
  )
}

genesetsFormError <- function() {
  "\n'genesets' must be \"msigdb\" (with msigdb_collection) or your own genesets: a named list, hypeR::gsets or hypeR::rgsets object.\n"
}

#' Resolve genesets for hypeR
#'
#' @description Returns genesets in a form \code{hypeR::hypeR()} accepts. Pass
#' \code{genesets = "msigdb"} with \code{msigdb_collection} (and optionally
#' \code{msigdb_species}, \code{msigdb_subcollection}) to fetch an MSigDB
#' collection, or pass your own genesets object.
#'
#' @section Required and optional arguments:
#' \code{genesets} is required, and \code{msigdb_collection} is required with
#' \code{genesets = "msigdb"}. The other \code{msigdb_*} arguments are
#' optional (species defaults to \code{"Homo sapiens"}, no subcollection, no
#' label cleaning) and apply only to \code{"msigdb"}; passing them with your
#' own genesets is an error.
#'
#' @param genesets Either the string \code{"msigdb"}, or your own genesets: a
#' named list of character vectors, a \code{hypeR::gsets} object, or a
#' \code{hypeR::rgsets} object.
#' @param msigdb_species Species for MSigDB, e.g. \code{"Homo sapiens"} or
#' \code{"Mus musculus"}. Defaults to \code{"Homo sapiens"}. Only valid with
#' \code{genesets = "msigdb"}.
#' @param msigdb_collection MSigDB collection, e.g. \code{"H"}, \code{"C2"},
#' \code{"MH"}. Required with \code{genesets = "msigdb"}.
#' @param msigdb_subcollection Optional MSigDB subcollection, e.g.
#' \code{"CP:REACTOME"}. Only valid with \code{genesets = "msigdb"}.
#' @param msigdb_clean Logical; clean geneset labels via
#' \code{hypeR::gsets$new(clean = TRUE)}. Defaults to \code{FALSE}.
#'
#' @return For \code{"msigdb"}, a \code{hypeR::gsets} object named after the
#' collection (e.g. \code{"C2.CP:REACTOME"}) and versioned by msigdbr. Otherwise
#' the supplied genesets, unchanged.
#'
#' @details Mouse-native collections (\code{MH}, \code{M1}-\code{M8}) use the
#' mouse MSigDB database; mouse requests for human collections and all other
#' species use the human database with ortholog mapping.
#'
#' @examples
#' \dontrun{
#' hallmark <- SigRepo::getHypeRGenesets("msigdb", msigdb_collection = "H")
#' reactome_mouse <- SigRepo::getHypeRGenesets(
#'   "msigdb",
#'   msigdb_species = "Mus musculus",
#'   msigdb_collection = "C2",
#'   msigdb_subcollection = "CP:REACTOME"
#' )
#' }
#'
#' @export
getHypeRGenesets <- function(
    genesets,
    msigdb_species = NULL,
    msigdb_collection = NULL,
    msigdb_subcollection = NULL,
    msigdb_clean = FALSE
) {
  if (base::missing(genesets) || base::is.null(genesets)) {
    base::stop(genesetsFormError())
  }

  if (base::is.character(genesets)) {
    if (!(base::length(genesets) == 1L && base::identical(genesets, "msigdb"))) {
      base::stop(genesetsFormError())
    }

    if (base::is.null(msigdb_collection) || base::length(msigdb_collection) != 1L ||
        base::is.na(msigdb_collection) || !base::nzchar(msigdb_collection)) {
      base::stop("\ngenesets = \"msigdb\" requires 'msigdb_collection' (e.g. \"H\", \"C2\", \"MH\").\n")
    }

    return(fetchMsigdbGsets(
      species = if (base::is.null(msigdb_species)) "Homo sapiens" else msigdb_species,
      collection = msigdb_collection,
      subcollection = msigdb_subcollection,
      clean = msigdb_clean
    ))
  }

  msigdb_supplied <- c(
    msigdb_species = !base::is.null(msigdb_species),
    msigdb_collection = !base::is.null(msigdb_collection),
    msigdb_subcollection = !base::is.null(msigdb_subcollection)
  )
  if (base::any(msigdb_supplied)) {
    base::stop(base::sprintf(
      "\n%s only apply when genesets = \"msigdb\".\n",
      base::paste(base::names(msigdb_supplied)[msigdb_supplied], collapse = ", ")
    ))
  }

  if (methods::is(genesets, "gsets") || methods::is(genesets, "rgsets")) {
    return(genesets)
  }

  if (methods::is(genesets, "list") && base::length(genesets) > 0 &&
      !base::is.null(base::names(genesets)) && !base::any(base::names(genesets) %in% c("", NA))) {
    return(genesets)
  }

  base::stop(genesetsFormError())
}
