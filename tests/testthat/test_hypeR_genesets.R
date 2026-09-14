test_that("msigdbDbSpecies maps species and collection to the MSigDB database", {
  expect_equal(SigRepo:::msigdbDbSpecies("Homo sapiens", "C2"), "HS")
  expect_equal(SigRepo:::msigdbDbSpecies("Homo Sapiens", "H"), "HS")
  expect_equal(SigRepo:::msigdbDbSpecies("Mus musculus", "MH"), "MM")
  expect_equal(SigRepo:::msigdbDbSpecies("Mus musculus", "M8"), "MM")
  expect_equal(SigRepo:::msigdbDbSpecies("Mus musculus", "C2"), "HS")
  expect_equal(SigRepo:::msigdbDbSpecies("Mus musculus", "H"), "HS")
  expect_equal(SigRepo:::msigdbDbSpecies("Rattus norvegicus", "H"), "HS")
})

test_that("getHypeRGenesets passes a named list, gsets and rgsets through unchanged", {
  testthat::skip_if_not_installed("hypeR")

  gs <- list(SET_1 = c("A", "B"), SET_2 = c("C"))
  expect_identical(SigRepo::getHypeRGenesets(gs), gs)

  gsets_obj <- hypeR::gsets$new(gs, name = "custom", version = "v1", quiet = TRUE)
  expect_identical(SigRepo::getHypeRGenesets(gsets_obj), gsets_obj)

  rgsets_like <- structure(list(), class = c("rgsets", "R6"))
  expect_identical(SigRepo::getHypeRGenesets(rgsets_like), rgsets_like)
})

test_that("getHypeRGenesets rejects anything that is not msigdb or a genesets object", {
  form_error <- "'genesets' must be \"msigdb\""
  expect_error(SigRepo::getHypeRGenesets("mygeneset"), form_error, fixed = TRUE)
  expect_error(SigRepo::getHypeRGenesets(c("msigdb", "H")), form_error, fixed = TRUE)
  expect_error(SigRepo::getHypeRGenesets(NULL), form_error, fixed = TRUE)
  expect_error(SigRepo::getHypeRGenesets(list(c("A", "B"))), form_error, fixed = TRUE)
  expect_error(SigRepo::getHypeRGenesets(list()), form_error, fixed = TRUE)
})

test_that("getHypeRGenesets requires msigdb_collection for msigdb and forbids msigdb_* otherwise", {
  expect_error(
    SigRepo::getHypeRGenesets("msigdb"),
    "genesets = \"msigdb\" requires 'msigdb_collection'", fixed = TRUE
  )
  expect_error(
    SigRepo::getHypeRGenesets(list(S = "A"), msigdb_collection = "H", msigdb_species = "Mus musculus"),
    "msigdb_species, msigdb_collection only apply when genesets = \"msigdb\"", fixed = TRUE
  )
})

test_that("getHypeRGenesets forwards msigdb arguments with a human default species", {
  testthat::local_mocked_bindings(
    fetchMsigdbGsets = function(species, collection, subcollection = NULL, clean = FALSE) {
      list(species = species, collection = collection, subcollection = subcollection, clean = clean)
    },
    .package = "SigRepo"
  )

  out <- SigRepo::getHypeRGenesets("msigdb", msigdb_collection = "C2", msigdb_subcollection = "CP:REACTOME")
  expect_equal(out, list(species = "Homo sapiens", collection = "C2", subcollection = "CP:REACTOME", clean = FALSE))
})

test_that("fetchMsigdbGsets builds a named, versioned gsets with the right db_species", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("msigdbr")

  seen <- NULL
  testthat::local_mocked_bindings(
    callMsigdbr = function(args) {
      seen <<- args
      data.frame(
        gs_name = c("SET_1", "SET_1", "SET_1", "SET_2"),
        gene_symbol = c("Abc1", "Abc1", "Def2", NA),
        stringsAsFactors = FALSE
      )
    },
    .package = "SigRepo"
  )

  gs <- SigRepo:::fetchMsigdbGsets("Mus musculus", "C2", "CP:REACTOME")

  expect_equal(seen, list(species = "Mus musculus", db_species = "HS", collection = "C2", subcollection = "CP:REACTOME"))
  expect_true(methods::is(gs, "gsets"))
  expect_equal(gs$name, "C2.CP:REACTOME")
  expect_equal(gs$version, paste0("v", as.character(utils::packageVersion("msigdbr"))))
  expect_equal(gs$genesets, list(SET_1 = c("Abc1", "Def2")))
})

test_that("fetchMsigdbGsets errors clearly when MSigDB returns nothing", {
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("msigdbr")

  testthat::local_mocked_bindings(
    callMsigdbr = function(args) data.frame(gs_name = character(), gene_symbol = character()),
    .package = "SigRepo"
  )

  expect_error(
    SigRepo:::fetchMsigdbGsets("Homo sapiens", "NOPE"),
    "msigdbr::msigdbr_collections()", fixed = TRUE
  )
})

test_that("fetchMsigdbGsets retrieves Hallmark from MSigDB", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("hypeR")
  testthat::skip_if_not_installed("msigdbr")

  gs <- tryCatch(
    SigRepo:::fetchMsigdbGsets("Homo sapiens", "H"),
    error = function(e) testthat::skip(paste("MSigDB unavailable:", conditionMessage(e)))
  )

  expect_true(methods::is(gs, "gsets"))
  expect_equal(gs$name, "H")
  expect_true("HALLMARK_APOPTOSIS" %in% names(gs$genesets))
})
