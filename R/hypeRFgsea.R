# fgsea defaults of hypeR's vignette wrapper (hypeR vignettes/docs/fgsea.Rmd).
HYPER_FGSEA_DEFAULT_ARGS <- base::list(sampleSize = 101, minSize = 1, maxSize = Inf)

#' Evaluate `expr` after set.seed(seed), restoring the caller's RNG state
#'
#' `expr` is a promise, so it runs after set.seed(). seed = NULL evaluates it
#' with the session RNG and leaves that RNG advanced.
#' @noRd
withHypeRSeed <- function(seed, expr) {
  if (base::is.null(seed)) {
    return(expr)
  }
  env <- base::globalenv()
  had_seed <- base::exists(".Random.seed", envir = env, inherits = FALSE)
  old_seed <- if (had_seed) base::get(".Random.seed", envir = env, inherits = FALSE) else NULL
  base::on.exit({
    if (had_seed) {
      base::assign(".Random.seed", old_seed, envir = env)
    } else if (base::exists(".Random.seed", envir = env, inherits = FALSE)) {
      base::rm(".Random.seed", envir = env)
    }
  })
  base::set.seed(seed)
  expr
}

#' fgseaMultilevel() result as a hypeR-style data frame
#'
#' Columns follow hypeR's fgsea vignette wrapper, plus `hits` (the leading edge
#' in hypeR's " , " format) so rctbl_build() works. Rows with an NA p-value are
#' removed.
#' @noRd
fgseaHypeRTable <- function(raw, stats, pathways) {
  raw <- raw[!base::is.na(raw$pval), , drop = FALSE]
  leading_edge <- raw$leadingEdge
  base::data.frame(
    label = base::as.character(raw$pathway),
    pval = base::signif(raw$pval, 2),
    fdr = base::signif(raw$padj, 2),
    lte = raw$log2err,
    es = raw$ES,
    nes = raw$NES,
    signature = base::length(stats),
    geneset = base::vapply(base::as.character(raw$pathway), function(x) base::length(pathways[[x]]), base::integer(1), USE.NAMES = FALSE),
    overlap = base::as.integer(raw$size),
    le = base::vapply(leading_edge, function(x) base::paste(x, collapse = ","), base::character(1)),
    hits = base::vapply(leading_edge, function(x) base::paste(x, collapse = " , "), base::character(1)),
    stringsAsFactors = FALSE
  )
}

#' One fgseaMultilevel() run on one ranking, split into up/down hyp objects
#'
#' @param stats Named numeric ranking (signed scores).
#' @param genesets Named list, hypeR::gsets or hypeR::rgsets.
#' @param background Number (no effect) or gene vector (genesets reduced to it).
#' @param direction "up", "down" or "both".
#' @return list(hyps = named list with "up" and/or "down" hyp, dropped = chr,
#'   warnings = chr)
#' @noRd
runFgseaHyps <- function(stats, genesets, background, direction, power, seed, fgsea_args, quiet) {
  gsets_obj <- if (methods::is(genesets, "gsets") || methods::is(genesets, "rgsets")) {
    genesets
  } else {
    hypeR::gsets$new(genesets, quiet = TRUE)
  }
  if (base::is.character(background)) {
    gsets_obj <- gsets_obj$reduce(background)
  }
  pathways <- gsets_obj$genesets
  effective_args <- utils::modifyList(HYPER_FGSEA_DEFAULT_ARGS, fgsea_args)

  warnings <- base::character()
  run_fgsea <- function() {
    base::do.call(fgsea::fgseaMultilevel, c(base::list(pathways = pathways, stats = stats, gseaParam = power), effective_args))
  }
  raw <- withHypeRSeed(seed, base::withCallingHandlers(
    {
      if (quiet) {
        result <- NULL
        utils::capture.output(result <- run_fgsea())
        result
      } else {
        run_fgsea()
      }
    },
    warning = function(w) {
      warnings <<- c(warnings, base::conditionMessage(w))
      base::invokeRestart("muffleWarning")
    }
  ))
  raw <- base::as.data.frame(raw)

  table <- fgseaHypeRTable(raw, stats, pathways)
  dropped <- base::setdiff(base::names(pathways), table$label)

  info <- base::list(
    "hypeR" = base::paste0("v", utils::packageVersion("hypeR")),
    "Signature Head" = base::paste(utils::head(base::names(stats)), collapse = ","),
    "Signature Size" = base::length(stats),
    "Signature Type" = "weighted",
    "Genesets" = gsets_obj$info(),
    "Background" = if (base::is.character(background)) base::length(background) else background,
    "P-Value" = 1,
    "FDR" = 1,
    "Test" = "fgsea",
    "Power" = power,
    "Absolute" = FALSE,
    "fgsea" = base::paste0("v", utils::packageVersion("fgsea")),
    "Sample Size" = effective_args$sampleSize,
    "Min Size" = effective_args$minSize,
    "Max Size" = effective_args$maxSize,
    "Seed" = if (base::is.null(seed)) "" else seed,
    "fgsea Args" = if (base::length(fgsea_args) == 0) "" else base::paste(
      base::sprintf("%s=%s", base::names(fgsea_args), base::vapply(fgsea_args, function(v) base::paste(base::format(v), collapse = ","), base::character(1))),
      collapse = "; "
    )
  )
  info <- base::lapply(info, base::as.character)
  args <- base::list(
    signature = stats, genesets = gsets_obj, test = "fgsea", background = background, power = power,
    absolute = FALSE, pval = 1, fdr = 1, seed = seed, fgsea_args = effective_args
  )

  sides <- if (base::identical(direction, "both")) c("up", "down") else direction
  hyps <- base::lapply(stats::setNames(sides, sides), function(side) {
    rows <- if (side == "up") table$es > 0 else table$es < 0
    side_data <- table[rows, , drop = FALSE]
    side_data <- side_data[base::order(side_data$pval, side_data$es), , drop = FALSE]
    base::rownames(side_data) <- NULL
    hypeR::hyp$new(data = side_data, plots = base::list(), args = args, info = info)
  })

  base::list(hyps = hyps, dropped = dropped, warnings = base::unique(warnings))
}
