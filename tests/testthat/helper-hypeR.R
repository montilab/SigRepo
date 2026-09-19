# Fixtures for the hypeR client tests. testthat sources helper-*.R before the
# test files. OmicSignature requires group_label to be a factor.

hyper_sig_table <- function(symbols = TRUE) {
  tbl <- data.frame(
    probe_id = c("p1", "p2", "p3", "p4"),
    feature_name = c("f1", "f2", "f3", "f4"),
    score = c(3, 2, -2, -3),
    group_label = factor(c("Old", "Old", "Young", "Young")),
    stringsAsFactors = FALSE
  )
  if (symbols) {
    tbl$symbol <- c("A", "B", "C", "D")
  }
  tbl
}

# Full difexp: gene_symbol column, symbol E measured twice (scores 0.5 and -1).
hyper_difexp_table <- function() {
  data.frame(
    probe_id = paste0("p", 1:6),
    feature_name = paste0("f", 1:6),
    gene_symbol = c("A", "B", "C", "D", "E", "E"),
    score = c(3, 2, -2, -3, 0.5, -1),
    p_value = c(0.001, 0.01, 0.01, 0.001, 0.5, 0.2),
    adj_p = c(0.01, 0.05, 0.05, 0.01, 0.6, 0.3),
    group_label = factor(c("Old", "Old", "Young", "Young", "Old", "Young")),
    stringsAsFactors = FALSE
  )
}

make_hyper_sig <- function(name = "sig_a",
                           signature = hyper_sig_table(),
                           difexp = NULL,
                           direction_type = "bi-directional",
                           organism = "Homo sapiens",
                           assay_type = "transcriptomics") {
  OmicSignature::OmicSignature$new(
    metadata = list(
      signature_name = name,
      assay_type = assay_type,
      phenotype = "test_phenotype",
      organism = organism,
      direction_type = direction_type,
      others = list()
    ),
    signature = signature,
    difexp = difexp,
    print_message = FALSE
  )
}

hyper_genesets <- function() {
  list(
    SET_AB = c("A", "B", "X1", "X2"),
    SET_CD = c("C", "D", "X3"),
    SET_E = c("E", "X4", "X5")
  )
}

# Signature whose kstest vector is exactly c(A = 2, D = 1, B = 0, C = -1):
# B scores exactly 0, so a geneset hitting only B has no weight.
make_hyper_zero_score_sig <- function() {
  make_hyper_sig(
    name = "zero",
    signature = data.frame(
      probe_id = c("p1", "p4"), feature_name = c("f1", "f4"), score = c(2, 1),
      group_label = factor(c("Up", "Up")), symbol = c("A", "D")
    ),
    difexp = data.frame(
      probe_id = paste0("p", 1:4), feature_name = paste0("f", 1:4),
      gene_symbol = c("A", "B", "C", "D"), score = c(2, 0, -1, 1),
      p_value = 0.01, adj_p = 0.05, group_label = factor(c("Up", "Up", "Down", "Up"))
    )
  )
}

hyper_zero_score_genesets <- function() {
  list(S1 = c("A", "D"), S2 = "B", S3 = "C")
}

# Signature whose kstest vector is exactly c(A = 3, B = 2, C = -1), for
# genesets that contain every query gene.
make_hyper_cover_sig <- function() {
  make_hyper_sig(
    name = "cover",
    signature = data.frame(
      probe_id = c("p1", "p2"), feature_name = c("f1", "f2"), score = c(3, 2),
      group_label = factor(c("Up", "Up")), symbol = c("A", "B")
    ),
    difexp = data.frame(
      probe_id = paste0("p", 1:3), feature_name = paste0("f", 1:3),
      gene_symbol = c("A", "B", "C"), score = c(3, 2, -1),
      p_value = 0.01, adj_p = 0.05, group_label = factor(c("Up", "Up", "Down"))
    )
  )
}

# runHypeR() for the toy fixtures. Their hypergeometric queries hold 1-4 genes,
# below runHypeR()'s default min_query_genes = 4, so tests about anything else
# lower it; every other argument keeps runHypeR()'s defaults.
runToyHypeR <- function(..., min_query_genes = 1) {
  SigRepo::runHypeR(..., min_query_genes = min_query_genes)
}

# Categorical signature with two categories (red, white). Signature table:
# red up A, red down B, red C scores 0; white up D and F, white down E.
# Difexp is long: one row per gene per category, each with that category's
# signed score. `red_difexp_scores` overrides red's difexp scores (A-F).
make_hyper_categorical_sig <- function(red_difexp_scores = c(2, -1, 0.5, -0.3, 0.2, 0.1)) {
  OmicSignature::OmicSignature$new(
    metadata = list(
      signature_name = "cat",
      assay_type = "transcriptomics",
      phenotype = "test_phenotype",
      organism = "Homo sapiens",
      direction_type = "categorical",
      category_num = 2,
      others = list()
    ),
    signature = data.frame(
      probe_id = paste0("p", 1:6), feature_name = paste0("f", 1:6), symbol = c("A", "B", "C", "D", "E", "F"),
      score = c(2, -1, 0, 3, -2, 1), group_label = factor(rep(c("red", "white"), each = 3)),
      stringsAsFactors = FALSE
    ),
    difexp = data.frame(
      probe_id = rep(paste0("p", 1:6), 2), feature_name = rep(paste0("f", 1:6), 2),
      gene_symbol = rep(c("A", "B", "C", "D", "E", "F"), 2),
      score = c(red_difexp_scores, -1, 0.4, -2, 3, 1, -0.5),
      p_value = 0.01, adj_p = 0.05, group_label = factor(rep(c("red", "white"), each = 6)),
      stringsAsFactors = FALSE
    ),
    print_message = FALSE
  )
}

# A difexp the default background accepts as complete: proteomics (no
# transcriptome row minimum), more than twice the 4-row signature, and p-values
# up to 0.6. Measures A-E plus G-J.
hyper_complete_difexp_table <- function() {
  extra <- data.frame(
    probe_id = paste0("p", 7:10), feature_name = paste0("f", 7:10), gene_symbol = c("G", "H", "I", "J"),
    score = c(1.5, -1.5, 0.3, -0.3), p_value = 0.4, adj_p = 0.5, group_label = factor(c("Old", "Young", "Old", "Young")),
    stringsAsFactors = FALSE
  )
  rbind(hyper_difexp_table(), extra)
}

# ---- fgsea fixtures ----

# 40 genes G01-G40 with distinct signed scores, highest first: G01-G20 score
# 4.0 down to 0.2, G21-G40 score -0.1 down to -3.9 (no score is 0).
hyper_fgsea_stats <- function() {
  stats::setNames(c(seq(4, 0.2, by = -0.2), seq(-0.1, -3.9, by = -0.2)), sprintf("G%02d", 1:40))
}

# TOP/TOP_MIX sit at the top (ES > 0) and share 4 genes; BOTTOM/BOTTOM_MIX sit
# at the bottom (ES < 0) and share 4 genes, so hyp_emap() has an edge on each
# side. MIDDLE straddles the centre symmetrically, so fgsea gives it ES = 0.
hyper_fgsea_genesets <- function() {
  genes <- sprintf("G%02d", 1:40)
  list(
    TOP = genes[1:8],
    TOP_MIX = c(genes[1:4], genes[9:12]),
    BOTTOM = genes[33:40],
    BOTTOM_MIX = c(genes[37:40], genes[29:32]),
    MIDDLE = genes[17:24],
    SPREAD = genes[c(2, 11, 20, 29, 38)]
  )
}

# Bi-directional signature whose difexp ranks to hyper_fgsea_stats().
make_hyper_fgsea_sig <- function(name = "fg") {
  stats <- hyper_fgsea_stats()
  difexp <- data.frame(
    probe_id = paste0("p", 1:40), feature_name = paste0("f", 1:40), gene_symbol = names(stats),
    score = unname(stats), p_value = 0.01, adj_p = 0.05,
    group_label = factor(ifelse(stats > 0, "Up", "Down")), stringsAsFactors = FALSE
  )
  make_hyper_sig(name, signature = difexp[c(1:4, 37:40), c("probe_id", "feature_name", "score", "group_label", "gene_symbol")],
                 difexp = difexp)
}

# ---- plotting fixtures ----

# A hypeR hyp built directly from a results table, for plot-data tests that
# need exact p-values/FDRs. `extra` adds columns (e.g. nes, score); `info`
# adds info keys on top of Test.
make_dot_hyp <- function(labels, fdr, test = "hypergeometric", extra = list(), info = list()) {
  data <- data.frame(
    label = labels, pval = fdr / 2, fdr = fdr,
    geneset = seq_along(labels) * 10L, overlap = seq_along(labels), stringsAsFactors = FALSE
  )
  for (column in names(extra)) {
    data[[column]] <- extra[[column]]
  }
  hypeR::hyp$new(data = data, info = c(list(Test = test), info))
}

# 11 signed scores (no zero) and a geneset with a single hit at position 6: its
# running sum reaches exactly +0.5 and -0.5 (weighted) or +/-5/11 (ranked), the
# tie behind the production HALLMARK_NOTCH_SIGNALING case.
hyper_tie_stats <- function() {
  stats::setNames(c(5, 4, 3, 2, 1, 0.5, -1, -2, -3, -4, -5), sprintf("T%02d", 1:11))
}

hyper_tie_genesets <- function() {
  list(TIE = "T06", TOP = c("T01", "T02", "T03"), BOT = c("T09", "T10", "T11"))
}
