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
                           organism = "Homo sapiens") {
  OmicSignature::OmicSignature$new(
    metadata = list(
      signature_name = name,
      assay_type = "transcriptomics",
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
