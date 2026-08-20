# Activation and behavior tests

## Should activate

- "Create a SigRepo OmicSignature Rmd from this paper and supplement."
- "Review whether this selected protein table should use `difexp = NULL`."
- "Fix the probe IDs in this OmicSignature workflow."
- "Map these symbols to Ensembl v114 and prepare the review audit."
- "Validate this study before moving it to `approved_examples`."
- "Create a collection for these tissue-specific signatures."

## Should not activate

- "Run a standard DESeq2 analysis for my manuscript" when no SigRepo ingestion
  is requested.
- "Explain what log2 fold change means."
- "Create a general gene-set enrichment plot."
- "Summarize this omics paper" without a SigRepo object or ingestion goal.

## Expected stop behavior

The skill must stop rather than guess when:

- the contrast numerator/reference is unknown;
- the selection threshold is absent;
- the table may already be filtered but the rule is unknown;
- a sample type cannot be matched to the controlled vocabulary;
- a one-to-many mapping requires biological review;
- a publication identifier is unavailable.
