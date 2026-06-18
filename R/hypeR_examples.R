annotate_module_ui <- function(id) {
  ns <- NS(id)
  page_selector <- paste0("#", ns("annotate_page"))
  
  tagList(
    tags$style(HTML(paste0("
      ", page_selector, " {
        padding-top: 28px;
        padding-bottom: 32px;
      }

      ", page_selector, " .annotate-hero {
        margin-bottom: 18px;
        padding: 24px 28px;
        border-radius: 14px;
        background: linear-gradient(135deg, #0f3b63 0%, #1b5d8f 100%);
        color: #ffffff;
        box-shadow: 0 10px 24px rgba(15, 59, 99, 0.18);
      }

      ", page_selector, " .annotate-hero h2 {
        margin-top: 0;
        margin-bottom: 8px;
        font-weight: 700;
      }

      ", page_selector, " .annotate-hero p {
        margin-bottom: 0;
        color: rgba(255, 255, 255, 0.88);
      }

      ", page_selector, " .annotate-card {
        margin-bottom: 18px;
        padding: 20px 22px;
        border: 1px solid #d9e3ec;
        border-radius: 12px;
        background: #ffffff;
        box-shadow: 0 6px 18px rgba(15, 32, 56, 0.06);
      }

      ", page_selector, " .annotate-card h3,
      ", page_selector, " .annotate-card h4 {
        margin-top: 0;
        margin-bottom: 12px;
        color: #17324d;
        font-weight: 600;
      }

      ", page_selector, " .annotate-step-label {
        display: inline-block;
        margin-bottom: 10px;
        padding: 4px 10px;
        border-radius: 999px;
        background: #e9f2f9;
        color: #0f4d7c;
        font-size: 12px;
        font-weight: 700;
        letter-spacing: 0.04em;
        text-transform: uppercase;
      }

      ", page_selector, " .annotate-summary-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 12px;
      }

      ", page_selector, " .annotate-summary-item {
        padding: 12px 14px;
        border-radius: 10px;
        background: #f6f9fc;
        border: 1px solid #e1ebf2;
      }

      ", page_selector, " .annotate-summary-item strong {
        display: block;
        margin-bottom: 4px;
        color: #0f3b63;
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.04em;
      }

      ", page_selector, " .annotate-summary-item span {
        color: #17324d;
        font-size: 15px;
        font-weight: 600;
      }

      ", page_selector, " .annotate-feedback {
        margin-top: 10px;
      }

      ", page_selector, " .annotate-actions {
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
        margin-top: 14px;
      }

      ", page_selector, " .annotate-empty {
        padding: 18px;
        border: 1px dashed #c5d5e3;
        border-radius: 10px;
        background: #f8fbfd;
        color: #4b647e;
      }

      ", page_selector, " .annotate-results-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 12px;
        flex-wrap: wrap;
        margin-bottom: 16px;
      }

      ", page_selector, " .annotate-results-actions .btn {
        margin-left: 8px;
      }

      ", page_selector, " .annotate-results-body {
        display: grid;
        grid-template-columns: minmax(0, 1fr);
        gap: 22px;
      }

      ", page_selector, " .annotate-results-section h4 {
        margin-top: 0;
        margin-bottom: 12px;
        color: #17324d;
        font-weight: 600;
      }

      ", page_selector, " .annotate-signature-key {
        margin-top: 14px;
      }

      ", page_selector, " .annotate-signature-key .dataTables_wrapper {
        font-size: 12px;
      }

      ", page_selector, " .geneset-filter-group {
        padding: 16px;
        border: 1px solid #d9e3ec;
        border-radius: 10px;
        background: #f8fbfd;
      }

      ", page_selector, " .geneset-filter-heading h4 {
        margin-top: 0;
        margin-bottom: 6px;
      }

      ", page_selector, " .geneset-filter-heading p {
        margin-bottom: 14px;
        color: #597189;
      }

      ", page_selector, " .geneset-filter-actions {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 12px;
        flex-wrap: wrap;
        margin-bottom: 12px;
      }

      ", page_selector, " .geneset-status {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        padding: 8px 12px;
        border-radius: 999px;
        font-size: 13px;
        font-weight: 600;
      }

      ", page_selector, " .geneset-status-pending {
        background: #eef3f7;
        color: #4b647e;
      }

      ", page_selector, " .geneset-status-ready {
        background: #e7f5ec;
        color: #21663c;
      }

      ", page_selector, " .geneset-summary-text {
        margin-top: 10px;
        margin-bottom: 0;
        color: #3f5873;
        font-size: 13px;
      }
    "))),
    
    div(
      id = ns("annotate_page"),
      
      div(
        class = "annotate-hero",
        tags$h2("Annotate Signatures"),
        tags$p(
          "Configure an enrichment analysis, choose a geneset collection, review your selections, and run hypeR from a single workflow."
        )
      ),
      
      fluidRow(
        column(
          width = 4,
          
          div(
            class = "annotate-card",
            span(class = "annotate-step-label", "Step 1"),
            tags$h3("Analysis Setup"),
            textInput(
              ns("experiment_label"),
              "Experiment Label",
              placeholder = "Example: Knockout Experiment"
            ),
            radioButtons(
              ns("enrichment_type"),
              "Enrichment Method",
              choices = c(
                "Hypergeometric" = "hypergeo",
                "KS Test" = "kstest",
                "GSEA" = "gsea",
                "GEM Hypergeometric" = "gem_hypergeo",
                "GEM Weighted" = "gem_weighted"
              ),
              inline = FALSE
            ),
            helpText(
              "Hypergeometric uses feature lists. KS Test and GSEA expect ranked signatures. GEM modes map metabolite signatures to genes before enrichment."
            ),
            uiOutput(ns("gem_options")),
            numericInput(
              ns("enrichment_thresh"),
              "FDR Threshold",
              value = 0.05,
              min = 0,
              max = 1,
              step = 0.01
            ),
            numericInput(
              ns("enrichment_bg"),
              "Background Gene Count",
              value = 36000,
              min = 1,
              step = 100
            )
          ),
          
          div(
            class = "annotate-card",
            span(class = "annotate-step-label", "Step 2"),
            tags$h3("Geneset Selection"),
            tags$p(
              "Use the filter set below to define the genesets included in this enrichment run."
            ),
            selectInput(
              ns("species"),
              "Species",
              choices = msigdbr::msigdbr_species()$species_name,
              selected = "Homo sapiens"
            ),
            genesets_hypeR_UI(ns("genesets"))
          )
        ),
        
        column(
          width = 8,
          
          div(
            class = "annotate-card",
            span(class = "annotate-step-label", "Step 3"),
            tags$h3("Select Signatures"),
            tags$p(
              "Choose up to 10 signatures from the repository, then add them to the analysis."
            ),
            DT::DTOutput(ns("signature_hypeR")),
            div(
              class = "annotate-actions",
              actionButton(
                ns("signature_add"),
                "Add Selected Signatures",
                class = "btn-primary"
              )
            ),
            div(
              class = "annotate-feedback",
              uiOutput(ns("signature_feedback"))
            )
          ),
          
          div(
            class = "annotate-card",
            span(class = "annotate-step-label", "Step 4"),
            tags$h3("Review and Run"),
            uiOutput(ns("analysis_summary")),
            div(
              class = "annotate-actions",
              actionButton(
                ns("enrichment_do"),
                "Run Enrichment",
                class = "btn-primary"
              ),
              actionButton(
                ns("experiment_reset"),
                "New Experiment",
                class = "btn-default"
              )
            )
          ),
          
          div(
            class = "annotate-card",
            tags$h3("Current Selection"),
            uiOutput(ns("signature_preview"))
          )
        )
      ),
      
      div(
        class = "annotate-card",
        div(
          class = "annotate-results-header",
          div(
            tagList(
              span(class = "annotate-step-label", "Results"),
              tags$h3("Experiment Results")
            )
          ),
          div(
            class = "annotate-results-actions",
            downloadButton(ns("generate_report"), "Export Analysis Bundle"),
            downloadButton(ns("export_hyp"), "Export Hype Object")
          )
        ),
        uiOutput(ns("enrichment"))
      )
    )
  )
}


hype_dotplot_data <- function(hyp, fdr_threshold, top = 30, abrv = 50) {
  if (is.null(fdr_threshold) || length(fdr_threshold) == 0 || is.na(fdr_threshold)) {
    fdr_threshold <- 1
  }
  
  empty_df <- data.frame(
    signature = character(),
    label = character(),
    fdr = numeric(),
    geneset_size = numeric(),
    stringsAsFactors = FALSE
  )
  
  hyp_entries <- if (methods::is(hyp, "multihyp")) {
    hyp$data
  } else if (is.list(hyp) && all(c("info", "data") %in% names(hyp))) {
    list(Enrichment = hyp)
  } else if (is.list(hyp) && length(hyp) > 0 && all(vapply(hyp, function(x) is.list(x) && all(c("info", "data") %in% names(x)), logical(1)))) {
    hyp
  } else {
    list(Enrichment = hyp)
  }
  
  if (is.null(names(hyp_entries)) || any(!nzchar(names(hyp_entries)))) {
    names(hyp_entries) <- paste("Signature", seq_along(hyp_entries))
  }
  
  plot_dfs <- lapply(seq_along(hyp_entries), function(i) {
    hyp_entry <- hyp_entries[[i]]
    hyp_df <- if (is.data.frame(hyp_entry)) hyp_entry else hyp_entry$data
    
    if (is.null(hyp_df) || !is.data.frame(hyp_df) || nrow(hyp_df) == 0) {
      return(NULL)
    }
    
    if (!all(c("label", "fdr") %in% names(hyp_df))) {
      return(NULL)
    }
    
    hyp_df$fdr <- suppressWarnings(as.numeric(hyp_df$fdr))
    hyp_df <- hyp_df[!is.na(hyp_df$fdr) & hyp_df$fdr <= fdr_threshold, , drop = FALSE]
    
    if (nrow(hyp_df) == 0) {
      return(NULL)
    }
    
    geneset_size <- rep(1, nrow(hyp_df))
    if ("geneset" %in% names(hyp_df)) {
      geneset_size <- suppressWarnings(as.numeric(hyp_df$geneset))
      geneset_size[is.na(geneset_size) | geneset_size <= 0] <- 1
    }
    
    data.frame(
      signature = names(hyp_entries)[[i]],
      label = substr(as.character(hyp_df$label), 1, abrv),
      fdr = hyp_df$fdr,
      geneset_size = geneset_size,
      stringsAsFactors = FALSE
    )
  })
  
  plot_dfs <- plot_dfs[!vapply(plot_dfs, is.null, logical(1))]
  
  if (length(plot_dfs) == 0) {
    return(empty_df)
  }
  
  plot_df <- do.call(rbind, plot_dfs)
  label_rank <- stats::aggregate(fdr ~ label, data = plot_df, FUN = min)
  label_rank <- label_rank[order(label_rank$fdr), , drop = FALSE]
  top_labels <- head(label_rank$label, top)
  plot_df <- plot_df[plot_df$label %in% top_labels, , drop = FALSE]
  plot_df[order(plot_df$fdr), , drop = FALSE]
}


build_dotplot_figure <- function(plot_df, title = "") {
  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = signature_label,
      y = label,
      color = fdr_plot,
      size = geneset_size
    )
  ) +
    ggplot2::geom_point(alpha = 0.86) +
    ggplot2::scale_color_continuous(
      low = "#E53935",
      high = "#114357",
      trans = "log10",
      guide = ggplot2::guide_colorbar(reverse = TRUE)
    ) +
    ggplot2::scale_size_continuous(trans = "log10") +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = NULL,
      color = "FDR",
      size = "Geneset Size"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.text.x = ggplot2::element_text(face = "bold"),
      panel.grid.major.y = ggplot2::element_line(color = "#e6edf3"),
      panel.grid.minor = ggplot2::element_blank()
    )
}


build_enrichment_signatures <- function(sig_objs, sig_list) {
  extract_signature_table <- function(sig_obj) {
    table_candidates <- list(sig_obj$signature, sig_obj$difexp)
    
    for (candidate in table_candidates) {
      if (is.data.frame(candidate) && nrow(candidate) > 0) {
        return(candidate)
      }
    }
    
    NULL
  }
  
  find_first_column <- function(df, candidates) {
    matched <- candidates[candidates %in% names(df)][1]
    if (is.na(matched) || !nzchar(matched)) {
      return(NULL)
    }
    matched
  }
  
  build_group_component <- function(group_label, suffix = NULL) {
    clean_group <- if (is.null(group_label) || is.na(group_label) || !nzchar(group_label)) {
      "All Features"
    } else {
      group_label
    }
    
    if (is.null(suffix) || !nzchar(suffix)) {
      return(clean_group)
    }
    
    if (identical(clean_group, "All Features")) {
      return(suffix)
    }
    
    paste0(clean_group, "_", suffix)
  }
  
  recover_symbol_column <- function(signature_df, sig_obj, symbol_col) {
    if (!is.null(symbol_col)) {
      return(list(data = signature_df, symbol_col = symbol_col))
    }
    
    difexp_df <- sig_obj$difexp
    if (!is.data.frame(difexp_df) || nrow(difexp_df) == 0) {
      return(list(data = signature_df, symbol_col = NULL))
    }
    
    difexp_symbol_col <- find_first_column(
      difexp_df,
      c("symbol", "gene_symbol", "feature_name", "gene", "gene_name")
    )
    if (is.null(difexp_symbol_col)) {
      return(list(data = signature_df, symbol_col = NULL))
    }
    
    map_col <- NULL
    if ("probe_id" %in% names(signature_df) && "probe_id" %in% names(difexp_df)) {
      map_col <- "probe_id"
    } else if ("feature_name" %in% names(signature_df) && "feature_name" %in% names(difexp_df)) {
      map_col <- "feature_name"
    }
    
    if (is.null(map_col)) {
      return(list(data = signature_df, symbol_col = NULL))
    }
    
    key_values <- as.character(difexp_df[[map_col]])
    symbol_values <- as.character(difexp_df[[difexp_symbol_col]])
    valid <- !is.na(key_values) & nzchar(key_values) & !is.na(symbol_values) & nzchar(symbol_values)
    
    if (!any(valid)) {
      return(list(data = signature_df, symbol_col = NULL))
    }
    
    symbol_map <- stats::setNames(symbol_values[valid], key_values[valid])
    signature_keys <- as.character(signature_df[[map_col]])
    recovered_symbols <- unname(symbol_map[signature_keys])
    
    if (all(is.na(recovered_symbols) | !nzchar(recovered_symbols))) {
      return(list(data = signature_df, symbol_col = NULL))
    }
    
    signature_df$symbol <- recovered_symbols
    list(data = signature_df, symbol_col = "symbol")
  }
  
  empty_result <- list(
    vectors = list(),
    metadata = data.frame(
      signature = character(),
      signature_name = character(),
      group_label = character(),
      signature_order = numeric(),
      group_order = numeric(),
      stringsAsFactors = FALSE
    )
  )
  
  if (length(sig_objs) == 0 || length(sig_list) == 0) {
    return(empty_result)
  }
  
  sig_names <- vapply(sig_list, `[[`, character(1), "signature_name")
  vectors <- list()
  metadata <- vector("list", length(sig_objs))
  metadata_idx <- 0
  
  for (i in seq_along(sig_objs)) {
    sig_obj <- sig_objs[[i]]
    sig_name <- sig_names[[i]]
    
    signature_df <- extract_signature_table(sig_obj)
    if (is.null(signature_df)) {
      next
    }
    
    symbol_col <- find_first_column(
      signature_df,
      c("symbol", "gene_symbol", "feature_name", "gene", "gene_name")
    )
    if (identical(symbol_col, "feature_name")) {
      recovered <- recover_symbol_column(signature_df, sig_obj, NULL)
      signature_df <- recovered$data
      if (!is.null(recovered$symbol_col)) {
        symbol_col <- recovered$symbol_col
      }
    } else if (is.null(symbol_col)) {
      recovered <- recover_symbol_column(signature_df, sig_obj, symbol_col)
      signature_df <- recovered$data
      symbol_col <- recovered$symbol_col
    }
    
    if (is.null(symbol_col)) {
      next
    }
    
    signature_df$symbol <- as.character(signature_df[[symbol_col]])
    signature_df <- signature_df[!is.na(signature_df$symbol) & nzchar(signature_df$symbol), , drop = FALSE]
    
    if (nrow(signature_df) == 0) {
      next
    }
    
    if ("group_label" %in% names(signature_df)) {
      signature_df$group_label <- as.character(signature_df$group_label)
      signature_df$group_label[is.na(signature_df$group_label) | !nzchar(signature_df$group_label)] <- "All Features"
    } else {
      signature_df$group_label <- "All Features"
    }
    
    score_col <- find_first_column(signature_df, c("score", "t_stat", "stat", "t", "logfc", "logFC"))
    has_score <- !is.null(score_col)
    if (has_score) {
      signature_df$score_value <- suppressWarnings(as.numeric(signature_df[[score_col]]))
      has_score <- any(!is.na(signature_df$score_value))
    }
    
    group_levels <- unique(signature_df$group_label)
    group_order_index <- 0
    
    for (group_label in group_levels) {
      group_df <- signature_df[signature_df$group_label == group_label, , drop = FALSE]
      
      if (!has_score) {
        group_symbols <- unique(group_df$symbol)
        if (length(group_symbols) == 0) {
          next
        }
        
        group_component <- build_group_component(group_label)
        signature_key <- sprintf("%s | %s", sig_name, group_component)
        vectors[[signature_key]] <- group_symbols
        
        group_order_index <- group_order_index + 1
        metadata_idx <- metadata_idx + 1
        metadata[[metadata_idx]] <- data.frame(
          signature = signature_key,
          signature_name = sig_name,
          group_label = group_component,
          signature_order = i,
          group_order = group_order_index,
          stringsAsFactors = FALSE
        )
        next
      }
      
      direction_specs <- list(
        list(suffix = "up", keep = !is.na(group_df$score_value) & group_df$score_value >= 0),
        list(suffix = "dn", keep = !is.na(group_df$score_value) & group_df$score_value < 0)
      )
      
      for (direction in direction_specs) {
        group_symbols <- unique(group_df$symbol[direction$keep])
        if (length(group_symbols) == 0) {
          next
        }
        
        group_component <- build_group_component(group_label, direction$suffix)
        signature_key <- sprintf("%s | %s", sig_name, group_component)
        vectors[[signature_key]] <- group_symbols
        
        group_order_index <- group_order_index + 1
        metadata_idx <- metadata_idx + 1
        metadata[[metadata_idx]] <- data.frame(
          signature = signature_key,
          signature_name = sig_name,
          group_label = group_component,
          signature_order = i,
          group_order = group_order_index,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  metadata <- metadata[seq_len(metadata_idx)]
  metadata_df <- if (length(metadata) > 0) do.call(rbind, metadata) else empty_result$metadata
  
  list(
    vectors = vectors,
    metadata = metadata_df
  )
}


build_ranked_enrichment_signatures <- function(sig_objs, sig_list, mode = c("ks", "gsea")) {
  find_first_column <- function(df, candidates) {
    matched <- candidates[candidates %in% names(df)][1]
    if (is.na(matched) || !nzchar(matched)) {
      return(NULL)
    }
    matched
  }
  
  build_group_component <- function(group_label, suffix = NULL) {
    clean_group <- if (is.null(group_label) || is.na(group_label) || !nzchar(group_label)) {
      "All Features"
    } else {
      group_label
    }
    
    if (is.null(suffix) || !nzchar(suffix)) {
      return(clean_group)
    }
    
    if (identical(clean_group, "All Features")) {
      return(suffix)
    }
    
    paste0(clean_group, "_", suffix)
  }
  
  mode <- match.arg(mode)
  empty_result <- list(
    vectors = list(),
    metadata = data.frame(
      signature = character(),
      signature_name = character(),
      group_label = character(),
      signature_order = numeric(),
      group_order = numeric(),
      stringsAsFactors = FALSE
    )
  )
  
  if (length(sig_objs) == 0 || length(sig_list) == 0) {
    return(empty_result)
  }
  
  sig_names <- vapply(sig_list, `[[`, character(1), "signature_name")
  rank_candidates <- c("score", "t_stat", "stat", "t", "logfc", "logFC")
  vectors <- list()
  metadata <- vector("list", length(sig_objs) * 4)
  metadata_idx <- 0
  
  for (i in seq_along(sig_objs)) {
    sig_obj <- sig_objs[[i]]
    sig_name <- sig_names[[i]]
    difexp_df <- sig_obj$difexp
    
    if (is.null(difexp_df) || !is.data.frame(difexp_df) || nrow(difexp_df) == 0) {
      next
    }
    
    symbol_col <- find_first_column(
      difexp_df,
      c("symbol", "gene_symbol", "feature_name", "gene", "gene_name")
    )
    if (is.null(symbol_col)) {
      next
    }
    
    rank_col <- find_first_column(difexp_df, rank_candidates)
    if (is.null(rank_col)) {
      next
    }
    
    difexp_df$symbol <- as.character(difexp_df[[symbol_col]])
    difexp_df$rank_value <- suppressWarnings(as.numeric(difexp_df[[rank_col]]))
    difexp_df <- difexp_df[!is.na(difexp_df$symbol) & nzchar(difexp_df$symbol) & !is.na(difexp_df$rank_value), , drop = FALSE]
    
    if (nrow(difexp_df) == 0) {
      next
    }
    
    if (identical(mode, "gsea")) {
      rank_df <- difexp_df[, c("symbol", "rank_value"), drop = FALSE]
      rank_df <- rank_df[order(-abs(rank_df$rank_value)), , drop = FALSE]
      rank_df <- rank_df[!duplicated(rank_df$symbol), , drop = FALSE]
      
      if (nrow(rank_df) == 0) {
        next
      }
      
      ordered_df <- rank_df[order(rank_df$rank_value, decreasing = TRUE), , drop = FALSE]
      ranked_signature <- stats::setNames(ordered_df$rank_value, ordered_df$symbol)
      
      if (length(ranked_signature) == 0) {
        next
      }
      
      signature_key <- sprintf("%s | Ranked", sig_name)
      vectors[[signature_key]] <- ranked_signature
      
      metadata_idx <- metadata_idx + 1
      metadata[[metadata_idx]] <- data.frame(
        signature = signature_key,
        signature_name = sig_name,
        group_label = "Ranked",
        signature_order = i,
        group_order = 1,
        stringsAsFactors = FALSE
      )
    } else {
      if ("group_label" %in% names(difexp_df)) {
        difexp_df$group_label <- as.character(difexp_df$group_label)
        difexp_df$group_label[is.na(difexp_df$group_label) | !nzchar(difexp_df$group_label)] <- "All Features"
      } else {
        difexp_df$group_label <- "All Features"
      }
      
      group_levels <- unique(difexp_df$group_label)
      group_order_index <- 0
      direction_specs <- list(
        list(suffix = "up", decreasing = TRUE),
        list(suffix = "dn", decreasing = FALSE)
      )
      
      for (group_label in group_levels) {
        group_df <- difexp_df[difexp_df$group_label == group_label, , drop = FALSE]
        rank_df <- group_df[, c("symbol", "rank_value"), drop = FALSE]
        rank_df <- rank_df[order(-abs(rank_df$rank_value)), , drop = FALSE]
        rank_df <- rank_df[!duplicated(rank_df$symbol), , drop = FALSE]
        
        if (nrow(rank_df) == 0) {
          next
        }
        
        for (direction in direction_specs) {
          ordered_df <- rank_df[order(rank_df$rank_value, decreasing = direction$decreasing), , drop = FALSE]
          ranked_signature <- stats::setNames(ordered_df$rank_value, ordered_df$symbol)
          
          if (length(ranked_signature) == 0) {
            next
          }
          
          group_component <- build_group_component(group_label, direction$suffix)
          signature_key <- sprintf("%s | %s", sig_name, group_component)
          vectors[[signature_key]] <- ranked_signature
          
          group_order_index <- group_order_index + 1
          metadata_idx <- metadata_idx + 1
          metadata[[metadata_idx]] <- data.frame(
            signature = signature_key,
            signature_name = sig_name,
            group_label = group_component,
            signature_order = i,
            group_order = group_order_index,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  metadata <- metadata[seq_len(metadata_idx)]
  metadata_df <- if (length(metadata) > 0) do.call(rbind, metadata) else empty_result$metadata
  
  list(
    vectors = vectors,
    metadata = metadata_df
  )
}


extract_hyp_results_table <- function(hyp) {
  if (inherits(hyp, "multihyp")) {
    result_list <- Map(function(signature_name, hyp_obj) {
      df <- hyp_obj$data
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(NULL)
      }
      
      df$signature <- signature_name
      df
    }, names(hyp$data), hyp$data)
    
    result_list <- result_list[!vapply(result_list, is.null, logical(1))]
    
    if (length(result_list) == 0) {
      return(data.frame())
    }
    
    return(do.call(rbind, result_list))
  }
  
  if (is.list(hyp) && all(c("info", "data") %in% names(hyp))) {
    df <- hyp$data
    if (is.null(df) || !is.data.frame(df)) {
      return(data.frame())
    }
    
    if (!"signature" %in% names(df) || all(is.na(df$signature)) || !any(nzchar(as.character(df$signature)))) {
      df$signature <- "Signature"
    }
    return(df)
  }
  
  if (is.list(hyp) && length(hyp) > 0 && all(vapply(hyp, function(x) is.list(x) && all(c("info", "data") %in% names(x)), logical(1)))) {
    result_list <- Map(function(signature_name, hyp_obj) {
      df <- hyp_obj$data
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(NULL)
      }
      
      df$signature <- signature_name
      df
    }, names(hyp), hyp)
    
    result_list <- result_list[!vapply(result_list, is.null, logical(1))]
    
    if (length(result_list) == 0) {
      return(data.frame())
    }
    
    return(do.call(rbind, result_list))
  }
  
  df <- hyp$data
  if (is.null(df) || !is.data.frame(df)) {
    return(data.frame())
  }
  
  if (!"signature" %in% names(df) || all(is.na(df$signature)) || !any(nzchar(as.character(df$signature)))) {
    df$signature <- "Signature"
  }
  df
}


hypergem_available <- function() {
  isTRUE(getOption("sigrepo.hypergem_available", FALSE)) ||
    "hypeR.GEM" %in% loadedNamespaces() ||
    requireNamespace("hypeR.GEM", quietly = TRUE)
}


hypergem_export <- function(name) {
  getExportedValue("hypeR.GEM", name)
}


build_gem_enrichment_signatures <- function(sig_objs, sig_list, reference_key = "refmet_name") {
  empty_result <- list(
    signatures = list(),
    metadata = data.frame(
      signature = character(),
      signature_name = character(),
      group_label = character(),
      signature_order = numeric(),
      group_order = numeric(),
      stringsAsFactors = FALSE
    )
  )
  
  if (length(sig_objs) == 0 || length(sig_list) == 0) {
    return(empty_result)
  }
  
  sig_names <- vapply(sig_list, `[[`, character(1), "signature_name")
  signatures <- list()
  metadata <- vector("list", length(sig_objs) * 4)
  metadata_idx <- 0
  
  for (i in seq_along(sig_objs)) {
    sig_obj <- sig_objs[[i]]
    sig_name <- sig_names[[i]]
    signature_df <- sig_obj$signature
    
    if (!is.data.frame(signature_df) || nrow(signature_df) == 0) {
      next
    }
    
    if (!reference_key %in% names(signature_df)) {
      next
    }
    
    signature_df <- signature_df[!is.na(signature_df[[reference_key]]) & nzchar(as.character(signature_df[[reference_key]])), , drop = FALSE]
    if (nrow(signature_df) == 0) {
      next
    }
    
    if ("group_label" %in% names(signature_df)) {
      signature_df$group_label <- as.character(signature_df$group_label)
      signature_df$group_label[is.na(signature_df$group_label) | !nzchar(signature_df$group_label)] <- "All Features"
    } else {
      signature_df$group_label <- "All Features"
    }
    
    score_candidates <- c("score", "logfc", "logFC")
    score_col <- score_candidates[score_candidates %in% names(signature_df)][1]
    has_score <- !is.na(score_col) && nzchar(score_col)
    if (has_score) {
      signature_df$score_value <- suppressWarnings(as.numeric(signature_df[[score_col]]))
      has_score <- any(!is.na(signature_df$score_value))
    }
    
    group_levels <- unique(signature_df$group_label)
    group_order_index <- 0
    
    for (group_label in group_levels) {
      group_df <- signature_df[signature_df$group_label == group_label, , drop = FALSE]
      
      if (!has_score) {
        signature_key <- sprintf("%s | %s", sig_name, group_label)
        signatures[[signature_key]] <- group_df
        
        group_order_index <- group_order_index + 1
        metadata_idx <- metadata_idx + 1
        metadata[[metadata_idx]] <- data.frame(
          signature = signature_key,
          signature_name = sig_name,
          group_label = group_label,
          signature_order = i,
          group_order = group_order_index,
          stringsAsFactors = FALSE
        )
        next
      }
      
      direction_specs <- list(
        list(suffix = "up", keep = !is.na(group_df$score_value) & group_df$score_value >= 0),
        list(suffix = "dn", keep = !is.na(group_df$score_value) & group_df$score_value < 0)
      )
      
      for (direction in direction_specs) {
        split_df <- group_df[direction$keep, , drop = FALSE]
        if (nrow(split_df) == 0) {
          next
        }
        
        group_component <- if (identical(group_label, "All Features")) {
          direction$suffix
        } else {
          paste0(group_label, "_", direction$suffix)
        }
        
        signature_key <- sprintf("%s | %s", sig_name, group_component)
        signatures[[signature_key]] <- split_df
        
        group_order_index <- group_order_index + 1
        metadata_idx <- metadata_idx + 1
        metadata[[metadata_idx]] <- data.frame(
          signature = signature_key,
          signature_name = sig_name,
          group_label = group_component,
          signature_order = i,
          group_order = group_order_index,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  metadata <- metadata[seq_len(metadata_idx)]
  metadata_df <- if (length(metadata) > 0) do.call(rbind, metadata) else empty_result$metadata
  
  list(
    signatures = signatures,
    metadata = metadata_df
  )
}


compute_gsea_curve_data <- function(ranked_signature, geneset_genes, power = 1) {
  signature_scores <- as.numeric(ranked_signature)
  signature_genes <- names(ranked_signature)
  
  valid <- !is.na(signature_scores) & !is.na(signature_genes) & nzchar(signature_genes)
  signature_scores <- signature_scores[valid]
  signature_genes <- signature_genes[valid]
  
  if (length(signature_scores) == 0) {
    stop("Ranked signature did not contain any valid values.", call. = FALSE)
  }
  
  geneset_genes <- unique(as.character(geneset_genes))
  geneset_genes <- geneset_genes[!is.na(geneset_genes) & nzchar(geneset_genes)]
  
  hit_index <- which(signature_genes %in% geneset_genes)
  if (length(hit_index) == 0) {
    stop("Selected geneset has no overlap with the ranked signature.", call. = FALSE)
  }
  
  hit_weights <- abs(signature_scores[hit_index])^power
  weight_sum <- sum(hit_weights)
  if (!is.finite(weight_sum) || weight_sum == 0) {
    hit_weights <- rep(1, length(hit_index))
    weight_sum <- sum(hit_weights)
  }
  
  n_total <- length(signature_scores)
  n_miss <- n_total - length(hit_index)
  miss_penalty <- if (n_miss > 0) 1 / n_miss else 0
  
  increments <- rep(-miss_penalty, n_total)
  increments[hit_index] <- hit_weights / weight_sum
  running_score <- cumsum(increments)
  
  max_idx <- which.max(running_score)
  min_idx <- which.min(running_score)
  
  if (abs(running_score[max_idx]) >= abs(running_score[min_idx])) {
    es_index <- max_idx
    es_score <- running_score[max_idx]
    es_direction <- "positive"
    leading_edge_hits <- hit_index[hit_index <= es_index]
    trailing_edge_hits <- hit_index[hit_index > es_index]
  } else {
    es_index <- min_idx
    es_score <- running_score[min_idx]
    es_direction <- "negative"
    leading_edge_hits <- hit_index[hit_index >= es_index]
    trailing_edge_hits <- hit_index[hit_index < es_index]
  }
  
  curve_df <- data.frame(
    position = seq_len(n_total),
    gene = signature_genes,
    score = signature_scores,
    running_score = running_score,
    hit = seq_len(n_total) %in% hit_index,
    edge_group = ifelse(
      seq_len(n_total) %in% leading_edge_hits,
      "Leading Edge",
      ifelse(seq_len(n_total) %in% trailing_edge_hits, "Trailing Edge", "Other")
    ),
    stringsAsFactors = FALSE
  )
  
  list(
    curve = curve_df,
    hit_index = hit_index,
    es_index = es_index,
    es_score = es_score,
    es_direction = es_direction,
    leading_edge_genes = signature_genes[leading_edge_hits],
    trailing_edge_genes = signature_genes[trailing_edge_hits]
  )
}


build_gsea_curve_figure <- function(curve_info) {
  curve_df <- curve_info$curve
  
  hit_df <- curve_df[curve_df$hit, , drop = FALSE]
  hit_df$hit_height <- ifelse(hit_df$edge_group == "Leading Edge", -0.08, -0.14)
  
  ggplot2::ggplot(curve_df, ggplot2::aes(x = position, y = running_score)) +
    ggplot2::geom_hline(yintercept = 0, color = "#aab7c4", linewidth = 0.4) +
    ggplot2::geom_line(color = "#0f4d7c", linewidth = 1) +
    ggplot2::geom_vline(
      xintercept = curve_info$es_index,
      color = "#d04a02",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    ggplot2::geom_segment(
      data = hit_df,
      ggplot2::aes(
        x = position,
        xend = position,
        y = hit_height,
        yend = 0,
        color = edge_group
      ),
      inherit.aes = FALSE,
      linewidth = 0.45,
      show.legend = TRUE
    ) +
    ggplot2::scale_color_manual(
      values = c("Leading Edge" = "#d04a02", "Trailing Edge" = "#6c8aa5", "Other" = "#6c8aa5"),
      breaks = c("Leading Edge", "Trailing Edge"),
      name = NULL
    ) +
    ggplot2::labs(
      title = curve_info$geneset_name,
      subtitle = sprintf(
        "%s | ES = %.3f | Leading edge = %s genes | Trailing edge = %s genes",
        curve_info$signature_key,
        curve_info$es_score,
        length(curve_info$leading_edge_genes),
        length(curve_info$trailing_edge_genes)
      ),
      x = "Ranked Features",
      y = "Running Enrichment Score"
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "#4e6782"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "top"
    )
}


flatten_report_value <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NA_character_)
  }
  
  if (is.list(value)) {
    named_parts <- names(value)
    if (is.null(named_parts) || all(is.na(named_parts)) || !any(nzchar(named_parts))) {
      return(paste(vapply(value, flatten_report_value, character(1)), collapse = "; "))
    }
    part_values <- mapply(
      function(name, item) sprintf("%s=%s", name, flatten_report_value(item)),
      named_parts,
      value,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
    return(paste(part_values, collapse = "; "))
  }
  
  pasted <- paste(as.character(value), collapse = ", ")
  if (!nzchar(pasted)) {
    NA_character_
  } else {
    pasted
  }
}


`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}


build_signature_metadata_table <- function(sig_objs, sig_list) {
  if (length(sig_objs) == 0 || length(sig_list) == 0) {
    return(data.frame())
  }
  
  sig_names <- vapply(sig_list, `[[`, character(1), "signature_name")
  rows <- lapply(seq_along(sig_objs), function(i) {
    sig_obj <- sig_objs[[i]]
    metadata <- sig_obj$metadata
    if (is.null(metadata)) {
      metadata <- list()
    }
    
    data.frame(
      signature_name = sig_names[[i]],
      signature_id = flatten_report_value(metadata$signature_id),
      organism = flatten_report_value(metadata$organism),
      assay_type = flatten_report_value(metadata$assay_type),
      phenotype = flatten_report_value(metadata$phenotype),
      direction_type = flatten_report_value(metadata$direction_type),
      description = flatten_report_value(metadata$description),
      score_cutoff = flatten_report_value(metadata$score_cutoff),
      adj_p_cutoff = flatten_report_value(metadata$adj_p_cutoff),
      logfc_cutoff = flatten_report_value(metadata$logfc_cutoff),
      p_value_cutoff = flatten_report_value(metadata$p_value_cutoff),
      keywords = flatten_report_value(metadata$keywords),
      sample_type = flatten_report_value(metadata$sample_type),
      platform = flatten_report_value(metadata$platform_name %||% metadata$platform),
      covariates = flatten_report_value(metadata$covariates),
      author = flatten_report_value(metadata$author %||% metadata$user_name),
      year = flatten_report_value(metadata$year),
      others = flatten_report_value(metadata$others),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, rows)
}


sanitize_report_table <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }
  
  out <- df
  for (col_name in names(out)) {
    column <- out[[col_name]]
    if (is.list(column)) {
      out[[col_name]] <- vapply(column, flatten_report_value, character(1))
    }
  }
  out
}


build_report_settings_table <- function(experiment_label, context, geneset_names, fdr_threshold, background) {
  method_label <- if (is.null(context$method)) {
    "Unknown"
  } else {
    dplyr::recode(
      context$method,
      hypergeo = "Hypergeometric",
      kstest = "KS Test",
      gsea = "GSEA",
      gem_hypergeo = "GEM Hypergeometric",
      gem_weighted = "GEM Weighted",
      .default = as.character(context$method)
    )
  }
  
  data.frame(
    Setting = c("Experiment", "Method", "Geneset Count", "FDR Threshold", "Background", "Generated"),
    Value = c(
      if (nzchar(experiment_label)) experiment_label else "Untitled analysis",
      method_label,
      length(geneset_names),
      as.character(fdr_threshold),
      as.character(background),
      format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    ),
    stringsAsFactors = FALSE
  )
}


annotate_module_server <- function(id, signature_db, user_conn_handler) {
  moduleServer(id, function(input, output, session) {
    max_signature_count <- 10
    active_signatures <- reactiveVal(list())
    run_feedback <- reactiveVal(NULL)
    hyp_result <- reactiveVal(NULL)
    enrichment_context <- reactiveVal(NULL)
    signature_plot_metadata <- reactiveVal(
      data.frame(
        signature = character(),
        signature_name = character(),
        group_label = character(),
        stringsAsFactors = FALSE
      )
    )
    
    output$signature_hypeR <- renderDT({
      df <- signature_db()
      
      DatatableFX(
        df = df,
        hidden_columns = c(0, 6, 7, 8, 9, 11, 12, 14, 15, 16, 18, 19, 21, 22, 24, 25, 26),
        scrollY = "300px",
        row_selection = "multiple"
      )
    }, server = TRUE)
    
    genesets <- genesets_hypeR_Server(
      id = "genesets",
      species = reactive(input$species),
      clean = TRUE
    )
    
    output$gem_options <- renderUI({
      if (!input$enrichment_type %in% c("gem_hypergeo", "gem_weighted")) {
        return(NULL)
      }
      
      tagList(
        textInput(
          session$ns("gem_reference_key"),
          "Metabolite Reference Column",
          value = "refmet_name",
          placeholder = "Example: refmet_name"
        ),
        selectInput(
          session$ns("gem_species"),
          "GEM Species",
          choices = c("Human", "Mouse", "Rat", "Zebrafish", "Worm", "Other"),
          selected = "Human"
        ),
        checkboxInput(
          session$ns("gem_directional"),
          "Use directional GEM mapping",
          value = FALSE
        ),
        checkboxInput(
          session$ns("gem_merge"),
          "Merge compartment-specific metabolites",
          value = TRUE
        ),
        numericInput(
          session$ns("gem_promiscuous_threshold"),
          "Promiscuous Metabolite Threshold",
          value = 500,
          min = 0,
          step = 10
        ),
        numericInput(
          session$ns("gem_min_metabolite"),
          "Minimum Driving Metabolites per Geneset",
          value = 0,
          min = 0,
          step = 1
        ),
        if (identical(input$enrichment_type, "gem_weighted")) {
          textInput(
            session$ns("gem_weighted_by"),
            "Weight Column",
            value = "one_minus_fdr",
            placeholder = "Example: one_minus_fdr"
          )
        } else {
          NULL
        },
        if (!hypergem_available()) {
          div(
            class = "alert alert-warning",
            if (!is.null(getOption("sigrepo.hypergem_error"))) {
              sprintf("hypeR.GEM could not be loaded: %s", getOption("sigrepo.hypergem_error"))
            } else {
              "hypeR.GEM is not currently available in this app environment."
            }
          )
        } else {
          NULL
        }
      )
    })
    
    observeEvent(input$signature_add, {
      selected_rows <- input$signature_hypeR_rows_selected
      
      if (length(selected_rows) == 0) {
        run_feedback(list(
          type = "warning",
          text = "Select at least one signature before adding it to the analysis."
        ))
        return()
      }
      
      df <- signature_db()
      req(!is.null(df))
      
      sig_rows <- df[selected_rows, , drop = FALSE]
      current <- active_signatures()
      added_count <- 0
      skipped_limit_count <- 0
      
      for (i in seq_len(nrow(sig_rows))) {
        sig_row <- sig_rows[i, ]
        key <- sig_row$signature_name
        
        if (!key %in% names(current)) {
          if (length(current) >= max_signature_count) {
            skipped_limit_count <- skipped_limit_count + 1
            next
          }
          
          current[[key]] <- list(
            experiment = input$experiment_label,
            signature_name = sig_row$signature_name,
            signature_id = sig_row$signature_id,
            perturbation = if ("perturbation" %in% names(sig_row)) sig_row$perturbation else NA_character_
          )
          added_count <- added_count + 1
        }
      }
      
      active_signatures(current)
      hyp_result(NULL)
      enrichment_context(NULL)
      signature_plot_metadata(signature_plot_metadata()[0, , drop = FALSE])
      
      run_feedback(list(
        type = if (skipped_limit_count > 0) "warning" else if (added_count > 0) "success" else "info",
        text = if (skipped_limit_count > 0) {
          sprintf(
            "%s signature(s) added. The analysis is limited to %s signatures, so %s selection(s) were skipped.",
            added_count,
            max_signature_count,
            skipped_limit_count
          )
        } else if (added_count > 0) {
          sprintf("%s signature(s) added to the analysis.", added_count)
        } else {
          "All selected signatures are already in the current analysis."
        }
      ))
    })
    
    observeEvent(input$experiment_reset, {
      active_signatures(list())
      hyp_result(NULL)
      enrichment_context(NULL)
      run_feedback(NULL)
      signature_plot_metadata(signature_plot_metadata()[0, , drop = FALSE])
      
      updateTextInput(session, "experiment_label", value = "")
      updateRadioButtons(session, "enrichment_type", selected = "hypergeo")
      updateNumericInput(session, "enrichment_thresh", value = 0.05)
      updateNumericInput(session, "enrichment_bg", value = 36000)
      
      DT::selectRows(DT::dataTableProxy("signature_hypeR", session = session), NULL)
      showNotification("Experiment selections and results were reset.", type = "message")
    })
    
    output$signature_feedback <- renderUI({
      feedback <- run_feedback()
      if (is.null(feedback)) {
        return(NULL)
      }
      
      class_name <- switch(
        feedback$type,
        success = "alert alert-success",
        warning = "alert alert-warning",
        info = "alert alert-info",
        "alert alert-info"
      )
      
      tags$div(class = class_name, feedback$text)
    })
    
    output$analysis_summary <- renderUI({
      sig_list <- active_signatures()
      gsets <- genesets()
      
      summary_value <- function(value, empty = "Not set") {
        if (is.null(value) || identical(value, "") || (length(value) == 0)) empty else as.character(value)
      }
      
      div(
        class = "annotate-summary-grid",
        div(
          class = "annotate-summary-item",
          tags$strong("Experiment"),
          tags$span(summary_value(input$experiment_label, "Untitled analysis"))
        ),
        div(
          class = "annotate-summary-item",
          tags$strong("Method"),
          tags$span(
            dplyr::recode(
              input$enrichment_type,
              hypergeo = "Hypergeometric",
              kstest = "KS Test",
              gsea = "GSEA",
              gem_hypergeo = "GEM Hypergeometric",
              gem_weighted = "GEM Weighted"
            )
          )
        ),
        div(
          class = "annotate-summary-item",
          tags$strong("Selected Signatures"),
          tags$span(sprintf("%s / %s", length(sig_list), max_signature_count))
        ),
        div(
          class = "annotate-summary-item",
          tags$strong("Selected Genesets"),
          tags$span(length(gsets))
        ),
        div(
          class = "annotate-summary-item",
          tags$strong("FDR Threshold"),
          tags$span(summary_value(input$enrichment_thresh))
        ),
        div(
          class = "annotate-summary-item",
          tags$strong("Background"),
          tags$span(summary_value(input$enrichment_bg))
        )
      )
    })
    
    output$signature_preview <- renderUI({
      sig_list <- active_signatures()
      gsets <- genesets()
      
      if (length(sig_list) == 0) {
        return(
          div(
            class = "annotate-empty",
            "No signatures have been added yet. Select signatures from the table above to start the analysis."
          )
        )
      }
      
      sig_names <- vapply(sig_list, `[[`, character(1), "signature_name")
      df <- signature_db()
      req(df)
      
      selected_df <- df[df$signature_name %in% sig_names, , drop = FALSE]
      preview_df <- selected_df[, intersect(c("signature_name", "signature_id", "perturbation"), names(selected_df)), drop = FALSE]
      geneset_names <- names(gsets)
      
      tagList(
        tags$p(
          "Review the selected signatures and genesets before running enrichment."
        ),
        DT::datatable(
          preview_df,
          rownames = FALSE,
          options = list(
            scrollX = TRUE,
            pageLength = 5,
            dom = "tip"
          ),
          class = "compact stripe hover"
        ),
        tags$hr(),
        tags$h4("Geneset Collections"),
        if (length(geneset_names) > 0) {
          tags$ul(lapply(head(geneset_names, 10), tags$li))
        } else {
          div(
            class = "annotate-empty",
            "No genesets have been fetched yet. Choose a collection and subcategory, then fetch genesets."
          )
        },
        if (length(geneset_names) > 10) {
          tags$p(sprintf("Showing 10 of %s genesets selected.", length(geneset_names)))
        }
      )
    })
    
    observeEvent(input$enrichment_do, {
      sig_list <- active_signatures()
      gsets <- genesets()
      
      if (length(sig_list) == 0) {
        showNotification("Add at least one signature before running enrichment.", type = "error")
        return()
      }
      
      if (length(gsets) == 0) {
        showNotification("Fetch at least one geneset collection before running enrichment.", type = "error")
        return()
      }
      
      sig_ids <- vapply(sig_list, function(sig) as.numeric(sig$signature_id), numeric(1))
      
      sig_objs <- SigRepo::getSignature(
        conn_handler = user_conn_handler(),
        signature_id = sig_ids
      )
      
      enrichment_method <- input$enrichment_type
      is_gem_method <- enrichment_method %in% c("gem_hypergeo", "gem_weighted")
      
      if (is_gem_method) {
        if (!hypergem_available()) {
          showNotification("hypeR.GEM is not available in the current app environment.", type = "error")
          return()
        }
        
        reference_key <- trimws(input$gem_reference_key)
        if (!nzchar(reference_key)) {
          showNotification("Provide a metabolite reference column for GEM enrichment.", type = "error")
          return()
        }
        
        enrichment_inputs <- build_gem_enrichment_signatures(
          sig_objs,
          sig_list,
          reference_key = reference_key
        )
        gem_signatures <- enrichment_inputs$signatures
        
        if (length(gem_signatures) == 0) {
          showNotification(
            sprintf("No selected signatures contained the required GEM reference column '%s'.", reference_key),
            type = "error"
          )
          return()
        }
        
        signature2gene_fn <- hypergem_export("signature2gene")
        gem_enrichment_fn <- hypergem_export("enrichment")
        
        gem_obj <- signature2gene_fn(
          signatures = gem_signatures,
          species = input$gem_species,
          directional = isTRUE(input$gem_directional),
          merge = isTRUE(input$gem_merge),
          reference_key = reference_key,
          promiscuous_threshold = input$gem_promiscuous_threshold,
          ensemble_id = FALSE,
          background = NULL
        )
        
        hyp <- gem_enrichment_fn(
          hypeR_GEM_obj = gem_obj,
          genesets = gsets,
          genesets_name = "Selected Genesets",
          method = if (identical(enrichment_method, "gem_weighted")) "weighted" else "unweighted",
          weighted_by = if (!is.null(input$gem_weighted_by) && nzchar(input$gem_weighted_by)) input$gem_weighted_by else "one_minus_fdr",
          min_metabolite = input$gem_min_metabolite,
          background = input$enrichment_bg
        )
        
        hyp_result(hyp)
        enrichment_context(list(
          method = enrichment_method,
          test = "gem",
          engine = "hypeR_GEM",
          genesets = gsets,
          gem_reference_key = reference_key
        ))
        signature_plot_metadata(enrichment_inputs$metadata)
        showNotification("GEM enrichment analysis completed.", type = "message")
      } else {
        enrichment_test <- dplyr::recode(
          input$enrichment_type,
          hypergeo = "hypergeo",
          kstest = "ks",
          gsea = "ks"
        )
        
        enrichment_inputs <- if (identical(enrichment_test, "hypergeo")) {
          build_enrichment_signatures(sig_objs, sig_list)
        } else if (identical(enrichment_method, "gsea")) {
          build_ranked_enrichment_signatures(sig_objs, sig_list, mode = "gsea")
        } else {
          build_ranked_enrichment_signatures(sig_objs, sig_list, mode = "ks")
        }
        signature_vectors <- enrichment_inputs$vectors
        
        if (length(signature_vectors) == 0) {
          showNotification(
            if (identical(enrichment_test, "hypergeo")) {
              "No valid signatures were available to run enrichment."
            } else {
              "No ranked signatures were available to run KS/GSEA-style enrichment. Check that the signatures contain numeric score columns."
            },
            type = "error"
          )
          return()
        }
        
        hype_args <- list(
          signature = signature_vectors,
          genesets = gsets,
          test = enrichment_test,
          background = input$enrichment_bg,
          fdr = input$enrichment_thresh,
          plotting = FALSE,
          quiet = TRUE
        )
        
        if (identical(enrichment_method, "gsea")) {
          hype_args$absolute <- FALSE
          hype_args$power <- 1
        }
        
        hyp <- do.call(hypeR::hypeR, hype_args)
        
        hyp_result(hyp)
        enrichment_context(list(
          method = enrichment_method,
          test = enrichment_test,
          engine = "hypeR",
          signature_vectors = signature_vectors,
          genesets = gsets
        ))
        signature_plot_metadata(enrichment_inputs$metadata)
        showNotification(
          if (identical(enrichment_method, "gsea")) {
            "GSEA-style preranked enrichment completed."
          } else {
            "Enrichment analysis completed."
          },
          type = "message"
        )
      }
    })
    
    dotplot_data <- reactive({
      hyp <- hyp_result()
      req(hyp)
      
      plot_df <- hype_dotplot_data(hyp, fdr_threshold = input$enrichment_thresh)
      validate(need(nrow(plot_df) > 0, "No enriched genesets passed the selected FDR threshold."))
      
      plot_metadata <- signature_plot_metadata()
      validate(need(nrow(plot_metadata) > 0, "Signature grouping metadata was not available for plotting."))
      
      signature_lookup <- unique(
        plot_metadata[, c("signature", "signature_name", "group_label", "signature_order", "group_order"), drop = FALSE]
      )
      signature_lookup <- signature_lookup[order(signature_lookup$signature_order, signature_lookup$group_order), , drop = FALSE]
      
      signature_id_lookup <- unique(signature_lookup[, c("signature_name", "signature_order"), drop = FALSE])
      signature_id_lookup <- signature_id_lookup[order(signature_id_lookup$signature_order), , drop = FALSE]
      signature_id_lookup$plot_id <- paste0("S", seq_len(nrow(signature_id_lookup)))
      
      signature_lookup <- merge(
        signature_lookup,
        signature_id_lookup,
        by = c("signature_name", "signature_order"),
        all.x = TRUE,
        sort = FALSE
      )
      signature_lookup <- signature_lookup[order(signature_lookup$signature_order, signature_lookup$group_order), , drop = FALSE]
      signature_lookup$signature_label <- ifelse(
        nzchar(signature_lookup$group_label) & signature_lookup$group_label != "All Features",
        paste(signature_lookup$plot_id, signature_lookup$group_label),
        signature_lookup$plot_id
      )
      
      plot_df <- merge(plot_df, signature_lookup, by = "signature", all.x = TRUE, sort = FALSE)
      plot_df$signature_label <- factor(plot_df$signature_label, levels = signature_lookup$signature_label)
      plot_df$label <- factor(plot_df$label, levels = rev(unique(plot_df$label)))
      
      positive_fdr <- plot_df$fdr[plot_df$fdr > 0]
      min_positive_fdr <- if (length(positive_fdr) > 0) min(positive_fdr, na.rm = TRUE) else .Machine$double.xmin
      plot_df$fdr_plot <- pmax(plot_df$fdr, min_positive_fdr / 10)
      
      list(
        plot_df = plot_df,
        signature_lookup = signature_lookup[, c("plot_id", "signature_label", "signature_name", "group_label"), drop = FALSE]
      )
    })
    
    output$dotplot <- renderPlot({
      plot_df <- dotplot_data()$plot_df
      build_dotplot_figure(plot_df, title = input$experiment_label)
    })
    
    output$dotplot_signature_key <- DT::renderDT({
      key_df <- dotplot_data()$signature_lookup
      names(key_df) <- c("Signature ID", "Plot Label", "Signature", "Group Label")
      
      DT::datatable(
        key_df,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          dom = "tip",
          scrollX = TRUE
        ),
        class = "compact stripe hover"
      )
    })
    
    output$generic_results_table <- DT::renderDT({
      hyp <- hyp_result()
      req(hyp)
      
      results_df <- extract_hyp_results_table(hyp)
      validate(need(nrow(results_df) > 0, "No results are available."))
      
      DT::datatable(
        results_df,
        rownames = FALSE,
        options = list(
          pageLength = 15,
          scrollX = TRUE
        ),
        class = "compact stripe hover"
      )
    })
    
    gsea_detail_signature_choices <- reactive({
      context <- enrichment_context()
      req(context)
      req(identical(context$method, "gsea"))
      
      metadata_df <- signature_plot_metadata()
      req(nrow(metadata_df) > 0)
      
      signature_lookup <- unique(
        metadata_df[, c("signature", "signature_name", "signature_order", "group_label"), drop = FALSE]
      )
      signature_lookup <- signature_lookup[order(signature_lookup$signature_order), , drop = FALSE]
      
      label_df <- unique(signature_lookup[, c("signature_name", "signature_order"), drop = FALSE])
      label_df <- label_df[order(label_df$signature_order), , drop = FALSE]
      label_df$plot_id <- paste0("S", seq_len(nrow(label_df)))
      
      signature_lookup <- merge(
        signature_lookup,
        label_df,
        by = c("signature_name", "signature_order"),
        all.x = TRUE,
        sort = FALSE
      )
      signature_lookup$display_label <- paste(signature_lookup$plot_id, signature_lookup$group_label)
      
      stats::setNames(signature_lookup$signature, signature_lookup$display_label)
    })
    
    gsea_results_table <- reactive({
      context <- enrichment_context()
      hyp <- hyp_result()
      req(context, hyp)
      req(identical(context$method, "gsea"))
      
      results_df <- extract_hyp_results_table(hyp)
      validate(need(nrow(results_df) > 0, "No GSEA results are available."))
      results_df
    })
    
    observeEvent(gsea_detail_signature_choices(), {
      choices <- gsea_detail_signature_choices()
      selected <- isolate(input$gsea_detail_signature)
      
      if (is.null(selected) || !selected %in% unname(choices)) {
        selected <- unname(choices)[[1]]
      }
      
      updateSelectInput(
        session,
        "gsea_detail_signature",
        choices = choices,
        selected = selected
      )
    }, ignoreInit = FALSE)
    
    observeEvent(
      list(gsea_results_table(), input$gsea_detail_signature),
      {
        results_df <- gsea_results_table()
        signature_key <- input$gsea_detail_signature
        if (is.null(signature_key) || !nzchar(signature_key)) {
          return()
        }
        
        signature_results <- results_df[results_df$signature == signature_key, , drop = FALSE]
        signature_results <- signature_results[order(signature_results$fdr, -abs(signature_results$score)), , drop = FALSE]
        
        geneset_choices <- unique(signature_results$label)
        if (length(geneset_choices) == 0) {
          return()
        }
        
        selected_geneset <- isolate(input$gsea_detail_geneset)
        if (is.null(selected_geneset) || !selected_geneset %in% geneset_choices) {
          selected_geneset <- geneset_choices[[1]]
        }
        
        updateSelectInput(
          session,
          "gsea_detail_geneset",
          choices = geneset_choices,
          selected = selected_geneset
        )
      },
      ignoreInit = FALSE
    )
    
    gsea_curve_data <- reactive({
      context <- enrichment_context()
      req(context)
      req(identical(context$method, "gsea"))
      
      signature_key <- input$gsea_detail_signature
      geneset_name <- input$gsea_detail_geneset
      req(signature_key, geneset_name)
      
      ranked_signature <- context$signature_vectors[[signature_key]]
      geneset_genes <- context$genesets[[geneset_name]]
      req(!is.null(ranked_signature), !is.null(geneset_genes))
      
      curve_info <- compute_gsea_curve_data(
        ranked_signature = ranked_signature,
        geneset_genes = geneset_genes,
        power = 1
      )
      
      curve_info$signature_key <- signature_key
      curve_info$geneset_name <- geneset_name
      curve_info
    })
    
    output$gsea_detail_controls <- renderUI({
      context <- enrichment_context()
      
      if (is.null(context) || !identical(context$method, "gsea")) {
        return(NULL)
      }
      
      tagList(
        div(
          class = "annotate-actions",
          selectInput(
            session$ns("gsea_detail_signature"),
            "Signature",
            choices = gsea_detail_signature_choices()
          ),
          selectInput(
            session$ns("gsea_detail_geneset"),
            "Geneset",
            choices = character(0)
          )
        )
      )
    })
    
    output$gsea_curve_plot <- renderPlot({
      curve_info <- gsea_curve_data()
      build_gsea_curve_figure(curve_info)
    })
    
    output$gsea_edge_summary <- renderUI({
      curve_info <- gsea_curve_data()
      
      edge_text <- function(values) {
        if (length(values) == 0) {
          "None"
        } else {
          paste(utils::head(values, 12), collapse = ", ")
        }
      }
      
      tagList(
        div(
          class = "annotate-summary-grid",
          div(
            class = "annotate-summary-item",
            tags$strong("Enrichment Score"),
            tags$span(sprintf("%.3f", curve_info$es_score))
          ),
          div(
            class = "annotate-summary-item",
            tags$strong("Direction"),
            tags$span(if (identical(curve_info$es_direction, "positive")) "Leading edge at top" else "Leading edge at bottom")
          )
        ),
        tags$p(
          class = "geneset-summary-text",
          tags$strong("Leading Edge: "),
          edge_text(curve_info$leading_edge_genes)
        ),
        tags$p(
          class = "geneset-summary-text",
          tags$strong("Trailing Edge: "),
          edge_text(curve_info$trailing_edge_genes)
        )
      )
    })
    
    output$enrichment <- renderUI({
      hyp <- hyp_result()
      context <- enrichment_context()
      
      if (is.null(hyp)) {
        return(
          div(
            class = "annotate-empty",
            "Results will appear here after you run an enrichment analysis."
          )
        )
      }
      
      use_generic_results_table <- !is.null(context) && identical(context$engine, "hypeR_GEM")
      
      gsea_detail_section <- if (!is.null(context) && identical(context$method, "gsea")) {
        div(
          class = "annotate-results-section",
          tags$h4("Enrichment Curve Detail"),
          uiOutput(session$ns("gsea_detail_controls")),
          plotOutput(session$ns("gsea_curve_plot"), height = "420px", width = "100%"),
          uiOutput(session$ns("gsea_edge_summary"))
        )
      } else {
        NULL
      }
      
      div(
        class = "annotate-results-body",
        div(
          class = "annotate-results-section",
          tags$h4("Results Table"),
          if (use_generic_results_table) {
            DT::DTOutput(session$ns("generic_results_table"))
          } else {
            hypeR::rctbl_build(hyp)
          }
        ),
        div(
          class = "annotate-results-section",
          tags$h4("Dotplot"),
          plotOutput(session$ns("dotplot"), height = "400px", width = "100%"),
          div(
            class = "annotate-signature-key",
            tags$h4("Signature Key"),
            DT::DTOutput(session$ns("dotplot_signature_key"))
          )
        ),
        gsea_detail_section
      )
    })
    
    output$generate_report <- downloadHandler(
      filename = function() {
        label <- input$experiment_label
        if (is.null(label) || !nzchar(label)) {
          label <- "sigrepo_annotate_analysis_bundle"
        }
        
        safe_label <- gsub("[^A-Za-z0-9_-]+", "_", label)
        sprintf("%s_%s.zip", safe_label, format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(file) {
        hyp <- hyp_result()
        context <- enrichment_context()
        req(hyp, context)
        
        sig_list <- active_signatures()
        sig_ids <- vapply(sig_list, function(sig) as.numeric(sig$signature_id), numeric(1))
        sig_objs <- SigRepo::getSignature(
          conn_handler = user_conn_handler(),
          signature_id = sig_ids
        )
        
        bundle_dir <- tempfile("annotate_analysis_bundle_")
        dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
        on.exit(unlink(bundle_dir, recursive = TRUE, force = TRUE), add = TRUE)
        
        plots_dir <- file.path(bundle_dir, "plots")
        dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
        
        plot_paths <- list()
        
        dotplot_payload <- tryCatch(dotplot_data(), error = function(e) NULL)
        if (!is.null(dotplot_payload) && is.data.frame(dotplot_payload$plot_df) && nrow(dotplot_payload$plot_df) > 0) {
          dotplot_path <- file.path(plots_dir, "dotplot.png")
          ggplot2::ggsave(
            filename = dotplot_path,
            plot = build_dotplot_figure(dotplot_payload$plot_df, title = input$experiment_label),
            width = 11,
            height = 7,
            dpi = 150
          )
          plot_paths$dotplot <- file.path("plots", "dotplot.png")
        }
        
        if (identical(context$method, "gsea")) {
          curve_info <- tryCatch(gsea_curve_data(), error = function(e) NULL)
          if (!is.null(curve_info)) {
            gsea_path <- file.path(plots_dir, "gsea_curve.png")
            ggplot2::ggsave(
              filename = gsea_path,
              plot = build_gsea_curve_figure(curve_info),
              width = 11,
              height = 6,
              dpi = 150
            )
            plot_paths$gsea_curve <- file.path("plots", "gsea_curve.png")
          }
        }
        
        results_df <- sanitize_report_table(extract_hyp_results_table(hyp))
        signature_key_df <- sanitize_report_table(
          tryCatch(dotplot_data()$signature_lookup, error = function(e) data.frame())
        )
        metadata_df <- sanitize_report_table(build_signature_metadata_table(sig_objs, sig_list))
        settings_df <- build_report_settings_table(
          experiment_label = input$experiment_label,
          context = context,
          geneset_names = names(context$genesets %||% list()),
          fdr_threshold = input$enrichment_thresh,
          background = input$enrichment_bg
        )
        
        analysis_context <- list(
          title = if (nzchar(input$experiment_label)) input$experiment_label else "SigRepo Annotate Analysis",
          settings_df = settings_df,
          signature_metadata_df = metadata_df,
          signature_key_df = signature_key_df,
          results_df = results_df,
          geneset_names = names(context$genesets %||% list()),
          plot_paths = plot_paths,
          method = context$method %||% "unknown",
          engine = context$engine %||% "hypeR"
        )
        
        saveRDS(hyp, file.path(bundle_dir, "hyp_result.rds"))
        saveRDS(analysis_context, file.path(bundle_dir, "analysis_context.rds"))
        
        readme_lines <- c(
          "# SigRepo Annotate Analysis Bundle",
          "",
          "This bundle contains:",
          "- `hyp_result.rds`: exported hype/hypGEM result object",
          "- `analysis_context.rds`: analysis settings, metadata, results tables, and plot key",
          "- `plots/`: exported figures captured at export time",
          "",
          "Suggested workflow:",
          "1. Unzip this bundle.",
          "2. Load `hyp_result.rds` and/or `analysis_context.rds` in R.",
          "3. Reuse the PNG figures in `plots/` for slides or notes."
        )
        writeLines(readme_lines, con = file.path(bundle_dir, "README.md"))
        
        old_wd <- getwd()
        on.exit(setwd(old_wd), add = TRUE)
        setwd(bundle_dir)
        utils::zip(
          zipfile = file,
          files = list.files(".", recursive = TRUE, all.files = FALSE)
        )
      }
    )
    
    output$export_hyp <- downloadHandler(
      filename = function() {
        label <- input$experiment_label
        if (is.null(label) || !nzchar(label)) {
          label <- "sigrepo_enrichment"
        }
        
        safe_label <- gsub("[^A-Za-z0-9_-]+", "_", label)
        sprintf("%s_hype_%s.rds", safe_label, format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(file) {
        hyp <- hyp_result()
        req(hyp)
        saveRDS(hyp, file)
      }
    )
  })
}
