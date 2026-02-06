plot_correlation_heatmap <- function(
    df,
    vars = NULL,
    contains = NULL,
    regex = NULL,
    method = "spearman"
) {
  
  # ===============================
  # Select variables
  # ===============================
  
  all_names <- names(df)
  
  selected_vars <- vars
  
  if (!is.null(contains)) {
    selected_vars <- unique(c(
      selected_vars,
      unlist(
        lapply(
          contains,
          function(x) all_names[grepl(x, all_names, fixed = TRUE)]
        )
      )
    ))
  }
  
  if (!is.null(regex)) {
    selected_vars <- unique(c(
      selected_vars,
      all_names[grepl(regex, all_names)]
    ))
  }
  
  if (is.null(selected_vars) || length(selected_vars) < 2) {
    stop("Please select at least two variables using vars, contains, or regex.")
  }
  
  selected_vars <- intersect(selected_vars, all_names)
  
  # ===============================
  # Subset numeric data
  # ===============================
  
  cor_df <- df[, selected_vars, drop = FALSE]
  cor_df <- cor_df[, vapply(cor_df, is.numeric, logical(1)), drop = FALSE]
  
  if (ncol(cor_df) < 2) {
    stop("Fewer than two numeric variables selected.")
  }
  
  # ===============================
  # Correlation matrix
  # ===============================
  
  cor_mat <- stats::cor(
    cor_df,
    method = method,
    use = "pairwise.complete.obs"
  )
  
  # ===============================
  # P-value matrix
  # ===============================
  
  p_mat <- matrix(
    NA_real_,
    nrow = ncol(cor_df),
    ncol = ncol(cor_df),
    dimnames = list(colnames(cor_df), colnames(cor_df))
  )
  
  for (i in seq_len(ncol(cor_df))) {
    for (j in seq_len(ncol(cor_df))) {
      if (i != j) {
        p_mat[i, j] <- stats::cor.test(
          cor_df[[i]],
          cor_df[[j]],
          method = method
        )$p.value
      }
    }
  }
  
  # ===============================
  # Long-format table for plotting
  # ===============================
  
  plot_df <- expand.grid(
    Var1 = colnames(cor_mat),
    Var2 = colnames(cor_mat),
    stringsAsFactors = FALSE
  )
  
  plot_df$cor <- as.vector(cor_mat)
  plot_df$p   <- as.vector(p_mat)
  
  plot_df$label <- paste0(
    "r = ", round(plot_df$cor, 2),
    "\n",
    "p = ", round(plot_df$p, 2)
  )
  
  # ===============================
  # Heat map
  # ===============================
  
  ggplot2::ggplot(plot_df, ggplot2::aes(Var1, Var2, fill = cor)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3) +
    ggplot2::scale_fill_gradient2(
      low = "#4575b4",
      mid = "white",
      high = "#d73027",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Correlation"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = paste("Pairwise", method, "correlations")
    )
}
