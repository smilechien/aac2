# kano.R
# Robust Kano plot helpers for Shiny and PNG download.
# Defines: make_kano_plot(), save_kano_png(), render_kano_plot()

make_kano_plot <- function(nodes,
                           x_col = NULL,
                           y_col = NULL,
                           label_col = "name",
                           cluster_col = NULL,
                           title = "Kano plot",
                           label_size = 5.2,
                           base_size = 16) {
  stopifnot(is.data.frame(nodes))
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("ggrepel", quietly = TRUE)) stop("Package 'ggrepel' is required.")

  nd <- as.data.frame(nodes, stringsAsFactors = FALSE)
  nms <- names(nd)

  if (is.null(label_col) || !(label_col %in% nms)) {
    label_col <- if ("label" %in% nms) "label" else nms[1]
  }
  if (is.null(cluster_col)) {
    cluster_col <- intersect(c("carac", "cluster", "membership", "group", "community"), nms)[1]
  }
  if (is.na(cluster_col) || length(cluster_col) == 0) {
    nd$..cluster <- "C1"
    cluster_col <- "..cluster"
  }

  if (is.null(x_col)) x_col <- intersect(c("sil_width", "SS", "ss", "x", "value2", "edge", "WCD"), nms)[1]
  if (is.null(y_col)) y_col <- intersect(c("value", "count", "Count", "freq", "Freq", "weighted_score", "WCD", "y"), nms)[1]

  if (is.na(x_col) || length(x_col) == 0 || !(x_col %in% nms)) {
    nd$..x <- seq_len(nrow(nd))
    x_col <- "..x"
  }
  if (is.na(y_col) || length(y_col) == 0 || !(y_col %in% nms)) {
    nd$..y <- seq_len(nrow(nd))
    y_col <- "..y"
  }

  nd$..x <- suppressWarnings(as.numeric(nd[[x_col]]))
  nd$..y <- suppressWarnings(as.numeric(nd[[y_col]]))
  nd$..label <- as.character(nd[[label_col]])
  nd$..cluster <- as.factor(nd[[cluster_col]])
  nd <- nd[is.finite(nd$..x) & is.finite(nd$..y) & !is.na(nd$..label) & nzchar(nd$..label), , drop = FALSE]

  if (!nrow(nd)) stop("No valid rows for Kano plot.")

  x_mid <- stats::median(nd$..x, na.rm = TRUE)
  y_mid <- stats::median(nd$..y, na.rm = TRUE)

  ggplot2::ggplot(nd, ggplot2::aes(x = .data$..x, y = .data$..y, color = .data$..cluster)) +
    ggplot2::geom_hline(yintercept = y_mid, linetype = "dashed", linewidth = 0.6, color = "grey55") +
    ggplot2::geom_vline(xintercept = x_mid, linetype = "dashed", linewidth = 0.6, color = "grey55") +
    ggplot2::geom_point(size = 4.0, alpha = 0.90) +
    ggrepel::geom_text_repel(ggplot2::aes(label = .data$..label),
                             size = label_size, fontface = "bold",
                             max.overlaps = Inf, box.padding = 0.45,
                             min.segment.length = 0) +
    ggplot2::labs(title = title, x = x_col, y = y_col, color = "Cluster") +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 4, hjust = 0.5),
      axis.title = ggplot2::element_text(face = "bold", size = base_size + 1),
      axis.text  = ggplot2::element_text(face = "bold", size = base_size - 1),
      legend.title = ggplot2::element_text(face = "bold"),
      legend.text  = ggplot2::element_text(face = "bold")
    )
}

save_kano_png <- function(file, nodes, width = 14, height = 10, dpi = 180, ...) {
  p <- make_kano_plot(nodes, ...)
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(filename = file, width = width, height = height, units = "in", res = dpi)
  } else {
    grDevices::png(filename = file, width = width, height = height, units = "in", res = dpi, type = "cairo")
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  print(p)
  invisible(file)
}

render_kano_plot <- function(nodes, ...) {
  print(make_kano_plot(nodes, ...))
}
