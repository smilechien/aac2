# sankey.R
# Robust Sankey-style plot helpers using ggplot2, avoiding empty PNG downloads.
# Defines: make_sankey_plot(), save_sankey_png(), render_sankey_plot()

.standardize_edges_for_sankey <- function(edges) {
  stopifnot(is.data.frame(edges))
  ed <- as.data.frame(edges, stringsAsFactors = FALSE)
  nms <- names(ed)
  from_col <- intersect(c("from", "source", "Source", "Leader", "leader", "cluster", "Cluster"), nms)[1]
  to_col   <- intersect(c("to", "target", "Target", "follower", "Follower", "name", "term"), nms)[1]
  val_col  <- intersect(c("value", "Value", "weight", "Weight", "WCD", "w", "n", "count", "Count"), nms)[1]

  if (is.na(from_col) || length(from_col) == 0) from_col <- nms[1]
  if (is.na(to_col) || length(to_col) == 0) to_col <- nms[min(2, length(nms))]
  if (identical(from_col, to_col) && length(nms) >= 2) to_col <- nms[2]

  out <- data.frame(
    from  = as.character(ed[[from_col]]),
    to    = as.character(ed[[to_col]]),
    value = if (!is.na(val_col) && length(val_col) > 0) suppressWarnings(as.numeric(ed[[val_col]])) else 1,
    stringsAsFactors = FALSE
  )
  out$value[!is.finite(out$value)] <- 1
  out <- out[!is.na(out$from) & nzchar(out$from) & !is.na(out$to) & nzchar(out$to), , drop = FALSE]
  out
}

make_sankey_plot <- function(edges,
                             title = "Sankey plot",
                             label_size = 5.2,
                             base_size = 16) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  ed <- .standardize_edges_for_sankey(edges)
  if (!nrow(ed)) stop("No valid edges for Sankey plot.")

  left <- aggregate(value ~ from, ed, sum, na.rm = TRUE)
  right <- aggregate(value ~ to, ed, sum, na.rm = TRUE)
  left <- left[order(left$value, decreasing = TRUE), , drop = FALSE]
  right <- right[order(right$value, decreasing = TRUE), , drop = FALSE]
  left$y <- seq_len(nrow(left))
  right$y <- seq_len(nrow(right))
  ed$y_from <- left$y[match(ed$from, left$from)]
  ed$y_to <- right$y[match(ed$to, right$to)]

  ggplot2::ggplot(ed) +
    ggplot2::geom_curve(ggplot2::aes(x = 1, y = .data$y_from, xend = 2, yend = .data$y_to,
                                     linewidth = .data$value, color = .data$from),
                        curvature = 0.25, alpha = 0.55, lineend = "round") +
    ggplot2::geom_text(data = left, ggplot2::aes(x = 0.92, y = .data$y, label = .data$from),
                       hjust = 1, size = label_size, fontface = "bold") +
    ggplot2::geom_text(data = right, ggplot2::aes(x = 2.08, y = .data$y, label = .data$to),
                       hjust = 0, size = label_size, fontface = "bold") +
    ggplot2::scale_linewidth(range = c(0.7, 5.0), guide = "none") +
    ggplot2::coord_cartesian(xlim = c(0.35, 2.65), clip = "off") +
    ggplot2::labs(title = title, x = NULL, y = NULL, color = "Source") +
    ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 4, hjust = 0.5),
      legend.position = "none",
      plot.margin = ggplot2::margin(20, 120, 20, 120)
    )
}

save_sankey_png <- function(file, edges, width = 16, height = 10, dpi = 180, ...) {
  p <- make_sankey_plot(edges, ...)
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(filename = file, width = width, height = height, units = "in", res = dpi)
  } else {
    grDevices::png(filename = file, width = width, height = height, units = "in", res = dpi, type = "cairo")
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  print(p)
  invisible(file)
}

render_sankey_plot <- function(edges, ...) {
  print(make_sankey_plot(edges, ...))
}
