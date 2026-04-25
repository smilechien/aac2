suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# ---- device guard (robust) ---------------------------------------------------
ensure_device <- function(width = 12, height = 8, res = 144) {
  if (!is.null(grDevices::dev.list())) return(FALSE)
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(filename = tempfile(fileext = ".png"),
                  width = width, height = height, units = "in", res = res)
  } else if (capabilities("png")) {
    png(filename = tempfile(fileext = ".png"),
        width = width, height = height, units = "in", res = res, type = "cairo")
  } else {
    pdf(file = tempfile(fileext = ".pdf"), width = width, height = height)
  }
  TRUE
}

# Build nodes0 if user didn't provide one
make_nodes0 <- function(sil_df, nodes = NULL) {
  if (is.data.frame(nodes) && all(c("name","carac") %in% names(nodes))) {
    dplyr::transmute(nodes,
                     name  = as.character(name),
                     carac = suppressWarnings(as.integer(carac)),
                     value = if ("value" %in% names(nodes)) as.numeric(value) else 1)
  } else {
    dplyr::transmute(sil_df,
                     name  = as.character(name),
                     carac = suppressWarnings(as.integer(carac)),
                     value = if ("value" %in% names(sil_df)) as.numeric(value) else 1)
  }
}

# ---- panel function -----------------------------------------------------------
render_panel <- function(
  sil_df,
  nodes0,
  results = NULL,                 # optional table with "OVERALL" and per-cluster rows
  res     = NULL,                 # optional list with res$W_km (weighted adjacency)
  nodes   = NULL,                 # optional nodes with value/value2 labels
  top_n   = NULL,                 # show top N rows (by value then SS). NULL = all
  font_scale = 1.30,
  aac_col = "#A23B3B",
  aac_side = c("left","right"),   # where to print AAC numbers relative to arrows
  gap_AAC_to_arrow_frac = 0.18,   # AAC ↔ arrows gap (fraction of plot width)
  gap_label_to_AAC_frac = 0.11,   # left labels ↔ AAC gap (fraction of plot width)
  neighbor_side = c("right","left"),     # which bar edge to anchor to (tip/start)
  neighbor_on_bar = TRUE,                 # <— NEW: draw label ON the colored bar
  neighbor_inset_frac_of_bar = 0.02,      # inset (as fraction of that bar's length)
  neighbor_at_bar_end = TRUE,             # ignored when neighbor_on_bar = TRUE
  neighbor_gap_frac   = 0.020,            # used only when drawing outside the bar
  footer_label = sprintf("Made at raschonline.com/raschonline/cbpkano.asp?cbp=FLCA2allfiles on %s", Sys.Date()),
  footer_adj   = 0,
  footer_col   = "grey30"
) {
  aac_side <- match.arg(aac_side)
  neighbor_side <- match.arg(neighbor_side)

  # v3 safety guard ------------------------------------------------------------
  # graphics::strwidth() throws: "不是所有的 is.character(txt) 都是 TRUE"
  # when a label component is NULL/NA/factor/numeric.  This local wrapper
  # makes all width measurements character-safe inside render_panel().
  .strwidth_base <- graphics::strwidth
  safe_chr_v3 <- function(x, default = "") {
    if (is.null(x) || length(x) == 0) return(default)
    x <- as.character(x)[1]
    if (is.na(x) || !nzchar(x)) default else x
  }
  strwidth <- function(s, ...) {
    if (is.null(s) || length(s) == 0) s <- ""
    .strwidth_base(as.character(s), ...)
  }






  # scales
  S <- function(x) x * font_scale
  labels_cex <- S(0.84); header_cex <- S(1.35); row3_cex <- S(1.20)
  over_cex <- S(1.30); right_cex <- S(1.25); aac_cex <- S(1.15)
  axis_cex <- S(1.00); title_cex <- S(1.10); footer_cex <- S(0.90)

  stopifnot(is.data.frame(sil_df), "name" %in% names(sil_df), "sil_width" %in% names(sil_df))
  if (is.null(top_n)) top_n <- nrow(sil_df)

  # helpers
  f2 <- function(x, d = 2) {
    x <- suppressWarnings(as.numeric(x))
    if (!is.finite(x)) return("")
    sprintf(paste0("%.", d, "f"), x)
  }

  clampU <- function(x, usr, pad = 0.01) {
    w <- diff(usr[1:2]); left <- usr[1] + pad*w; right <- usr[2] - pad*w
    pmax(pmin(x, right), left)
  }
  map_lwd <- function(w, lmin = 1.2, lmax = 4) {
    ifelse(!is.finite(w), 1.2, {
      w0 <- pmax(0, w); w1 <- w0 / (stats::quantile(w0, 0.95, na.rm = TRUE) + 1e-9)
      pmin(lmax, pmax(lmin, 1 + 3 * w1))
    })
  }
  measure_text_inches <- function(lbls, cex = 1) {
    out <- tryCatch(suppressWarnings(max(strwidth(as.character(lbls), units = "inches", cex = cex))),
                    error = function(e) NA_real_)
    if (is.finite(out)) out else 3
  }
  split_bold_left <- function(s) {
    m <- regexpr("\\s*\\(", s)
    if (m[1] > 0) { left <- substr(s, 1, m[1]-1); right <- substr(s, m[1], nchar(s)) } else { left <- s; right <- "" }
    list(left = left, right = right)
  }
  draw_mixed_right <- function(x, y, s, cex = labels_cex, col_left = "black", col_right = "black") {
    pr <- split_bold_left(s); wr <- strwidth(pr$right, units = "user", cex = cex, font = 1)
    text(x, y, pr$right, adj = c(1, 0.5), cex = cex, font = 1, col = col_right, xpd = NA)
    text(x - wr, y, pr$left,  adj = c(1, 0.5), cex = cex, font = 2, col = col_left,  xpd = NA)
  }
  to_user_dx <- function(dx_in) grconvertX(dx_in, "inches", "user") - grconvertX(0, "inches", "user")

  place_row_fit <- function(lbls, y, cols, cex = header_cex,
                            left_pad_in = 0.06, right_pad_in = 0.06, gap_in = 0.06,
                            max_shrink_iter = 8, shrink_factor = 0.94) {
    lbls <- as.character(lbls); cols <- rep_len(cols, length(lbls))
    usr  <- par("usr"); L <- usr[1] + to_user_dx(left_pad_in); R <- usr[2] - to_user_dx(right_pad_in)
    avail <- max(1e-6, R - L)
    cex_try <- cex; iter <- 0
    repeat {
      w_in <- strwidth(lbls, units = "inches", cex = cex_try)
      w_u  <- to_user_dx(w_in); gap_u <- to_user_dx(gap_in)
      need <- sum(w_u) + gap_u * (length(lbls) - 1)
      if (need <= avail || iter >= max_shrink_iter) break
      cex_try <- cex_try * shrink_factor; iter <- iter + 1
    }
    extra <- max(0, avail - (sum(w_u) + gap_u * (length(lbls) - 1)))
    add_per_gap <- if (length(lbls) > 1) extra / (length(lbls) - 1) else 0
    x <- numeric(length(lbls)); x[1] <- L
    for (i in seq_along(lbls)) {
      if (i > 1) x[i] <- x[i-1] + w_u[i-1] + gap_u + add_per_gap
      text(x[i], y, lbls[i], adj = 0, cex = cex_try, font = 2, col = cols[i])
    }
    invisible(x)
  }

  # optional enrichment from nodes
  if (is.data.frame(nodes) && all(c("name","value","value2","carac") %in% names(nodes))) {
    nodes$name <- as.character(nodes$name)
    k  <- if ("name2" %in% names(sil_df)) as.character(sil_df$name2) else as.character(sil_df$name)
    ix <- match(k, nodes$name)
    if (!"value"  %in% names(sil_df)) sil_df$value  <- NA_real_
    if (!"value2" %in% names(sil_df)) sil_df$value2 <- NA_real_
    if (!"carac"  %in% names(sil_df)) sil_df$carac  <- NA_integer_
    ok <- !is.na(ix)
    sil_df$value[ok]  <- nodes$value[ix[ok]]
    sil_df$value2[ok] <- nodes$value2[ix[ok]]
    sil_df$carac[ok]  <- nodes$carac[ix[ok]]
  }

  # cluster coercion + optional cols
  if ("carac" %in% names(sil_df)) {
    sil_df$carac <- suppressWarnings(as.integer(as.character(sil_df$carac)))
  } else if ("cluster" %in% names(sil_df)) {
    sil_df$carac <- suppressWarnings(as.integer(gsub("^C","", as.character(sil_df$cluster))))
  } else stop("`sil_df` needs `carac` or `cluster`.")
  opt <- c("value","value2","wsel","role","neighbor_index","neighbor_name","neighborC")
  for (cc in opt) if (!cc %in% names(sil_df)) sil_df[[cc]] <- NA

  # selection: top-N by value then SS
  TOTAL <- if (is.null(top_n)) nrow(sil_df) else as.integer(top_n)
  if (!is.finite(TOTAL) || TOTAL <= 0) TOTAL <- nrow(sil_df)
  val <- suppressWarnings(as.numeric(sil_df$value))
  ss  <- suppressWarnings(as.numeric(sil_df$sil_width))
  ord <- order(ifelse(is.finite(val), val, -Inf),
               ifelse(is.finite(ss),  ss,  -Inf),
               decreasing = c(TRUE, TRUE), na.last = TRUE)
  sel <- utils::head(seq_len(nrow(sil_df))[ord], TOTAL)
  sil_plot <- sil_df[sel, , drop = FALSE]
  if (!nrow(sil_plot)) { plot.new(); text(0.5, 0.5, "No rows selected", cex = 1.4, font = 2, col = "red"); return(invisible()) }
  sil_plot <- sil_plot[order(sil_plot$carac,
                             -suppressWarnings(as.numeric(sil_plot$value)),
                             -suppressWarnings(as.numeric(sil_plot$sil_width)),
                             na.last = TRUE), , drop = FALSE] %>% as_tibble()

  # ---- AAC (global + per cluster) --------------------------------------------
  base_for_aac  <- if ("value2" %in% names(sil_plot)) sil_plot$value2 else if ("value" %in% names(sil_plot)) sil_plot$value else sil_plot$sil_width
  base_for_aacv <- if ("value"  %in% names(sil_plot)) sil_plot$value  else if ("value2" %in% names(sil_plot)) sil_plot$value2 else sil_plot$sil_width

  AAC_global <- {
    v <- suppressWarnings(as.numeric(base_for_aac)); v <- v[is.finite(v)]
    if (length(v) >= 3) {
      t3 <- sort(v, TRUE)[1:3]; if (min(t3) <= 0) t3 <- t3 + abs(min(t3)) + 1e-3
      r <- (t3[1]/t3[2])/(t3[2]/t3[3]); if (is.finite(r) && r > 0) round(r/(1+r), 2) else NA_real_
    } else NA_real_
  }
  AAC_globalv <- {
    v <- suppressWarnings(as.numeric(base_for_aacv)); v <- v[is.finite(v)]
    if (length(v) >= 3) {
      t3 <- sort(v, TRUE)[1:3]; if (min(t3) <= 0) t3 <- t3 + abs(min(t3)) + 1e-3
      r <- (t3[1]/t3[2])/(t3[2]/t3[3]); if (is.finite(r) && r > 0) round(r/(1+r), 2) else NA_real_
    } else NA_real_
  }

clv <- sort(unique(na.omit(sil_df$carac))) 
aac_by_cluster <- setNames(rep(NA_real_, length(clv)), as.character(clv))

# Choose the base vector depending on which column exists
base_all <- if ("value" %in% names(sil_df)) {
  sil_df$value
} else if ("value2" %in% names(sil_df)) {
  sil_df$value2
} else {
  sil_df$sil_width
}

for (cc in clv) {
  v <- suppressWarnings(as.numeric(base_all[sil_df$carac == cc]))
  v <- v[is.finite(v)]

  if (length(v) == 1) {
    aac_by_cluster[as.character(cc)] <- 0.5

  } else if (length(v) == 2) {
    t3 <- sort(v, decreasing = TRUE)
    if (min(t3) <= 0) t3 <- t3 + abs(min(t3)) + 1e-3
    ratio <- t3[1] / t3[2]
    aac_by_cluster[as.character(cc)] <- ratio / (1 + ratio)

  } else if (length(v) >= 3) {
    t3 <- sort(v, decreasing = TRUE)[1:3]
    if (min(t3) <= 0) t3 <- t3 + abs(min(t3)) + 1e-3
    r <- (t3[1] / t3[2]) / (t3[2] / t3[3])
    if (is.finite(r) && r > 0) {
      aac_by_cluster[as.character(cc)] <- r / (1 + r)
    }
  }
}


  # ---- header / layout --------------------------------------------------------
  SS_overall <- mean(sil_df$sil_width, na.rm = TRUE)
  Qw_overall <- Qu_overall <- D_overall <- QD_overall <- Dmax_over <- QDmax_over <- NA_real_
  if (is.data.frame(results) && "Cluster" %in% names(results)) {
    pick <- function(choices){
      nm <- names(results); i <- match(tolower(choices), tolower(nm)); i <- i[!is.na(i)]; if (length(i)) nm[i[1]] else NULL
    }
    ri <- match("OVERALL", results$Cluster)
    if (is.finite(ri)) {
      cQw <- pick(c("Qw","Q_w","Qweighted","Qw_mean"))
      cQu <- pick(c("Qu","Q_u","Qunweighted","Qu_mean"))
      cSS <- pick(c("SS","Silhouette","MeanSS","AvgSS","S"))
      if (!is.null(cQw)) Qw_overall <- suppressWarnings(as.numeric(results[[cQw]][ri]))
      if (!is.null(cQu)) Qu_overall <- suppressWarnings(as.numeric(results[[cQu]][ri]))
      if (!is.null(cSS)) SS_overall <- suppressWarnings(as.numeric(results[[cSS]][ri]))
      if ("D_GiniSimpson" %in% names(results)) D_overall  <- suppressWarnings(as.numeric(results$D_GiniSimpson[ri]))
      if ("Q_over_D"      %in% names(results)) QD_overall <- suppressWarnings(as.numeric(results$Q_over_D[ri]))
      if ("OneMinus_1_over_k" %in% names(results)) Dmax_over <- suppressWarnings(as.numeric(results$OneMinus_1_over_k[ri]))
      if ("Q_over_Dmax_eff"   %in% names(results)) QDmax_over <- suppressWarnings(as.numeric(results$Q_over_Dmax_eff[ri]))
    }
  }

  build_label <- function(i){
    parts <- c(
      if ("value"  %in% names(sil_plot))  format(sil_plot$value[i],  trim = TRUE, scientific = FALSE) else NULL,
      if ("value2" %in% names(sil_plot))  format(sil_plot$value2[i], trim = TRUE, scientific = FALSE) else NULL,
      f2(sil_plot$sil_width[i]),
      if ("carac"  %in% names(sil_plot)) paste0("C", sil_plot$carac[i]) else NULL
    )
    paste0(sil_plot$name[i], ifelse(is.na(sil_plot$neighborC[i]), "", paste0("#", sil_plot$neighborC[i])), " (", paste(parts, collapse = "|"), ")")
  }
  lbl <- vapply(seq_len(nrow(sil_plot)), build_label, character(1))
  dev_in <- tryCatch(grDevices::dev.size("in"), error = function(e) c(12,8))
  left_needed <- measure_text_inches(lbl, cex = labels_cex) + 0.45
  left_in <- max(0.25, min(left_needed, dev_in[1] - 3.2 - 0.8))

  arrow_center <- -0.135; arrow_half <- 0.045; arrow_shift <- 0.10
  glyph_left   <- arrow_center - arrow_half + arrow_shift
  glyph_right  <- arrow_center + arrow_half + arrow_shift
  xmin <- min(-0.55, glyph_left - 0.20); xmax <- 1.02

  has_footer <- is.character(footer_label) && nzchar(footer_label)
  par(oma = c(if (has_footer) 1.2 else 0, 0, 0, 0), family = "sans")

  # Header
  par(fig = c(0,1, 0.78,1), mai = c(0.12, left_in, 0.12, 3.2), new = FALSE, xpd = NA)
  plot.new(); plot.window(xlim = c(0,1), ylim = c(0,1))
  safe_isfin <- function(x) is.finite(x) & !is.na(x)
  if (!safe_isfin(D_overall)) {
    cc <- sil_df$carac; cc <- cc[!is.na(cc)]
    if (length(cc) > 0) { pk <- as.numeric(table(cc))/length(cc); D_overall <- 1 - sum(pk^2) }
  }
  if (!safe_isfin(Dmax_over)) {
    k <- length(unique(na.omit(sil_df$carac))); Dmax_over <- if (k >= 1) 1 - 1/k else NA_real_
  }
  if (!safe_isfin(QD_overall) && safe_isfin(Qu_overall) && safe_isfin(D_overall) && D_overall > 0) QD_overall <- Qu_overall / D_overall
  if (!safe_isfin(QDmax_over) && safe_isfin(Qu_overall) && safe_isfin(Dmax_over) && Dmax_over > 0) QDmax_over <- Qu_overall / Dmax_over

  labs1 <- c(
    paste0("D20=", f2(D_overall)),
    paste0("Q/D20=",     f2(QD_overall)),
    paste0("Qmax=1-HHI=", f2(Dmax_over)),
    paste0("Q*=Q/Qmax=",  f2(QDmax_over))
  )
  cols1 <- c("#15803d", "black", "#6b21a8", "red")
  place_row_fit(labs1, y = 0.78, cols = cols1, cex = header_cex,
                left_pad_in = 0.06, right_pad_in = 0.06, gap_in = 0.06)

  row1 <- paste0("SS=", f2(SS_overall), " | Qw=", f2(Qw_overall),
                 " | Qu=", f2(Qu_overall), " | AAC=", f2(AAC_globalv), " | AAC2=", f2(AAC_global))
  over_cex_fit <- over_cex
  suppressWarnings({
    w_in <- tryCatch(strwidth(row1, units = "inches", cex = over_cex_fit, font = 2), error = function(e) NA_real_)
    if (is.finite(w_in) && w_in > 0) {
      target_in <- max(4.0, dev_in[1] - left_in - 3.5)
      if (is.finite(target_in) && target_in > 0) {
        over_cex_fit <- min(over_cex_fit, over_cex_fit * (target_in / w_in) * 0.98)
      }
    }
  })
  text(0.5, 0.43, row1, adj = 0.5, cex = max(S(0.95), over_cex_fit), font = 2, col = "red")
  text(0.05, 0.10, "adj.C (n|edge|SS|C#)", adj = 0.8, cex = row3_cex, font = 2)
  #text(0.30, 0.10, "AAC2",  adj = 0.5, cex = row3_cex, font = 2, col = aac_col)
  #text(0.40, 0.10, "WCD",  adj = 0.0, cex = row3_cex, font = 2)
  #text(0.55, 0.10, "adj.mC",adj = 0.0, cex = row3_cex, font = 2)
  text(0.88, 0.10, "                  (SS|Qw|Qu|n)", adj = 0, cex = row3_cex, font = 2)

  # Chart
  par(fig = c(0,1, 0,0.78), mai = c(0.90, left_in, 0.25, 3.2), new = TRUE, xpd = NA)
  plot.new()
  n_bar <- nrow(sil_plot); y_pos <- if (n_bar) seq_len(n_bar) else 1
  plot.window(xlim = c(xmin, xmax), ylim = c(max(y_pos) + 1, 0))
  usr <- par("usr"); dx <- diff(usr[1:2])

  if (aac_side == "right") x_AAC <- clampU(glyph_right + gap_AAC_to_arrow_frac * dx, usr) else x_AAC <- clampU(glyph_left - gap_AAC_to_arrow_frac * dx, usr)
  main_x <- clampU(x_AAC - gap_label_to_AAC_frac * dx, usr)

  # bars
  present_cl <- sort(unique(na.omit(sil_plot$carac)))
  pal <- grDevices::hcl.colors(max(1, length(present_cl)), "Set3")
  bar_cols <- pal[ as.integer(factor(sil_plot$carac, levels = present_cl)) ]
  if (n_bar) rect(0, y_pos - 0.35, sil_plot$sil_width, y_pos + 0.35, col = bar_cols, border = NA)

  # reference + axis
  abline(v = 0.70, lty = 2, col = "red", lwd = 3.2)
  axis(1, cex.axis = axis_cex)
  freq <- table(na.omit(nodes0$carac))
  C <- length(freq)
  p <- if (C) as.numeric(freq)/sum(freq) else NA
  GS <- if (C) 1 - sum(p^2) else NA_real_
  mtext("Silhouette width", side = 1, line = 2.1, cex = title_cex, font = 2)
 
 

   
 


# ensure C exists (falls back to # of unique clusters); v3 handles nodes=NULL safely
nodes_for_footer <- if (is.data.frame(nodes) && nrow(nodes) > 0) nodes else nodes0
if (!"value" %in% names(nodes_for_footer)) nodes_for_footer$value <- 1
if (!"carac" %in% names(nodes_for_footer)) {
  nodes_for_footer$carac <- suppressWarnings(as.integer(sil_df$carac[match(nodes_for_footer$name, sil_df$name)]))
}
C <- length(unique(na.omit(nodes_for_footer$carac)))

mtext(sprintf("(n20=%d, n=%d, v20=%d, v=%d, C#=%d, Dn(GSI:Gini-Simpson)=%.2f)",
              nrow(nodes_for_footer), nrow(nodes0),
              as.integer(sum(nodes_for_footer$value, na.rm = TRUE)),
              as.integer(sum(nodes0$value, na.rm = TRUE)),
              C,  GS),
      side = 1, line = 3.0, cex = S(1.10), font = 2)




  mtext("0.7", side = 1, at = 0.70, col = "red", cex = S(1.00), font = 2, line = 0.9)

  # left labels (names + metrics), right-aligned at main_x
  build_label2 <- function(i){
    parts <- c(
      if ("value"  %in% names(sil_plot))  format(sil_plot$value[i],  trim = TRUE, scientific = FALSE) else NULL,
      if ("value2" %in% names(sil_plot))  format(sil_plot$value2[i], trim = TRUE, scientific = FALSE) else NULL,
      f2(sil_plot$sil_width[i]),
      if ("carac"  %in% names(sil_plot)) paste0("C", sil_plot$carac[i]) else NULL,
       if ("nov"  %in% names(sil_plot))  format(sil_plot$nov[i],  trim = TRUE, scientific = FALSE) else NULL
    )
    paste0(sil_plot$name[i], ifelse(is.na(sil_plot$neighborC[i]), "", paste0("#", sil_plot$neighborC[i])), " (", paste(parts, collapse="|"), ")")
  }
  lab <- vapply(seq_len(n_bar), build_label2, character(1))
  par(xpd = NA); for (i in seq_along(y_pos)) draw_mixed_right(main_x, y_pos[i], lab[i], cex = labels_cex); par(xpd = FALSE)

  # WCD arrows
  par(xpd = NA)
  for (i in seq_len(n_bar)) {
    w <- suppressWarnings(as.numeric(sil_plot$wsel[i])); lw <- map_lwd(w)
    if (!is.na(sil_plot$role[i]) && sil_plot$role[i] == "leader") {
      arrows(glyph_left,  y_pos[i], glyph_right, y_pos[i], length = 0.08, angle = 18, lwd = lw, col = "#d62728", lty = 1, code = 2)
    } else if (is.finite(w)) {
      arrows(glyph_right, y_pos[i], glyph_left,  y_pos[i], length = 0.08, angle = 18, lwd = lw, col = "#1f77b4", lty = 1, code = 2)
    } else {
      arrows(glyph_left,  y_pos[i], glyph_right, y_pos[i], length = 0.08, angle = 18, lwd = 1.3, col = "grey60", lty = 3, code = 2)
    }
  }
  par(xpd = FALSE)

  # ---------- Neighbor labels ON the bars (or outside if you prefer) ----------
  # pick text color for contrast
  txt_contrast <- function(hex) {
    rgb <- grDevices::col2rgb(hex)/255
    # relative luminance
    L <- 0.2126*rgb[1,] + 0.7152*rgb[2,] + 0.0722*rgb[3,]
    ifelse(L < 0.53, "white", "black")
  }
  nn <- if ("neighbor_name" %in% names(sil_plot)) sil_plot$neighbor_name else sil_plot$name
  if ("neighborcluster" %in% names(sil_plot)) {
    nn <- ifelse(!is.na(sil_plot$neighborcluster) & nzchar(nn), sprintf("%s@%s", nn, sil_plot$neighborcluster), nn)
  }

 # Neighbor labels: RIGHT-ALIGN at the bar tip (slightly inside)
 # Neighbor labels: LEFT-ALIGN to the right of the bar tip
 # Neighbor labels: start at the bar tip (left-aligned)
 # ── Neighbor labels: snap RIGHT EDGE to the LEFT EDGE of each SS bar ─────────
 # ── Neighbor labels: start at the RIGHT edge of each SS bar ────────────────
 # ── Neighbor labels: start at the LEFT edge of each SS bar (flow right) ──
par(xpd = NA)

# Build labels
nn <- if ("neighbor_name" %in% names(sil_plot)) sil_plot$neighbor_name else sil_plot$name
if ("neighborcluster" %in% names(sil_plot)) {
  nn <- ifelse(!is.na(sil_plot$neighborcluster) & nzchar(nn),
               sprintf("%s@%s", nn, sil_plot$neighborcluster), nn)
}

# Left edge of the bar:
# - for positive SS, bars go 0 → SS  ⇒ left edge = 0
# - for negative SS, bars go SS → 0  ⇒ left edge = SS
left_edge <- pmin(0, suppressWarnings(as.numeric(sil_plot$sil_width)))

# colored relation arrows before neighbor labels
arrow_dir <- if ("role" %in% names(sil_plot)) {
  ifelse(sil_plot$role %in% c("leader","out","source"), "out",
         ifelse(sil_plot$role %in% c("follower","in","target"), "in", "both"))
} else {
  rep("both", length(y_pos))
}
arrow_sym <- ifelse(arrow_dir == "out", "→",
                    ifelse(arrow_dir == "in", "←", "↔"))
arrow_col <- ifelse(arrow_dir == "out", "red",
                    ifelse(arrow_dir == "in", "blue", "gray50"))

x_arrow <- left_edge - 0.050 * diff(par("usr")[1:2])
x_label <- left_edge - 0.006 * diff(par("usr")[1:2])

text(x = x_arrow, y = y_pos, labels = arrow_sym, adj = c(1, 0.5),
     cex = labels_cex * 0.92, col = arrow_col, xpd = NA)
text(x = x_label, y = y_pos, labels = nn, adj = c(0, 0.5),
     cex = labels_cex * 0.88, col = "black", xpd = NA)

par(xpd = FALSE)





  # ---------------------------------------------------------------------------

  # AAC at cluster leaders
  by_cl <- split(seq_len(n_bar), factor(sil_plot$carac, levels = present_cl))
  v2_num <- suppressWarnings(as.numeric(base_for_aac))
  leader_idx <- vapply(seq_along(present_cl), function(jj){
    idxj <- by_cl[[jj]]; if (!length(idxj)) return(NA_integer_)
    lid <- if (!is.null(sil_plot$role)) idxj[sil_plot$role[idxj] == "leader"] else integer(0)
    if (length(lid)) return(lid[1])
    if (any(is.finite(v2_num[idxj])))       return(idxj[ which.max(replace(v2_num[idxj],            !is.finite(v2_num[idxj]),            -Inf)) ])
    if (any(is.finite(sil_plot$sil_width[idxj]))) return(idxj[ which.max(replace(sil_plot$sil_width[idxj], !is.finite(sil_plot$sil_width[idxj]), -Inf)) ])
    NA_integer_
  }, integer(1))
  par(xpd = NA)
  aac_by_cluster <- aac_by_cluster
  for (jj in which(is.finite(leader_idx))) {
    i <- leader_idx[jj]; clj <- present_cl[jj]
    aacv <- suppressWarnings(as.numeric(aac_by_cluster[as.character(clj)]))
    if (is.finite(aacv)) text(x_AAC, y_pos[i], sprintf("%.2f", aacv), cex = aac_cex, font = 2, col = aac_col)
  }
  par(xpd = FALSE)

  # right-side per-cluster summary (SS|Qw|Qu|n)
  present_cl_chr <- as.character(present_cl)
  ss_per <- setNames(rep(NA_real_, length(present_cl_chr)), present_cl_chr)
  qw_per <- qu_per <- ss_per
  if (is.data.frame(results) && "Cluster" %in% names(results)) {
    get_col <- function(choices, df){ nm <- names(df); idx <- match(tolower(choices), tolower(nm)); idx <- idx[!is.na(idx)]; if (length(idx)) nm[idx[1]] else NULL }
    cSS <- get_col(c("SS","Silhouette","MeanSS","AvgSS","S"), results)
    cQw <- get_col(c("Qw","Q_w","Qweighted","Qw_mean"), results)
    cQu <- get_col(c("Qu","Q_u","Qunweighted","Qu_mean"), results)
    key <- paste0("C", present_cl_chr); ri <- match(key, results$Cluster)
    if (!is.null(cSS)) ss_per[!is.na(ri)] <- suppressWarnings(as.numeric(results[[cSS]][ri[!is.na(ri)]]))
    if (!is.null(cQw)) qw_per[!is.na(ri)] <- suppressWarnings(as.numeric(results[[cQw]][ri[!is.na(ri)]]))
    if (!is.null(cQu)) qu_per[!is.na(ri)] <- suppressWarnings(as.numeric(results[[cQu]][ri[!is.na(ri)]]))
  }
  miss_ss <- !is.finite(ss_per)
  if (any(miss_ss)) for (cc in present_cl_chr[miss_ss]) ss_per[as.character(cc)] <- mean(sil_df$sil_width[sil_df$carac == as.integer(cc)], na.rm = TRUE)

  ### n_full <- setNames(vapply(present_cl_chr, function(cc) sum(sil_df$carac == as.integer(cc), na.rm = TRUE), integer(1)), present_cl_chr)
n_full <- setNames(
  vapply(
    present_cl_chr,
    function(cc) sum(sil_df$carac == as.integer(cc), na.rm = TRUE),
    integer(1)
  ),
  present_cl_chr
)



by_rows <- split(seq_len(n_bar), factor(sil_plot$carac, levels = as.integer(present_cl_chr)))
  mid_y <- vapply(by_rows, function(ix) mean(y_pos[ix], na.rm = TRUE), numeric(1))

  cexs <- S(1.20)
  width_in <- function(txt) {
    txt <- safe_chr_v3(txt)
    strwidth(txt, units = "inches", cex = cexs)
  }
  make_str <- function(cc){
    aacv <- suppressWarnings(as.numeric(aac_by_cluster[as.character(cc)]))
    list(
      cl  = sprintf("C%s", cc),
      aac = if (is.finite(aacv)) sprintf("AAC=%.2f", aacv) else "AAC=0.00",
      ss  = sprintf("|SS=%s", f2(ss_per[cc])),
      qw  = if (is.finite(qw_per[cc])) sprintf("|%.2f", qw_per[cc]) else "|0.00",
      qu  = if (is.finite(qu_per[cc])) sprintf("|%.2f", qu_per[cc]) else "|0.00",
      n   = sprintf("|n=%d", n_full[cc]))
  }
  all_str <- lapply(present_cl_chr, make_str)
  all_str <- lapply(all_str, function(s) {
    for (nm in c("cl", "aac", "ss", "qw", "qu", "n")) s[[nm]] <- safe_chr_v3(s[[nm]])
    s
  })
  max_w <- list(
    cl  = max(vapply(all_str, function(s) width_in(s$cl),  numeric(1))),
    aac = max(vapply(all_str, function(s) width_in(s$aac), numeric(1))),
    ss  = max(vapply(all_str, function(s) width_in(s$ss),  numeric(1))),
    qw  = max(vapply(all_str, function(s) width_in(s$qw),  numeric(1))),
    qu  = max(vapply(all_str, function(s) width_in(s$qu),  numeric(1))),
    n   = max(vapply(all_str, function(s) width_in(s$n),   numeric(1)))
  )
  gap_in <- 0.10
  right_edge_in <- grconvertX(par("usr")[2], "user", "inches") + par("mai")[4] - 0.06
  x_n_in  <- right_edge_in
  x_qu_in <- x_n_in  - max_w$n   - gap_in
  x_qw_in <- x_qu_in - max_w$qu  - gap_in
  x_ss_in <- x_qw_in - max_w$qw  - gap_in
  x_n  <- grconvertX(x_n_in,  "inches", "user")
  x_Qu <- grconvertX(x_qu_in, "inches", "user")
  x_Qw <- grconvertX(x_qw_in, "inches", "user")
  x_SS <- grconvertX(x_ss_in, "inches", "user")

  # place cluster number and AAC into the space between cluster-number area and SS bars
  x_bar_left <- 0
  xr <- diff(par("usr")[1:2])
  x_CL  <- x_bar_left - 0.22 * xr
  x_AAC <- x_bar_left - 0.08 * xr

  par(xpd = NA)
  for (ii in seq_along(present_cl_chr)) {
    cc <- present_cl_chr[ii]; yy <- mid_y[ii]; s <- make_str(cc)
    text(x_CL,  yy, safe_chr_v3(s$cl),  adj = c(1, 0.5), cex = cexs, font = 2, col = "black")
    # patched: suppress duplicate AAC=... label here; AAC value is already shown at cluster leaders and in the header summary
    text(x_SS,  yy, safe_chr_v3(s$ss),  adj = c(1, 0.5), cex = cexs, font = 2, col = "red")
    text(x_Qw,  yy, safe_chr_v3(s$qw),  adj = c(1, 0.5), cex = cexs, font = 2, col = "red")
    text(x_Qu,  yy, safe_chr_v3(s$qu),  adj = c(1, 0.5), cex = cexs, font = 2, col = "blue")
    text(x_n,   yy, safe_chr_v3(s$n),   adj = c(1, 0.5), cex = cexs, font = 2, col = "red")
  }
  par(xpd = FALSE)

  if (has_footer) mtext(footer_label, side = 1, outer = TRUE, line = 0.5, adj = footer_adj, cex = footer_cex, col = footer_col, font = 2)

  invisible(TRUE)
}



# ---- Shiny wrapper expected by app.R -----------------------------------------
render_real_ssplot <- function(
  sil_df,
  nodes = NULL,
  results = NULL,
  res = NULL,
  top_n = NULL,
  font_scale = 1.30,
  ...
) {
  if (!is.data.frame(sil_df) || !nrow(sil_df)) {
    plot.new(); text(0.5, 0.5, "No silhouette data available", cex = 1.4, font = 2, col = "red")
    return(invisible(FALSE))
  }
  nodes0 <- make_nodes0(sil_df, nodes)
  if (is.null(nodes) || !is.data.frame(nodes) || !nrow(nodes)) nodes <- nodes0
  render_panel(
    sil_df = sil_df,
    nodes0 = nodes0,
    results = results,
    res = res,
    nodes = nodes,
    top_n = top_n,
    font_scale = font_scale,
    ...
  )
}

message("[renderSSplot.R] v3 character-safe SSplot loaded")


#
# Disable the example/demo code that follows.  When renderSSplot.R is sourced
# inside the Shiny application, executing this block would overwrite or
# conflict with application-provided variables (e.g. `sil_df`) and generate
# unintended plots or side effects.  To prevent that, we wrap the entire
# demonstration code in `if (FALSE)` so it never runs at runtime.
if (FALSE) {

# ── DEMO DATA (only if you don't already have `sil_df`) ───────────────────────
if (!exists("sil_df", inherits = TRUE)) {
  set.seed(1); n <- 20; cl <- rep(1:5, length.out = n)
  nm <-  sil_df$name
  sil_df <- tibble(
    name = nm,
    sil_width = round(runif(n, 0.2, 0.95), 2),
    carac = cl,
    value = round(runif(n, 0.4, 2.0), 2),
    value2 = round(runif(n, 5, 50), 1),
    wsel = runif(n, 0, 5),
    role = ifelse(!duplicated(cl), "leader", NA),
    neighbor_index = sample(seq_len(n), n, replace = TRUE),
    neighbor_name  = sample(nm, n, replace = TRUE),
    neighborC = sample(1:length(unique(sil_df$cluster)), n, replace = TRUE)
  )
  results <- tibble(
    Cluster = c("OVERALL", paste0("C", sort(unique(cl)))),
    SS = c(mean(sil_df$sil_width), tapply(sil_df$sil_width, sil_df$carac, mean)),
    Qw = runif(1 + length(unique(cl)), 0.6, 0.9),
    Qu = runif(1 + length(unique(cl)), 0.6, 0.9),
    D_GiniSimpson = c(0.76, rep(NA, length(unique(cl)))),
    Q_over_D      = c(1.00, rep(NA, length(unique(cl)))),
    OneMinus_1_over_k = c(0.80, rep(NA, length(unique(cl)))),
    Q_over_Dmax_eff   = c(NA,  rep(NA, length(unique(cl))))
  )
  W <- matrix(runif(n*n, 0, 1), n, n); W <- (W + t(W))/2; diag(W) <- 0
  rownames(W) <- colnames(W) <- sil_df$name
  res <- list(W_km = W)
}

## The following block was originally used for on-screen preview and saving a
## demonstration silhouette plot when sourcing this file directly. When this
## file is sourced within a Shiny app or other non-interactive context,
## executing this code can lead to unwanted side effects (for example, saving
## files to the working directory or attempting to open image viewers). To
## avoid these issues, the entire block is guarded behind an `if (FALSE)` so
## that it never runs automatically. If you wish to preview the panel
## manually, change `FALSE` to `TRUE` and source the file interactively.
if (FALSE) {
  # Build nodes0 if needed
  if (!exists("nodes0", inherits = TRUE)) nodes0 <- make_nodes0(sil_df, if (exists("nodes")) nodes else NULL)

  # ── On-screen preview ----------------------------------------------------------
  opened <- ensure_device()
  par(family = "sans")
  render_panel(
    sil_df   = sil_df,
    nodes0   = nodes0,
    results  = if (exists("results")) results else NULL,
    res      = if (exists("res")) res else NULL,
    nodes    = if (exists("nodes")) nodes else NULL,
    top_n    = nrow(sil_df),
    aac_col  = "#A23B3B",
    aac_side = "left",
    neighbor_side = "right",     # anchor at bar tip
    neighbor_on_bar = TRUE       # <— draw ON the bar
  )
  if (opened) dev.off()

  # ── Save PNG -------------------------------------------------------------------
  outfile <- file.path(getwd(), "silhouette_panel.png")
  H <- 3 + 0.28 * nrow(sil_df)
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(outfile, width = 12, height = H, units = "in", res = 150)
  } else {
    png(outfile, width = 12, height = H, units = "in", res = 150, type = "cairo")
  }
  par(family = "sans")
  render_panel(
    sil_df   = sil_df,
    nodes0   = nodes0,
    results  = if (exists("results")) results else NULL,
    res      = if (exists("res")) res else NULL,
    nodes    = if (exists("nodes")) nodes else NULL,
    top_n    = nrow(sil_df),
    aac_col  = "#A23B3B",
    aac_side = "left",
    neighbor_side = "right",
    neighbor_on_bar = TRUE,
    neighbor_inset_frac_of_bar = 0.03,  # a little inset from the tip
    footer_adj = 0
  )
  dev.off()
  cat("Saved PNG to:", normalizePath(outfile, winslash = "/"), "\n")
  if (.Platform$OS.type == "windows") try(shell.exec(normalizePath(outfile)), silent = TRUE)
}

# Close the disabled example code
}
