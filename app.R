
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(igraph)
library(visNetwork)
library(DT)
library(readr)
library(readxl)
library(ggplot2)

# ---- App directory + optional real FLCA-SIL-MA module ----
# PNG export patch: v4c direct SSplot ggplot download.
APP_DIR <- tryCatch({
  of <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
  if (!is.null(of) && nzchar(of)) dirname(normalizePath(of, winslash = "/", mustWork = FALSE)) else getwd()
}, error = function(e) getwd())

flca_module_path <- file.path(APP_DIR, "flca_ms_sil_module.R")
.real_flca_module_loaded <- FALSE
if (file.exists(flca_module_path)) {
  try({
    source(flca_module_path, local = FALSE, encoding = "UTF-8")
    .real_flca_module_loaded <- exists("FLCA_run", mode = "function") || exists("FLCA_nodes_edges", mode = "function")
    message("[app.R] real FLCA-SIL-MA module loaded: ", flca_module_path)
  }, silent = TRUE)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---- Optional strict English tail keyword engine ----
en_tail_engine_path <- file.path(APP_DIR, "en_tail_keyword_engine.R")
.en_tail_engine_loaded <- FALSE
if (file.exists(en_tail_engine_path)) {
  try({
    source(en_tail_engine_path, local = FALSE, encoding = "UTF-8")
    .en_tail_engine_loaded <- exists("extract_units_from_text_en", mode = "function") &&
      exists("normalize_term_en", mode = "function") &&
      exists("select_subset_free_terms_en", mode = "function")
    message("[app.R] strict English tail keyword engine loaded: ", en_tail_engine_path)
  }, silent = TRUE)
}


# ---- Optional external plot helpers: keep the stable homepage unchanged ----
# These source files only add helper functions. They do not rebuild the UI/homepage.
source_optional_app <- function(filename) {
  path <- file.path(APP_DIR, filename)
  if (file.exists(path)) {
    tryCatch({
      source(path, local = FALSE, encoding = "UTF-8")
      message("[app.R] sourced helper: ", path)
      TRUE
    }, error = function(e) {
      warning("[app.R] failed to source ", filename, ": ", conditionMessage(e))
      FALSE
    })
  } else {
    message("[app.R] optional helper not found: ", path)
    FALSE
  }
}
.real_ssplot_loaded <- source_optional_app("renderSSplot.R") && exists("render_real_ssplot", mode = "function")
.kano_helper_loaded <- source_optional_app("kano.R")
.sankey_helper_loaded <- source_optional_app("sankey.R")

# Manual author keywords are entered by users when keywords are difficult to extract
# from the document. They are normalized and forced into Step 3 together with
# document-detected author keywords.
parse_manual_author_keywords_app <- function(x) {
  x <- as.character(x %||% "")
  if (!length(x) || !nzchar(trimws(x))) return(character(0))
  x <- gsub("\\r\\n?", "\\n", x, perl = TRUE)
  x <- gsub("[;；,，]+", "\\n", x, perl = TRUE)
  bits <- unlist(strsplit(x, "\\n+", perl = TRUE), use.names = FALSE)
  bits <- trimws(bits)
  bits <- bits[nzchar(bits)]
  if (exists("normalize_author_keywords_unique_app", mode = "function")) {
    bits <- normalize_author_keywords_unique_app(bits)
  } else {
    bits <- tolower(bits)
    bits <- gsub("[‐‑–—_/.:]+", " ", bits, perl = TRUE)
    bits <- gsub("[^[:alnum:] -]+", " ", bits, perl = TRUE)
    bits <- gsub("[[:space:]]+", " ", bits, perl = TRUE)
    bits <- trimws(bits)
  }
  unique(bits[nzchar(bits)])
}

# Convert the app's sil_df/cluster_summary objects into the real renderSSplot.R format.
build_real_ssplot_input_app <- function(analysis_obj) {
  sil <- analysis_obj$sil_df
  selected <- analysis_obj$selected %||% data.frame()
  export_nodes <- analysis_obj$export_nodes %||% data.frame()
  cluster_summary <- analysis_obj$cluster_summary %||% data.frame()

  if (is.null(sil) || !is.data.frame(sil) || !nrow(sil)) {
    stop("No silhouette data available for SSplot.", call. = FALSE)
  }
  nm <- names(sil)
  name_col <- if ("name" %in% nm) "name" else if ("term" %in% nm) "term" else nm[[1]]
  ss_col <- if ("sil_width" %in% nm) "sil_width" else if ("ss" %in% nm) "ss" else NA_character_
  cl_col <- if ("carac" %in% nm) "carac" else if ("topic" %in% nm) "topic" else if ("cluster" %in% nm) "cluster" else NA_character_
  if (is.na(ss_col) || is.na(cl_col)) stop("SSplot needs silhouette width and cluster columns.", call. = FALSE)

  sil_real <- sil |>
    dplyr::transmute(
      name = as.character(.data[[name_col]]),
      sil_width = suppressWarnings(as.numeric(.data[[ss_col]])),
      carac = suppressWarnings(as.integer(gsub("^C", "", as.character(.data[[cl_col]])))),
      value = if ("value" %in% names(sil)) suppressWarnings(as.numeric(.data$value)) else NA_real_,
      role = if ("is_leader" %in% names(sil)) ifelse(.data$is_leader %in% TRUE, "leader", "follower") else NA_character_,
      neighbor_name = if ("leader" %in% names(sil)) as.character(.data$leader) else as.character(.data[[name_col]]),
      neighborC = suppressWarnings(as.integer(gsub("^C", "", as.character(.data[[cl_col]])))),
      wsel = if ("value" %in% names(sil)) suppressWarnings(as.numeric(.data$value)) else NA_real_
    ) |>
    dplyr::filter(!is.na(name), nzchar(name), is.finite(sil_width), !is.na(carac)) |>
    dplyr::distinct(name, .keep_all = TRUE)

  if (is.data.frame(selected) && nrow(selected)) {
    selected_name <- if ("term" %in% names(selected)) as.character(selected$term) else if ("name" %in% names(selected)) as.character(selected$name) else as.character(selected[[1]])
    selected_score <- if ("score" %in% names(selected)) suppressWarnings(as.numeric(selected$score)) else if ("value" %in% names(selected)) suppressWarnings(as.numeric(selected$value)) else rep(NA_real_, nrow(selected))
    selected_role <- if ("is_leader" %in% names(selected)) ifelse(selected$is_leader %in% TRUE, "leader", "follower") else rep(NA_character_, nrow(selected))
    selected_leader <- if ("leader" %in% names(selected)) as.character(selected$leader) else rep(NA_character_, nrow(selected))
    selected2 <- tibble::tibble(name = selected_name, score_value = selected_score, selected_role = selected_role, selected_leader = selected_leader) |>
      dplyr::distinct(name, .keep_all = TRUE)
    sil_real <- sil_real |>
      dplyr::left_join(selected2, by = "name") |>
      dplyr::mutate(
        value = ifelse(is.finite(value), value, score_value),
        role = ifelse(is.na(role) | !nzchar(role), selected_role, role),
        neighbor_name = ifelse(is.na(neighbor_name) | !nzchar(neighbor_name), selected_leader, neighbor_name),
        wsel = ifelse(is.finite(wsel), wsel, score_value)
      ) |>
      dplyr::select(-score_value, -selected_role, -selected_leader)
  }

  if (is.data.frame(export_nodes) && nrow(export_nodes) && all(c("name", "value2") %in% names(export_nodes))) {
    en2 <- export_nodes |>
      dplyr::transmute(name = as.character(name), value2 = suppressWarnings(as.numeric(value2))) |>
      dplyr::distinct(name, .keep_all = TRUE)
    sil_real <- sil_real |> dplyr::left_join(en2, by = "name")
  } else {
    sil_real$value2 <- NA_real_
  }
  sil_real$value <- ifelse(is.finite(sil_real$value), sil_real$value, 1)
  sil_real$value2 <- ifelse(is.finite(sil_real$value2), sil_real$value2, sil_real$value)
  sil_real$neighbor_name <- ifelse(is.na(sil_real$neighbor_name) | !nzchar(sil_real$neighbor_name), sil_real$name, sil_real$neighbor_name)

  nodes_real <- sil_real |>
    dplyr::transmute(name, carac, value, value2) |>
    dplyr::distinct(name, .keep_all = TRUE)

  overall_q <- NA_real_
  if (is.data.frame(cluster_summary) && nrow(cluster_summary) && "modularity_Q" %in% names(cluster_summary)) {
    qv <- suppressWarnings(as.numeric(cluster_summary$modularity_Q))
    qv <- qv[is.finite(qv)]
    if (length(qv)) overall_q <- qv[[1]]
  }
  overall_row <- tibble::tibble(
    Cluster = "OVERALL",
    SS = mean(sil_real$sil_width, na.rm = TRUE),
    Q = overall_q,
    Qw = overall_q,
    Qu = overall_q
  )
  if (is.data.frame(cluster_summary) && nrow(cluster_summary)) {
    csum <- cluster_summary |>
      dplyr::transmute(
        Cluster = paste0("C", as.integer(topic)),
        SS = suppressWarnings(as.numeric(cluster_ss)),
        Q = suppressWarnings(as.numeric(modularity_Q)),
        Qw = suppressWarnings(as.numeric(modularity_Q)),
        Qu = suppressWarnings(as.numeric(modularity_Q))
      )
    results_real <- dplyr::bind_rows(overall_row, csum)
  } else {
    csum <- sil_real |>
      dplyr::group_by(carac) |>
      dplyr::summarise(SS = mean(sil_width, na.rm = TRUE), .groups = "drop") |>
      dplyr::transmute(Cluster = paste0("C", carac), SS = SS, Q = NA_real_, Qw = NA_real_, Qu = NA_real_)
    results_real <- dplyr::bind_rows(overall_row, csum)
  }

  list(sil_df = sil_real, nodes = nodes_real, results = results_real, res = NULL)
}



safe_read_upload <- function(path, name) {
  ext <- tolower(tools::file_ext(name %||% path))

  if (ext == "csv") {
    return(readr::read_csv(path, show_col_types = FALSE))
  }

  if (ext %in% c("xlsx", "xls")) {
    return(as.data.frame(readxl::read_excel(path)))
  }

  if (ext == "txt") {
    txt <- readLines(path, warn = FALSE, encoding = "UTF-8")
    txt <- txt[nzchar(trimws(txt))]
    return(data.frame(doc_id = seq_along(txt), text = txt, stringsAsFactors = FALSE))
  }

  if (ext == "pdf") {
    if (!requireNamespace("pdftools", quietly = TRUE)) {
      stop("Package 'pdftools' is required for PDF upload. Please install.packages('pdftools').")
    }
    pages <- pdftools::pdf_text(path)
    # Keep PDF line breaks. Collapsing all whitespace into one line makes it
    # impossible to reliably capture author keyword blocks such as:
    # Keywords:
    # R-shiny app
    # NLP
    # Text mining
    # ...
    # A B S T R A C T
    pages <- gsub("\r\n?", "\n", pages, perl = TRUE)
    pages <- gsub("[ \t\f\v]+", " ", pages, perl = TRUE)
    pages <- gsub(" *\n *", "\n", pages, perl = TRUE)
    pages <- trimws(pages)
    pages <- pages[nzchar(pages)]
    if (!length(pages)) stop("No readable text found in PDF.")
    return(data.frame(doc_id = seq_along(pages), text = pages, stringsAsFactors = FALSE))
  }

  if (ext == "docx") {
    if (!requireNamespace("officer", quietly = TRUE)) {
      stop("Package 'officer' is required for DOCX upload. Please install.packages('officer').")
    }
    doc <- officer::read_docx(path)
    s <- officer::docx_summary(doc)
    txt <- s$text %||% character(0)
    txt <- as.character(txt)
    txt <- trimws(txt)
    txt <- txt[nzchar(txt)]
    if (!length(txt)) stop("No readable text found in DOCX.")
    return(data.frame(doc_id = seq_along(txt), text = txt, stringsAsFactors = FALSE))
  }

  stop("Supported file types: .csv, .xlsx, .xls, .txt, .docx, .pdf")
}

safe_read_url_input <- function(url) {
  url <- trimws(as.character(url %||% ""))
  if (!nzchar(url)) stop("URL is empty.")
  if (!grepl("^https?://", url, ignore.case = TRUE)) {
    stop("URL must start with http:// or https://")
  }

  clean_url <- sub("[?#].*$", "", url)
  ext <- tolower(tools::file_ext(clean_url))
  if (!nzchar(ext)) ext <- "html"
  if (!ext %in% c("html", "htm", "pdf", "txt", "csv", "xlsx", "xls", "docx")) ext <- "html"

  tmp <- tempfile(fileext = paste0(".", ext))
  utils::download.file(url, tmp, mode = "wb", quiet = TRUE)

  if (ext %in% c("pdf", "txt", "csv", "xlsx", "xls", "docx")) {
    return(safe_read_upload(tmp, paste0("downloaded.", ext)))
  }

  html_txt <- paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = "
")
  if (!nzchar(trimws(html_txt))) stop("No readable HTML text was downloaded from the URL.")

  if (requireNamespace("xml2", quietly = TRUE)) {
    doc <- tryCatch(xml2::read_html(html_txt), error = function(e) NULL)
    if (!is.null(doc)) {
      junk <- xml2::xml_find_all(doc, ".//script|.//style|.//noscript")
      if (length(junk)) xml2::xml_remove(junk)
      body <- xml2::xml_text(doc)
    } else {
      body <- html_txt
    }
  } else {
    body <- gsub("(?is)<script.*?</script>", " ", html_txt, perl = TRUE)
    body <- gsub("(?is)<style.*?</style>", " ", body, perl = TRUE)
    body <- gsub("(?s)<[^>]+>", " ", body, perl = TRUE)
  }

  body <- gsub("&nbsp;", " ", body, fixed = TRUE)
  body <- gsub("&amp;", "&", body, fixed = TRUE)
  body <- gsub("[	

]+", " ", body, perl = TRUE)
  body <- gsub(" *
+ *", "
", body, perl = TRUE)
  body <- trimws(body)
  if (!nzchar(body)) stop("No readable body text found in downloaded HTML.")

  data.frame(doc_id = "url_1", text = body, source_url = url, stringsAsFactors = FALSE)
}

# ---- Real FLCA-SIL-MA integration helpers ----
.standardize_edge_terms <- function(edges) {
  if (is.null(edges) || !is.data.frame(edges) || nrow(edges) == 0) {
    return(tibble(term1 = character(), term2 = character(), WCD = numeric()))
  }
  ed <- as.data.frame(edges, stringsAsFactors = FALSE)
  cn <- names(ed)
  if (all(c("term1","term2","WCD") %in% cn)) {
    out <- ed |> transmute(term1 = as.character(term1), term2 = as.character(term2), WCD = suppressWarnings(as.numeric(WCD)))
  } else if (all(c("Leader","follower","WCD") %in% cn)) {
    out <- ed |> transmute(term1 = as.character(Leader), term2 = as.character(follower), WCD = suppressWarnings(as.numeric(WCD)))
  } else if (all(c("Leader","Follower","WCD") %in% cn)) {
    out <- ed |> transmute(term1 = as.character(Leader), term2 = as.character(Follower), WCD = suppressWarnings(as.numeric(WCD)))
  } else if (all(c("Source","Target","WCD") %in% cn)) {
    out <- ed |> transmute(term1 = as.character(Source), term2 = as.character(Target), WCD = suppressWarnings(as.numeric(WCD)))
  } else if (ncol(ed) >= 3) {
    out <- tibble(term1 = as.character(ed[[1]]), term2 = as.character(ed[[2]]), WCD = suppressWarnings(as.numeric(ed[[3]])))
  } else {
    out <- tibble(term1 = character(), term2 = character(), WCD = numeric())
  }
  out |>
    filter(!is.na(term1), !is.na(term2), nzchar(term1), nzchar(term2), term1 != term2, is.finite(WCD), WCD > 0) |>
    mutate(WCD = as.numeric(WCD))
}

.split_single_cluster_if_needed_app <- function(nodes_flca) {
  if (is.null(nodes_flca) || !nrow(nodes_flca)) return(nodes_flca)
  if (!"carac" %in% names(nodes_flca)) nodes_flca$carac <- 1L
  if (length(unique(nodes_flca$carac)) <= 1L && nrow(nodes_flca) >= 2L) {
    nodes_flca <- nodes_flca |>
      arrange(desc(value), name)
    split_index <- ceiling(nrow(nodes_flca) / 2)
    nodes_flca$carac <- c(rep(1L, split_index), rep(2L, nrow(nodes_flca) - split_index))
  }
  nodes_flca
}

.renumber_clusters_by_value_app <- function(nodes_flca) {
  if (is.null(nodes_flca) || !nrow(nodes_flca)) return(nodes_flca)
  nodes_flca$carac <- as.character(nodes_flca$carac)
  cs <- tapply(nodes_flca$value, nodes_flca$carac, function(x) max(suppressWarnings(as.numeric(x)), na.rm = TRUE))
  cs[!is.finite(cs)] <- 0
  ord <- names(cs)[order(-cs, suppressWarnings(as.numeric(names(cs))), na.last = TRUE)]
  map <- setNames(seq_along(ord), ord)
  nodes_flca$carac <- as.integer(map[nodes_flca$carac])
  nodes_flca
}

.apply_real_flca_to_nodes_edges <- function(nodes, edges, verbose = FALSE) {
  nodes0 <- as.data.frame(nodes, stringsAsFactors = FALSE)
  if (!"name" %in% names(nodes0) && "term" %in% names(nodes0)) nodes0$name <- nodes0$term
  if (!"value" %in% names(nodes0) && "score" %in% names(nodes0)) nodes0$value <- nodes0$score
  if (!"value2" %in% names(nodes0)) nodes0$value2 <- nodes0$value
  nodes0 <- nodes0 |>
    transmute(
      name = as.character(name),
      value = suppressWarnings(as.numeric(value)),
      value2 = suppressWarnings(as.numeric(value2))
    ) |>
    filter(!is.na(name), nzchar(name)) |>
    distinct(name, .keep_all = TRUE) |>
    mutate(
      value = ifelse(is.finite(value), value, 0),
      value2 = ifelse(is.finite(value2), value2, value)
    ) |>
    arrange(desc(value), name)

  edges0 <- .standardize_edge_terms(edges) |>
    filter(term1 %in% nodes0$name, term2 %in% nodes0$name)

  method <- "internal_fallback_no_real_flca"
  flca_nodes <- nodes0 |> mutate(carac = seq_len(nrow(nodes0)))
  flca_edges <- edges0

  if ((exists("FLCA_run", mode = "function") || exists("FLCA_nodes_edges", mode = "function")) &&
      nrow(nodes0) >= 2 && nrow(edges0) > 0) {

    # Prefer FLCA_run(): it contains the module's one-cluster splitting logic.
    res <- NULL
    if (exists("FLCA_run", mode = "function")) {
      res <- tryCatch(
        FLCA_run(list(
          nodes = nodes0 |> transmute(name = name, value = value),
          data  = edges0 |> transmute(term1 = term1, term2 = term2, WCD = WCD)
        )),
        error = function(e) NULL
      )
      if (!is.null(res)) method <- "FLCA_run"
    }

    # Fallback to FLCA_nodes_edges() if needed.
    if (is.null(res) && exists("FLCA_nodes_edges", mode = "function")) {
      res <- tryCatch(
        FLCA_nodes_edges(
          nodes = nodes0 |> transmute(name = name, value = value, value2 = value2),
          edges = edges0 |> transmute(term1 = term1, term2 = term2, WCD = WCD),
          verbose = verbose
        ),
        error = function(e) NULL
      )
      if (!is.null(res)) method <- "FLCA_nodes_edges"
    }

    if (!is.null(res)) {
      # Parse FLCA nodes
      if (is.list(res) && !is.null(res$nodes)) {
        rn <- as.data.frame(res$nodes, stringsAsFactors = FALSE)
      } else if (is.list(res) && !is.null(res$name)) {
        rn <- data.frame(
          name = as.character(res$name),
          value = suppressWarnings(as.numeric(res$value %||% 0)),
          carac = suppressWarnings(as.integer(res$membership %||% res$carac %||% seq_along(res$name))),
          stringsAsFactors = FALSE
        )
      } else {
        rn <- NULL
      }

      if (!is.null(rn) && nrow(rn)) {
        if (!"name" %in% names(rn) && ncol(rn) >= 1) names(rn)[1] <- "name"
        if (!"value" %in% names(rn)) rn$value <- nodes0$value[match(as.character(rn$name), nodes0$name)]
        if (!"carac" %in% names(rn)) {
          if ("membership" %in% names(rn)) rn$carac <- rn$membership
          else if ("cluster" %in% names(rn)) rn$carac <- rn$cluster
          else rn$carac <- seq_len(nrow(rn))
        }
        flca_nodes <- rn |>
          transmute(
            name = as.character(name),
            value = suppressWarnings(as.numeric(value)),
            carac = as.character(carac)
          ) |>
          filter(name %in% nodes0$name) |>
          left_join(nodes0 |> select(name, value0 = value, value2), by = "name") |>
          mutate(
            value = ifelse(is.finite(value), value, value0),
            value2 = ifelse(is.finite(value2), value2, value),
            carac = ifelse(is.na(carac) | !nzchar(carac), "1", carac)
          ) |>
          select(name, value, value2, carac)
      }

      # Parse FLCA leader-follower edges
      if (is.list(res) && !is.null(res$data)) {
        flca_edges <- .standardize_edge_terms(res$data)
      } else if (is.list(res) && !is.null(res$Leader) && !is.null(res$follower)) {
        flca_edges <- tibble(term1 = as.character(res$Leader), term2 = as.character(res$follower), WCD = suppressWarnings(as.numeric(res$WCD %||% 1)))
      }
      flca_edges <- flca_edges |>
        filter(term1 %in% nodes0$name, term2 %in% nodes0$name, term1 != term2, is.finite(WCD), WCD > 0)
    }
  }

  # Apply module-equivalent safeguard: if FLCA returns one cluster, split it into two by value.
  flca_nodes <- .split_single_cluster_if_needed_app(flca_nodes)
  flca_nodes <- .renumber_clusters_by_value_app(flca_nodes)

  # Fill missing nodes after FLCA.
  missing_names <- setdiff(nodes0$name, flca_nodes$name)
  if (length(missing_names)) {
    extra <- nodes0 |> filter(name %in% missing_names) |> mutate(carac = max(flca_nodes$carac %||% 0L, na.rm = TRUE) + seq_along(missing_names))
    flca_nodes <- bind_rows(flca_nodes, extra)
  }

  # Final one-edge-per-follower relation set using FLCA/module clusters.
  score_map <- setNames(nodes0$value, nodes0$name)
  if (nrow(flca_edges)) {
    flca_edges <- flca_edges |>
      mutate(
        source_score = suppressWarnings(as.numeric(score_map[term1])),
        WCD = suppressWarnings(as.numeric(WCD))
      ) |>
      group_by(term2) |>
      arrange(desc(WCD), desc(source_score), term1, .by_group = TRUE) |>
      slice_head(n = 1) |>
      ungroup() |>
      transmute(term1 = as.character(term1), term2 = as.character(term2), WCD = as.integer(round(WCD)), edge_type = "leader_follower")
  } else {
    flca_edges <- tibble(term1 = character(), term2 = character(), WCD = integer(), edge_type = character())
  }

  list(nodes = flca_nodes, edges = flca_edges, method = method)
}

.make_selected_from_flca <- function(score_tbl, selected_terms, flca_obj, co_edges_sel = NULL) {
  flca_nodes <- flca_obj$nodes |>
    transmute(term = as.character(name), topic = as.integer(carac), flca_value = suppressWarnings(as.numeric(value)))
  if (!nrow(flca_nodes)) {
    flca_nodes <- tibble(term = selected_terms, topic = seq_along(selected_terms), flca_value = NA_real_)
  }

  degree_tbl <- bind_rows(
    (co_edges_sel %||% tibble()) |> transmute(term = term1, WCD = WCD),
    (co_edges_sel %||% tibble()) |> transmute(term = term2, WCD = WCD)
  ) |>
    group_by(term) |>
    summarise(degree = sum(as.numeric(WCD), na.rm = TRUE), .groups = "drop")

  base <- score_tbl |>
    filter(term %in% selected_terms) |>
    select(term, doc_freq, tfidf_sum, source_type, author_keyword, score, exact_in_document) |>
    left_join(flca_nodes, by = "term") |>
    left_join(degree_tbl, by = "term") |>
    mutate(
      topic = ifelse(is.na(topic), max(topic, na.rm = TRUE) + row_number(), topic),
      degree = ifelse(is.na(degree) | !is.finite(degree), 0, degree),
      score = ifelse(is.finite(score), score, flca_value)
    ) |>
    select(term, topic, degree, doc_freq, tfidf_sum, score, source_type, author_keyword, exact_in_document)

  leader_candidates <- flca_obj$edges$term1 %||% character(0)
  leader_map <- base |>
    group_by(topic) |>
    arrange(desc(term %in% leader_candidates), desc(score), desc(degree), term, .by_group = TRUE) |>
    summarise(topic_leader = first(term), .groups = "drop")

  base |>
    left_join(leader_map, by = "topic") |>
    group_by(topic) |>
    arrange(desc(term == topic_leader), desc(score), desc(degree), term, .by_group = TRUE) |>
    mutate(
      topic_rank = row_number(),
      leader = topic_leader,
      is_leader = term == leader,
      value = as.numeric(score)
    ) |>
    ungroup() |>
    select(-topic_leader) |>
    arrange(topic, desc(is_leader), desc(score), term)
}

.compute_silhouette_from_flca_module <- function(selected, edges) {
  if (exists("compute_silhouette_df", mode = "function") && is.data.frame(edges) && nrow(edges)) {
    out <- tryCatch({
      nd <- selected |> transmute(name = term, value = score, carac = topic)
      ed <- edges |> transmute(Leader = term1, Follower = term2, WCD = WCD)
      s <- compute_silhouette_df(nd, ed)
      if (!is.null(s) && is.data.frame(s) && nrow(s)) {
        s |>
          transmute(
            term = as.character(name),
            topic = as.integer(selected$topic[match(as.character(name), selected$term)]),
            leader = as.character(selected$leader[match(as.character(name), selected$term)]),
            is_leader = as.logical(selected$is_leader[match(as.character(name), selected$term)]),
            value = as.numeric(selected$score[match(as.character(name), selected$term)]),
            ss = suppressWarnings(as.numeric(if ("ssi" %in% names(s)) ssi else if ("sil_width" %in% names(s)) sil_width else 0))
          )
      } else NULL
    }, error = function(e) NULL)
    if (!is.null(out)) return(out)
  }
  NULL
}

has_nodes_edges_sheets <- function(path, name = NULL) {
  ext <- tolower(tools::file_ext(name %||% path))
  if (!ext %in% c("xlsx", "xls")) return(FALSE)
  s <- tryCatch(tolower(readxl::excel_sheets(path)), error = function(e) character(0))
  all(c("nodes", "edges") %in% s)
}

read_nodes_edges_bundle <- function(path, name = NULL) {
  stopifnot(has_nodes_edges_sheets(path, name))
  s <- tolower(readxl::excel_sheets(path))
  idx <- function(nm) which(s == tolower(nm))[1]
  nodes <- as.data.frame(readxl::read_excel(path, sheet = idx("nodes")))
  edges <- as.data.frame(readxl::read_excel(path, sheet = idx("edges")))
  sil_df <- if ("sil_df" %in% s) as.data.frame(readxl::read_excel(path, sheet = idx("sil_df"))) else NULL
  validation <- if ("validation" %in% s) as.data.frame(readxl::read_excel(path, sheet = idx("validation"))) else NULL
  list(upload_type = "nodes_edges_bundle", nodes = nodes, edges = edges, sil_df = sil_df, validation = validation)
}

build_analysis_from_nodes_edges_bundle <- function(bundle, top_n = 20L) {
  nodes_raw <- bundle$nodes %||% data.frame()
  edges_raw <- bundle$edges %||% data.frame()
  sil_in <- bundle$sil_df
  val_in <- bundle$validation

  names(nodes_raw) <- trimws(names(nodes_raw))
  names(edges_raw) <- trimws(names(edges_raw))
  if (!all(c("name", "value") %in% names(nodes_raw))) stop("nodes sheet must contain at least columns: name, value")
  if (!all(c("term1", "term2", "WCD") %in% names(edges_raw))) stop("edges sheet must contain columns: term1, term2, WCD")

  nodes0 <- nodes_raw |>
    transmute(
      name = as.character(name),
      value = suppressWarnings(as.numeric(value)),
      value2 = suppressWarnings(as.numeric(if ("value2" %in% names(nodes_raw)) value2 else value))
    ) |>
    filter(!is.na(name), nzchar(name)) |>
    distinct(name, .keep_all = TRUE) |>
    mutate(
      value = ifelse(is.finite(value), value, 0),
      value2 = ifelse(is.finite(value2), value2, value)
    ) |>
    arrange(desc(value), name)

  if (nrow(nodes0) > as.integer(top_n %||% 20L)) nodes0 <- nodes0 |> slice_head(n = as.integer(top_n %||% 20L))

  edges0 <- edges_raw |>
    transmute(term1 = as.character(term1), term2 = as.character(term2), WCD = suppressWarnings(as.numeric(WCD))) |>
    filter(!is.na(term1), !is.na(term2), nzchar(term1), nzchar(term2),
           term1 %in% nodes0$name, term2 %in% nodes0$name, is.finite(WCD), WCD > 0)

  # REAL FLCA-SIL-MA is applied here even when a ready-made nodes+edges XLSX is uploaded.
  flca_obj <- .apply_real_flca_to_nodes_edges(nodes0, edges0, verbose = FALSE)

  score_tbl <- nodes0 |>
    transmute(
      term = name,
      doc_freq = NA_real_,
      tfidf_sum = value / 100,
      source_type = "xlsx_bundle",
      author_keyword = FALSE,
      score = value,
      exact_in_document = NA
    )

  selected <- .make_selected_from_flca(score_tbl, nodes0$name, flca_obj, edges0)

  export_edges <- flca_obj$edges |>
    filter(term1 %in% selected$term, term2 %in% selected$term, term1 != term2) |>
    transmute(term1 = as.character(term1), term2 = as.character(term2), WCD = as.integer(round(WCD)))

  if (!nrow(export_edges)) {
    export_edges <- edges0 |>
      transmute(term1 = as.character(term1), term2 = as.character(term2), WCD = as.integer(round(WCD)))
  }

  # If a sil_df sheet exists, keep its SS values but replace cluster/leader with FLCA results.
  if (!is.null(sil_in) && is.data.frame(sil_in) && nrow(sil_in)) {
    names(sil_in) <- trimws(names(sil_in))
    nm <- names(sil_in)
    if (all(c("name", "sil_width") %in% nm)) {
      sil_df <- sil_in |>
        transmute(
          term = as.character(name),
          ss_uploaded = suppressWarnings(as.numeric(sil_width)),
          value_uploaded = suppressWarnings(as.numeric(if ("value" %in% nm) value else NA_real_))
        ) |>
        filter(term %in% selected$term) |>
        right_join(selected |> transmute(term, topic, leader, is_leader, value = score), by = "term") |>
        mutate(
          ss = ifelse(is.finite(ss_uploaded), ss_uploaded, 0),
          value = ifelse(is.finite(value_uploaded), value_uploaded, value)
        ) |>
        select(term, topic, leader, is_leader, value, ss)
    } else {
      sil_df <- .compute_silhouette_from_flca_module(selected, export_edges) %||% compute_silhouette_table(selected, export_edges)
    }
  } else {
    sil_df <- .compute_silhouette_from_flca_module(selected, export_edges) %||% compute_silhouette_table(selected, export_edges)
  }

  cluster_summary <- compute_cluster_summary(selected, export_edges, sil_df)
  aac_dashboard <- compute_aac_dashboard(selected, cluster_summary)
  overall_aac <- mean(aac_dashboard$AAC, na.rm = TRUE)

  export_nodes <- selected |>
    transmute(
      name = term,
      value = as.numeric(score),
      value2 = {
        vv <- nodes0$value2[match(term, nodes0$name)]
        ifelse(is.finite(vv), vv, 0)
      }
    )

  validation <- validate_export_tables(export_nodes, export_edges) |>
    mutate(result = as.character(result)) |>
    bind_rows(tibble(check = "real_flca_module_loaded", result = as.character(isTRUE(.real_flca_module_loaded)))) |>
    bind_rows(tibble(check = "real_flca_method", result = as.character(flca_obj$method))) |>
    bind_rows(tibble(check = "flca_cluster_count", result = as.character(length(unique(selected$topic)))))

  if (!is.null(val_in) && is.data.frame(val_in) && all(c("check", "result") %in% names(val_in))) {
    validation <- bind_rows(validation, val_in |> transmute(check = as.character(check), result = as.character(result)))
  }

  g <- igraph::graph_from_data_frame(export_edges |> transmute(from = term1, to = term2, weight = WCD), directed = TRUE, vertices = data.frame(name = selected$term))
  if (igraph::ecount(g) > 0) igraph::E(g)$weight <- export_edges$WCD

  list(
    docs = tibble(doc_id = integer(), text = character()),
    author_keywords = character(0),
    extracted = tibble(),
    ranked = selected |> transmute(doc_id = NA_integer_, term = term, tf_idf = score),
    selected = selected,
    edges = export_edges,
    co_edges = edges0,
    graph = g,
    sil_df = sil_df,
    cluster_summary = cluster_summary,
    aac_dashboard = aac_dashboard,
    overall_aac = overall_aac,
    extraction_log = tibble(
      item = c("source_mode", "file_type", "draw_method", "real_flca_method", "flca_cluster_count"),
      value = c("uploaded_nodes_edges_xlsx", "nodes+edges bundle", "direct visual rendering after real FLCA-SIL-MA clustering", flca_obj$method, as.character(length(unique(selected$topic))))
    ),
    export_nodes = export_nodes,
    export_edges = export_edges,
    validation = validation
  )
}

default_stopwords <- unique(c(
  tidytext::stop_words$word,
  letters,
  tolower(month.name),
  tolower(month.abb),
  c(
    "figure", "fig", "table", "study", "studies", "method", "methods",
    "result", "results", "discussion", "introduction", "conclusion",
    "references", "reference", "appendix", "supplementary", "metadata",
    "received", "accepted", "available", "online", "copyright", "license",
    "doi", "https", "http", "www", "org", "com", "net", "html", "pdf",
    "cran", "package", "version", "vol", "issue", "pp", "pages",
    "author", "authors", "email", "abstract", "keywords",
    "using", "used", "based", "data", "analysis", "model", "models"
  )
))

is_valid_token <- function(x, min_chars = 3L) {
  x <- tolower(trimws(as.character(x)))
  ok <- !is.na(x) &
    nzchar(x) &
    str_detect(x, "[[:alpha:]]") &
    !str_detect(x, "^[0-9]+$") &
    !x %in% default_stopwords &
    (nchar(x) >= min_chars | x %in% c("r", "ai", "nlp", "co"))
  ok
}

is_noise_phrase <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[is.na(x)] <- ""
  has_alpha <- str_detect(x, "[[:alpha:]]")
  has_bad <- str_detect(
    x,
    "\\b(doi|https?|www|org|com|net|html|pdf|cran|package|softwarex|license|copyright|received|accepted|available|references?)\\b"
  )
  too_numeric <- vapply(strsplit(x, "[[:space:]]+"), function(v) {
    v <- v[nzchar(v)]
    if (!length(v)) return(TRUE)
    mean(str_detect(v, "^[0-9_./:-]+$")) >= 0.5
  }, logical(1))
  !has_alpha | has_bad | too_numeric
}

clean_term <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[‐‑–—_/.:;]+", " ", x, perl = TRUE)
  x <- gsub("[^[:alnum:] -]+", " ", x, perl = TRUE)
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  trimws(x)
}

strip_author_metadata_and_references_app <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("\r\n?", "\n", x, perl = TRUE)
  x <- gsub("(?is)(^|\n)[[:space:]]*(references|bibliography)[[:space:]]*[:：]?[[:space:]]*\n.*$", " ", x, perl = TRUE)

  # Drop typical article metadata lines, but keep the actual keyword block because
  # author keywords are extracted from the raw text before this cleanup step.
  lines <- unlist(strsplit(x, "\n", fixed = FALSE), use.names = FALSE)
  lines <- trimws(lines)
  if (!length(lines)) return("")
  bad_line <- grepl(
    paste0(
      "^(contents lists available|journal homepage|metadata|current code version|permanent link|legal code license|",
      "code versioning system|software code languages|compilation requirements|if available|support email|",
      "corresponding author|email addresses?|received |available online|copyright|open access|",
      "declaration of competing interest|credit authorship contribution statement|acknowledgement|appendix|",
      "supplementary data|data availability)"
    ),
    tolower(lines), perl = TRUE
  )
  bad_line <- bad_line |
    grepl("https?://|www[.]|\\bdoi\\b|\\b10[.][0-9]{4,9}/", lines, ignore.case = TRUE, perl = TRUE) |
    grepl("^[*∗]?[[:space:]]*corresponding author", lines, ignore.case = TRUE, perl = TRUE)
  lines <- lines[!bad_line]
  x <- paste(lines, collapse = " ")
  x <- gsub("https?://[^[:space:]]+", " ", x, perl = TRUE)
  x <- gsub("www[.][^[:space:]]+", " ", x, perl = TRUE)
  x <- gsub("\\bdoi\\b[[:space:]:]*[^[:space:]]+", " ", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\b10[.][0-9]{4,9}/[^[:space:]]+", " ", x, perl = TRUE)
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  trimws(x)
}

remove_reference_noise <- function(docs_tbl) {
  docs_tbl |>
    mutate(text = vapply(as.character(text), strip_author_metadata_and_references_app, character(1))) |>
    filter(!is.na(text), nzchar(trimws(text)))
}

# ---- ChatGPT-like exact phrase helpers ----
# These helpers keep the app deterministic, but make the output closer to
# the ChatGPT-style result requested in the prompt: preserve author keywords,
# prefer meaningful exact 2--4 word phrases, and score before Top-N selection.
normalize_exact_phrase <- function(x) {
  x <- tolower(as.character(x %||% ""))
  x <- gsub("[‐‑–—]", "-", x, perl = TRUE)
  x <- gsub("[_/]", " ", x, perl = TRUE)
  x <- gsub("[^[:alnum:] -]+", " ", x, perl = TRUE)
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  trimws(x)
}

phrase_in_text <- function(phrase, text) {
  p <- normalize_exact_phrase(phrase)
  if (!nzchar(p)) return(FALSE)
  x <- normalize_exact_phrase(text)
  pat <- paste0("(^|[[:space:]])", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", p), "($|[[:space:]])")
  grepl(pat, x, perl = TRUE)
}

extract_author_keywords_from_docs <- function(docs_tbl) {
  if (!is.data.frame(docs_tbl) || !"text" %in% names(docs_tbl)) return(character(0))

  raw_txt <- paste(as.character(docs_tbl$text %||% ""), collapse = "\n")
  raw_txt <- gsub("\r\n?", "\n", raw_txt, perl = TRUE)
  raw_txt <- gsub("[\t\f\v]+", " ", raw_txt, perl = TRUE)

  .norm_line <- function(z) {
    z <- trimws(as.character(z %||% ""))
    z <- gsub("[[:space:]]+", " ", z, perl = TRUE)
    z
  }

  .is_kw_heading_line <- function(z) {
    z <- .norm_line(z)
    if (!nzchar(z)) return(FALSE)
    z0 <- tolower(z)
    # STRICT: match real keyword headings only, not a phrase such as
    # "automatic keyword extraction from individual documents" in references.
    # Allow two-column PDF prefix such as "A R T I C L E I N F O Keywords:".
    heading_pat <- "(^|\\barticle[[:space:]]+info\\b.*|a[[:space:]]*r[[:space:]]*t[[:space:]]*i[[:space:]]*c[[:space:]]*l[[:space:]]*e[[:space:]]*i[[:space:]]*n[[:space:]]*f[[:space:]]*o.*)(author[[:space:]]+keywords?|key[[:space:]]*words?|keywords?|index[[:space:]]+terms?)\\s*[:：-]?\\s*$"
    inline_pat  <- "\\b(author[[:space:]]+keywords?|key[[:space:]]*words?|keywords?|index[[:space:]]+terms?)\\s*[:：]\\s*\\S+"
    grepl(heading_pat, z0, perl = TRUE) || grepl(inline_pat, z0, perl = TRUE)
  }

  .starts_stop_heading <- function(z) {
    z0 <- tolower(.norm_line(z))
    z0 <- gsub("^[[:punct:][:space:]]+", "", z0, perl = TRUE)
    compact <- gsub("[[:space:][:punct:]]+", "", z0, perl = TRUE)
    grepl("^(abstract|metadata|highlights|introduction|background|methods?|materials|results|discussion|conclusions?|references|acknowledg|data[[:space:]]+availability)\\b", z0, perl = TRUE) ||
      grepl("^(a[[:space:]]*b[[:space:]]*s[[:space:]]*t[[:space:]]*r[[:space:]]*a[[:space:]]*c[[:space:]]*t)\\b", z0, perl = TRUE) ||
      grepl("^(current[[:space:]]+code[[:space:]]+version|permanent[[:space:]]+link|received[[:space:]]|softwarex[[:space:]])", z0, perl = TRUE) ||
      compact %in% c("abstract", "metadata", "articleinfo", "highlights")
  }

  .remove_after_stop <- function(z) {
    z <- as.character(z)
    z <- sub("(?is)[[:space:]]+(A[[:space:]]*B[[:space:]]*S[[:space:]]*T[[:space:]]*R[[:space:]]*A[[:space:]]*C[[:space:]]*T|Abstract|Metadata|Highlights|Current[[:space:]]+code[[:space:]]+version|Received)\\b.*$", "", z, perl = TRUE)
    trimws(z)
  }

  .after_kw_heading <- function(z) {
    z <- .norm_line(z)
    # Only remove real heading syntax. Do not remove arbitrary occurrences of
    # keyword/keywords in ordinary sentences or references.
    out <- sub("(?is)^.*?\\b(author[[:space:]]+keywords?|key[[:space:]]*words?|keywords?|index[[:space:]]+terms?)\\b[[:space:]]*[:：-][[:space:]]*", "", z, perl = TRUE)
    out <- .remove_after_stop(out)
    if (identical(trimws(out), trimws(z))) "" else out
  }

  .split_kw_bits <- function(x, max_words = 8L) {
    x <- gsub("[•·]", ";", x, perl = TRUE)
    x <- gsub("\u00a0", " ", x, fixed = TRUE)
    x <- .remove_after_stop(x)
    bits <- unlist(strsplit(paste(x, collapse = "\n"), "\\s*(?:\\n|;|,|\\||。|；|、)\\s*", perl = TRUE), use.names = FALSE)
    bits <- normalize_exact_phrase(bits)
    bits <- bits[nzchar(bits)]
    bits <- bits[!grepl("^(a b s t r a c t|abstract|article info|metadata|highlights|keywords?|key words?|author keywords?|index terms?)$", bits, perl = TRUE)]
    bits <- bits[phrase_word_count(bits) <= max_words]
    bits <- bits[!is_noise_phrase(bits)]
    bits <- bits[!bits %in% default_stopwords]
    unique(bits)
  }

  lines <- unlist(strsplit(raw_txt, "\n", fixed = TRUE), use.names = FALSE)
  lines <- .norm_line(lines)
  lines <- lines[nzchar(lines)]

  # Author keywords are normally before Abstract.  Restrict scanning to the
  # pre-abstract header area to avoid capturing reference titles that contain
  # the word "keyword".
  abs_idx <- which(vapply(lines, .starts_stop_heading, logical(1)))
  first_abs <- if (length(abs_idx)) abs_idx[[1]] else min(length(lines), 120L)
  scan_end <- min(length(lines), max(first_abs - 1L, 1L), 120L)
  scan_lines <- if (length(lines)) lines[seq_len(scan_end)] else character(0)

  captured <- character(0)
  if (length(scan_lines)) {
    for (i in seq_along(scan_lines)) {
      li <- scan_lines[[i]]
      if (!.is_kw_heading_line(li)) next

      after <- .after_kw_heading(li)
      tmp <- character(0)
      if (nzchar(after)) tmp <- c(tmp, after)

      # If heading alone, collect only short keyword rows until Abstract/Metadata.
      if (i < length(scan_lines)) {
        for (j in seq.int(i + 1L, min(length(scan_lines), i + 12L))) {
          lj <- .norm_line(scan_lines[[j]])
          if (!nzchar(lj)) next
          if (.starts_stop_heading(lj)) break
          if (grepl("^(contents lists available|softwarex|journal homepage|article info|a r t i c l e i n f o)$", tolower(lj), perl = TRUE)) next
          # Keyword rows are short labels or delimiter-separated inline terms.
          # Long prose is abstract/body, not author keyword.
          wc <- phrase_word_count(lj)
          if (nchar(lj) > 90 && !grepl("[,;；、|]", lj, perl = TRUE)) break
          if (wc > 10L && !grepl("[,;；、|]", lj, perl = TRUE)) break
          tmp <- c(tmp, .remove_after_stop(lj))
        }
      }
      if (length(tmp)) captured <- c(captured, tmp)
    }
  }

  # Fallback for text collapsed into one line, but still stop before Abstract.
  if (!length(captured)) {
    txt <- gsub("[[:space:]]+", " ", raw_txt, perl = TRUE)
    pat <- "(?is)\\b(?:author[[:space:]]+keywords?|key[[:space:]]*words?|keywords?|index[[:space:]]+terms?)\\b[[:space:]]*[:：-][[:space:]]*(.{0,450}?)(?=\\b(?:A[[:space:]]*B[[:space:]]*S[[:space:]]*T[[:space:]]*R[[:space:]]*A[[:space:]]*C[[:space:]]*T|abstract|metadata|highlights|current[[:space:]]+code[[:space:]]+version|received|softwarex|1[.]?[[:space:]]+motivation)\\b|$)"
    m <- stringr::str_match_all(txt, stringr::regex(pat, ignore_case = TRUE))[[1]]
    if (!is.null(m) && nrow(m)) captured <- m[, 2]
  }

  if (!length(captured)) return(character(0))
  .split_kw_bits(captured, max_words = 8L)
}

inject_author_keywords <- function(docs_tbl, doc_terms, author_keywords) {
  author_keywords <- normalize_author_keywords_unique_app(author_keywords)
  author_keywords <- unique(author_keywords[nzchar(author_keywords)])
  if (!length(author_keywords)) return(doc_terms)

  if (!"author_keyword" %in% names(doc_terms)) doc_terms$author_keyword <- FALSE
  rows <- list()
  k <- 0L
  doc_ids <- as.character(docs_tbl$doc_id %||% seq_len(nrow(docs_tbl)))
  for (kw in author_keywords) {
    hit <- vapply(as.character(docs_tbl$text %||% ""), function(z) phrase_in_text(kw, z), logical(1))
    use_ids <- doc_ids[hit]
    if (!length(use_ids)) use_ids <- doc_ids[seq_len(min(1L, length(doc_ids)))]
    for (did in use_ids) {
      k <- k + 1L
      rows[[k]] <- tibble(
        doc_id = as.character(did),
        term = kw,
        source_type = "author_keyword",
        exact_surface_phrase = TRUE,
        author_keyword = TRUE
      )
    }
  }
  if (!length(rows)) return(doc_terms)
  bind_rows(doc_terms, bind_rows(rows)) |>
    mutate(author_keyword = ifelse(is.na(author_keyword), FALSE, author_keyword)) |>
    distinct(doc_id, term, source_type, exact_surface_phrase, author_keyword)
}

log_author_keywords_debug <- function(author_keywords, docs_tbl_raw = NULL, docs_tbl_used = NULL, protect_author_keywords = TRUE) {
  ak <- normalize_exact_phrase(author_keywords)
  ak <- unique(ak[nzchar(ak)])
  message("
================ AUTHOR KEYWORDS DEBUG ================")
  message("[author-keyword protection active] ", isTRUE(protect_author_keywords))
  message("[detected n] ", length(ak))
  if (length(ak)) {
    message("[detected list] ", paste(ak, collapse = " | "))
  } else {
    message("[detected list] <empty>")
  }
  if (is.data.frame(docs_tbl_raw) && "text" %in% names(docs_tbl_raw) && nrow(docs_tbl_raw) > 0) {
    raw_txt <- paste(as.character(docs_tbl_raw$text[seq_len(min(2L, nrow(docs_tbl_raw)))]), collapse = "
---PAGE/ROW BREAK---
")
    raw_lines <- unlist(strsplit(raw_txt, "
", fixed = TRUE), use.names = FALSE)
    raw_lines <- trimws(raw_lines)
    raw_lines <- raw_lines[nzchar(raw_lines)]
    hit <- grep("\\b(author[[:space:]]+keywords?|key[[:space:]]*words?|keywords?|index[[:space:]]+terms?)\\s*[:：]", tolower(raw_lines), perl = TRUE)
    message("[raw keyword heading line index] ", if (length(hit)) paste(hit, collapse = ", ") else "<none in first 2 text units>")
    if (length(hit)) {
      i <- hit[1]
      j2 <- min(length(raw_lines), i + 10L)
      message("[raw keyword block preview]")
      message(paste(sprintf("  %02d: %s", i:j2, raw_lines[i:j2]), collapse = "
"))
    } else {
      message("[raw first lines preview]")
      message(paste(sprintf("  %02d: %s", seq_len(min(12L, length(raw_lines))), raw_lines[seq_len(min(12L, length(raw_lines)))]), collapse = "
"))
    }
  }
  message("=======================================================
")
  invisible(ak)
}

is_weak_chatgpt_fragment <- function(term) {
  x <- normalize_exact_phrase(term)
  if (!nzchar(x)) return(TRUE)
  w <- unlist(strsplit(x, "[[:space:]]+", perl = TRUE), use.names = FALSE)
  w <- w[nzchar(w)]
  if (!length(w)) return(TRUE)

  bad_any <- c(
    "algorithm applied adjacency", "detection algorithm applied adjacency",
    "red network module", "green network module", "blue network module",
    "providing context aware natural", "statistics and lexical",
    "network module highlights", "loading the corpus standard",
    "contents lists available", "journal homepage", "science direct"
  )
  if (any(vapply(bad_any, function(b) grepl(b, x, fixed = TRUE), logical(1)))) return(TRUE)

  bad_start_end <- c(
    "applied", "applying", "providing", "altering", "without", "with", "through",
    "across", "into", "from", "that", "which", "this", "these", "those", "using",
    "used", "based", "including", "demonstrating", "enabling", "allowing",
    "ensuring", "requiring", "requires", "supporting", "support", "supports",
    "contains", "contain", "includes", "include", "shown", "showing", "red", "green",
    "blue", "purple", "orange", "standard", "current", "appendix", "supplementary"
  )
  if (w[1] %in% bad_start_end || w[length(w)] %in% bad_start_end) return(TRUE)
  if (any(w %in% c("doi", "http", "https", "www", "copyright", "license"))) return(TRUE)
  FALSE
}

compute_silhouette_table <- function(selected, co_edges) {
  if (!nrow(selected)) {
    return(tibble(term = character(), topic = integer(), leader = character(), is_leader = logical(), value = numeric(), ss = numeric()))
  }
  out <- selected |>
    transmute(
      term = as.character(term),
      topic = as.integer(topic),
      leader = as.character(leader),
      is_leader = as.logical(is_leader),
      value = as.numeric(score),
      ss = 0
    )

  if (nrow(out) < 3 || length(unique(out$topic)) < 2 || !requireNamespace("cluster", quietly = TRUE)) {
    return(out)
  }

  terms <- out$term
  n <- length(terms)
  W <- matrix(0, nrow = n, ncol = n, dimnames = list(terms, terms))
  if (is.data.frame(co_edges) && nrow(co_edges)) {
    for (i in seq_len(nrow(co_edges))) {
      a <- as.character(co_edges$term1[[i]])
      b <- as.character(co_edges$term2[[i]])
      w <- suppressWarnings(as.numeric(co_edges$WCD[[i]]))
      if (a %in% terms && b %in% terms && is.finite(w)) {
        W[a, b] <- w
        W[b, a] <- w
      }
    }
  }
  max_w <- max(W, na.rm = TRUE)
  if (!is.finite(max_w) || max_w <= 0) return(out)
  D <- 1 - (W / max_w)
  diag(D) <- 0
  ss_obj <- tryCatch(cluster::silhouette(out$topic, stats::as.dist(D)), error = function(e) NULL)
  if (is.null(ss_obj)) return(out)
  ss_df <- as.data.frame(ss_obj)
  out$ss <- as.numeric(ss_df$sil_width)
  out
}

compute_cluster_summary <- function(selected, co_edges, sil_df) {
  if (!nrow(selected)) return(tibble(topic = integer(), n_terms = integer(), leader = character(), cluster_ss = numeric(), modularity_Q = numeric(), leader_aac = numeric()))
  mod_q <- NA_real_
  if (is.data.frame(co_edges) && nrow(co_edges) && nrow(selected) >= 2) {
    g <- tryCatch(igraph::graph_from_data_frame(co_edges |> transmute(from = term1, to = term2, weight = WCD), directed = FALSE, vertices = data.frame(name = selected$term)), error = function(e) NULL)
    if (!is.null(g) && igraph::ecount(g) > 0 && length(unique(selected$topic)) > 1) {
      memb <- selected$topic[match(igraph::V(g)$name, selected$term)]
      mod_q <- tryCatch(igraph::modularity(g, memb, weights = igraph::E(g)$weight), error = function(e) NA_real_)
    }
  }

  leader_aac_df <- selected |>
    mutate(score_num = suppressWarnings(as.numeric(score))) |>
    group_by(topic) |>
    arrange(desc(score_num), term, .by_group = TRUE) |>
    summarise(
      n_terms = n(),
      leader = first(term),
      s1 = first(score_num),
      s2 = dplyr::nth(score_num, 2, default = NA_real_),
      s3 = dplyr::nth(score_num, 3, default = NA_real_),
      leader_aac = dplyr::case_when(
        n_terms <= 0 ~ NA_real_,
        n_terms == 1 ~ 1,
        n_terms == 2 ~ 0.5,
        is.finite(s1) && is.finite(s2) && is.finite(s3) && s2 > 0 && s3 > 0 ~ {
          r <- (s1 / s2) / (s2 / s3)
          r / (1 + r)
        },
        is.finite(s1) && is.finite(s2) && s2 > 0 ~ {
          r <- (s1 / s2)
          r / (1 + r)
        },
        TRUE ~ NA_real_
      ),
      .groups = "drop"
    )

  selected |>
    group_by(topic) |>
    summarise(
      n_terms = n(),
      leader = first(term[is_leader %in% TRUE] %||% first(term)),
      .groups = "drop"
    ) |>
    left_join(sil_df |> group_by(topic) |> summarise(cluster_ss = mean(ss, na.rm = TRUE), .groups = "drop"), by = "topic") |>
    left_join(leader_aac_df |> select(topic, leader_aac), by = "topic") |>
    mutate(modularity_Q = mod_q)
}


compute_aac_dashboard <- function(selected, cluster_summary = NULL) {
  if (is.null(selected) || !is.data.frame(selected) || !nrow(selected)) {
    return(tibble(topic = integer(), leader = character(), n_terms = integer(), score1 = numeric(), score2 = numeric(), score3 = numeric(), r = numeric(), AAC = numeric(), cluster_ss = numeric()))
  }

  out <- selected |>
    mutate(score_num = suppressWarnings(as.numeric(score))) |>
    group_by(topic) |>
    arrange(desc(score_num), term, .by_group = TRUE) |>
    summarise(
      n_terms = n(),
      leader = first(term),
      score1 = first(score_num),
      score2 = dplyr::nth(score_num, 2, default = NA_real_),
      score3 = dplyr::nth(score_num, 3, default = NA_real_),
      r = dplyr::case_when(
        n_terms >= 3 & is.finite(score1) & is.finite(score2) & is.finite(score3) & score2 > 0 & score3 > 0 ~ (score1 / score2) / (score2 / score3),
        n_terms == 2 ~ 1,
        is.finite(score1) & is.finite(score2) & score2 > 0 ~ score1 / score2,
        TRUE ~ NA_real_
      ),
      AAC = dplyr::case_when(
        n_terms == 1 ~ 1,
        n_terms == 2 ~ 0.5,
        is.finite(r) ~ r / (1 + r),
        TRUE ~ NA_real_
      ),
      .groups = "drop"
    )

  if (!is.null(cluster_summary) && is.data.frame(cluster_summary) && "cluster_ss" %in% names(cluster_summary)) {
    out <- out |> left_join(cluster_summary |> select(topic, cluster_ss), by = "topic")
  } else {
    out$cluster_ss <- NA_real_
  }
  out |> arrange(topic)
}

make_kano_data <- function(selected, edges) {
  if (is.null(selected) || !nrow(selected)) {
    return(tibble(name = character(), value = numeric(), topic = integer(), leader = logical(), outW = numeric(), inW = numeric(), performance = numeric()))
  }
  nd <- selected |>
    transmute(name = as.character(term), value = as.numeric(score), topic = as.integer(topic), leader = as.logical(is_leader))
  e <- edges |>
    transmute(Leader = as.character(term1), follower = as.character(term2), WCD = as.numeric(WCD))
  outw <- e |> group_by(Leader) |> summarise(outW = sum(WCD, na.rm = TRUE), .groups = "drop") |> rename(name = Leader)
  inw <- e |> group_by(follower) |> summarise(inW = sum(WCD, na.rm = TRUE), .groups = "drop") |> rename(name = follower)
  nd |>
    left_join(outw, by = "name") |>
    left_join(inw, by = "name") |>
    mutate(
      outW = ifelse(is.na(outW), 0, outW),
      inW = ifelse(is.na(inW), 0, inW),
      performance = outW - inW
    )
}

make_kano_shapes <- function(df, x_center = NULL, y_center = NULL) {
  if (is.null(x_center)) x_center <- mean(df$value, na.rm = TRUE)
  if (is.null(y_center)) y_center <- mean(df$performance, na.rm = TRUE)

  max_x <- max(df$value, na.rm = TRUE)
  min_x <- min(df$value, na.rm = TRUE)
  max_y <- max(df$performance, na.rm = TRUE)
  min_y <- min(df$performance, na.rm = TRUE)
  dx <- max_x - min_x; if (!is.finite(dx) || dx <= 0) dx <- 1
  dy <- max_y - min_y; if (!is.finite(dy) || dy <= 0) dy <- 1

  t <- seq(0, 1, length.out = 400)
  spread_x <- dx * 1.10
  spread_y <- max(dy * 0.95, 0.8)

  lower_curve <- data.frame(
    x = t * spread_x - spread_x/2 + x_center,
    y = y_center - spread_y * (1 - t)^2
  )
  upper_curve <- data.frame(
    x = -t * spread_x + spread_x/2 + x_center,
    y = y_center + spread_y * (1 - t)^2
  )
  wing_poly <- rbind(upper_curve, lower_curve[rev(seq_len(nrow(lower_curve))), ])

  visual_ratio <- 1/1.5
  wing_dense <- rbind(lower_curve, upper_curve)
  dxv <- wing_dense$x - x_center
  dyv <- (wing_dense$y - y_center) * visual_ratio
  wing_outer_radius <- suppressWarnings(max(sqrt(dxv^2 + dyv^2), na.rm = TRUE))
  if (!is.finite(wing_outer_radius) || wing_outer_radius <= 0) wing_outer_radius <- max(dx, dy)

  theta <- seq(0, 2*pi, length.out = 600)
  circle_radius <- wing_outer_radius * 0.55
  circle_data <- data.frame(
    x = x_center + circle_radius * cos(theta),
    y = y_center + (circle_radius * sin(theta)) / visual_ratio
  )

  hub_r <- max(wing_outer_radius * 0.12, min(wing_outer_radius * 0.30, spread_y * visual_ratio * 0.35))
  theta_h <- seq(0, 2*pi, length.out = 360)
  hub_circle <- data.frame(
    x = x_center + hub_r * cos(theta_h),
    y = y_center + (hub_r * sin(theta_h)) / (visual_ratio*1.25)
  )
  hub_r2 <- min(hub_r * 2, wing_outer_radius * 0.92)
  hub_circle2 <- data.frame(
    x = x_center + hub_r2 * cos(theta_h),
    y = y_center + (hub_r2 * sin(theta_h)) / (visual_ratio*1.25)
  )

  list(
    x_center = x_center,
    y_center = y_center,
    wing_poly = wing_poly,
    lower_curve = lower_curve,
    upper_curve = upper_curve,
    circle_data = circle_data,
    hub_circle = hub_circle,
    hub_circle2 = hub_circle2,
    x_limits = c(min_x - dx * 0.55, max_x + dx * 0.55),
    y_limits = c(min_y - dy * 0.70, max_y + dy * 0.95)
  )
}

make_sankeymatic_code_app <- function(selected, edges, top_n_links = 200) {
  # SankeyMATIC-ready text, following sankeyplot-style output:
  #   Leader [WCD] follower #000000
  #   : node_name #RRGGBB
  # No separate node table is required; node colors are defined by ':' lines.
  if (is.null(edges) || !is.data.frame(edges) || !nrow(edges)) return("# No leader-follower edges available yet.")

  selected <- as.data.frame(selected, stringsAsFactors = FALSE)
  edges <- as.data.frame(edges, stringsAsFactors = FALSE)

  edge_names <- names(edges)
  if (all(c("term1", "term2", "WCD") %in% edge_names)) {
    e <- edges |> transmute(Leader = as.character(term1), follower = as.character(term2), WCD = suppressWarnings(as.numeric(WCD)))
  } else if (all(c("Leader", "follower", "WCD") %in% edge_names)) {
    e <- edges |> transmute(Leader = as.character(Leader), follower = as.character(follower), WCD = suppressWarnings(as.numeric(WCD)))
  } else if (all(c("Leader", "Follower", "WCD") %in% edge_names)) {
    e <- edges |> transmute(Leader = as.character(Leader), follower = as.character(Follower), WCD = suppressWarnings(as.numeric(WCD)))
  } else {
    e <- edges[, seq_len(min(3, ncol(edges))), drop = FALSE]
    names(e)[1:3] <- c("Leader", "follower", "WCD")
    e <- e |> transmute(Leader = as.character(Leader), follower = as.character(follower), WCD = suppressWarnings(as.numeric(WCD)))
  }

  e <- e |>
    filter(!is.na(Leader), !is.na(follower), nzchar(Leader), nzchar(follower), Leader != follower, is.finite(WCD), WCD > 0) |>
    arrange(desc(WCD), Leader, follower) |>
    slice_head(n = top_n_links)

  if (!nrow(e)) return("# No valid SankeyMATIC links after filtering.")

  # Determine node cluster/group for color lines. Use selected$topic first, then carac/cluster if present.
  node_names <- unique(c(e$Leader, e$follower))
  if (!nrow(selected)) {
    nd <- data.frame(name = node_names, group = "1", stringsAsFactors = FALSE)
  } else {
    nm_col <- if ("term" %in% names(selected)) "term" else if ("name" %in% names(selected)) "name" else names(selected)[1]
    grp_col <- if ("topic" %in% names(selected)) "topic" else if ("carac" %in% names(selected)) "carac" else if ("cluster" %in% names(selected)) "cluster" else NA_character_
    nd <- selected |>
      transmute(
        name = as.character(.data[[nm_col]]),
        group = if (!is.na(grp_col)) as.character(.data[[grp_col]]) else "1"
      ) |>
      filter(!is.na(name), nzchar(name)) |>
      distinct(name, .keep_all = TRUE)
    missing_nodes <- setdiff(node_names, nd$name)
    if (length(missing_nodes)) {
      nd <- bind_rows(nd, data.frame(name = missing_nodes, group = "1", stringsAsFactors = FALSE))
    }
    nd <- nd |> filter(name %in% node_names)
  }

  specified_colors <- c(
    "#FF0000", "#0000FF", "#998000", "#008000", "#800080", "#FFC0CB",
    "#000000", "#ADD8E6", "#FF4500", "#A52A2A", "#8B4513", "#FF8C00",
    "#32CD32", "#4682B4", "#9400D3", "#FFD700", "#C0C0C0", "#DC143C",
    "#1E90FF"
  )
  groups <- unique(as.character(nd$group))
  groups <- groups[!is.na(groups) & nzchar(groups)]
  if (!length(groups)) groups <- "1"
  group_colors <- setNames(rep(specified_colors, length.out = length(groups)), groups)
  nd$color <- unname(group_colors[as.character(nd$group)])
  nd$color[is.na(nd$color)] <- "#808080"

  # Flow lines. SankeyMATIC accepts an optional color at the end of each link line.
  # Keep links black, as in the user's sankeyplot reference.
  w <- e$WCD
  w[!is.finite(w)] <- 1
  flow_lines <- sprintf("%s [%s] %s #000000", e$Leader, signif(w, 3), e$follower)

  # Node color definitions. These are not a separate node table; they are SankeyMATIC ':' lines.
  nd <- nd |> arrange(match(name, node_names)) |> distinct(name, .keep_all = TRUE)
  color_lines <- sprintf(": %s %s", nd$name, nd$color)

  paste(c(flow_lines, "", "# Node colors by FLCA cluster", color_lines), collapse = "\n")
}

make_rowwise_semantic_csv <- function(analysis_obj, top_per_row = 20L) {
  if (is.null(analysis_obj) || is.null(analysis_obj$ranked) || !is.data.frame(analysis_obj$ranked)) {
    return(tibble(doc_id = character(), n_terms = integer(), semantic_phrases = character()))
  }
  ranked <- analysis_obj$ranked
  if (!nrow(ranked) || !all(c("doc_id", "term") %in% names(ranked))) {
    return(tibble(doc_id = character(), n_terms = integer(), semantic_phrases = character()))
  }

  ranked <- ranked |>
    mutate(
      doc_id = as.character(doc_id),
      term = as.character(term),
      score_for_row = dplyr::case_when(
        "tf_idf" %in% names(ranked) ~ suppressWarnings(as.numeric(tf_idf)),
        "score" %in% names(ranked) ~ suppressWarnings(as.numeric(score)),
        TRUE ~ 0
      )
    ) |>
    filter(!is.na(doc_id), nzchar(doc_id), !is.na(term), nzchar(term)) |>
    group_by(doc_id, term) |>
    summarise(score_for_row = max(score_for_row, na.rm = TRUE), .groups = "drop") |>
    mutate(score_for_row = ifelse(is.finite(score_for_row), score_for_row, 0)) |>
    arrange(doc_id, desc(score_for_row), term)

  if (!nrow(ranked)) {
    return(tibble(doc_id = character(), n_terms = integer(), semantic_phrases = character()))
  }

  top_tbl <- ranked |>
    group_by(doc_id) |>
    slice_head(n = as.integer(top_per_row %||% 20L)) |>
    mutate(rank = row_number()) |>
    ungroup()

  wide_terms <- top_tbl |>
    select(doc_id, rank, term) |>
    mutate(rank = paste0("term_", rank)) |>
    tidyr::pivot_wider(names_from = rank, values_from = term, values_fill = "")

  summary_tbl <- top_tbl |>
    group_by(doc_id) |>
    summarise(
      n_terms = n(),
      semantic_phrases = paste(term, collapse = "; "),
      .groups = "drop"
    )

  docs <- analysis_obj$docs
  if (!is.null(docs) && is.data.frame(docs) && all(c("doc_id", "text") %in% names(docs))) {
    doc_info <- docs |>
      transmute(doc_id = as.character(doc_id), text = as.character(text)) |>
      distinct(doc_id, .keep_all = TRUE)
    summary_tbl <- summary_tbl |>
      left_join(doc_info, by = "doc_id") |>
      relocate(text, .after = doc_id)
  }

  summary_tbl |>
    left_join(wide_terms, by = "doc_id") |>
    arrange(suppressWarnings(as.numeric(doc_id)), doc_id)
}

make_extraction_log <- function(author_keywords, extract_mode, scoring_formula, n_candidates, n_selected) {
  tibble(
    item = c("extraction_mode", "author_keywords", "scoring_formula", "candidate_terms", "selected_terms"),
    value = c(
      as.character(extract_mode %||% "semantic"),
      paste(author_keywords, collapse = "; "),
      scoring_formula,
      as.character(n_candidates),
      as.character(n_selected)
    )
  )
}

prep_docs <- function(df, text_col = NULL, doc_col = NULL) {
  if (!is.null(text_col) && nzchar(text_col) && text_col %in% names(df)) {
    out <- df |>
      mutate(
        doc_id = if (!is.null(doc_col) && nzchar(doc_col) && doc_col %in% names(df)) as.character(.data[[doc_col]]) else as.character(seq_len(n())),
        text = as.character(.data[[text_col]])
      ) |>
      filter(!is.na(text), str_trim(text) != "") |>
      select(doc_id, text)
  } else {
    if (!all(c("doc_id", "text") %in% names(df))) stop("Input must contain a text column, or already have doc_id and text.")
    out <- df |>
      mutate(
        doc_id = as.character(doc_id),
        text = as.character(text)
      ) |>
      filter(!is.na(text), str_trim(text) != "") |>
      select(doc_id, text)
  }
  out
}

combine_docs_for_semantic_mode <- function(docs_tbl) {
  if (is.null(docs_tbl) || !nrow(docs_tbl)) return(tibble(doc_id = character(), text = character()))
  txt <- paste(docs_tbl$text, collapse = " ")
  tibble(doc_id = "1", text = txt)
}

extract_single_terms <- function(docs_tbl, min_chars = 3L) {
  docs_tbl |>
    tidytext::unnest_tokens(term, text) |>
    mutate(term = clean_term(term), source_type = "single", exact_surface_phrase = TRUE) |>
    filter(is_valid_token(term, min_chars = min_chars)) |>
    distinct(doc_id, term, source_type, exact_surface_phrase)
}

extract_ngrams <- function(docs_tbl, n_values = c(2L, 3L), min_chars = 3L) {
  out <- lapply(n_values, function(nv) {
    docs_tbl |>
      tidytext::unnest_tokens(term, text, token = "ngrams", n = nv) |>
      mutate(term = clean_term(term)) |>
      filter(term != "") |>
      separate(term, into = paste0("w", seq_len(nv)), sep = " ", remove = FALSE, fill = "right") |>
      filter(if_all(starts_with("w"), ~ !is.na(.x) & .x != "")) |>
      filter(if_all(starts_with("w"), ~ is_valid_token(.x, min_chars = min_chars))) |>
      rowwise() |>
      mutate(
        words = list(c_across(starts_with("w"))),
        stop_n = sum(unlist(words) %in% default_stopwords),
        alpha_n = sum(str_detect(unlist(words), "[[:alpha:]]")),
        numeric_n = sum(str_detect(unlist(words), "^[0-9]+$"))
      ) |>
      ungroup() |>
      filter(
        stop_n == 0,
        alpha_n == nv,
        numeric_n == 0,
        !is_noise_phrase(term)
      ) |>
      select(doc_id, term) |>
      distinct()
  })
  bind_rows(out) |>
    mutate(term = clean_term(term), source_type = "ngram", exact_surface_phrase = TRUE) |>
    filter(term != "", !is_noise_phrase(term)) |>
    distinct(doc_id, term, source_type, exact_surface_phrase)
}

extract_named_entity_phrases <- function(docs_tbl, max_words = 4L) {
  pat <- "\\b(?:[A-Z][a-z]+|[A-Z]{2,}|[A-Z][a-z]+[-][A-Z][a-z]+)(?:[[:space:]]+(?:[A-Z][a-z]+|[A-Z]{2,}|[A-Z][a-z]+[-][A-Z][a-z]+)){1,3}\\b"

  raw <- docs_tbl |>
    rowwise() |>
    mutate(matches = list(stringr::str_extract_all(text, stringr::regex(pat, multiline = TRUE))[[1]])) |>
    ungroup() |>
    select(doc_id, matches) |>
    tidyr::unnest(matches)

  if (!"matches" %in% names(raw) || nrow(raw) == 0) {
    return(tibble(doc_id = character(), term = character(), source_type = character(), exact_surface_phrase = logical()))
  }

  raw |>
    transmute(
      doc_id = as.character(doc_id),
      term = clean_term(matches),
      source_type = "named_entity",
      exact_surface_phrase = TRUE
    ) |>
    filter(term != "", !is_noise_phrase(term)) |>
    mutate(n_words = stringr::str_count(term, "[[:space:]]+") + 1L) |>
    filter(n_words >= 2L, n_words <= max_words) |>
    filter(!term %in% c("introduction", "materials methods", "results discussion")) |>
    distinct(doc_id, term, source_type, exact_surface_phrase)
}


phrase_word_count <- function(x) {
  x <- trimws(as.character(x))
  ifelse(nzchar(x), stringr::str_count(x, "[[:space:]]+") + 1L, 0L)
}

word_boundary_contains <- function(big, small) {
  big <- clean_term(big)
  small <- clean_term(small)
  if (!nzchar(big) || !nzchar(small) || identical(big, small)) return(FALSE)
  grepl(
    paste0("(^|[[:space:]])", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", small), "($|[[:space:]])"),
    big,
    perl = TRUE
  )
}

# Near-duplicate phrase guard for final Top-N selection.
# This complements the existing subset rule. Example: "text analysis" and
# "text analytics" are not substring matches, but they share the same stem-like
# core tokens after normalization, so only the stronger one remains.
.keyword_root_token_app <- function(w) {
  w <- tolower(as.character(w %||% ""))
  w <- gsub("[^[:alnum:]]+", "", w, perl = TRUE)
  if (!nzchar(w)) return("")

  # Conservative professional keyword families; do not merge phrases merely
  # because they share a common word such as "data".
  if (w %in% c("analysis", "analyses", "analytic", "analytics", "analyze", "analyzing", "analyser", "analyzer")) return("analy")
  if (w %in% c("model", "models", "modeling", "modelling", "modeled", "modelled")) return("model")
  if (w %in% c("visualization", "visualisation", "visualizations", "visualisations", "visualize", "visualise", "visualizing", "visualising")) return("visualiz")
  if (w %in% c("tokenization", "tokenisation", "tokenize", "tokenise", "tokenizing", "tokenising")) return("tokeniz")
  if (w %in% c("lemmatization", "lemmatisation", "lemmatize", "lemmatise", "lemmatizing", "lemmatising")) return("lemmatiz")
  if (w %in% c("reproducibility", "reproducible", "reproduce", "reproducing")) return("reproduc")

  # Generic suffix cleanup, intentionally mild.
  w <- sub("(izations|isations|ization|isation|zations|zation)$", "iz", w, perl = TRUE)
  w <- sub("(ibilities|ibility)$", "ible", w, perl = TRUE)
  w <- sub("(ics)$", "ic", w, perl = TRUE)
  w <- sub("(ing|ed)$", "", w, perl = TRUE)
  if (nchar(w) > 4) w <- sub("s$", "", w, perl = TRUE)
  w
}

.keyword_root_set_app <- function(term) {
  x <- clean_term(term)
  toks <- unlist(strsplit(x, "[[:space:]]+", perl = TRUE), use.names = FALSE)
  toks <- toks[nzchar(toks)]
  if (!length(toks)) return(character(0))
  roots <- vapply(toks, .keyword_root_token_app, character(1))
  roots <- roots[nzchar(roots)]
  unique(roots)
}

.keyword_near_duplicate_app <- function(a, b, threshold = 0.67) {
  a <- clean_term(a); b <- clean_term(b)
  if (!nzchar(a) || !nzchar(b) || identical(a, b)) return(FALSE)
  if (word_boundary_contains(a, b) || word_boundary_contains(b, a)) return(TRUE)

  ra <- .keyword_root_set_app(a)
  rb <- .keyword_root_set_app(b)
  if (!length(ra) || !length(rb)) return(FALSE)
  inter <- length(intersect(ra, rb))
  union <- length(union(ra, rb))
  if (!union) return(FALSE)
  jaccard <- inter / union
  overlap <- inter / min(length(ra), length(rb))

  # Removes "text analysis" vs "text analytics", but keeps "data visualization"
  # vs "data import" because they only share one generic token.
  (jaccard >= threshold) || (overlap >= 0.80 && inter >= 2L)
}

.keyword_near_duplicate_terms_app <- function(term, kept) {
  if (!length(kept)) return(character(0))
  kept[vapply(kept, function(k) .keyword_near_duplicate_app(term, k), logical(1))]
}

select_subset_free_terms <- function(score_tbl, top_n = 20L, max_overlap = TRUE, forced_terms = character(0), apply_chatgpt_filter = TRUE) {
  if (!nrow(score_tbl)) return(character(0))

  forced_terms <- normalize_author_keywords_unique_app(forced_terms)
  forced_terms <- unique(forced_terms[nzchar(forced_terms)])

  tbl <- score_tbl |>
    mutate(
      n_words = phrase_word_count(term),
      term = clean_term(term),
      is_forced = .data$term %in% forced_terms
    ) |>
    filter(nzchar(term), if (isTRUE(apply_chatgpt_filter)) (!vapply(term, is_weak_chatgpt_fragment, logical(1)) | is_forced) else TRUE) |>
    arrange(desc(is_forced), desc(score), desc(doc_freq), desc(degree), desc(n_words), term)

  kept <- character(0)

  for (tm in tbl$term) {
    if (length(kept) >= top_n && !(tm %in% forced_terms)) break

    tm_forced <- tm %in% forced_terms
    is_sub_of_kept <- any(vapply(kept, function(k) word_boundary_contains(k, tm), logical(1)))
    contains_kept <- kept[vapply(kept, function(k) word_boundary_contains(tm, k), logical(1))]
    near_dup_kept <- .keyword_near_duplicate_terms_app(tm, kept)

    # Author-defined keywords are protected: they are retained even if nested.
    # Non-author candidates are also removed when they are near-duplicates of
    # an already kept stronger phrase, e.g. text analysis vs text analytics.
    if (!tm_forced && (is_sub_of_kept || length(near_dup_kept))) next

    if (length(contains_kept) || length(near_dup_kept)) {
      removable <- unique(c(contains_kept, near_dup_kept))
      removable <- removable[!(removable %in% forced_terms)]
      if (length(removable)) kept <- setdiff(kept, removable)
    }

    kept <- unique(c(kept, tm))
  }

  forced_present <- forced_terms[forced_terms %in% tbl$term]
  kept <- unique(c(forced_present, kept))
  if (length(kept) > top_n) {
    forced_keep <- kept[kept %in% forced_terms]
    other_keep <- kept[!(kept %in% forced_terms)]
    combined <- unique(c(forced_keep, other_keep))
    kept <- combined[seq_len(min(top_n, length(combined)))]
  }
  unique(kept)
}


extract_semantic_phrases <- function(docs_tbl, min_words = 2L, max_words = 4L, min_chars = 3L) {
  # v5.4: exact surface phrase extraction.
  # Phrases are generated as contiguous n-grams from the cleaned sentence.
  # Stopwords are NOT removed before n-gram construction, so the returned
  # phrase is an exact normalized surface phrase from the document.

  head_terms <- c(
    "application", "applications", "interface", "interfaces",
    "visualization", "visualisation", "mining", "processing",
    "learning", "classification", "clustering", "detection",
    "summarization", "summarisation", "embedding", "embeddings",
    "language", "languages", "corpus", "corpora",
    "topic", "topics", "algorithm", "algorithms", "software",
    "workflow", "workflows", "interpretation", "reproducibility",
    "lexicon", "lexicons", "tagging", "lemmatization", "tokenization",
    "sentiment", "polarity", "cooccurrence", "occurrence",
    "network", "networks", "framework", "system", "systems",
    "matrix", "relationships", "models", "model", "assistant",
    "architecture", "modules", "analysis", "methods", "environment",
    "platform", "features", "functionalities", "datasets", "data"
  )

  technical_terms <- c(
    "nlp", "text", "network", "networks", "visualization",
    "visualisation", "mining", "learning", "topic", "topics",
    "modeling", "modelling", "shiny", "semantic", "co", "occurrence",
    "cooccurrence", "embedding", "embeddings", "corpus", "corpora",
    "sentiment", "polarity", "tokenization", "lemmatization",
    "classification", "clustering", "algorithm", "algorithms",
    "language", "processing", "software", "workflow", "tf", "idf",
    "ai", "r", "udpipe", "word2vec", "igraph", "visnetwork"
  )

  bad_start_end <- c(
    "applied", "applying", "providing", "altering", "without", "with",
    "through", "across", "into", "from", "that", "which", "this",
    "these", "those", "using", "used", "based", "including",
    "demonstrating", "enabling", "allowing", "ensuring", "requiring",
    "requires", "supporting", "support", "supports", "contains",
    "contain", "includes", "include", "shown", "showing", "red",
    "green", "blue", "purple", "orange"
  )

  bad_any <- c(
    "fig", "figure", "table", "copyright", "license", "received",
    "accepted", "available online", "email", "corresponding author",
    "science direct", "journal homepage", "contents lists",
    "red cluster", "green cluster", "blue cluster", "purple cluster",
    "orange cluster"
  )

  clean_for_surface <- function(x) {
    x <- as.character(x)
    x <- gsub("https?://[^[:space:]]+", " ", x, perl = TRUE)
    x <- gsub("www[.][^[:space:]]+", " ", x, perl = TRUE)
    x <- gsub("\\b10[.][0-9]{4,9}/[^[:space:]]+", " ", x, perl = TRUE)
    x <- gsub("\\bdoi\\b[^[:space:]]*", " ", x, ignore.case = TRUE, perl = TRUE)
    x <- gsub("[‐‑–—]", "-", x, perl = TRUE)
    x <- gsub("[_/]", " ", x, perl = TRUE)
    x <- gsub("[^[:alnum:] .;:,!?()-]+", " ", x, perl = TRUE)
    x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
    trimws(x)
  }

  split_sentences <- function(x) {
    x <- clean_for_surface(x)
    s <- unlist(strsplit(x, "[.;:!?]+", perl = TRUE), use.names = FALSE)
    s <- trimws(s)
    s[nzchar(s)]
  }

  normalize_surface_phrase <- function(x) {
    x <- tolower(as.character(x))
    x <- gsub("[‐‑–—]", "-", x, perl = TRUE)
    x <- gsub("[_/]", " ", x, perl = TRUE)
    x <- gsub("[^[:alnum:] -]+", " ", x, perl = TRUE)
    x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
    trimws(x)
  }

  valid_exact_phrase <- function(p) {
    p0 <- normalize_surface_phrase(p)
    if (!nzchar(p0)) return(FALSE)
    if (is_noise_phrase(p0)) return(FALSE)

    if (any(vapply(bad_any, function(b) grepl(b, p0, fixed = TRUE), logical(1)))) {
      return(FALSE)
    }

    w <- unlist(strsplit(p0, "[[:space:]]+", perl = TRUE), use.names = FALSE)
    w <- w[nzchar(w)]

    if (length(w) < min_words || length(w) > max_words) return(FALSE)
    if (any(str_detect(w, "^[0-9]+$"))) return(FALSE)
    if (!any(str_detect(w, "[[:alpha:]]"))) return(FALSE)

    # First and last token must be content-like. Internal stopwords are allowed
    # because this is exact surface phrase extraction.
    if (w[1] %in% default_stopwords || w[length(w)] %in% default_stopwords) return(FALSE)
    if (w[1] %in% bad_start_end || w[length(w)] %in% bad_start_end) return(FALSE)

    # At least half of tokens should be content-like.
    content_n <- sum(is_valid_token(w, min_chars = min_chars))
    if (content_n < ceiling(length(w) / 2)) return(FALSE)

    has_head <- w[length(w)] %in% head_terms
    has_technical <- any(w %in% technical_terms)
    has_known_bigram <- grepl(
      "\b(natural language processing|text mining|data visualization|topic modeling|co occurrence|word embeddings|sentiment detection|polarity detection|community detection|adjacency matrix|r shiny|shiny application|pre processing|part of speech|universal dependencies|language models|ai assistant)\b",
      p0,
      perl = TRUE
    )

    has_head || has_technical || has_known_bigram
  }

  make_exact_ngrams_from_sentence <- function(sent) {
    sent_clean <- clean_for_surface(sent)
    toks <- unlist(strsplit(sent_clean, "[[:space:]]+", perl = TRUE), use.names = FALSE)
    toks <- toks[nzchar(toks)]

    if (length(toks) < min_words) return(character(0))

    out <- character(0)
    for (n in seq.int(min_words, min(max_words, length(toks)))) {
      idx <- seq_len(length(toks) - n + 1L)
      grams <- vapply(idx, function(i) {
        paste(toks[i:(i + n - 1L)], collapse = " ")
      }, character(1))
      out <- c(out, grams)
    }

    out_norm <- unique(normalize_surface_phrase(out))
    out_norm[vapply(out_norm, valid_exact_phrase, logical(1))]
  }

  ans <- docs_tbl |>
    rowwise() |>
    mutate(sentences = list(split_sentences(text))) |>
    ungroup() |>
    select(doc_id, sentences) |>
    tidyr::unnest(sentences) |>
    rowwise() |>
    mutate(term = list(make_exact_ngrams_from_sentence(sentences))) |>
    ungroup() |>
    select(doc_id, term) |>
    tidyr::unnest(term) |>
    mutate(
      term = normalize_surface_phrase(term),
      source_type = "exact_semantic_phrase",
      exact_surface_phrase = TRUE
    ) |>
    filter(term != "", !is_noise_phrase(term)) |>
    distinct(doc_id, term, source_type, exact_surface_phrase)

  ans
}

# Convert hyphen/dash to space before candidate extraction; other symbols are treated as comma breaks by en_tail_keyword_engine.R.
prepare_text_for_en_tail_app <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("[‐‑–—-]+", " ", x, perl = TRUE)
  x
}

normalize_author_keyword_for_tail_app <- function(x) {
  # Vector-preserving normalizer. Do not drop blanks or apply unique() here,
  # because this function is also used inside mutate(); returning fewer rows
  # causes errors such as size 118 vs 91.
  x <- as.character(x %||% "")
  x <- gsub("[‐‑–—-]+", " ", x, perl = TRUE)
  if (exists("normalize_term_en", mode = "function")) {
    x <- vapply(x, function(z) {
      out <- tryCatch(normalize_term_en(z), error = function(e) normalize_exact_phrase(z))
      if (length(out) == 0) "" else as.character(out[[1]])
    }, character(1))
  } else {
    x <- normalize_exact_phrase(x)
  }
  x <- normalize_exact_phrase(x)
  x[is.na(x)] <- ""
  x
}

normalize_author_keywords_unique_app <- function(x) {
  x <- normalize_author_keyword_for_tail_app(x)
  unique(x[nzchar(x)])
}


# ---- ChatGPT-like keyword-level controls: strict professional single-word endings ----
# Step 1 allows only 2-4 word phrases plus single words ending in:
# ...zation / ...ization / ...isation / ...ibility.
# Broad ...tion nouns such as analysis/application/extraction/validation are NOT Step-1 singles.
.prof_single_suffix_app <- function(term) {
  x <- normalize_author_keyword_for_tail_app(term)
  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x) || grepl("[[:space:]]", x, perl = TRUE)) return(FALSE)

  # Standalone suffix fragments are never valid keywords.
  # IMPORTANT: suffix recognition uses ...zation/...ization/...isation/...ibility,
  # not generic ...tion. Therefore broad nouns such as "analysis",
  # "application", "collection", "extraction", and "validation" are not
  # rewarded as professional single terms.
  bad_suffix_fragments <- c(
    "tion", "sion", "ation", "zation", "isation", "ization",
    "ability", "ibility", "bility", "ment", "ness", "ing", "ed", "ly"
  )
  if (x %in% bad_suffix_fragments) return(FALSE)
  if (nchar(x) < 6L) return(FALSE)

  grepl("(zation|ization|isation|ibility)$", x, ignore.case = TRUE, perl = TRUE)
}

# Single words eligible for optional phrase recovery.
# IMPORTANT: this follows the same strict suffix rule as Step 1.
# Broad words such as analysis/application/extraction/validation are not
# expansion targets; they can appear only as part of naturally extracted 2-4 word phrases.
.prof_single_expansion_target_app <- function(term) {
  .prof_single_suffix_app(term)
}

.bad_start_keyword_app <- function(term) {
  x <- normalize_author_keyword_for_tail_app(term)
  toks <- unlist(strsplit(x, "\\s+", perl = TRUE), use.names = FALSE)
  toks <- toks[nzchar(toks)]
  if (!length(toks)) return(TRUE)
  bad_start <- c(
    "accompanied", "automatically", "collapsible", "correspondence",
    "facilitating", "further", "generating", "hierarchical", "inputs",
    "institutional", "intuitive", "optional", "despite", "including",
    "include", "includes", "maintains", "demonstrating", "analyzing",
    "combining", "downloads", "core", "checks"
  )
  toks[[1]] %in% bad_start
}

.contains_broken_keyword_token_app <- function(term) {
  x <- normalize_author_keyword_for_tail_app(term)
  toks <- unlist(strsplit(x, "\\s+", perl = TRUE), use.names = FALSE)
  toks <- toks[nzchar(toks)]
  bad_tokens <- c("ware", "analy", "port", "im", "sum", "marization", "consci", "tion", "sion", "ation", "zation", "isation", "ization")
  any(toks %in% bad_tokens)
}

.keyword_length_weight_app <- function(term) {
  x <- normalize_author_keyword_for_tail_app(term)
  n_words <- stringr::str_count(x, "[[:space:]]+") + 1L
  is_prof <- vapply(x, .prof_single_suffix_app, logical(1))
  dplyr::case_when(
    n_words == 1L & is_prof ~ 1.60,
    n_words == 1L ~ 0.80,
    n_words == 2L ~ 1.25,
    n_words == 3L ~ 1.00,
    n_words == 4L ~ 0.85,
    TRUE ~ 1.00
  )
}

strict_en_tail_term_ok_app <- function(term, author_keyword = FALSE, max_words = 4L) {
  term <- normalize_author_keyword_for_tail_app(term)
  if (!length(term) || !nzchar(term[[1]])) return(FALSE)
  term <- term[[1]]
  if (isTRUE(author_keyword)) return(TRUE)

  if (grepl("[0-9]", term, perl = TRUE)) return(FALSE)
  if (grepl("\\b(author metadata|metadata|references?|bibliography|journal homepage|copyright|received|accepted|available online|corresponding author|email addresses?|appendix|supplementary|data availability)\\b", term, ignore.case = TRUE, perl = TRUE)) return(FALSE)
  n_words <- length(strsplit(term, "\\s+", perl = TRUE)[[1]])
  # Keyword-level rule: prefer 1-word professional terms and 2-4 word semantic phrases.
  # Four-word terms are allowed only after strict en-tail filtering.
  if (n_words == 1L && !.prof_single_suffix_app(term) && !(term %in% c("nlp", "tall", "ai", "r"))) return(FALSE)
  if (n_words > max_words) return(FALSE)
  if (.bad_start_keyword_app(term)) return(FALSE)
  if (.contains_broken_keyword_token_app(term)) return(FALSE)

  if (exists("contains_blacklisted_content_en", mode = "function")) {
    if (isTRUE(tryCatch(contains_blacklisted_content_en(term), error = function(e) TRUE))) return(FALSE)
  }
  if (exists("looks_like_metadata_en", mode = "function")) {
    if (isTRUE(tryCatch(looks_like_metadata_en(term), error = function(e) FALSE))) return(FALSE)
  }
  if (exists("unit_ok_en", mode = "function")) {
    if (!isTRUE(tryCatch(unit_ok_en(term, max_words = max_words), error = function(e) FALSE))) return(FALSE)
  }

  bad_edge <- c("a", "an", "the", "and", "or", "but", "that", "which", "whose", "while", "whereas", "because", "before", "after",
                "using", "used", "through", "via", "from", "into", "onto", "upon", "than", "with", "without", "under", "over",
                "in", "on", "at", "by", "for", "to", "of", "as", "per", "is", "are", "was", "were", "be", "been", "being")
  toks <- strsplit(term, "\\s+", perl = TRUE)[[1]]
  if (length(toks) && (toks[1] %in% bad_edge || toks[length(toks)] %in% bad_edge)) return(FALSE)
  TRUE
}

strict_filter_doc_terms_en_tail_app <- function(doc_terms, author_keywords = character(0), max_words = 4L) {
  if (is.null(doc_terms) || !is.data.frame(doc_terms) || !nrow(doc_terms)) return(doc_terms)
  if (!"author_keyword" %in% names(doc_terms)) doc_terms$author_keyword <- FALSE
  author_keywords <- normalize_author_keywords_unique_app(author_keywords)
  doc_terms |>
    mutate(
      term = normalize_author_keyword_for_tail_app(term),
      author_keyword = (author_keyword %in% TRUE) | term %in% author_keywords,
      n_words = stringr::str_count(term, "[[:space:]]+") + 1L,
      .strict_ok = vapply(seq_along(term), function(i) strict_en_tail_term_ok_app(term[[i]], author_keyword = author_keyword[[i]], max_words = max_words), logical(1))
    ) |>
    filter(.strict_ok, nzchar(term)) |>
    select(-.strict_ok, -n_words) |>
    distinct(doc_id, term, source_type, exact_surface_phrase, author_keyword, .keep_all = TRUE)
}

strict_filter_score_tbl_en_tail_app <- function(score_tbl, forced_terms = character(0), max_words = 4L) {
  if (is.null(score_tbl) || !is.data.frame(score_tbl) || !nrow(score_tbl)) return(score_tbl)
  forced_terms <- normalize_author_keywords_unique_app(forced_terms)
  if (!"author_keyword" %in% names(score_tbl)) score_tbl$author_keyword <- FALSE
  score_tbl |>
    mutate(
      term = normalize_author_keyword_for_tail_app(term),
      author_keyword = (author_keyword %in% TRUE) | term %in% forced_terms,
      .strict_ok = vapply(seq_along(term), function(i) strict_en_tail_term_ok_app(term[[i]], author_keyword = author_keyword[[i]], max_words = max_words), logical(1))
    ) |>
    filter(.strict_ok, nzchar(term)) |>
    select(-.strict_ok) |>
    distinct(term, .keep_all = TRUE)
}


strict_filter_any_terms_en_tail_app <- function(x, forced_terms = character(0), max_words = 4L) {
  forced_terms <- normalize_author_keywords_unique_app(forced_terms)
  x <- normalize_author_keyword_for_tail_app(x)
  x <- x[nzchar(x)]
  if (!length(x)) return(character(0))
  keep <- vapply(x, function(z) strict_en_tail_term_ok_app(z, author_keyword = z %in% forced_terms, max_words = max_words), logical(1))
  unique(x[keep])
}

extract_semantic_phrases_en_tail_app <- function(docs_tbl, min_chars = 3L, max_words = 4L, pre_top_n = 100L) {
  # Four-stage semantic candidate rule requested by user:
  # Step 1: MDPI-style candidate phrase extraction, restricted to 2-4 words.
  # Step 2: Filter by en_tail_keyword_engine.R and remove smaller phrases involved in longer phrases within Top-100.
  # Step 3: Author keywords are injected later by inject_author_keywords().
  # Step 4: Top-20 + edges are selected in flca_sil_ma_screen_top20(), then FLCA-SIL-MA is applied.
  if (is.null(docs_tbl) || !nrow(docs_tbl)) {
    return(tibble(doc_id = character(), term = character(), source_type = character(), exact_surface_phrase = logical()))
  }

  out <- lapply(seq_len(nrow(docs_tbl)), function(i) {
    doc_id <- as.character(docs_tbl$doc_id[[i]] %||% i)
    txt <- prepare_text_for_en_tail_app(docs_tbl$text[[i]] %||% "")

    # Step 1: Candidate extraction. Prefer the en-tail comma-first extractor,
    # which follows the uploaded engine rule: hyphen -> space; other symbols -> comma;
    # blacklist/whitelist and break words are used there.
    units <- character(0)
    if (isTRUE(.en_tail_engine_loaded) && exists("extract_units_from_text_en", mode = "function")) {
      units <- tryCatch(
        extract_units_from_text_en(txt, max_words = max_words),
        error = function(e) character(0)
      )
    }

    # IMPORTANT: no legacy n-gram fallback here.
    # The old fallback could explode on long PDF lines and bypass the en-tail rule,
    # causing >4-word or numeric/reference phrases to re-enter the final SSplot.
    if (!length(units)) {
      units <- character(0)
    }

    # Keep only a bounded, unique set per document before scoring. This prevents
    # slow TF-IDF/edge generation when a PDF page creates many repeated fragments.
    units <- unique(units)
    if (length(units) > 1000L) units <- utils::head(units, 1000L)

    units <- normalize_author_keyword_for_tail_app(units)
    units <- units[nzchar(units)]
    units <- units[vapply(units, function(z) strict_en_tail_term_ok_app(z, author_keyword = FALSE, max_words = max_words), logical(1))]
    if (!length(units)) {
      return(tibble(doc_id = character(), term = character(), source_type = character(), exact_surface_phrase = logical()))
    }
    tibble(
      doc_id = doc_id,
      term = units,
      source_type = "mdpi_en_tail_semantic_phrase",
      exact_surface_phrase = TRUE
    )
  })

  cand <- bind_rows(out) |>
    mutate(term = normalize_author_keyword_for_tail_app(term)) |>
    filter(nzchar(term)) |>
    filter(vapply(term, function(z) strict_en_tail_term_ok_app(z, author_keyword = FALSE, max_words = max_words), logical(1))) |>
    distinct(doc_id, term, source_type, exact_surface_phrase)

  if (!nrow(cand)) return(cand)

  # Step 2: Top-100 phrase pool, then remove small phrases contained in longer phrases.
  freq_tbl <- cand |>
    count(term, name = "freq") |>
    mutate(n_words = stringr::str_count(term, "[[:space:]]+") + 1L) |>
    arrange(desc(freq), desc(n_words), desc(nchar(term)), term) |>
    slice_head(n = pre_top_n)

  if (nrow(freq_tbl)) {
    if (exists("select_subset_free_terms_en", mode = "function")) {
      keep_terms <- tryCatch(
        select_subset_free_terms_en(freq_tbl$term, scores = freq_tbl$freq, top_n = pre_top_n),
        error = function(e) character(0)
      )
    } else {
      keep_terms <- character(0)
      for (tm in freq_tbl$term) {
        is_small_inside_long <- any(vapply(keep_terms, function(k) word_boundary_contains(k, tm), logical(1)))
        contains_kept <- keep_terms[vapply(keep_terms, function(k) word_boundary_contains(tm, k), logical(1))]
        if (is_small_inside_long) next
        if (length(contains_kept)) keep_terms <- setdiff(keep_terms, contains_kept)
        keep_terms <- unique(c(keep_terms, tm))
        if (length(keep_terms) >= pre_top_n) break
      }
    }
    keep_terms <- normalize_author_keywords_unique_app(keep_terms)
    keep_terms <- keep_terms[nzchar(keep_terms)]
    cand <- cand |> filter(term %in% keep_terms)
  }

  cand |>
    filter(nzchar(term)) |>
    distinct(doc_id, term, source_type, exact_surface_phrase)
}



# ---- Optional OpenAI API extraction helpers ----
# These helpers preserve the original homepage and the original TF-IDF/en-tail pipeline.
# They only replace Step 1 candidate extraction when the user selects an API mode.
openai_api_key_available_app <- function() {
  key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  nzchar(trimws(key))
}

strip_json_fences_app <- function(x) {
  x <- as.character(x %||% "")
  x <- trimws(x)
  x <- gsub("^```[A-Za-z0-9_-]*[[:space:]]*", "", x, perl = TRUE)
  x <- gsub("[[:space:]]*```$", "", x, perl = TRUE)
  trimws(x)
}

parse_api_terms_json_app <- function(content) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for API extraction. Install it with install.packages('jsonlite'), or use TF-IDF-only mode.", call. = FALSE)
  }
  x <- strip_json_fences_app(content)
  parsed <- tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(parsed)) {
    s <- regexpr("\\{", x, perl = TRUE)[[1]]
    e_all <- gregexpr("\\}", x, perl = TRUE)[[1]]
    e <- if (length(e_all) && e_all[[1]] > 0) max(e_all) else -1L
    if (s > 0 && e > s) {
      parsed <- tryCatch(jsonlite::fromJSON(substr(x, s, e), simplifyVector = FALSE), error = function(e) NULL)
    }
  }
  if (is.null(parsed)) stop("API returned text, but it was not valid JSON.", call. = FALSE)

  raw_terms <- parsed$terms %||% parsed$keywords %||% parsed$phrases %||% parsed
  terms <- character(0)
  if (is.character(raw_terms)) {
    terms <- raw_terms
  } else if (is.list(raw_terms)) {
    terms <- vapply(raw_terms, function(z) {
      if (is.character(z)) return(z[[1]] %||% "")
      if (is.list(z)) return(as.character(z$term %||% z$keyword %||% z$phrase %||% z[[1]] %||% ""))
      ""
    }, character(1))
  }
  terms <- normalize_author_keyword_for_tail_app(terms)
  unique(terms[nzchar(terms)])
}

build_api_document_text_app <- function(docs_tbl, max_chars = 16000L) {
  if (is.null(docs_tbl) || !nrow(docs_tbl)) return("")
  if (!"doc_id" %in% names(docs_tbl)) docs_tbl$doc_id <- seq_len(nrow(docs_tbl))
  if (!"text" %in% names(docs_tbl)) docs_tbl$text <- as.character(docs_tbl[[1]])
  lines <- paste0("[", docs_tbl$doc_id, "] ", as.character(docs_tbl$text))
  txt <- paste(lines, collapse = "\n\n")
  txt <- gsub("[[:space:]]+", " ", txt, perl = TRUE)
  txt <- trimws(txt)
  if (nchar(txt, type = "chars") > max_chars) {
    txt <- substr(txt, 1L, max_chars)
  }
  txt
}

openai_extract_doc_terms_app <- function(docs_tbl, top_n = 20L, author_keywords = character(0), model = Sys.getenv("OPENAI_MODEL", unset = "gpt-4o-mini")) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for API extraction. Install it with install.packages('httr2'), or use TF-IDF-only mode.", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for API extraction. Install it with install.packages('jsonlite'), or use TF-IDF-only mode.", call. = FALSE)
  }
  api_key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  api_key <- trimws(api_key)
  if (!nzchar(api_key)) {
    stop("OPENAI_API_KEY is not set. Use TF-IDF-only mode, or set OPENAI_API_KEY before running the app.", call. = FALSE)
  }

  top_n <- as.integer(top_n %||% 20L)
  if (!is.finite(top_n) || top_n < 1L) top_n <- 20L
  top_n <- min(100L, top_n)
  forced <- normalize_author_keywords_unique_app(author_keywords)
  forced <- forced[nzchar(forced)]
  doc_text <- build_api_document_text_app(docs_tbl)
  if (!nzchar(doc_text)) stop("No document text was available for API extraction.", call. = FALSE)

  user_prompt <- paste0(
    "Extract up to ", top_n, " keyword-level semantic terms from the document.\n",
    "Rules:\n",
    "1. Prefer exact author keywords, acronyms, and professional noun phrases.\n",
    "2. Prefer 1-4 word keyword-level terms; avoid long sentence fragments.\n",
    "3. Exclude references, metadata, DOI/URL fragments, page headers, numbers-only terms, and generic words.\n",
    "4. Do not invent terms not supported by the document.\n",
    "5. Return JSON only in this exact shape: {\"terms\":[{\"term\":\"...\"}]}.\n\n",
    "Forced author/user keywords, if present: ", ifelse(length(forced), paste(forced, collapse = "; "), "none"), "\n\n",
    "Document text:\n", doc_text
  )

  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model,
      temperature = 0,
      response_format = list(type = "json_object"),
      messages = list(
        list(role = "system", content = "You are a strict academic keyword extraction engine. Return valid JSON only."),
        list(role = "user", content = user_prompt)
      )
    )) |>
    httr2::req_timeout(90)

  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  content <- body$choices[[1]]$message$content %||% ""
  terms <- parse_api_terms_json_app(content)

  # Keep forced author keywords available to Step 3 even if the model missed them.
  terms <- unique(c(forced, terms))
  terms <- terms[vapply(terms, function(z) strict_en_tail_term_ok_app(z, author_keyword = z %in% forced, max_words = 4L), logical(1))]
  terms <- head(unique(terms), top_n)
  if (!length(terms)) stop("API extraction returned no usable terms after filtering.", call. = FALSE)

  # One shared pseudo-document preserves co-occurrence edges; weighted pseudo-documents preserve API ranking order.
  all_rows <- tibble::tibble(
    doc_id = "openai_api_terms_all",
    term = terms,
    source_type = "openai_api_semantic_phrase",
    exact_surface_phrase = TRUE,
    author_keyword = terms %in% forced
  )
  ranks <- seq_along(terms)
  repeat_n <- pmax(1L, top_n + 1L - ranks)
  rank_doc_id <- unlist(lapply(seq_along(ranks), function(i) {
    paste0("openai_api_rank_", sprintf("%02d", ranks[[i]]), "_", sprintf("%03d", seq_len(repeat_n[[i]])))
  }), use.names = FALSE)
  rank_rows <- tibble::tibble(
    doc_id = rank_doc_id,
    term = rep(terms, times = repeat_n),
    source_type = "openai_api_rank_weighted",
    exact_surface_phrase = TRUE,
    author_keyword = rep(terms %in% forced, times = repeat_n)
  )
  dplyr::bind_rows(all_rows, rank_rows)
}

resolve_api_tf_idf_doc_terms_app <- function(docs_tbl, api_mode = "auto", tfidf_mode = "semantic", top_n = 20L, author_keywords = character(0)) {
  api_mode <- as.character(api_mode %||% "auto")
  api_available <- openai_api_key_available_app()
  api_attempted <- FALSE
  api_used <- FALSE
  api_error <- ""
  engine_used <- "tfidf"

  should_try_api <- api_mode %in% c("api_first", "api_only") || (identical(api_mode, "auto") && isTRUE(api_available))

  if (isTRUE(should_try_api)) {
    api_attempted <- TRUE
    res <- tryCatch(
      openai_extract_doc_terms_app(docs_tbl, top_n = top_n, author_keywords = author_keywords),
      error = function(e) e
    )
    if (!inherits(res, "error")) {
      api_used <- TRUE
      engine_used <- "openai_api"
      return(list(
        doc_terms = res,
        engine_used = engine_used,
        api_mode = api_mode,
        api_available = api_available,
        api_attempted = api_attempted,
        api_used = api_used,
        api_error = api_error
      ))
    }
    api_error <- conditionMessage(res)
    if (identical(api_mode, "api_only")) {
      stop(paste0("API-only mode failed: ", api_error), call. = FALSE)
    }
    engine_used <- "tfidf_fallback"
  } else if (identical(api_mode, "auto") && !isTRUE(api_available)) {
    engine_used <- "tfidf_auto_no_api_key"
  } else if (identical(api_mode, "tfidf_only")) {
    engine_used <- "tfidf_only"
  }

  tfidf_terms <- extract_terms_by_mode(docs_tbl, mode = tfidf_mode, min_chars = 3L)
  list(
    doc_terms = tfidf_terms,
    engine_used = engine_used,
    api_mode = api_mode,
    api_available = api_available,
    api_attempted = api_attempted,
    api_used = api_used,
    api_error = api_error
  )
}

extract_terms_by_mode <- function(docs_tbl, mode = "semantic", min_chars = 3L) {
  mode <- mode %||% "semantic"

  if (mode == "single") {
    return(extract_single_terms(docs_tbl, min_chars = min_chars))
  }

  if (mode == "ngram") {
    return(extract_ngrams(docs_tbl, n_values = c(2L, 3L), min_chars = min_chars))
  }

  if (mode == "semantic") {
    return(extract_semantic_phrases_en_tail_app(docs_tbl, min_chars = min_chars, max_words = 4L))
  }

  if (mode == "ner") {
    return(extract_named_entity_phrases(docs_tbl))
  }

  if (mode == "combined") {
    return(bind_rows(
      extract_semantic_phrases_en_tail_app(docs_tbl, min_chars = min_chars, max_words = 4L),
      extract_named_entity_phrases(docs_tbl)
    ) |>
      distinct(doc_id, term, source_type, exact_surface_phrase))
  }

  stop("Unknown extraction mode.")
}

rank_all_terms <- function(doc_terms) {
  if (nrow(doc_terms) == 0) {
    return(tibble(
      doc_id = character(), term = character(), source_type = character(),
      exact_surface_phrase = logical(), author_keyword = logical(), tf_idf = numeric()
    ))
  }

  if (!("exact_surface_phrase" %in% names(doc_terms))) doc_terms$exact_surface_phrase <- TRUE
  if (!("author_keyword" %in% names(doc_terms))) doc_terms$author_keyword <- FALSE

  doc_terms |>
    mutate(
      doc_id = as.character(doc_id),
      term = normalize_exact_phrase(term),
      source_type = as.character(source_type %||% "unknown"),
      exact_surface_phrase = exact_surface_phrase %in% TRUE,
      author_keyword = author_keyword %in% TRUE
    ) |>
    filter(nzchar(term), !is_noise_phrase(term) | author_keyword) |>
    group_by(doc_id, term) |>
    summarise(
      n = n(),
      source_type = paste(sort(unique(source_type)), collapse = "+"),
      exact_surface_phrase = all(exact_surface_phrase %in% TRUE),
      author_keyword = any(author_keyword %in% TRUE),
      .groups = "drop"
    ) |>
    tidytext::bind_tf_idf(term, doc_id, n) |>
    mutate(tf_idf = ifelse(is.finite(tf_idf), tf_idf, 0))
}


# Expand allowed suffix-only single terms to the best available 2-4 word phrase.
# Example: if "tokenization" and "text tokenization" both appear, the single term
# can be merged into the first/best professional phrase rather than occupying
# a Top-20 slot alone.
expand_professional_single_terms_app <- function(all_ranked, forced_terms = character(0), max_words = 4L) {
  if (is.null(all_ranked) || !is.data.frame(all_ranked) || !nrow(all_ranked)) return(all_ranked)
  if (!"term" %in% names(all_ranked)) return(all_ranked)
  if (!"tf_idf" %in% names(all_ranked)) all_ranked$tf_idf <- 0
  if (!"doc_id" %in% names(all_ranked)) all_ranked$doc_id <- seq_len(nrow(all_ranked))
  if (!"source_type" %in% names(all_ranked)) all_ranked$source_type <- "unknown"
  if (!"author_keyword" %in% names(all_ranked)) all_ranked$author_keyword <- FALSE

  forced_terms <- normalize_author_keywords_unique_app(forced_terms)
  acronym_keep <- c("nlp", "tall", "ai", "r")

  ranked0 <- all_ranked |>
    mutate(
      term = normalize_author_keyword_for_tail_app(term),
      author_keyword = (author_keyword %in% TRUE) | term %in% forced_terms,
      n_words = stringr::str_count(term, "[[:space:]]+") + 1L
    ) |>
    filter(nzchar(term))

  score_pool <- ranked0 |>
    group_by(term) |>
    summarise(
      tfidf_sum = sum(suppressWarnings(as.numeric(tf_idf)), na.rm = TRUE),
      doc_freq = n_distinct(doc_id),
      n_words = dplyr::first(n_words),
      author_keyword = any(author_keyword %in% TRUE),
      .groups = "drop"
    )

  singles <- score_pool |>
    filter(
      n_words == 1L,
      vapply(term, .prof_single_suffix_app, logical(1)),
      !(term %in% acronym_keep),
      !(term %in% forced_terms),
      !(author_keyword %in% TRUE)
    )

  phrases <- score_pool |>
    filter(n_words >= 2L, n_words <= max_words) |>
    filter(vapply(term, function(z) strict_en_tail_term_ok_app(z, author_keyword = z %in% forced_terms, max_words = max_words), logical(1)))

  if (!nrow(singles) || !nrow(phrases)) return(ranked0 |> select(-n_words))

  replace_to <- character(0)
  for (s in singles$term) {
    pat_any <- paste0("\\b", gsub("([.^$|()\\[\\]{}*+?\\\\-])", "\\\\\\1", s, perl = TRUE), "\\b")
    pat_end <- paste0(pat_any, "$")
    cand <- phrases |>
      filter(grepl(pat_any, term, perl = TRUE), term != s) |>
      mutate(
        ends_with_single = grepl(pat_end, term, perl = TRUE),
        is_two_word = n_words == 2L
      ) |>
      arrange(desc(ends_with_single), desc(is_two_word), desc(tfidf_sum), desc(doc_freq), n_words, term)
    if (nrow(cand)) replace_to[[s]] <- cand$term[[1]]
  }

  if (!length(replace_to)) return(ranked0 |> select(-n_words))

  ranked0 |>
    mutate(
      .old_term = term,
      term = ifelse(term %in% names(replace_to), unname(replace_to[term]), term),
      source_type = ifelse(.old_term != term, paste0(source_type, "+expanded_professional_single"), source_type),
      exact_surface_phrase = TRUE
    ) |>
    select(-.old_term, -n_words)
}


# Search the original document text for the first valid 2-4 word phrase that
# ends with an allowed professional single term. This implements the strict rule:
# only ...zation/...ization/...isation/...ibility singles are eligible for recovery.
# Broad ...tion words are not searched as single-term targets here.
.extract_prof_single_phrase_map_from_docs_app <- function(docs_tbl, singles, max_words = 4L) {
  singles <- normalize_author_keyword_for_tail_app(singles)
  singles <- unique(singles[nzchar(singles)])
  singles <- singles[vapply(singles, .prof_single_expansion_target_app, logical(1))]
  if (!length(singles) || is.null(docs_tbl) || !is.data.frame(docs_tbl) || !"text" %in% names(docs_tbl)) {
    return(tibble::tibble(from = character(), to = character()))
  }

  txt <- paste(as.character(docs_tbl$text %||% ""), collapse = " ")
  txt <- prepare_text_for_en_tail_app(txt)
  txt <- tolower(txt)
  txt <- gsub("[^a-z0-9]+", " ", txt, perl = TRUE)
  txt <- gsub("[[:space:]]+", " ", txt, perl = TRUE)
  toks <- unlist(strsplit(trimws(txt), "[[:space:]]+", perl = TRUE), use.names = FALSE)
  toks <- toks[nzchar(toks)]
  if (!length(toks)) return(tibble::tibble(from = character(), to = character()))

  bad_context <- c(
    "a", "an", "the", "and", "or", "but", "that", "which", "while", "whereas",
    "is", "are", "was", "were", "be", "been", "being", "to", "of", "for", "on", "in", "at", "by", "as", "from", "into", "with", "without",
    "this", "these", "those", "their", "its", "our", "we", "using", "used", "based", "via",
    "figure", "table", "section", "references", "appendix", "doi", "http", "https", "www"
  )

  rows <- lapply(singles, function(s) {
    pos <- which(toks == s)
    if (!length(pos)) return(NULL)
    for (i in pos) {
      # Prefer the first 2-word phrase from top to bottom, then 3- and 4-word phrases.
      for (k in seq_len(max_words - 1L)) {
        start <- i - k
        if (start < 1L) next
        cand_toks <- toks[start:i]
        if (length(cand_toks) < 2L || length(cand_toks) > max_words) next
        if (any(cand_toks %in% bad_context)) next
        cand <- paste(cand_toks, collapse = " ")
        if (identical(cand, s)) next
        ok <- tryCatch(strict_en_tail_term_ok_app(cand, author_keyword = FALSE, max_words = max_words), error = function(e) FALSE)
        if (isTRUE(ok)) return(tibble::tibble(from = s, to = cand))
      }
    }
    NULL
  })
  out <- dplyr::bind_rows(rows)
  if (!nrow(out)) return(tibble::tibble(from = character(), to = character()))
  out |> dplyr::distinct(from, .keep_all = TRUE)
}

expand_professional_singles_using_docs_app <- function(docs_tbl, doc_terms, author_keywords = character(0), max_words = 4L) {
  if (is.null(doc_terms) || !is.data.frame(doc_terms) || !nrow(doc_terms) || !"term" %in% names(doc_terms)) {
    return(list(doc_terms = doc_terms, expansion_map = tibble::tibble(from = character(), to = character())))
  }
  if (!"source_type" %in% names(doc_terms)) doc_terms$source_type <- "unknown"
  if (!"exact_surface_phrase" %in% names(doc_terms)) doc_terms$exact_surface_phrase <- TRUE
  if (!"author_keyword" %in% names(doc_terms)) doc_terms$author_keyword <- FALSE

  forced <- normalize_author_keywords_unique_app(author_keywords)
  acronym_keep <- c("nlp", "tall", "ai", "r")
  terms <- normalize_author_keyword_for_tail_app(doc_terms$term)
  n_words <- stringr::str_count(terms, "[[:space:]]+") + 1L
  singles <- unique(terms[n_words == 1L & !(terms %in% forced) & !(terms %in% acronym_keep)])
  singles <- singles[vapply(singles, .prof_single_expansion_target_app, logical(1))]
  expansion_map <- .extract_prof_single_phrase_map_from_docs_app(docs_tbl, singles, max_words = max_words)
  if (!nrow(expansion_map)) {
    return(list(doc_terms = doc_terms, expansion_map = expansion_map))
  }

  repl <- stats::setNames(expansion_map$to, expansion_map$from)
  out <- doc_terms |>
    dplyr::mutate(
      .old_term = normalize_author_keyword_for_tail_app(term),
      term = dplyr::if_else(.old_term %in% names(repl), unname(repl[.old_term]), normalize_author_keyword_for_tail_app(term)),
      source_type = dplyr::if_else(.old_term %in% names(repl), paste0(source_type, "+doc_context_expanded_single"), source_type),
      exact_surface_phrase = TRUE
    ) |>
    dplyr::select(-.old_term)

  list(doc_terms = out, expansion_map = expansion_map)
}

build_all_edges <- function(selected_terms, doc_terms) {
  selected_terms <- unique(selected_terms[nzchar(selected_terms)])
  dt <- doc_terms |> filter(term %in% selected_terms)
  pairs <- dt |>
    group_by(doc_id) |>
    summarise(terms = list(sort(unique(term))), .groups = "drop") |>
    mutate(pairs = lapply(terms, function(x) {
      if (length(x) < 2) return(NULL)
      m <- utils::combn(x, 2)
      tibble(from = m[1, ], to = m[2, ])
    })) |>
    tidyr::unnest(pairs)

  if (nrow(pairs) == 0) {
    if (length(selected_terms) < 2) {
      return(tibble(from = character(), to = character(), weight = numeric()))
    }
    return(tibble(
      from = selected_terms[-length(selected_terms)],
      to = selected_terms[-1],
      weight = 1
    ))
  }

  pairs |>
    count(from, to, name = "weight") |>
    arrange(desc(weight), from, to)
}


.force_author_terms_into_selected <- function(selected, score_tbl, forced_terms = character(0), top_n = 20L) {
  forced_terms <- normalize_exact_phrase(forced_terms)
  forced_terms <- unique(forced_terms[nzchar(forced_terms)])
  if (!length(forced_terms)) return(selected)

  top_n <- as.integer(top_n %||% 20L)
  if (!is.finite(top_n) || top_n < 1L) top_n <- 20L

  if (is.null(selected) || !is.data.frame(selected)) selected <- tibble()
  if (!nrow(selected)) {
    selected <- tibble(
      term = character(), topic = integer(), degree = numeric(), doc_freq = numeric(),
      tfidf_sum = numeric(), score = numeric(), source_type = character(),
      author_keyword = logical(), exact_in_document = logical(), leader = character(),
      is_leader = logical(), topic_rank = integer(), value = numeric()
    )
  }

  # Standardize required selected columns.
  req_cols <- list(
    term = character(), topic = integer(), degree = numeric(), doc_freq = numeric(),
    tfidf_sum = numeric(), score = numeric(), source_type = character(),
    author_keyword = logical(), exact_in_document = logical(), leader = character(),
    is_leader = logical(), topic_rank = integer(), value = numeric()
  )
  for (nm in names(req_cols)) {
    if (!nm %in% names(selected)) selected[[nm]] <- req_cols[[nm]][0]
  }

  selected <- selected |>
    mutate(
      term = normalize_exact_phrase(term),
      score = suppressWarnings(as.numeric(score)),
      value = suppressWarnings(as.numeric(value)),
      topic = suppressWarnings(as.integer(topic)),
      degree = suppressWarnings(as.numeric(degree)),
      doc_freq = suppressWarnings(as.numeric(doc_freq)),
      tfidf_sum = suppressWarnings(as.numeric(tfidf_sum)),
      author_keyword = (author_keyword %in% TRUE) | term %in% forced_terms,
      source_type = as.character(source_type),
      exact_in_document = exact_in_document %in% TRUE,
      leader = as.character(leader),
      is_leader = is_leader %in% TRUE,
      topic_rank = suppressWarnings(as.integer(topic_rank))
    ) |>
    filter(nzchar(term)) |>
    distinct(term, .keep_all = TRUE)

  if (is.null(score_tbl) || !is.data.frame(score_tbl)) score_tbl <- tibble()
  if (nrow(score_tbl)) {
    score_tbl <- score_tbl |>
      mutate(
        term = normalize_exact_phrase(term),
        score = suppressWarnings(as.numeric(score)),
        doc_freq = suppressWarnings(as.numeric(doc_freq)),
        tfidf_sum = suppressWarnings(as.numeric(tfidf_sum)),
        degree = suppressWarnings(as.numeric(degree)),
        source_type = as.character(source_type),
        exact_in_document = exact_in_document %in% TRUE,
        author_keyword = (author_keyword %in% TRUE) | term %in% forced_terms
      )
  }

  base_score <- suppressWarnings(max(selected$score, score_tbl$score, na.rm = TRUE))
  if (!is.finite(base_score)) base_score <- 0
  forced_score <- base_score + 1

  # Build or update forced rows. Missing author keywords get their own row.
  missing_forced <- setdiff(forced_terms, selected$term)
  rows <- list()
  if (length(missing_forced)) {
    next_topic <- suppressWarnings(max(selected$topic, na.rm = TRUE))
    if (!is.finite(next_topic)) next_topic <- 0L
    for (i in seq_along(missing_forced)) {
      ft <- missing_forced[[i]]
      sc <- score_tbl |> filter(term == ft) |> slice_head(n = 1)
      rows[[i]] <- tibble(
        term = ft,
        topic = as.integer(next_topic + i),
        degree = 0,
        doc_freq = if (nrow(sc)) suppressWarnings(as.numeric(sc$doc_freq[[1]])) else 1,
        tfidf_sum = if (nrow(sc)) suppressWarnings(as.numeric(sc$tfidf_sum[[1]])) else 0,
        score = forced_score + (length(missing_forced) - i) / 1000,
        source_type = if (nrow(sc)) paste0(sc$source_type[[1]], "+author_keyword_final_forced") else "author_keyword_final_forced",
        author_keyword = TRUE,
        exact_in_document = if (nrow(sc)) sc$exact_in_document[[1]] %in% TRUE else TRUE,
        leader = ft,
        is_leader = TRUE,
        topic_rank = 1L,
        value = forced_score + (length(missing_forced) - i) / 1000
      )
    }
  }
  if (length(rows)) selected <- bind_rows(selected, bind_rows(rows))

  # Final hard gate: every forced term has author_keyword=TRUE and high enough score.
  selected <- selected |>
    mutate(
      author_keyword = (author_keyword %in% TRUE) | term %in% forced_terms,
      source_type = ifelse(term %in% forced_terms & !grepl("author_keyword", source_type), paste0(source_type, "+author_keyword_final_forced"), source_type),
      score = ifelse(term %in% forced_terms, pmax(score, forced_score), score),
      value = ifelse(term %in% forced_terms, pmax(value, score), value),
      leader = ifelse(term %in% forced_terms & (!nzchar(leader) | is.na(leader)), term, leader),
      is_leader = ifelse(term %in% forced_terms & is.na(is_leader), TRUE, is_leader)
    ) |>
    arrange(desc(author_keyword), desc(score), term) |>
    distinct(term, .keep_all = TRUE)

  # If more than Top-N, keep all forced terms first, then fill with best non-forced terms.
  forced_part <- selected |> filter(term %in% forced_terms) |> arrange(match(term, forced_terms))
  other_part <- selected |> filter(!(term %in% forced_terms)) |> arrange(desc(score), term)
  out <- bind_rows(forced_part, head(other_part, max(0L, top_n - nrow(forced_part))))
  if (nrow(out) > top_n) out <- out |> slice_head(n = top_n)

  out |>
    mutate(
      topic = ifelse(is.na(topic) | !is.finite(topic), row_number(), topic),
      topic_rank = ifelse(is.na(topic_rank) | !is.finite(topic_rank), row_number(), topic_rank),
      value = ifelse(is.finite(value), value, score)
    )
}

flca_sil_ma_screen_top20 <- function(all_ranked, top_n = 20L, min_edge_docs = 1L, apply_chatgpt_filter = TRUE, forced_terms = character(0)) {
  forced_terms <- normalize_author_keywords_unique_app(forced_terms)
  forced_terms <- unique(forced_terms[nzchar(forced_terms)])
  if (!nrow(all_ranked)) {
    return(list(
      selected = tibble(term = character(), topic = integer(), degree = numeric(), doc_freq = integer(),
                        tfidf_sum = numeric(), score = numeric(), source_type = character(),
                        exact_in_document = logical(), leader = character(), is_leader = logical(),
                        topic_rank = integer()),
      edges = tibble(term1 = character(), term2 = character(), WCD = integer(), edge_type = character()),
      co_edges = tibble(term1 = character(), term2 = character(), WCD = integer()),
      flca_method = "none"
    ))
  }

  top_n <- as.integer(top_n %||% 20L)
  if (!is.finite(top_n) || top_n < 1L) top_n <- 20L

  # HARD RULE: author-made keywords are forced into Top-N.
  # They are not only boosted; they are injected if missing and occupy the first Top-N slots.
  if (length(forced_terms)) {
    if (!"author_keyword" %in% names(all_ranked)) all_ranked$author_keyword <- FALSE
    if (!"exact_surface_phrase" %in% names(all_ranked)) all_ranked$exact_surface_phrase <- TRUE
    if (!"source_type" %in% names(all_ranked)) all_ranked$source_type <- "unknown"
    existing_terms <- normalize_exact_phrase(all_ranked$term)
    missing_forced <- setdiff(forced_terms, existing_terms)
    if (length(missing_forced)) {
      forced_rows <- tibble(
        doc_id = "author_keyword_forced",
        term = missing_forced,
        source_type = "author_keyword_forced",
        exact_surface_phrase = TRUE,
        author_keyword = TRUE,
        tf_idf = 0
      )
      all_ranked <- bind_rows(all_ranked, forced_rows)
    }
    all_ranked <- all_ranked |>
      mutate(
        term = normalize_exact_phrase(term),
        author_keyword = (author_keyword %in% TRUE) | term %in% forced_terms,
        source_type = ifelse(term %in% forced_terms & !grepl("author_keyword", source_type), paste0(source_type, "+author_keyword_forced"), source_type),
        exact_surface_phrase = exact_surface_phrase %in% TRUE
      )
  }

  # Step 1. Score all extracted phrases WITHOUT building pairwise co-occurrence.
  score_tbl <- all_ranked |>
    group_by(term) |>
    summarise(
      doc_freq = n_distinct(doc_id),
      tfidf_sum = sum(tf_idf, na.rm = TRUE),
      source_type = paste(sort(unique(source_type)), collapse = "+"),
      exact_surface_phrase = all(exact_surface_phrase %in% TRUE),
      author_keyword = any(author_keyword %in% TRUE),
      .groups = "drop"
    ) |>
    mutate(
      n_words = str_count(term, "[[:space:]]+") + 1L,
      professional_single = vapply(term, .prof_single_suffix_app, logical(1)),
      length_weight = .keyword_length_weight_app(term),
      phrase_bonus = dplyr::case_when(
        n_words == 1L & professional_single ~ 3,
        n_words == 2L ~ 3,
        n_words == 3L ~ 2,
        n_words == 4L ~ 1,
        TRUE ~ 0
      ),
      entity_bonus = ifelse(str_detect(source_type, "named_entity"), 2, 0),
      author_keyword_bonus = ifelse(author_keyword, 1, 0),
      acronym_bonus = ifelse(term %in% c("nlp", "tall", "ai"), 1, 0),
      context_bonus = ifelse(str_detect(source_type, "exact_semantic_phrase"), 1, 0),
      tfidf_norm = ifelse(max(tfidf_sum, na.rm = TRUE) > 0, tfidf_sum / max(tfidf_sum, na.rm = TRUE), 0),
      df_norm = ifelse(max(doc_freq, na.rm = TRUE) > 0, doc_freq / max(doc_freq, na.rm = TRUE), 0),
      # keyword-level terms up to 4 words are allowed; weak sentence fragments are penalized upstream and here
      len_norm = dplyr::case_when(
        n_words == 1L & professional_single ~ 1.00,
        n_words == 2L ~ 0.95,
        n_words == 3L ~ 0.75,
        n_words == 4L ~ 0.60,
        TRUE ~ 0.20
      ),
      bonus_norm = pmin(phrase_bonus / 3 + entity_bonus / 2 + acronym_bonus + context_bonus, 4) / 4,
      score_raw = 100 * (0.55 * tfidf_norm + 0.25 * df_norm + 0.10 * len_norm + 0.10 * bonus_norm),
      # author keywords are protected elsewhere, but no longer monopolize score=100
      score = round(pmin(100, pmax(0, score_raw * length_weight + author_keyword_bonus * 2)), 3),
      exact_in_document = exact_surface_phrase,
      degree = 0
    ) |>
    filter(if (isTRUE(apply_chatgpt_filter)) (!vapply(term, is_weak_chatgpt_fragment, logical(1)) | author_keyword) else TRUE) |>
    arrange(desc(author_keyword), desc(score), desc(doc_freq), desc(n_words), term)

  # Hard en-tail gate before Top-N / FLCA: remove numeric phrases and phrases >4 words,
  # unless they are author-defined keywords.
  score_tbl <- strict_filter_score_tbl_en_tail_app(score_tbl, forced_terms = forced_terms, max_words = 4L)

  forced_terms_final <- unique(c(forced_terms, score_tbl$term[score_tbl$author_keyword %in% TRUE]))
  forced_terms_final <- forced_terms_final[forced_terms_final %in% score_tbl$term]

  # Step 2. Subset-free Top-N phrase selection with HARD author-keyword forcing.
  selected_terms <- select_subset_free_terms(score_tbl, top_n = top_n, forced_terms = forced_terms_final, apply_chatgpt_filter = apply_chatgpt_filter)
  # Final guard: if any author keyword was displaced by overlap/ordering, put it back and remove lowest non-author terms.
  forced_keep <- forced_terms_final[forced_terms_final %in% score_tbl$term]
  if (length(forced_keep)) {
    selected_terms <- unique(c(forced_keep, selected_terms))
    if (length(selected_terms) > top_n) {
      non_forced <- selected_terms[!(selected_terms %in% forced_keep)]
      selected_terms <- unique(c(forced_keep, head(non_forced, max(0L, top_n - length(forced_keep)))))
    }
  }

  # Step 3. Build co-occurrence edges ONLY among selected Top-N phrases.
  top_ranked <- all_ranked |>
    filter(term %in% selected_terms)

  co_edges_sel <- build_all_edges(selected_terms, top_ranked) |>
    transmute(term1 = from, term2 = to, WCD = as.integer(weight)) |>
    filter(WCD >= min_edge_docs)

  # Step 4. REAL FLCA-SIL-MA clustering from uploaded/source module.
  # This replaces the older lightweight Louvain fallback.
  flca_nodes_input <- score_tbl |>
    filter(term %in% selected_terms) |>
    transmute(name = term, value = score, value2 = score)

  flca_obj <- .apply_real_flca_to_nodes_edges(flca_nodes_input, co_edges_sel, verbose = FALSE)

  selected <- .make_selected_from_flca(score_tbl, selected_terms, flca_obj, co_edges_sel)

  # LAST GATE: force author-made keywords into the final selected node table.
  # This is after FLCA, so even if FLCA/major sampling drops isolated author terms,
  # they are reinserted and the lowest non-author terms are removed to keep Top-N.
  selected <- .force_author_terms_into_selected(selected, score_tbl, forced_terms_final, top_n = top_n)
  selected <- selected |>
    mutate(
      term = normalize_author_keyword_for_tail_app(term),
      author_keyword = (author_keyword %in% TRUE) | term %in% forced_terms_final,
      .strict_ok = vapply(seq_along(term), function(i) strict_en_tail_term_ok_app(term[[i]], author_keyword = author_keyword[[i]], max_words = 4L), logical(1))
    ) |>
    filter(.strict_ok, nzchar(term)) |>
    select(-.strict_ok) |>
    arrange(desc(author_keyword), desc(score), term) |>
    distinct(term, .keep_all = TRUE) |>
    slice_head(n = top_n)
  selected_terms <- selected$term

  # Keep only final FLCA leader-follower edges among selected nodes.
  edges_flca <- flca_obj$edges |>
    filter(term1 %in% selected$term, term2 %in% selected$term, term1 != term2) |>
    mutate(WCD = as.integer(round(WCD)), edge_type = "leader_follower")

  # If module returns no usable relation, keep deterministic leader-follower fallback per FLCA cluster.
  if (!nrow(edges_flca) && nrow(co_edges_sel)) {
    wcd_lookup <- co_edges_sel |>
      mutate(a = pmin(term1, term2), b = pmax(term1, term2)) |>
      select(a, b, WCD)

    followers <- selected |> filter(!is_leader)
    edges_flca <- followers |>
      mutate(a = pmin(leader, term), b = pmax(leader, term)) |>
      left_join(wcd_lookup, by = c("a", "b")) |>
      mutate(WCD = ifelse(is.na(WCD), 1L, as.integer(WCD))) |>
      transmute(term1 = leader, term2 = term, WCD = as.integer(WCD), edge_type = "leader_follower")
  }

  if (nrow(edges_flca)) {
    score_map <- selected |> transmute(term1 = term, leader_value = score)
    edges_flca <- edges_flca |>
      left_join(score_map, by = "term1") |>
      arrange(term2, desc(WCD), desc(leader_value), term1) |>
      group_by(term2) |>
      slice_head(n = 1) |>
      ungroup() |>
      select(term1, term2, WCD, edge_type)
  }

  # Re-mark leaders from actual final FLCA relation set; fallback = highest score per cluster.
  leader_candidates <- unique(edges_flca$term1)
  leader_map <- selected |>
    group_by(topic) |>
    arrange(desc(term %in% leader_candidates), desc(score), term, .by_group = TRUE) |>
    summarise(topic_leader = first(term), .groups = "drop")

  selected <- selected |>
    select(-any_of(c("leader","is_leader"))) |>
    left_join(leader_map, by = "topic") |>
    mutate(
      leader = topic_leader,
      is_leader = term == leader
    ) |>
    select(-topic_leader) |>
    group_by(topic) |>
    arrange(desc(is_leader), desc(score), term, .by_group = TRUE) |>
    mutate(topic_rank = row_number()) |>
    ungroup()

  list(selected = selected, edges = edges_flca, co_edges = co_edges_sel, flca_method = flca_obj$method)
}

make_export_tables <- function(selected, edges) {
  if (!nrow(selected)) {
    return(list(
      nodes = tibble(name = character(), value = numeric(), value2 = numeric()),
      edges = tibble(term1 = character(), term2 = character(), WCD = integer())
    ))
  }

  edges_out <- edges |>
    transmute(
      term1 = as.character(term1),
      term2 = as.character(term2),
      WCD = as.integer(WCD)
    ) |>
    filter(!is.na(WCD), WCD > 0L, nzchar(term1), nzchar(term2), term1 != term2)

  value2_tbl <- bind_rows(
    edges_out |> transmute(name = term1, WCD = WCD),
    edges_out |> transmute(name = term2, WCD = WCD)
  ) |>
    group_by(name) |>
    summarise(value2 = sum(WCD, na.rm = TRUE), .groups = "drop")

  nodes_out <- selected |>
    transmute(
      name = as.character(term),
      value = round(as.numeric(score), 3)
    ) |>
    left_join(value2_tbl, by = "name") |>
    mutate(value2 = ifelse(is.na(value2), 0, value2)) |>
    arrange(desc(value), name)

  list(nodes = nodes_out, edges = edges_out)
}

validate_export_tables <- function(nodes, edges) {
  checks <- list(
    nodes_n = nrow(nodes),
    edges_n = nrow(edges),
    followers_unique = !anyDuplicated(edges$term2),
    wcd_positive_integer = all(edges$WCD > 0 & edges$WCD == as.integer(edges$WCD)),
    value2_verified = TRUE
  )

  value2_check <- bind_rows(
    edges |> transmute(name = term1, WCD = WCD),
    edges |> transmute(name = term2, WCD = WCD)
  ) |>
    group_by(name) |>
    summarise(value2_calc = sum(WCD), .groups = "drop")

  node_check <- nodes |>
    left_join(value2_check, by = "name") |>
    mutate(value2_calc = ifelse(is.na(value2_calc), 0, value2_calc))

  checks$value2_verified <- all(abs(node_check$value2 - node_check$value2_calc) < 1e-9)

  tibble(
    check = names(checks),
    result = unlist(checks, use.names = FALSE)
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #run {
        background-color: #1E88E5;
        color: white;
        border-color: #1565C0;
        font-weight: 700;
        width: 100%;
        margin-top: 8px;
      }
      #run:hover {
        background-color: #1565C0;
        color: white;
      }
      .run-help {
        background: #E3F2FD;
        border-left: 4px solid #1E88E5;
        padding: 8px 10px;
        margin-top: 8px;
        border-radius: 6px;
        font-size: 13px;
      }
      .done-box {
        background: #E8F5E9;
        border-left: 4px solid #43A047;
        padding: 8px 10px;
        margin-bottom: 10px;
        border-radius: 6px;
        font-size: 13px;
        color: #1B5E20;
      }
      .working-box {
        background: #FFF8E1;
        border-left: 4px solid #F9A825;
        padding: 8px 10px;
        margin-bottom: 10px;
        border-radius: 6px;
        font-size: 13px;
        color: #5D4037;
      }
      .scale-wrap {
        width: 100%;
        height: 16px;
        background: #EEEEEE;
        border-radius: 10px;
        overflow: hidden;
        margin-top: 8px;
        border: 1px solid #CCCCCC;
      }
      .scale-bar {
        height: 100%;
        width: 100%;
        background: linear-gradient(90deg, #42A5F5, #1E88E5, #42A5F5);
        background-size: 200% 100%;
        animation: scaleMove 1.2s linear infinite;
      }
      @keyframes scaleMove {
        0% { background-position: 200% 0; }
        100% { background-position: -200% 0; }
      }
      .complete-scale-bar {
        height: 100%;
        width: 100%;
        background: #43A047;
      }
      .aacBox{
        font-size: 34px; font-weight: 800; text-align: center;
        padding: 10px 0; border-radius: 14px;
        border: 1px solid #ddd; margin-bottom: 12px;
        background: #fffdf7;
      }
      .subtxt{ text-align:center; color:#666; margin-top:-6px; margin-bottom:10px;}
      .metricRow{ display:flex; gap:10px; flex-wrap:wrap; margin-bottom:12px; }
      .metricCard{ flex:1; min-width:180px; border:1px solid #ddd; border-radius:12px; padding:12px; background:#fafafa; }
      .metricCard b{ font-size:24px; }
      .manualKwBox{
        background:#E3F2FD;
        border-left:4px solid #1E88E5;
        padding:8px 10px;
        margin:8px 0 10px 0;
        border-radius:6px;
      }
      .manualKwBox textarea{
        font-size:13px;
        line-height:1.35;
      }
      .openaiApiStatusReady{
        background:#E8F5E9;
        border-left:5px solid #2E7D32;
        color:#1B5E20;
        padding:10px 12px;
        margin:8px 0 12px 0;
        border-radius:8px;
        font-size:14px;
      }
      .openaiApiStatusNotReady{
        background:#FFEBEE;
        border-left:5px solid #C62828;
        color:#7F0000;
        padding:10px 12px;
        margin:8px 0 12px 0;
        border-radius:8px;
        font-size:14px;
      }
      .openaiApiStatusDisabled{
        background:#FFF8E1;
        border-left:5px solid #F9A825;
        color:#5D4037;
        padding:10px 12px;
        margin:8px 0 12px 0;
        border-radius:8px;
        font-size:14px;
      }
      .openaiApiStatusTitle{
        font-weight:800;
        font-size:15px;
      }
    "))
  ),
  titlePanel("Topic Detection Demo v5.7.1 — leader-column fix + XLSX export"),
  uiOutput("openai_api_home_status"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file",
        "Upload data file",
        accept = c(".csv", ".xlsx", ".xls", ".txt", ".docx", ".pdf")
      ),
      checkboxInput("use_url_input", "Analyze URL link instead of uploaded file", value = FALSE),
      textInput("url_input", "URL link for HTML/PDF/TXT/CSV/XLSX/DOCX", value = "", placeholder = "https://example.com/article.html or https://example.com/file.pdf"),
      uiOutput("text_col_ui"),
      uiOutput("doc_col_ui"),
      checkboxInput("exclude_refs", "Exclude references / DOI / URL-like lines", value = TRUE),
      checkboxInput("protect_author_keywords", "Author-keyword protection: force author-defined keywords into candidates and protect them in Top-N", value = TRUE),
      tags$div(
        class = "manualKwBox",
        textAreaInput(
          "manual_author_keywords",
          "Author input keywords forced into Step 3",
          value = "",
          rows = 5,
          placeholder = "One keyword per line, or separate keywords by ; or ,\nExample:\ntext mining; natural language processing; TALL; NLP"
        ),
        tags$small("Default is empty. These user-provided keywords are forced into Step 3 because some author keywords are difficult to extract from PDFs or DOCX files.")
      ),
      checkboxInput("apply_chatgpt_filter", "ChatGPT-like filtering: remove weak phrase fragments and prefer meaningful exact phrases", value = TRUE),
      checkboxInput("semantic_top20_by_row", "Semantic phrase Top 20 by row for CSV download (keeps original v2 semantic extraction rule)", value = FALSE),
      selectInput(
        "api_extract_mode",
        "API / TF-IDF extraction source",
        choices = c(
          "Auto-detect: API if OPENAI_API_KEY exists, otherwise TF-IDF" = "auto",
          "API-first: try ChatGPT API, then TF-IDF fallback" = "api_first",
          "API-only: require OPENAI_API_KEY" = "api_only",
          "TF-IDF-only: no API call" = "tfidf_only"
        ),
        selected = "auto"
      ),
      tags$small(HTML("API modes read <code>OPENAI_API_KEY</code> from the environment. See <code>README_API_ENV.md</code>.")),
      uiOutput("openai_api_sidebar_status"),
      selectInput(
        "extract_mode",
        "Extraction mode",
        choices = c(
          "single terms only" = "single",
          "bigrams/trigrams" = "ngram",
          "semantic phrases (ChatGPT-like exact)" = "semantic",
          "named-entity phrases" = "ner",
          "semantic phrases + named entities" = "combined"
        ),
        selected = "semantic"
      ),
      numericInput("top_n", "Top-N topic phrases", value = 20, min = 5, max = 100, step = 1),
      numericInput("min_edge_docs", "Min edge docs", value = 1, min = 1, step = 1),
      actionButton("run", "Run / re-run analysis"),
      br(),
      br(),
      tags$div(
        class = "run-help",
        tags$b("Download outputs"),
        tags$br(),
        downloadButton("download_xlsx", "Download nodes/edges XLSX"),
        tags$br(), tags$br(),
        downloadButton("download_network_png", "Download network PNG"),
        tags$br(), tags$br(),
        downloadButton("download_network_html", "Download network HTML"),
        tags$br(), tags$br(),
        downloadButton("download_ssplot", "Download SSplot PNG"),
        tags$br(), tags$br(),
        downloadButton("download_kano_png", "Download Kano PNG"),
        tags$br(), tags$br(),
        downloadButton("download_sankey_png", "Download Sankey PNG"),
        tags$br(), tags$br(),
        downloadButton("download_rowwise_phrases_csv", "Download row-wise semantic phrases CSV (.csv)"),
        tags$br(),
        tags$small("Click the button normally; do not use browser Save link as.")
      ),
      div(
        class = "run-help",
        strong("Behavior: "),
        "A file upload runs once automatically. After changing parameters, click Run / re-run analysis to rebuild the top-N topics. ",
        "The XLSX export contains exactly two analysis sheets: nodes and edges."
      )
    ),
    mainPanel(
      verbatimTextOutput("run_status"),
      uiOutput("done_status"),
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Preview", DTOutput("preview")),
        tabPanel("Top 20 phrases", DTOutput("topic_terms")),
        tabPanel("Network", tags$div(class = "run-help", HTML("<b>Legend:</b> star-shaped nodes (★) are cluster leaders; circles are followers. Only final leader → follower edges are shown.")), visNetworkOutput("net", height = "700px")),
        tabPanel("SSplot", plotOutput("ssplot", height = "760px")),
        tabPanel(span(style = "color:#c62828; font-weight:700;", "AAC dashboard"), uiOutput("aac_cards"), plotOutput("aac_plot", height = "420px"), DTOutput("aac_table"), tags$hr(), tags$h4("Interactive AAC formula demo"), div(class = "aacBox", textOutput("aac_demo_txt")), div(class = "subtxt", textOutput("aac_demo_abc_txt")), sliderInput("aac_demo_B", "Adjust B: A = 2.99 - B, C = 0.01", min = 0.01, max = 1.45, value = 0.8, step = 0.001), plotOutput("aac_demo_plot", height = "360px")),
        tabPanel("Kano",
                 uiOutput("kano_dashboard_ui"),
                 tags$hr(),
                 tags$h4("Static Kano plot"),
                 plotOutput("kano_plot", height = "650px")
        ),
        tabPanel("Sankey", plotOutput("sankey_plot", height = "650px"), tags$h4("SankeyMATIC code for sankeymatic.com"), verbatimTextOutput("sankey_code")),
        tabPanel("Cluster summary", DTOutput("cluster_summary")),
        tabPanel("Extraction log", DTOutput("extraction_log")),
        tabPanel("Final Report", DTOutput("final_report")),
        tabPanel("Processing log", DTOutput("processing_log")),
        tabPanel("Export nodes", DTOutput("export_nodes")),
        tabPanel("Export edges", DTOutput("export_edges")),
        tabPanel("Validation", DTOutput("validation")),
        tabPanel("All extracted phrases", DTOutput("extracted_terms")),
        tabPanel("Topic sizes", plotOutput("topic_sizes", height = "420px")),
        tabPanel("About", htmlOutput("about_html"))
      )
    )
  )
)



# ---- Built-in fallback Kano plot, used when external kano.R helper is absent ----
.render_kano_fallback_app <- function(nodes_df, out_png = NULL, title = "Kano plot") {
  nd <- as.data.frame(nodes_df %||% data.frame(), stringsAsFactors = FALSE)
  if (!nrow(nd)) {
    if (!is.null(out_png)) {
      grDevices::png(out_png, width = 1200, height = 900, res = 150)
      graphics::plot.new()
      graphics::text(0.5, 0.5, "No nodes available for Kano plot", cex = 1.2)
      grDevices::dev.off()
      return(out_png)
    }
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No nodes available for Kano plot", cex = 1.2)
    return(invisible(NULL))
  }
  if (!"name" %in% names(nd)) nd$name <- as.character(nd[[1]])
  if (!"value" %in% names(nd)) nd$value <- seq_len(nrow(nd))
  if (!"value2" %in% names(nd)) nd$value2 <- nd$value

  nd$value <- suppressWarnings(as.numeric(nd$value))
  nd$value2 <- suppressWarnings(as.numeric(nd$value2))
  nd$value[!is.finite(nd$value)] <- 0
  nd$value2[!is.finite(nd$value2)] <- 0

  scale01 <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x[!is.finite(x)] <- 0
    rg <- range(x, na.rm = TRUE)
    if (!is.finite(rg[1]) || !is.finite(rg[2]) || abs(diff(rg)) < 1e-12) return(rep(0.5, length(x)))
    (x - rg[1]) / diff(rg)
  }

  nd$x <- scale01(nd$value)
  nd$y <- scale01(nd$value2)

  if (!is.null(out_png)) {
    grDevices::png(out_png, width = 1400, height = 1000, res = 150)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  graphics::plot(
    nd$x, nd$y,
    xlim = c(0, 1), ylim = c(0, 1),
    xlab = "Importance score, normalized 0-100",
    ylab = "Edge / adjacent strength, normalized",
    main = title,
    pch = 21, bg = "lightblue", col = "grey30", cex = 1.8
  )
  graphics::abline(v = 0.5, h = 0.5, lty = 2, col = "red")
  graphics::text(nd$x, nd$y, labels = nd$name, pos = 4, cex = 0.75, xpd = NA)
  graphics::mtext("Fallback Kano renderer in app.R; external kano.R was not required.", side = 1, line = 4, cex = 0.8, col = "grey40")

  invisible(out_png %||% TRUE)
}
# ---- End built-in fallback Kano plot ----




.ensure_nonempty_kano_png_app <- function(path, nodes_df = NULL) {
  if (is.null(path) || !nzchar(path)) return(invisible(FALSE))
  need <- !file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size < 1000
  if (isTRUE(need)) {
    .render_kano_fallback_app(nodes_df %||% data.frame(), out_png = path, title = "Kano plot")
  }
  invisible(file.exists(path) && file.info(path)$size > 1000)
}


server <- function(input, output, session) {

  # v6 OpenAI API homepage status: show whether OPENAI_API_KEY is available.
  mask_openai_key_app <- function(key) {
    key <- trimws(as.character(key %||% ""))
    if (!nzchar(key)) return("<empty>")
    n <- nchar(key)
    if (n <= 12) return("********")
    paste0(substr(key, 1, 7), "...", substr(key, n - 3, n))
  }

  openai_api_status_ui_app <- reactive({
    key <- Sys.getenv("OPENAI_API_KEY", unset = "")
    key_ok <- nzchar(trimws(key))
    mode <- input$api_extract_mode %||% "auto"

    mode_label <- switch(
      mode,
      auto = "Auto-detect: API will run when the key is available; otherwise TF-IDF fallback is used.",
      api_first = "API-first: the app will try ChatGPT API first, then fall back to TF-IDF if the call fails.",
      api_only = "API-only: the app requires a valid OpenAI API key.",
      tfidf_only = "TF-IDF-only: API calls are disabled even if a key exists.",
      "Unknown mode."
    )

    if (isTRUE(key_ok) && identical(mode, "tfidf_only")) {
      return(tags$div(
        class = "openaiApiStatusDisabled",
        tags$div(class = "openaiApiStatusTitle", "OpenAI API status: KEY FOUND, BUT DISABLED"),
        tags$div(HTML(paste0("<b>OPENAI_API_KEY:</b> ", htmltools::htmlEscape(mask_openai_key_app(key))))),
        tags$div(HTML("<b>Current mode:</b> TF-IDF-only. Change API / TF-IDF extraction source to Auto, API-first, or API-only to use the API."))
      ))
    }

    if (isTRUE(key_ok)) {
      tags$div(
        class = "openaiApiStatusReady",
        tags$div(class = "openaiApiStatusTitle", "OpenAI API status: READY"),
        tags$div(HTML(paste0("<b>OPENAI_API_KEY:</b> ", htmltools::htmlEscape(mask_openai_key_app(key))))),
        tags$div(HTML(paste0("<b>Current mode:</b> ", htmltools::htmlEscape(mode_label))))
      )
    } else {
      tags$div(
        class = "openaiApiStatusNotReady",
        tags$div(class = "openaiApiStatusTitle", "OpenAI API status: NOT READY"),
        tags$div(HTML("<b>OPENAI_API_KEY:</b> empty or not detected.")),
        tags$div(HTML("The app will use TF-IDF fallback unless API-only is selected."))
      )
    }
  })

  output$openai_api_home_status <- renderUI({
    openai_api_status_ui_app()
  })

  output$openai_api_sidebar_status <- renderUI({
    openai_api_status_ui_app()
  })
  run_counter <- reactiveVal(0L)
  auto_file_name <- reactiveVal(NULL)
  last_done <- reactiveVal(NULL)
  processing_now <- reactiveVal(FALSE)
  processing_log <- reactiveVal(tibble(
    time = character(),
    status = character(),
    step = character(),
    elapsed_sec = numeric(),
    n = character(),
    details = character(),
    preview = character()
  ))

  flush_console_app <- function() {
    try(utils::flush.console(), silent = TRUE)
    invisible(NULL)
  }

  log_pipeline_step <- function(step, status = "END", n = NA, details = "", preview = character(0), start_time = NULL) {
    # Pure diagnostic log: only Top-20 term extraction stages are shown.
    # The last START row without a matching END row is the current pause point.
    keep <- grepl("^[1-4]([a-z])?\\.", as.character(step)) || identical(as.character(step), "ERROR")
    if (!isTRUE(keep)) return(invisible(NULL))

    now <- Sys.time()
    elapsed <- if (!is.null(start_time)) round(as.numeric(difftime(now, start_time, units = "secs")), 2) else NA_real_
    n_chr <- if (length(n) == 0 || all(is.na(n))) "" else paste(paste0(names(n), ifelse(nzchar(names(n)), "=", ""), as.character(n)), collapse = "; ")
    pv <- preview %||% character(0)
    pv <- as.character(pv)
    pv <- pv[nzchar(pv)]
    pv <- paste(head(unique(pv), 10), collapse = " | ")
    row <- tibble(
      time = format(now, "%H:%M:%S"),
      status = as.character(status),
      step = as.character(step),
      elapsed_sec = elapsed,
      n = n_chr,
      details = as.character(details),
      preview = pv
    )
    processing_log(bind_rows(processing_log(), row))
    message("[TOP20 TERM EXTRACTION] ", row$time, " | ", row$status, " | ", row$step,
            " | elapsed=", row$elapsed_sec, "s",
            if (nzchar(row$n)) paste0(" | ", row$n) else "",
            if (nzchar(row$details)) paste0(" | ", row$details) else "",
            if (nzchar(row$preview)) paste0(" | preview=", row$preview) else "")
    flush_console_app()
    invisible(row)
  }

  using_url <- reactive({
    isTRUE(input$use_url_input) && nzchar(trimws(input$url_input %||% ""))
  })

  active_source_label <- reactive({
    if (using_url()) trimws(input$url_input %||% "") else if (!is.null(input$file)) input$file$name else ""
  })

  uploaded_bundle <- reactive({
    if (using_url()) return(NULL)
    req(input$file)
    if (has_nodes_edges_sheets(input$file$datapath, input$file$name)) {
      read_nodes_edges_bundle(input$file$datapath, input$file$name)
    } else {
      NULL
    }
  })

  raw_data <- reactive({
    if (using_url()) {
      return(safe_read_url_input(input$url_input))
    }
    req(input$file)
    if (!is.null(uploaded_bundle())) {
      uploaded_bundle()$nodes
    } else {
      safe_read_upload(input$file$datapath, input$file$name)
    }
  })

  # Auto-run only after the file is uploaded AND the text-column input exists.
  # First show the visible scale bar, then start analysis after the UI flushes.
  observe({
    if (using_url()) {
      req(nzchar(trimws(input$url_input %||% "")))
    } else {
      req(input$file)
    }
    is_bundle <- !is.null(uploaded_bundle())
    if (!is_bundle) req(input$text_col)
    key <- paste(active_source_label(), if (is_bundle) "nodes_edges_bundle" else input$text_col, if (is_bundle) "" else (input$doc_col %||% ""), sep = "||")
    if (!identical(auto_file_name(), key)) {
      auto_file_name(key)
      processing_now(TRUE)
      processing_log(tibble(time = character(), status = character(), step = character(), elapsed_sec = numeric(), n = character(), details = character(), preview = character()))
      last_done(paste0(
        "<b>File uploaded.</b> Automatic processing has started for <code>",
        htmltools::htmlEscape(active_source_label()),
        "</code>."
      ))
      showNotification(if (using_url()) "URL link detected. Automatic analysis started." else "File uploaded. Automatic analysis started.", type = "message", duration = 4)

      session$onFlushed(function() {
        run_counter(isolate(run_counter()) + 1L)
      }, once = TRUE)
    }
  })

  observeEvent(input$run, {
    if (using_url()) req(nzchar(trimws(input$url_input %||% ""))) else req(input$file)
    processing_now(TRUE)
    processing_log(tibble(time = character(), status = character(), step = character(), elapsed_sec = numeric(), n = character(), details = character(), preview = character()))
    last_done("<b>Re-run started.</b> Parameters were changed; rebuilding topic phrases and network.")
    showNotification("Re-running analysis...", type = "message", duration = 3)

    session$onFlushed(function() {
      run_counter(isolate(run_counter()) + 1L)
    }, once = TRUE)
  }, ignoreInit = TRUE)

  output$text_col_ui <- renderUI({
    req(raw_data())
    if (!is.null(uploaded_bundle())) {
      div(class = "run-help", HTML("<b>Detected upload mode:</b> nodes+edges XLSX bundle. The app will draw visuals directly from the uploaded <code>nodes</code>, <code>edges</code>, and optional <code>sil_df</code> sheets."))
    } else {
      cols <- names(raw_data())
      if ("text" %in% cols) {
        selectInput("text_col", "Text column", choices = cols, selected = "text")
      } else {
        selectInput("text_col", "Text column", choices = cols, selected = cols[1])
      }
    }
  })

  output$doc_col_ui <- renderUI({
    req(raw_data())
    if (!is.null(uploaded_bundle())) {
      return(NULL)
    }
    cols <- c("", names(raw_data()))
    selected <- if ("doc_id" %in% cols) "doc_id" else ""
    selectInput("doc_col", "Document ID column (optional)", choices = cols, selected = selected)
  })

  output$preview <- renderDT({
    req(raw_data())
    datatable(head(raw_data(), 20), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$run_status <- renderText({
    if (!using_url() && is.null(input$file)) {
      return("Status: upload a file or check URL-link mode and enter a URL. After input, analysis starts automatically. A nodes+edges XLSX bundle is also supported.")
    }
    if (isTRUE(input$use_url_input) && !using_url()) {
      return("Status: URL-link mode is checked. Enter an http(s) URL to start.")
    }
    if (isTRUE(processing_now())) {
      return("Status: processing... the visible scale bar below indicates analysis is running.")
    }
    paste0(
      "Status: ready. Analysis run counter = ", run_counter(),
      ". Upload triggers automatic analysis. Parameter changes require Run / re-run analysis."
    )
  })

  output$done_status <- renderUI({
    x <- last_done()
    if (is.null(x)) return(NULL)

    if (isTRUE(processing_now())) {
      return(
        div(
          class = "working-box",
          HTML(x),
          div(class = "scale-wrap", div(class = "scale-bar")),
          tags$small("Processing is running. Please wait; the result will appear when the bar finishes.")
        )
      )
    }

    div(
      class = "done-box",
      HTML(x),
      div(class = "scale-wrap", div(class = "complete-scale-bar"))
    )
  })

  analysis <- eventReactive(run_counter(), {
    if (using_url()) req(nzchar(trimws(input$url_input %||% ""))) else req(input$file)
    analysis_start_time <- Sys.time()
    processing_log(tibble(time = character(), status = character(), step = character(), elapsed_sec = numeric(), n = character(), details = character(), preview = character()))
    log_pipeline_step("0. Start", details = paste0("source=", ifelse(using_url(), "URL", "upload"), "; input=", active_source_label()), start_time = analysis_start_time)
    tryCatch({
      withProgress(message = "Processing document", value = 0, {
        isolate({
          if (!is.null(uploaded_bundle())) {
            incProgress(0.25, detail = "Detected nodes+edges XLSX bundle...")
            log_pipeline_step("0. Nodes+edges bundle", details = "Detected XLSX nodes/edges bundle; phrase extraction is skipped.", start_time = analysis_start_time)
            out <- build_analysis_from_nodes_edges_bundle(uploaded_bundle(), top_n = as.integer(input$top_n %||% 20L))
            log_pipeline_step("4. FLCA/visual data ready", n = c(nodes = nrow(out$selected), edges = nrow(out$edges), clusters = length(unique(out$selected$topic %||% out$selected$carac %||% 1))), details = "Loaded bundle and built visual tables.", preview = out$selected$term %||% out$selected$name, start_time = analysis_start_time)
            incProgress(0.65, detail = "Drawing visuals directly from uploaded nodes / edges / sil_df...")
            processing_now(FALSE)
            last_done(paste0(
              "<b>Processing complete.</b> Uploaded nodes+edges XLSX detected. ",
              nrow(out$selected), " node(s) and ", nrow(out$edges), " edge(s) were loaded directly for visualization. ",
              "Network, SSplot, Kano, Sankey, and AAC dashboard are ready."
            ))
            showNotification("Nodes+edges XLSX loaded. Visuals were built directly from the uploaded sheets.", type = "message", duration = 5)
            updateTabsetPanel(session, "main_tabs", selected = "Network")
            incProgress(0.10, detail = "Finished.")
            out
          } else {
            incProgress(0.05, detail = "Preparing document units...")
            chosen_text_col <- if (!is.null(input$text_col) && nzchar(input$text_col)) input$text_col else names(raw_data())[1]
            chosen_doc_col <- if (!is.null(input$doc_col) && nzchar(input$doc_col)) input$doc_col else NULL
            docs_tbl <- prep_docs(
              raw_data(),
              text_col = chosen_text_col,
              doc_col = chosen_doc_col
            )
            docs_tbl_raw_for_author_kw <- docs_tbl
            log_pipeline_step("0. Prepared document units", n = nrow(docs_tbl), details = paste0("text_col=", chosen_text_col, "; doc_col=", chosen_doc_col %||% "<none>"), preview = head(docs_tbl$text, 3), start_time = analysis_start_time)

            incProgress(0.15, detail = "Cleaning references, DOI, and URL-like noise...")
            if (isTRUE(input$exclude_refs)) {
              docs_tbl <- remove_reference_noise(docs_tbl)
            }
            log_pipeline_step("0. Cleaned text", n = nrow(docs_tbl), details = paste0("exclude_refs=", isTRUE(input$exclude_refs), "; metadata/reference stripping applied before phrase extraction"), preview = head(docs_tbl$text, 3), start_time = analysis_start_time)
            # Original v2 semantic rule restored:
            # keep the original document units (rows/pages/paragraphs) for TF-IDF and phrase scoring.
            # The row-wise checkbox is used for row-wise CSV output, not to collapse all rows into one document.
            incProgress(0.20, detail = "Extracting author keywords and exact semantic phrases...")
            protect_author_keywords <- is.null(input$protect_author_keywords) || isTRUE(input$protect_author_keywords)
            apply_chatgpt_filter <- is.null(input$apply_chatgpt_filter) || isTRUE(input$apply_chatgpt_filter)
            # IMPORTANT: extract author keywords from the raw document table, before reference/noise cleanup can collapse or damage headings.
            detected_author_keywords <- if (protect_author_keywords) extract_author_keywords_from_docs(docs_tbl_raw_for_author_kw) else character(0)
            manual_author_keywords <- parse_manual_author_keywords_app(input$manual_author_keywords %||% "")
            author_keywords <- unique(c(detected_author_keywords, manual_author_keywords))
            if (FALSE) log_author_keywords_debug(author_keywords, docs_tbl_raw_for_author_kw, docs_tbl, protect_author_keywords)
            log_pipeline_step(
              "0. Author keywords detected",
              n = c(detected = length(detected_author_keywords), manual = length(manual_author_keywords), total = length(author_keywords)),
              details = paste0("author_keyword_protection=", protect_author_keywords, "; manual keywords are forced into Step 3"),
              preview = author_keywords,
              start_time = analysis_start_time
            )
            if (!length(author_keywords)) {
              log_pipeline_step("0. Author-keyword injection skipped", n = 0, details = "No detected or manually supplied author keywords; proceeding directly to Step 1 candidate extraction.", preview = character(0), start_time = analysis_start_time)
            }

            incProgress(0.08, detail = "Step 1/4: extracting 1-4 word keyword-level terms...")
            log_pipeline_step(
              "1. Phrase extraction",
              status = "START",
              n = c(docs = nrow(docs_tbl), chars = sum(nchar(docs_tbl$text), na.rm = TRUE)),
              details = "extract 1-4 word keyword-level terms; single words allowed only with ...zation/...ization/...isation/...ibility suffixes",
              preview = character(0),
              start_time = analysis_start_time
            )
            extraction_choice <- resolve_api_tf_idf_doc_terms_app(
              docs_tbl,
              api_mode = input$api_extract_mode %||% "auto",
              tfidf_mode = input$extract_mode,
              top_n = as.integer(input$top_n %||% 20L),
              author_keywords = author_keywords
            )
            extraction_engine_used <- extraction_choice$engine_used
            api_mode_used <- extraction_choice$api_mode
            api_key_available <- extraction_choice$api_available
            api_attempted <- extraction_choice$api_attempted
            api_used <- extraction_choice$api_used
            api_error <- extraction_choice$api_error
            extracted_step1 <- extraction_choice$doc_terms
            professional_expansion_step1 <- expand_professional_singles_using_docs_app(
              docs_tbl,
              extracted_step1,
              author_keywords = author_keywords,
              max_words = 4L
            )
            extracted_step1 <- professional_expansion_step1$doc_terms
            professional_expansion_map <- professional_expansion_step1$expansion_map
            log_pipeline_step(
              "1b. Professional single-term phrase recovery",
              status = "END",
              n = c(expanded = nrow(professional_expansion_map)),
              details = if (nrow(professional_expansion_map)) paste(paste(professional_expansion_map$from, "->", professional_expansion_map$to), collapse = "; ") else "no 2-4 word context phrase found for professional single terms",
              preview = if (nrow(professional_expansion_map)) professional_expansion_map$to else character(0),
              start_time = analysis_start_time
            )
            log_pipeline_step(
              "1a. API/TF-IDF source",
              status = "END",
              n = c(rows = nrow(extracted_step1), unique_terms = length(unique(extracted_step1$term %||% character(0)))),
              details = paste0("mode=", api_mode_used, "; engine=", extraction_engine_used, "; api_attempted=", api_attempted, "; api_used=", api_used, if (nzchar(api_error)) paste0("; api_error=", api_error) else ""),
              preview = extracted_step1$term,
              start_time = analysis_start_time
            )
            log_pipeline_step("0. Raw extraction returned", n = c(rows = nrow(extracted_step1), unique_terms = length(unique(extracted_step1$term %||% character(0)))), details = "Before hard gate.", preview = character(0), start_time = analysis_start_time)
            # HARD STEP-1 CONTRACT: before author keywords, candidates must be 2-4 words and no numeric/reference fragments.
            step1_before_n <- nrow(extracted_step1)
            extracted_step1 <- strict_filter_doc_terms_en_tail_app(extracted_step1, author_keywords = character(0), max_words = 4L)
            step1_removed_hard <- step1_before_n - nrow(extracted_step1)
            log_pipeline_step("1. Phrase extraction", status = "END", n = c(rows = nrow(extracted_step1), unique_terms = length(unique(extracted_step1$term)), hard_removed = step1_removed_hard), details = "1-4 word keyword-level terms; single words allowed only for ...zation/...ization/...isation/...ibility; broad ...tion singles removed; no metadata/reference fragments", preview = extracted_step1$term, start_time = analysis_start_time)

            incProgress(0.07, detail = "Step 2/4: applying en-tail strict filter and subset control...")
            log_pipeline_step("2. En-tail filtering", status = "START", n = c(rows = nrow(extracted_step1), unique_terms = length(unique(extracted_step1$term))), details = "blacklist/whitelist + subset removal in Top-100 pool", preview = character(0), start_time = analysis_start_time)
            extracted_step2 <- strict_filter_doc_terms_en_tail_app(extracted_step1, author_keywords = character(0), max_words = 4L)
            removed_step2 <- nrow(extracted_step1) - nrow(extracted_step2)
            log_pipeline_step("2. En-tail filtering", status = "END", n = c(rows = nrow(extracted_step2), unique_terms = length(unique(extracted_step2$term)), removed_rows = removed_step2), details = "finished en-tail strict filter", preview = extracted_step2$term, start_time = analysis_start_time)

            incProgress(0.05, detail = "Step 3/4: forcing author keywords back into candidates...")
            log_pipeline_step("3. Author keyword inclusion", status = "START", n = c(author_keywords = length(author_keywords), rows = nrow(extracted_step2)), details = "force detected author keywords into candidate table", preview = author_keywords, start_time = analysis_start_time)
            extracted <- extracted_step2
            if (protect_author_keywords || length(manual_author_keywords)) {
              extracted <- inject_author_keywords(docs_tbl, extracted, author_keywords)
              extracted <- strict_filter_doc_terms_en_tail_app(extracted, author_keywords = author_keywords, max_words = 4L)
            }
            log_pipeline_step("3. Author keyword inclusion", status = "END", n = c(rows = nrow(extracted), unique_terms = length(unique(extracted$term)), author_keywords = length(author_keywords)), details = "author keywords protected for Top-20", preview = c(author_keywords, extracted$term), start_time = analysis_start_time)
            incProgress(0.15, detail = "Computing phrase scores with optional author-keyword protection / ChatGPT-like filtering...")
            log_pipeline_step("3b. Scoring", status = "START", n = c(rows = nrow(extracted), unique_terms = length(unique(extracted$term))), details = "compute TF-IDF/document-frequency scores", preview = character(0), start_time = analysis_start_time)
            ranked <- rank_all_terms(extracted)
            ranked_before_expansion_n <- length(unique(ranked$term))
            ranked <- expand_professional_single_terms_app(ranked, forced_terms = author_keywords, max_words = 4L)
            ranked_after_expansion_n <- length(unique(ranked$term))
            log_pipeline_step(
              "3c. Professional single-term expansion",
              status = "END",
              n = c(before_unique = ranked_before_expansion_n, after_unique = ranked_after_expansion_n),
              details = "allowed suffix singles may be merged into the first/best available 2-4 word phrase; broad ...tion singles are not expansion targets",
              preview = ranked$term,
              start_time = analysis_start_time
            )
            log_pipeline_step("3b. Scoring", status = "END", n = c(rows = nrow(ranked), unique_terms = length(unique(ranked$term))), details = "scores ready before Top-20", preview = ranked |> arrange(desc(tf_idf)) |> pull(term), start_time = analysis_start_time)

            incProgress(0.20, detail = "Step 4/4: selecting Top 20, building edges, then FLCA-SIL-MA...")
            log_pipeline_step("4. Top20 + edges + FLCA-SIL-MA", status = "START", n = c(rows = nrow(ranked), top_n = as.integer(input$top_n %||% 20L)), details = "select Top-20, build edges, then run FLCA-SIL-MA", preview = character(0), start_time = analysis_start_time)
            screened <- flca_sil_ma_screen_top20(
              ranked,
              top_n = as.integer(input$top_n %||% 20L),
              min_edge_docs = as.integer(input$min_edge_docs %||% 1L),
              apply_chatgpt_filter = apply_chatgpt_filter,
              forced_terms = author_keywords
            )

            selected <- screened$selected
            edges <- screened$edges
            # FINAL CONTRACT: after FLCA, remove any non-author term that violates 2-4 words/no-numbers/no-reference rules.
            final_before_n <- nrow(selected)
            if (nrow(selected)) {
              selected <- selected |>
                mutate(
                  term = normalize_author_keyword_for_tail_app(term),
                  author_keyword = (author_keyword %in% TRUE) | term %in% normalize_author_keywords_unique_app(author_keywords),
                  .strict_ok = vapply(seq_along(term), function(i) strict_en_tail_term_ok_app(term[[i]], author_keyword = author_keyword[[i]], max_words = 4L), logical(1))
                ) |>
                filter(.strict_ok, nzchar(term)) |>
                select(-.strict_ok) |>
                distinct(term, .keep_all = TRUE)
              edges <- edges |> filter(term1 %in% selected$term, term2 %in% selected$term)
            }
            final_removed_hard <- final_before_n - nrow(selected)
            log_pipeline_step("4. Top20 + edges + FLCA-SIL-MA", status = "END", n = c(selected = nrow(selected), edges = nrow(edges), clusters = length(unique(selected$topic)), final_hard_removed = final_removed_hard), details = paste0("method=", screened$flca_method %||% "unknown", "; final hard gate checked"), preview = selected$term, start_time = analysis_start_time)

            if (!nrow(selected)) stop("No usable semantic phrases found. Try combined mode or lower filters.")

            incProgress(0.10, detail = "Building leader-follower topic network...")
            g_edges <- edges |>
              transmute(from = term1, to = term2, weight = WCD, edge_type = edge_type)

            g <- igraph::graph_from_data_frame(
              g_edges,
              directed = TRUE,
              vertices = data.frame(name = selected$term)
            )
            if (ecount(g) > 0) E(g)$weight <- g_edges$weight

            incProgress(0.10, detail = "Finished.")
            processing_now(FALSE)
            last_done(paste0(
              "<b>Processing complete.</b> ",
              nrow(selected), " top phrase(s) selected from ",
              length(unique(ranked$term)), " extracted candidate phrase(s). Co-occurrence edges were built only after Top-N selection; XLSX nodes/edges are ready. ",
              "Leader-follower network is ready. The Network tab is now displayed."
            ))
            showNotification("Processing complete. Topic network is ready. Switching to Network tab.", type = "message", duration = 5)
            updateTabsetPanel(session, "main_tabs", selected = "Network")

            export_tables <- make_export_tables(selected, edges)
            sil_df <- .compute_silhouette_from_flca_module(selected, edges) %||% compute_silhouette_table(selected, screened$co_edges)
            cluster_summary <- compute_cluster_summary(selected, edges, sil_df)
            aac_dashboard <- compute_aac_dashboard(selected, cluster_summary)
            overall_aac <- mean(aac_dashboard$AAC, na.rm = TRUE)
            scoring_formula <- "value = tfidf_sum*100 + doc_freq*5 + phrase_bonus + author_keyword_bonus + acronym_bonus + entity_bonus + context_bonus"
            extraction_log <- make_extraction_log(author_keywords, input$extract_mode, scoring_formula, length(unique(ranked$term)), nrow(selected)) |>
              bind_rows(tibble(item = "author_keyword_protection", value = as.character(protect_author_keywords))) |>
              bind_rows(tibble(item = "manual_author_keywords_forced_step3", value = paste(manual_author_keywords, collapse = "; "))) |>
              bind_rows(tibble(item = "chatgpt_like_filtering", value = as.character(apply_chatgpt_filter))) |>
              bind_rows(tibble(item = "input_source", value = ifelse(using_url(), "url_link", "uploaded_file"))) |>
              bind_rows(tibble(item = "input_url", value = ifelse(using_url(), trimws(input$url_input %||% ""), ""))) |>
              bind_rows(tibble(item = "api_extract_mode", value = as.character(api_mode_used))) |>
              bind_rows(tibble(item = "api_key_available", value = as.character(api_key_available))) |>
              bind_rows(tibble(item = "api_attempted", value = as.character(api_attempted))) |>
              bind_rows(tibble(item = "api_used", value = as.character(api_used))) |>
              bind_rows(tibble(item = "api_error", value = as.character(api_error))) |>
              bind_rows(tibble(item = "extraction_engine_used", value = as.character(extraction_engine_used))) |>
              bind_rows(tibble(item = "en_tail_keyword_engine_loaded", value = as.character(isTRUE(.en_tail_engine_loaded)))) |>
              bind_rows(tibble(item = "candidate_rule", value = ifelse(isTRUE(.en_tail_engine_loaded), "4-stage: en-tail/MDPI-style 2-4 word extraction with legacy fallback disabled -> en_tail top100 subset filter -> force author keywords -> Top20 edges + FLCA-SIL-MA", "fallback exact semantic phrase"))) |>
              bind_rows(tibble(item = "semantic_top20_by_row_csv", value = as.character(isTRUE(input$semantic_top20_by_row)))) |>
              bind_rows(tibble(item = "semantic_rule", value = "original_v2_document_unit_rule_restored")) |>
              bind_rows(tibble(item = "text_column_used", value = as.character(chosen_text_col))) |>
              bind_rows(tibble(item = "real_flca_method", value = screened$flca_method)) |>
              bind_rows(tibble(item = "flca_cluster_count", value = as.character(length(unique(selected$topic))))) |>
              bind_rows(tibble(item = "processing_log_rows", value = as.character(nrow(processing_log()))))
            final_report <- tibble(
              item = c(
                "api_tf_idf_mode_selected",
                "actual_engine_applied",
                "api_key_available",
                "api_attempted",
                "api_used",
                "api_error",
                "candidate_rule",
                "professional_single_expansion",
                "professional_expansion_map",
                "suffix_fragment_filter",
                "near_duplicate_filter",
                "top_n_selected",
                "cluster_count",
                "overall_aac"
              ),
              value = c(
                as.character(api_mode_used),
                as.character(extraction_engine_used),
                as.character(api_key_available),
                as.character(api_attempted),
                as.character(api_used),
                as.character(api_error),
                "1-4 word keyword-level terms; single words allowed only for ...zation/...ization/...isation/...ibility; broad ...tion singles removed",
                "ON: allowed suffix singles may be expanded into first/best available 2-4 word phrase before scoring",
                if (exists("professional_expansion_map") && nrow(professional_expansion_map)) paste(paste(professional_expansion_map$from, "->", professional_expansion_map$to), collapse = "; ") else "No matching 2-4 word context found",
                "ON: standalone suffix fragments such as tion/sion/ation/zation/ibility are excluded; suffix detection uses ...zation/...ization/...isation/...ibility only",
                "ON: final Top-N removes stem-like near duplicates, e.g. text analysis vs text analytics; the higher-scoring/earlier phrase is retained",
                as.character(nrow(selected)),
                as.character(length(unique(selected$topic))),
                as.character(round(overall_aac, 4))
              )
            )
            validation <- validate_export_tables(export_tables$nodes, export_tables$edges) |>
              mutate(result = as.character(result)) |>
              bind_rows(tibble(check = "author_keywords_retained", result = as.character(all(normalize_exact_phrase(author_keywords) %in% normalize_exact_phrase(export_tables$nodes$name) | !length(author_keywords))))) |>
              bind_rows(tibble(check = "missing_author_keywords", result = paste(setdiff(normalize_exact_phrase(author_keywords), normalize_exact_phrase(export_tables$nodes$name)), collapse = "; "))) |>
              bind_rows(tibble(check = "detected_author_keywords", result = paste(normalize_exact_phrase(detected_author_keywords), collapse = "; "))) |>
              bind_rows(tibble(check = "manual_author_keywords_forced_step3", result = paste(normalize_exact_phrase(manual_author_keywords), collapse = "; "))) |>
              bind_rows(tibble(check = "all_author_keywords_used_for_forcing", result = paste(normalize_exact_phrase(author_keywords), collapse = "; "))) |>
              bind_rows(tibble(check = "ssplot_same_top20", result = as.character(setequal(sil_df$term, export_tables$nodes$name)))) |>
              bind_rows(tibble(check = "overall_aac_finite", result = as.character(is.finite(overall_aac)))) |>
              bind_rows(tibble(check = "semantic_rule_restored", result = "original_v2_document_unit_rule")) |>
              bind_rows(tibble(check = "author_keyword_protection", result = as.character(protect_author_keywords))) |>
              bind_rows(tibble(check = "chatgpt_like_filtering", result = as.character(apply_chatgpt_filter))) |>
              bind_rows(tibble(check = "input_source", result = as.character(ifelse(using_url(), "url_link", "uploaded_file")))) |>
              bind_rows(tibble(check = "api_extract_mode", result = as.character(api_mode_used))) |>
              bind_rows(tibble(check = "api_key_available", result = as.character(api_key_available))) |>
              bind_rows(tibble(check = "api_attempted", result = as.character(api_attempted))) |>
              bind_rows(tibble(check = "api_used", result = as.character(api_used))) |>
              bind_rows(tibble(check = "api_error", result = as.character(api_error))) |>
              bind_rows(tibble(check = "extraction_engine_used", result = as.character(extraction_engine_used))) |>
              bind_rows(tibble(check = "en_tail_keyword_engine_loaded", result = as.character(isTRUE(.en_tail_engine_loaded)))) |>
              bind_rows(tibble(check = "semantic_top20_by_row_csv", result = as.character(isTRUE(input$semantic_top20_by_row)))) |>
              bind_rows(tibble(check = "real_flca_module_loaded", result = as.character(isTRUE(.real_flca_module_loaded)))) |>
              bind_rows(tibble(check = "real_flca_method", result = as.character(screened$flca_method))) |>
              bind_rows(tibble(check = "flca_cluster_count", result = as.character(length(unique(selected$topic)))))

            list(
              docs = docs_tbl,
              author_keywords = author_keywords,
              extracted = extracted,
              ranked = ranked,
              selected = selected,
              edges = edges,
              co_edges = screened$co_edges,
              graph = g,
              sil_df = sil_df,
              cluster_summary = cluster_summary,
              aac_dashboard = aac_dashboard,
              overall_aac = overall_aac,
              extraction_log = extraction_log,
              export_nodes = export_tables$nodes,
              export_edges = export_tables$edges,
              validation = validation,
              final_report = final_report,
              processing_log = processing_log()
            )
          }
        })
      })
    }, error = function(e) {
      log_pipeline_step("ERROR", status = "ERROR", details = conditionMessage(e), start_time = analysis_start_time)
      processing_now(FALSE)
      last_done(NULL)
      msg <- conditionMessage(e)
      if (!nzchar(msg) || identical(msg, "[object Object]")) {
        msg <- "Analysis failed during phrase screening or network construction. Try semantic phrases mode, Top-N = 20, Min edge docs = 1, and keep Exclude references checked."
      }
      showNotification(paste("Analysis failed:", msg), type = "error", duration = 8)
      stop(msg, call. = FALSE)
    })
  }, ignoreInit = TRUE)

  # Force analysis to execute immediately after upload/run_counter changes.
  # Without this, hidden tab outputs may not request analysis(), so the progress bar
  # may never appear while the Preview tab is active.
  observeEvent(run_counter(), {
    if (using_url()) req(nzchar(trimws(input$url_input %||% ""))) else req(input$file)
    if (is.null(uploaded_bundle())) req(input$text_col)
    analysis()
  }, ignoreInit = TRUE)

  output$extracted_terms <- renderDT({
    req(analysis())
    datatable(
      analysis()$ranked |>
        arrange(desc(tf_idf), doc_id, term),
      options = list(pageLength = 20, scrollX = TRUE)
    )
  })

  output$topic_terms <- renderDT({
    req(analysis())
    datatable(
      analysis()$selected |>
        arrange(desc(score), topic, term),
      options = list(pageLength = 20, scrollX = TRUE)
    )
  })

  output$export_nodes <- renderDT({
    req(analysis())
    datatable(
      analysis()$export_nodes,
      options = list(pageLength = 20, scrollX = TRUE)
    )
  })

  output$export_edges <- renderDT({
    req(analysis())
    datatable(
      analysis()$export_edges,
      options = list(pageLength = 20, scrollX = TRUE)
    )
  })

  output$validation <- renderDT({
    req(analysis())
    datatable(
      analysis()$validation,
      options = list(pageLength = 10, scrollX = TRUE, searching = FALSE)
    )
  })

  output$cluster_summary <- renderDT({
    req(analysis())
    datatable(
      analysis()$cluster_summary,
      options = list(pageLength = 20, scrollX = TRUE)
    )
  })

  output$extraction_log <- renderDT({
    req(analysis())
    datatable(
      analysis()$extraction_log,
      options = list(pageLength = 10, scrollX = TRUE, searching = FALSE)
    )
  })

  output$final_report <- renderDT({
    req(analysis())
    datatable(
      analysis()$final_report,
      options = list(pageLength = 20, scrollX = TRUE, searching = FALSE)
    )
  })

  output$processing_log <- renderDT({
    dat <- processing_log()
    if (is.null(dat) || !nrow(dat)) {
      dat <- tibble(time = character(), status = character(), step = character(), elapsed_sec = numeric(), n = character(), details = character(), preview = character())
    }
    datatable(
      dat,
      options = list(pageLength = 25, scrollX = TRUE, searching = FALSE, order = list(list(0, "asc")))
    )
  })

  draw_ssplot <- function(sil_df, cluster_summary) {
    if (!nrow(sil_df)) {
      plot.new(); text(0.5, 0.5, "No SS data available"); return(invisible(NULL))
    }
    df <- sil_df |>
      left_join(cluster_summary |> select(topic, cluster_ss, modularity_Q, leader_aac), by = "topic") |>
      mutate(
        score = suppressWarnings(as.numeric(value)),
        label = paste0(term, ifelse(is_leader, "  ★", "")),
        score_col_y = -0.13,
        aac_col_y = -0.04,
        ss_label_y = dplyr::case_when(
          is.na(ss) ~ 0,
          ss >= 0.10 ~ ss - 0.035,
          ss > 0 ~ ss / 2,
          TRUE ~ ss + 0.03
        ),
        label = factor(label, levels = rev(label[order(topic, desc(is_leader), desc(ss), term)]))
      )

    leader_df <- df |>
      filter(is_leader %in% TRUE) |>
      mutate(aac_label = format(round(leader_aac, 2), nsmall = 2))

    overall_ss <- mean(df$ss, na.rm = TRUE)
    overall_aac <- mean(cluster_summary$leader_aac, na.rm = TRUE)
    subtitle_txt <- paste0(
      "Overall SS = ", format(round(overall_ss, 3), nsmall = 3),
      "; Overall AAC = ", format(round(overall_aac, 2), nsmall = 2),
      ifelse(any(is.finite(df$modularity_Q)), paste0("; modularity Q = ", round(df$modularity_Q[which(is.finite(df$modularity_Q))[1]], 3)), "")
    )

    ggplot(df, aes(x = label, y = ss, fill = factor(topic))) +
      geom_col(width = 0.72, show.legend = TRUE) +
      coord_flip() +
      geom_hline(yintercept = 0, linewidth = 0.4) +
      geom_text(aes(y = score_col_y, label = format(round(score, 1), nsmall = 1)), size = 4.1, fontface = "bold", colour = "black") +
      geom_text(data = leader_df, aes(x = label, y = aac_col_y, label = aac_label), size = 4.8, colour = "red", fontface = "bold", inherit.aes = FALSE) +
      geom_text(aes(y = ss_label_y, label = paste0("SS=", format(round(ss, 2), nsmall = 2))), size = 3.6, colour = "black") +
      labs(
        title = "Top-N FLCA-SIL-MA SSplot",
        subtitle = subtitle_txt,
        caption = "★ = cluster leader; left column black = score; left column red = leader AAC; SS is shown on each SS bar.",
        x = NULL,
        y = "Silhouette width / SS",
        fill = "Cluster"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(face = "bold", size = 20),
        plot.caption = element_text(size = 12, hjust = 0),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        legend.position = "bottom"
      ) +
      expand_limits(y = c(min(-0.18, min(df$ss, na.rm = TRUE) - 0.1), max(0.30, max(df$ss, na.rm = TRUE) + 0.08)))
  }

  output$ssplot <- renderPlot({
    req(analysis())

    # v7b REAL SSPLOT RESTORE:
    # Use renderSSplot.R's real renderer for the screen plot.
    validate(need(exists("render_real_ssplot", mode = "function"),
                  "Real renderSSplot.R was not loaded. Please keep renderSSplot.R in the app folder."))

    tryCatch({
      ss_in <- build_real_ssplot_input_app(analysis())

      render_real_ssplot(
        sil_df = ss_in$sil_df,
        nodes = ss_in$nodes,
        results = ss_in$results,
        res = ss_in$res,
        top_n = min(as.integer(input$top_n %||% 20L), nrow(ss_in$sil_df)),
        font_scale = 1.65
      )
    }, error = function(e) {
      plot.new()
      text(0.5, 0.64, "Real SSplot drawing failed", cex = 1.45, font = 2)
      text(0.5, 0.50, conditionMessage(e), cex = 0.90)
      text(0.5, 0.38, "This is an error notice, not the simplified SSplot.", cex = 0.85)
    })
  }, width = 1600, height = 1000, res = 144)

  output$download_ssplot <- downloadHandler(
    filename = function() paste0("top20_real_SSplot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    contentType = "image/png",
    content = function(file) {
      req(analysis())

      if (!exists("render_real_ssplot", mode = "function")) {
        grDevices::png(filename = file, width = 1600, height = 1000, res = 144, bg = "white")
        on.exit({
          try({
            if (grDevices::dev.cur() > 1) grDevices::dev.off()
          }, silent = TRUE)
        }, add = TRUE)
        plot.new()
        text(0.5, 0.60, "Real SSplot PNG export failed", cex = 1.35, font = 2)
        text(0.5, 0.48, "render_real_ssplot() was not loaded. Keep renderSSplot.R in the app folder.", cex = 0.85)
        return(invisible(NULL))
      }

      tryCatch({
        ss_in <- build_real_ssplot_input_app(analysis())

        render_real_ssplot(
          sil_df = ss_in$sil_df,
          nodes = ss_in$nodes,
          results = ss_in$results,
          res = ss_in$res,
          top_n = min(as.integer(input$top_n %||% 20L), nrow(ss_in$sil_df)),
          outfile = file,
          width = 16,
          height = 10,
          dpi = 180,
          font_scale = 1.65
        )
      }, error = function(e) {
        grDevices::png(filename = file, width = 1600, height = 1000, res = 144, bg = "white")
        on.exit({
          try({
            if (grDevices::dev.cur() > 1) grDevices::dev.off()
          }, silent = TRUE)
        }, add = TRUE)
        plot.new()
        text(0.5, 0.60, "Real SSplot PNG export failed", cex = 1.35, font = 2)
        text(0.5, 0.48, conditionMessage(e), cex = 0.85)
      })
    }
  )

  output$aac_cards <- renderUI({
    req(analysis())
    cs <- analysis()$cluster_summary
    overall_ss <- mean(analysis()$sil_df$ss, na.rm = TRUE)
    overall_aac <- analysis()$overall_aac
    div(class = "metricRow",
        div(class = "metricCard", HTML(paste0("Overall SS<br><b>", format(round(overall_ss, 3), nsmall = 3), "</b>"))),
        div(class = "metricCard", HTML(paste0("Overall AAC<br><b>", format(round(overall_aac, 2), nsmall = 2), "</b>"))),
        div(class = "metricCard", HTML(paste0("Clusters<br><b>", nrow(cs), "</b>"))),
        div(class = "metricCard", HTML("Formula<br><b>AAC = r/(1+r)</b>"))
    )
  })

  output$aac_table <- renderDT({
    req(analysis())
    datatable(
      analysis()$aac_dashboard |>
        mutate(across(where(is.numeric), ~ round(.x, 4))),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$aac_plot <- renderPlot({
    req(analysis())
    df <- analysis()$aac_dashboard
    if (!nrow(df)) { plot.new(); text(0.5, 0.5, "No AAC data available"); return(invisible(NULL)) }
    ggplot(df, aes(x = factor(topic), y = AAC)) +
      geom_col() +
      geom_text(aes(label = format(round(AAC, 2), nsmall = 2)), vjust = -0.4, size = 5, fontface = "bold") +
      geom_hline(yintercept = mean(df$AAC, na.rm = TRUE), linetype = "dashed") +
      coord_cartesian(ylim = c(0, 1.05)) +
      labs(title = "AAC by cluster", subtitle = paste0("Overall AAC = ", format(round(mean(df$AAC, na.rm = TRUE), 2), nsmall = 2)), x = "Cluster", y = "AAC") +
      theme_minimal(base_size = 15)
  })

  aac_demo_vals <- reactive({
    B <- input$aac_demo_B %||% 0.8
    C <- 0.01
    B <- max(B, C)
    A <- 2.99 - B
    r <- (A * C) / (B^2)
    AAC <- r / (1 + r)
    list(A = A, B = B, C = C, r = r, AAC = AAC)
  })

  output$aac_demo_txt <- renderText({
    v <- aac_demo_vals()
    sprintf("AAC = %.2f", v$AAC)
  })

  output$aac_demo_abc_txt <- renderText({
    v <- aac_demo_vals()
    sprintf("A = %.3f, B = %.3f, C = %.2f", v$A, v$B, v$C)
  })

  output$aac_demo_plot <- renderPlot({
    v <- aac_demo_vals()
    df <- data.frame(var = factor(c("A", "B", "C"), levels = c("A", "B", "C")), val = c(v$A, v$B, v$C))
    ggplot(df, aes(x = var, y = val, group = 1)) +
      geom_line(linewidth = 1) +
      geom_point(size = 4) +
      scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5)) +
      labs(x = NULL, y = "Value", title = "Interactive AAC formula demo") +
      theme_minimal(base_size = 14)
  })

  draw_kano_plot <- function(analysis_obj) {
    df <- make_kano_data(analysis_obj$selected, analysis_obj$edges)
    if (!nrow(df)) { plot.new(); text(0.5, 0.5, "No Kano data available"); return(invisible(NULL)) }

    df <- df |>
      mutate(
        topic = as.factor(topic),
        leader = as.logical(leader),
        score_num = as.numeric(value),
        score_lbl = format(round(score_num, 1), nsmall = 1),
        term_lbl = ifelse(leader, paste0(name, " ★"), name),
        rank_score = rank(-score_num, ties.method = "first"),
        top3 = rank_score <= 3
      )

    overall_aac <- mean(analysis_obj$cluster_summary$leader_aac, na.rm = TRUE)
    center_x <- mean(df$value, na.rm = TRUE)
    center_y <- mean(df$performance, na.rm = TRUE)
    shp <- make_kano_shapes(df, x_center = center_x, y_center = center_y)
    y_top_annot <- shp$y_limits[2] - diff(shp$y_limits) * 0.05
    label_nudge_base <- diff(shp$y_limits) * 0.035

    ggplot(df, aes(x = value, y = performance)) +
      geom_polygon(data = shp$wing_poly, aes(x = x, y = y), inherit.aes = FALSE, fill = "lightskyblue1", alpha = .18, color = NA) +
      geom_path(data = shp$circle_data, aes(x = x, y = y), inherit.aes = FALSE, color = "hotpink3", linewidth = 0.4) +
      geom_polygon(data = shp$hub_circle, aes(x = x, y = y), inherit.aes = FALSE, fill = "white", color = NA) +
      geom_path(data = shp$hub_circle, aes(x = x, y = y), inherit.aes = FALSE, color = "purple", linewidth = 0.35) +
      geom_line(data = shp$lower_curve, aes(x = x, y = y), inherit.aes = FALSE, color = "blue", linewidth = 1.8) +
      geom_line(data = shp$upper_curve, aes(x = x, y = y), inherit.aes = FALSE, color = "blue", linewidth = 1.8) +
      geom_vline(xintercept = center_x, color = "red", linetype = "dotted", linewidth = 0.9, alpha = 0.85) +
      geom_hline(yintercept = center_y, color = "red", linetype = "dotted", linewidth = 0.9, alpha = 0.85) +
      geom_point(aes(size = score_num, fill = topic, shape = leader), color = "black", alpha = 0.9, stroke = 0.5) +
      geom_text(data = df |> filter(!top3), aes(label = score_lbl), color = "black", size = 3.1, fontface = "bold") +
      geom_text(data = df |> filter(top3), aes(label = score_lbl), color = "black", size = 4.2, fontface = "bold") +
      geom_text(data = df |> filter(!top3), aes(label = term_lbl), nudge_y = label_nudge_base, size = 3.0, show.legend = FALSE) +
      geom_text(data = df |> filter(top3), aes(label = term_lbl), nudge_y = label_nudge_base * 1.15, size = 4.2, fontface = "bold", show.legend = FALSE) +
      annotate("text", x = center_x, y = y_top_annot, label = paste0("Overall AAC = ", format(round(overall_aac, 2), nsmall = 2)), color = "red", fontface = "bold", size = 5.5) +
      scale_shape_manual(values = c(`FALSE` = 21, `TRUE` = 24), labels = c("Follower", "Cluster leader")) +
      scale_size(range = c(5, 15), guide = guide_legend(order = 3)) +
      coord_fixed(ratio = 1/1.5, clip = "off") +
      scale_x_continuous(limits = shp$x_limits) +
      scale_y_continuous(limits = shp$y_limits) +
      labs(
        title = "Real Kano plot",
        subtitle = "Two wings + two circles; all nodes are retained; only the top 3 nodes use larger fonts",
        x = "Importance / node score",
        y = "Performance (outW - inW)",
        fill = "Cluster",
        shape = "Role",
        size = "Score"
      ) +
      theme_minimal(base_size = 15) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom"
      )
  }

  output$kano_plot <- renderPlot({
    req(analysis())
    draw_kano_plot(analysis())
  })
  output$kano_dashboard_ui <- renderUI({
    if (requireNamespace("plotly", quietly = TRUE)) {
      tagList(
        div(class = "run-help", HTML("<b>Interactive Kano dashboard:</b> hover to inspect term, score, cluster, role, and performance. Drag to zoom; double-click to reset.")),
        plotly::plotlyOutput("kano_plotly", height = "720px")
      )
    } else {
      div(class = "run-help",
          HTML("<b>Interactive Kano dashboard requires package <code>plotly</code>.</b><br>Install it with <code>install.packages('plotly')</code>. The static Kano plot is still available below."))
    }
  })

  if (requireNamespace("plotly", quietly = TRUE)) {
    output$kano_plotly <- plotly::renderPlotly({
      req(analysis())
      p <- draw_kano_plot(analysis()) +
        ggplot2::aes(text = paste0(
          "Term: ", name,
          "<br>Score: ", round(score_num, 1),
          "<br>Cluster: ", topic,
          "<br>Role: ", ifelse(leader, "Cluster leader", "Follower"),
          "<br>Performance: ", round(performance, 2)
        ))
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(legend = list(orientation = "h"))
    })
  }


  draw_sankey_plot <- function(analysis_obj) {
    selected <- analysis_obj$selected
    edges <- analysis_obj$edges
    if (is.null(edges) || !nrow(edges)) { plot.new(); text(0.5, 0.5, "No Sankey data available"); return(invisible(NULL)) }
    e <- edges |>
      transmute(Leader = as.character(term1), follower = as.character(term2), WCD = as.numeric(WCD)) |>
      left_join(selected |> transmute(Leader = term, topic = topic), by = "Leader") |>
      arrange(topic, Leader, desc(WCD), follower)
    leaders <- e |> distinct(Leader, topic) |> arrange(topic, Leader) |> mutate(y1 = row_number())
    followers <- e |> distinct(follower, topic) |> arrange(topic, follower) |> mutate(y2 = row_number())
    ed <- e |> left_join(leaders, by = c("Leader", "topic")) |> left_join(followers, by = c("follower", "topic"))
    ggplot(ed) +
      geom_segment(aes(x = 1, xend = 2, y = y1, yend = y2, linewidth = WCD, color = factor(topic)), alpha = 0.65, arrow = arrow(length = grid::unit(0.12, "inches"), type = "closed")) +
      geom_text(data = leaders, aes(x = 0.95, y = y1, label = paste0("★ ", Leader), color = factor(topic)), hjust = 1, size = 4, fontface = "bold", show.legend = FALSE) +
      geom_text(data = followers, aes(x = 2.05, y = y2, label = follower, color = factor(topic)), hjust = 0, size = 3.8, show.legend = FALSE) +
      scale_x_continuous(limits = c(0.25, 2.9), breaks = c(1, 2), labels = c("Leaders", "Followers")) +
      scale_linewidth(range = c(0.5, 3.5)) +
      labs(title = "Leader-follower Sankey-style plot", subtitle = "Only final leader → follower links are shown", x = NULL, y = NULL, color = "Cluster", linewidth = "WCD") +
      theme_minimal(base_size = 15) +
      theme(axis.text.y = element_blank(), panel.grid = element_blank(), legend.position = "bottom")
  }

  output$sankey_plot <- renderPlot({
    req(analysis())
    draw_sankey_plot(analysis())
  })

  output$sankey_code <- renderText({
    req(analysis())
    make_sankeymatic_code_app(analysis()$selected, analysis()$edges)
  })

  output$download_kano_png <- downloadHandler(
    filename = function() paste0("top20_kano_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(analysis())
      png(file, width = 1400, height = 1000, res = 150)
      draw_kano_plot(analysis())
      dev.off()
    }
  )

  output$download_sankey_png <- downloadHandler(
    filename = function() paste0("top20_sankey_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(analysis())
      png(file, width = 1500, height = 1000, res = 150)
      draw_sankey_plot(analysis())
      dev.off()
    }
  )

  output$download_network_png <- downloadHandler(
    filename = function() paste0("top20_leader_follower_network_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(analysis())
      png(file, width = 1400, height = 1000, res = 150)
      draw_network_static(analysis())
      dev.off()
    }
  )

  output$download_network_html <- downloadHandler(
    filename = function() paste0("top20_leader_follower_network_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"),
    content = function(file) {
      req(analysis())
      if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
        stop("Package 'htmlwidgets' is required for HTML network export. Please install.packages('htmlwidgets').")
      }
      net_obj <- build_network_objects(analysis())
      widget <- visNetwork(net_obj$nodes, net_obj$edges) |>
        visEdges(arrows = "to", smooth = FALSE) |>
        visNodes(scaling = list(min = 12, max = 35)) |>
        visOptions(highlightNearest = TRUE) |>
        visPhysics(stabilization = TRUE)
      htmlwidgets::saveWidget(widget, file = file, selfcontained = TRUE)
    }
  )

  output$download_rowwise_phrases_csv <- downloadHandler(
    filename = function() {
      paste0("rowwise_semantic_phrases_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    contentType = "text/csv; charset=utf-8",
    content = function(file) {
      req(analysis())
      out <- make_rowwise_semantic_csv(analysis(), top_per_row = as.integer(input$top_n %||% 20L))
      if (!isTRUE(input$semantic_top20_by_row)) {
        # Keep the output available, but make clear it was generated from combined-column analysis.
        if (!"note" %in% names(out)) out$note <- "Generated while row-wise mode was unchecked; rows may represent combined text rather than one abstract per input row."
      }
      # Force a true CSV response for browsers/Windows Save dialog.
      # write_excel_csv adds a UTF-8 BOM so Excel opens Chinese/English text correctly.
      readr::write_excel_csv(out, file, na = "")
    }
  )
  outputOptions(output, "download_rowwise_phrases_csv", suspendWhenHidden = FALSE)

  output$download_xlsx <- downloadHandler(
    filename = function() {
      paste0("top20_nodes_edges_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(analysis())
      if (!requireNamespace("writexl", quietly = TRUE)) {
        stop("Package 'writexl' is required for XLSX export. Please install.packages('writexl').")
      }

      writexl::write_xlsx(
        list(
          nodes = analysis()$export_nodes,
          edges = analysis()$export_edges,
          sil_df = analysis()$sil_df,
          cluster_summary = analysis()$cluster_summary,
          aac_dashboard = analysis()$aac_dashboard,
          validation = analysis()$validation,
          final_report = analysis()$final_report,
          extraction_log = analysis()$extraction_log
        ),
        path = file
      )
    }
  )

  normalize_network_edges <- function(edges, node_ids) {
    if (is.null(edges) || !is.data.frame(edges) || !nrow(edges)) {
      return(data.frame(from = character(0), to = character(0), value = numeric(0), title = character(0), stringsAsFactors = FALSE))
    }

    e <- as.data.frame(edges, stringsAsFactors = FALSE, check.names = FALSE)
    nms <- names(e)

    if (all(c("term1", "term2") %in% nms)) {
      from <- e$term1; to <- e$term2
    } else if (all(c("Leader", "follower") %in% nms)) {
      from <- e$Leader; to <- e$follower
    } else if (all(c("Leader", "Follower") %in% nms)) {
      from <- e$Leader; to <- e$Follower
    } else if (all(c("from", "to") %in% nms)) {
      from <- e$from; to <- e$to
    } else if (ncol(e) >= 2) {
      from <- e[[1]]; to <- e[[2]]
    } else {
      return(data.frame(from = character(0), to = character(0), value = numeric(0), title = character(0), stringsAsFactors = FALSE))
    }

    w <- if ("WCD" %in% nms) e$WCD else if ("weight" %in% nms) e$weight else if ("value" %in% nms) e$value else rep(1, length(from))

    out <- data.frame(
      from = trimws(as.character(from)),
      to = trimws(as.character(to)),
      WCD = suppressWarnings(as.numeric(w)),
      stringsAsFactors = FALSE
    )
    out$from[is.na(out$from)] <- ""
    out$to[is.na(out$to)] <- ""
    out$WCD[!is.finite(out$WCD)] <- 1
    out <- out[nzchar(out$from) & nzchar(out$to) & out$from != out$to, , drop = FALSE]
    out <- out[out$from %in% node_ids & out$to %in% node_ids, , drop = FALSE]
    if (!nrow(out)) {
      return(data.frame(from = character(0), to = character(0), value = numeric(0), title = character(0), stringsAsFactors = FALSE))
    }
    out <- stats::aggregate(WCD ~ from + to, data = out, FUN = sum)
    out$value <- pmax(1, sqrt(pmax(out$WCD, 0)))
    out$title <- paste0("FLCA-SIL-MA leader-follower link; WCD = ", round(out$WCD, 3))
    out[, c("from", "to", "value", "title"), drop = FALSE]
  }

  build_network_objects <- function(analysis_obj) {
    selected <- as.data.frame(analysis_obj$selected, stringsAsFactors = FALSE)
    if (!nrow(selected)) {
      return(list(
        nodes = data.frame(id = character(0), label = character(0), value = numeric(0), group = character(0), shape = character(0), title = character(0), stringsAsFactors = FALSE),
        edges = data.frame(from = character(0), to = character(0), value = numeric(0), title = character(0), stringsAsFactors = FALSE)
      ))
    }

    if (!("term" %in% names(selected)) && "name" %in% names(selected)) selected$term <- selected$name
    if (!("score" %in% names(selected)) && "value" %in% names(selected)) selected$score <- selected$value
    if (!("topic" %in% names(selected))) selected$topic <- 1L
    if (!("is_leader" %in% names(selected))) {
      selected$is_leader <- ave(suppressWarnings(as.numeric(selected$score)), selected$topic, FUN = function(z) z == max(z, na.rm = TRUE)) %in% TRUE
    }
    if (!("leader" %in% names(selected))) selected$leader <- ifelse(selected$is_leader, selected$term, NA_character_)
    if (!("doc_freq" %in% names(selected))) selected$doc_freq <- NA_real_
    if (!("source_type" %in% names(selected))) selected$source_type <- "FLCA"
    if (!("author_keyword" %in% names(selected))) selected$author_keyword <- FALSE
    if (!("exact_in_document" %in% names(selected))) selected$exact_in_document <- NA_character_

    selected <- selected |>
      mutate(
        term = trimws(as.character(term)),
        topic = as.integer(topic),
        score = suppressWarnings(as.numeric(score)),
        doc_freq = suppressWarnings(as.numeric(doc_freq)),
        source_type = as.character(source_type),
        author_keyword = as.logical(author_keyword %in% TRUE),
        exact_in_document = as.character(exact_in_document),
        leader = as.character(leader),
        is_leader = as.logical(is_leader %in% TRUE)
      ) |>
      filter(!is.na(term), nzchar(term)) |>
      distinct(term, .keep_all = TRUE) |>
      mutate(.rank_score = rank(-score, ties.method = "first"), .top3 = .rank_score <= 3)

    selected$score[!is.finite(selected$score)] <- 0
    selected$topic[!is.finite(selected$topic)] <- 1L
    selected$source_type[is.na(selected$source_type)] <- "FLCA"
    selected$exact_in_document[is.na(selected$exact_in_document)] <- ""
    selected$leader[is.na(selected$leader)] <- ""

    sc <- selected$score
    rng <- range(sc, na.rm = TRUE)
    if (!all(is.finite(rng)) || diff(rng) <= 0) {
      scaled <- rep(16, length(sc))
    } else {
      scaled <- 10 + 30 * ((sc - rng[1]) / diff(rng))
    }
    scaled[!is.finite(scaled)] <- 16

    nodes <- selected |>
      mutate(.scaled = scaled) |>
      transmute(
        id = term,
        label = ifelse(is_leader, paste0("★ ", term), term),
        group = paste0("Topic ", topic),
        value = ifelse(is_leader, pmax(18, .scaled), pmax(8, .scaled)),
        shape = ifelse(is_leader, "triangle", "dot"),
        title = paste0(
          "<b>", htmltools::htmlEscape(term), "</b>",
          ifelse(.top3, " <span style='color:#d32f2f; font-weight:700'>(Top 3)</span>", ""),
          "<br>Topic: ", topic,
          "<br>Role: ", ifelse(is_leader, "Cluster leader", paste0("Follower of ", htmltools::htmlEscape(leader))),
          "<br>Score: ", round(score, 2),
          "<br>Doc freq: ", doc_freq,
          "<br>Source: ", htmltools::htmlEscape(source_type),
          "<br>Author keyword: ", author_keyword,
          "<br>Exact normalized surface phrase: ", htmltools::htmlEscape(exact_in_document)
        ),
        `font.size` = ifelse(.top3, 34, ifelse(is_leader, 25, 18)),
        `font.face` = ifelse(.top3, "bold", "normal")
      ) |>
      as.data.frame(stringsAsFactors = FALSE)

    edf <- normalize_network_edges(analysis_obj$edges, nodes$id)
    list(nodes = nodes, edges = edf)
  }

  draw_network_static <- function(analysis_obj) {
    net_obj <- build_network_objects(analysis_obj)
    if (!nrow(net_obj$nodes)) {
      plot.new(); text(0.5, 0.5, "No network data available"); return(invisible(NULL))
    }
    if (!nrow(net_obj$edges)) {
      plot.new(); text(0.5, 0.5, "No network edges available"); return(invisible(NULL))
    }
    vdat <- data.frame(
      name = net_obj$nodes$id,
      label = net_obj$nodes$label,
      group = net_obj$nodes$group,
      is_leader = grepl("^★", net_obj$nodes$label),
      label_cex = if ("font.size" %in% names(net_obj$nodes)) pmax(0.9, as.numeric(net_obj$nodes[["font.size"]]) / 22) else 0.9,
      stringsAsFactors = FALSE
    )
    g0 <- igraph::graph_from_data_frame(net_obj$edges |> dplyr::transmute(from = from, to = to, weight = value), directed = TRUE, vertices = vdat)
    lay <- igraph::layout_with_fr(g0)
    vsize <- ifelse(igraph::V(g0)$is_leader %in% TRUE, 28, 18)
    vsize <- pmax(vsize, 16 + 6 * (igraph::V(g0)$label_cex - 0.9))
    ewidth <- pmax(1, as.numeric(igraph::E(g0)$weight))
    plot(g0,
         layout = lay,
         vertex.size = vsize,
         vertex.label = igraph::V(g0)$label,
         vertex.label.cex = igraph::V(g0)$label_cex,
         vertex.color = as.integer(as.factor(igraph::V(g0)$group)),
         vertex.shape = ifelse(igraph::V(g0)$is_leader %in% TRUE, "square", "circle"),
         edge.arrow.size = 0.45,
         edge.width = ewidth,
         main = "Top-N leader-follower network")
  }

  output$net <- renderVisNetwork({
    req(analysis())
    tryCatch({
      net_obj <- build_network_objects(analysis())
      if (!nrow(net_obj$nodes)) stop("No valid network nodes after normalization.")
      visNetwork(net_obj$nodes, net_obj$edges) |>
        visEdges(arrows = "to", smooth = FALSE) |>
        visNodes(scaling = list(min = 10, max = 36), font = list(size = 18)) |>
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
        visPhysics(stabilization = TRUE)
    }, error = function(e) {
      showNotification(paste("Network rendering failed:", conditionMessage(e)), type = "error", duration = 8)
      fallback_nodes <- data.frame(
        id = "network_error",
        label = paste("Network error:", conditionMessage(e)),
        shape = "box",
        value = 20,
        title = conditionMessage(e),
        stringsAsFactors = FALSE
      )
      visNetwork(fallback_nodes, data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE))
    })
  })

  output$topic_sizes <- renderPlot({
    req(analysis())
    df <- analysis()$selected |>
      count(topic, name = "n_terms")
    ggplot(df, aes(x = factor(topic), y = n_terms)) +
      geom_col() +
      labs(x = "Topic", y = "Number of selected top phrases", title = "Topic sizes among screened top-N phrases") +
      theme_minimal(base_size = 14)
  })

  output$about_html <- renderUI({
    HTML(paste(
      "<h4>What changed in this ChatGPT-like version</h4>",
      "<ul>",
      "<li>Upload automatically runs analysis once and immediately shows a processing message.</li>",
      "<li>A progress bar shows the current step; a green message appears when processing is complete.</li>",
      "<li>Parameter changes require <b>Run / re-run analysis</b>, avoiding slow auto-reruns for large PDFs.</li>",
      "<li>Top-N phrases are selected first using phrase score; co-occurrence edges are built only among those selected phrases.</li>",
      "<li>XLSX export contains two sheets: nodes(name, value, value2) and edges(term1, term2, WCD).</li>",
      "<li>value is the node importance score; value2 is the sum of connected WCD values in the final leader-follower edges.</li>",
      "<li>FLCA-SIL-MA style screening keeps only the top-N topic phrases by score.</li>",
      "<li>Default top-N is 20, so the output focuses on major document topics.</li>",
      "<li>Exact surface phrase extraction: phrases are contiguous in the cleaned document text.</li><li>Author-defined keywords can be detected, injected, protected during subset-free filtering, and boosted when Author-keyword protection is checked.</li><li>SSplot tab uses the same Top-N nodes and the same cluster assignment as the network.</li><li>Each SSplot bar now shows SS; score (black) and leader AAC (red) are shown in the middle-left annotation space; overall SS and overall AAC are shown in the subtitle. The app can also draw directly from an uploaded nodes+edges XLSX bundle.</li><li>Kano, Sankey, and AAC dashboard tabs were added. SankeyMATIC code now includes flow lines and node color definition lines, ready to paste into sankeymatic.com. The Kano tab now uses a real Kano-style layout with two wings, two circles, top overall AAC annotation, and score-labeled bubbles.</li>",
      "<li>Subset-free screening: overlapping nested phrases are reduced before Top-N selection.</li>",
      "<li>The Final Report tab shows the selected API/TF-IDF mode and the actual engine applied after auto-detection or fallback.</li><li>The final visualization is a leader-follower graph: one leader per topic and one link from leader to each follower.</li>",
      "<li>Pairwise co-occurrence edges are used for scoring/clustering only, not drawn as the final network.</li>",
      "</ul>",
      "<h4>Are phrases changed by semantic meaning?</h4>",
      "<p>No. The app does not paraphrase or embed terms. Phrases are contiguous surface phrases from the cleaned document text. They are normalized by lowercasing, punctuation cleanup, hyphen/slash handling, and URL/DOI removal. The <code>exact_in_document</code> column means exact normalized surface phrase, not AI invention.</p>",
      "<h4>Score</h4>",
      "<p>Score = tf_idf_sum*100 + doc_freq*5 + phrase_bonus + author_keyword_bonus + acronym_bonus + entity_bonus + context_bonus. Author-keyword protection and ChatGPT-like filtering are controlled by sidebar checkboxes. Co-occurrence is built only after Top-N selection.</p>"
    ))
  })
}

shinyApp(ui, server)
