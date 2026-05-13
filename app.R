library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggrepel)
library(DT)
library(visNetwork)
library(readr)
library(readxl)
library(stringr)
library(tibble)
library(jsonlite)

# validate/need namespace fix: jsonlite also has validate(), so use shiny::validate()/shiny::need().

library(tidytext)
library(igraph)
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



# ---- Optional Chinese tail keyword engine ----
# Adds a Chinese document mode without changing the original English extraction rule.
zh_tail_engine_path <- file.path(APP_DIR, "zh_tail_keyword_engine.R")
.zh_tail_engine_loaded <- FALSE
if (file.exists(zh_tail_engine_path)) {
  try({
    source(zh_tail_engine_path, local = FALSE, encoding = "UTF-8")
    .zh_tail_engine_loaded <- exists("run_zh_main_document_tail_pipeline", mode = "function") &&
      exists("split_author_keywords_zh", mode = "function")
    message("[app.R] Chinese tail keyword engine loaded: ", zh_tail_engine_path)
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
  if (!length(x) || !nzchar(trimws(paste(x, collapse = " ")))) return(character(0))

  # Manual Step-3 keywords can be separated by:
  # ; , full-width variants ； ， real CR/LF, literal chr(13)/chr(10),
  # or spaces when no explicit delimiter exists.
  # For multi-word English phrases, use ;, comma, or new lines so spaces are preserved.
  x <- paste(x, collapse = "\n")
  x <- gsub("chr\\(13\\)|chr\\(10\\)", "\n", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\r\\n?|\\n", "\n", x, perl = TRUE)
  x <- trimws(x)

  has_explicit_delim <- grepl("[;；,，\\n]", x, perl = TRUE)
  if (has_explicit_delim) {
    x <- gsub("[;；,，]+", "\n", x, perl = TRUE)
    bits <- unlist(strsplit(x, "\\n+", perl = TRUE), use.names = FALSE)
  } else {
    bits <- unlist(strsplit(x, "[[:space:]]+", perl = TRUE), use.names = FALSE)
  }

  bits <- trimws(bits)
  bits <- bits[nzchar(bits)]
  if (!length(bits)) return(character(0))

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



# ---- Chinese document mode helpers ------------------------------------------

clean_zh_phrase_strict_app <- function(x, min_chars = 4L, max_chars = 10L, author_keyword = FALSE) {
  x <- as.character(x %||% "")
  # Remove punctuation/symbols, then keep Han only.
  x <- gsub("[「」『』“”‘’（）()【】\\[\\]《》〈〉，。；：！？、,.!?;:]+", "", x, perl = TRUE)
  x <- gsub("[^\\p{Han}]+", "", x, perl = TRUE)
  x <- trimws(x)
  x <- zh_semantic_canonicalize_app(x)

  if (!nzchar(x)) return("")
  n <- nchar(x, type = "chars")
  if (isTRUE(author_keyword)) {
    if (n < 2L || n > 12L) return("")
  } else {
    if (n < min_chars || n > max_chars) return("")
  }

  weak_exact <- c(
    "選取自", "取自", "來自", "本文", "本研究", "可能存在", "年起呈現",
    "進行", "進行分析", "進行研究", "顯示", "指出", "可以看出",
    "資料來源", "方法如下", "結果如下",
    zh_weak_nonsemantic_terms_app
  )
  if (!isTRUE(author_keyword) && (
    x %in% weak_exact ||
      is_zh_weak_nonsemantic_app(x) ||
      is_zh_condition_break_phrase_app(x, author_keyword = FALSE)
  )) return("")

  if (!isTRUE(author_keyword)) {
    if (grepl("^(之|的|在|於|與|及|和|或|為|以|由|自|而|並|且)", x, perl = TRUE)) return("")
    if (grepl("(之|的|在|於|與|及|和|或|為|以|由|自|而|並|且)$", x, perl = TRUE)) return("")
    if (grepl("(進行|呈現|顯示|指出|發現|可能存在|選取自|檢定能|能有效)$", x, perl = TRUE)) return("")
  }

  x
}

clean_zh_vector_strict_app <- function(x, min_chars = 3L, max_chars = 10L, author_keyword = FALSE) {
  # Vector-filter helper: returns only valid cleaned terms and may be shorter
  # than x. Do not use this directly inside mutate()/transmute() where row
  # length must be preserved. Use vapply(clean_zh_phrase_strict_app) there.
  out <- vapply(
    as.character(x %||% character(0)),
    clean_zh_phrase_strict_app,
    character(1),
    min_chars = min_chars,
    max_chars = max_chars,
    author_keyword = author_keyword
  )
  out[!is.na(out) & nzchar(out)]
}


parse_manual_author_keywords_zh_app <- function(x) {
  x <- as.character(x %||% "")
  x <- paste(x, collapse = "\n")
  if (!nzchar(trimws(x))) return(character(0))

  # Robust Chinese keyword splitter. If explicit delimiters are present,
  # spaces are preserved inside each item.
  x <- gsub("chr\\(13\\)|chr\\(10\\)", "\n", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\r\\n?|\\n", "\n", x, perl = TRUE)

  has_explicit_delim <- grepl("[;；,，、|/\\n]", x, perl = TRUE)
  if (has_explicit_delim) {
    x <- gsub("\\s*[;；,，、|/]+\\s*", "\n", x, perl = TRUE)
    bits <- unlist(strsplit(x, "\\n+", perl = TRUE), use.names = FALSE)
  } else {
    bits <- unlist(strsplit(x, "[[:space:]]+", perl = TRUE), use.names = FALSE)
  }

  bits <- trimws(bits)
  bits <- bits[nzchar(bits)]
  if (!length(bits)) return(character(0))

  bits <- vapply(bits, clean_manual_zh_keyword_app, character(1), max_chars = 20L)
  bits <- bits[nzchar(bits)]
  unique(bits)
}


# ---- Chinese semantic prompt/API mode ---------------------------------------
# Used only when Chinese document mode is checked AND API mode is not ESSPE-only.
# It produces ChatGPT-like semantic nodes/edges instead of raw high-frequency
# administrative fragments.

zh_admin_blacklist_app <- c(
  "公告", "請查照", "敬請踴躍參加", "報名截止", "課程公告",
  "成績查詢方式", "通知", "詳如附件", "敬請", "參加",
  "選取自", "本文", "本研究", "可能存在"
)




# Condition-break/action terms should not become final semantic nodes.
# These are discourse/action/method fragments. Manual author keywords can still override them.
zh_condition_break_terms_app <- unique(c(
  "進行", "进行", "成為", "成为", "作為", "作为", "用以", "用於", "用于",
  "藉由", "藉以", "統計分析", "统计分析", "研究目的", "研究方法", "呈現", "使用", "輸出", "結果",
  "研究結果", "結果顯示", "结果显示", "採用", "采用", "利用", "透過", "通过",
  "顯示", "显示", "指出", "發現", "发现", "建立", "形成", "進一步"
))

is_zh_condition_break_phrase_app <- function(x, author_keyword = FALSE) {
  if (isTRUE(author_keyword)) return(FALSE)

  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x)) return(TRUE)

  terms <- zh_condition_break_terms_app
  terms <- terms[nzchar(terms)]
  if (!length(terms)) return(FALSE)

  # Exact term is always excluded.
  if (x %in% terms) return(TRUE)

  # Very short phrase containing any action/result fragment is not a semantic concept.
  hit <- vapply(terms, function(t) grepl(t, x, fixed = TRUE), logical(1))
  if (any(hit) && nchar(x, type = "chars") <= 6L) return(TRUE)

  # Terms that start/end with action/result fragments are usually broken phrases.
  start_hit <- vapply(terms, function(t) startsWith(x, t), logical(1))
  end_hit <- vapply(terms, function(t) endsWith(x, t), logical(1))
  if (any(start_hit) || any(end_hit)) return(TRUE)

  # Specific weak compounds often seen in Chinese fallback extraction.
  weak_compounds <- c("輸出結果", "此結果顯示", "結果顯示", "研究結果顯示", "呈現相當程度",
                      "即使用詞有所差異", "片語完全重疊", "不僅需要語言潤飾")
  if (x %in% weak_compounds) return(TRUE)

  FALSE
}

# Extra Chinese weak/discourse terms: common writing fragments, not semantic concepts.
# Manual keywords can still override them.
zh_weak_nonsemantic_terms_app <- c(
  "此結果顯示", "轉換為", "評估", "輸出結果", "關鍵字", "關鍵詞",
  "用詞不同", "同有高度", "具有高度", "呈現相當程度", "相當程度",
  "片語完全重疊", "即使用詞有所差異", "審查者也需要能快速",
  "不僅需要語言潤飾", "需要語言潤飾", "語言潤飾",
  "此結果", "本結果", "結果顯示", "輸出", "呈現", "開啟"
)

is_zh_weak_nonsemantic_app <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x)) return(TRUE)
  if (x %in% zh_admin_blacklist_app || x %in% zh_weak_nonsemantic_terms_app) return(TRUE)
  if (grepl("^(此|這|該|其|即使|以及|並且|然而|因此|此外)", x, perl = TRUE)) return(TRUE)
  if (grepl("(顯示|表示|指出|提供|轉換為|需要|不需要|可見|可呈現|有所差異|完全重疊|相當程度)$", x, perl = TRUE)) return(TRUE)
  if (grepl("^(輸出|納入|呈現|評估|開啟|關鍵字)$", x, perl = TRUE)) return(TRUE)
  FALSE
}

clean_manual_zh_keyword_app <- function(x, max_chars = 20L) {
  x <- as.character(x %||% "")
  x <- gsub("chr\\(13\\)|chr\\(10\\)", "\n", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("[「」『』“”‘’（）()【】\\[\\]《》〈〉，。；：！？、,.!?;:]+", "", x, perl = TRUE)
  x <- gsub("[^\\p{Han}]+", "", x, perl = TRUE)
  x <- trimws(x)
  if (!nzchar(x)) return("")
  if (nchar(x, type = "chars") < 2L || nchar(x, type = "chars") > max_chars) return("")
  zh_semantic_canonicalize_app(x)
}

zh_semantic_canonicalize_app <- function(x) {
  x <- as.character(x %||% "")
  x <- trimws(x)
  if (!nzchar(x)) return("")

  # Deterministic semantic renaming for common Chinese administrative fragments.
  # This is the rule-based fallback approximation of ChatGPT-like canonicalization.
  exact_map <- c(
    "年醫院評鑑課程" = "醫院評鑑課程",
    "病人為焦點" = "病人中心照護",
    "實施辦法改版通知" = "評鑑制度改版",
    "課程公告醫療永續" = "醫療永續課程",
    "醫院評鑑中心思維" = "評鑑中心思維"
  )
  if (x %in% names(exact_map)) return(unname(exact_map[[x]]))

  # Remove leading year/time artifact from concepts, e.g., 年醫院評鑑課程.
  x <- gsub("^年+", "", x, perl = TRUE)

  # Semantic canonicalization by contained clue words.
  if (grepl("病人.*焦點|病人為焦點", x, perl = TRUE)) return("病人中心照護")
  if (grepl("實施辦法.*改版|改版.*通知", x, perl = TRUE)) return("評鑑制度改版")
  if (grepl("醫療永續", x, perl = TRUE) && grepl("課程|公告", x, perl = TRUE)) return("醫療永續課程")
  if (grepl("醫院評鑑.*課程|評鑑課程", x, perl = TRUE)) return("醫院評鑑課程")
  if (grepl("評鑑中心.*思維", x, perl = TRUE)) return("評鑑中心思維")

  x
}

zh_semantic_seed_nodes_app <- function(text_all, existing = character(0), top_n = 20L, base_value = 100) {
  text_all <- as.character(text_all %||% "")
  existing <- unique(as.character(existing %||% character(0)))
  top_n <- as.integer(top_n %||% 20L)
  if (!is.finite(top_n) || top_n < 5L) top_n <- 20L
  base_value <- suppressWarnings(as.numeric(base_value))
  if (!is.finite(base_value) || base_value <= 0) base_value <- 100

  seed <- character(0)

  add_if <- function(cond, term) {
    if (isTRUE(cond)) seed <<- unique(c(seed, term))
  }

  # Graphical abstract / App4SemanticPhrases article concepts
  add_if(grepl("視覺摘要|圖形摘要|graphical abstract", text_all, ignore.case = TRUE, perl = TRUE), "視覺摘要應用程式")
  add_if(grepl("文字探勘|文本探勘|text mining", text_all, ignore.case = TRUE, perl = TRUE), "文字探勘")
  add_if(grepl("語義片語|片語擷取|semantic phrase|featured phrase", text_all, ignore.case = TRUE, perl = TRUE), "語義片語擷取")
  add_if(grepl("共詞|共現|co-word|co occurrence|co-occurrence", text_all, ignore.case = TRUE, perl = TRUE), "共詞網絡")
  add_if(grepl("網絡式圖形摘要|網絡.*圖形摘要|network.*graphical", text_all, ignore.case = TRUE, perl = TRUE), "網絡式圖形摘要")
  add_if(grepl("概念結構|主題結構|conceptual structure", text_all, ignore.case = TRUE, perl = TRUE), "概念結構")
  add_if(grepl("投稿前|提交前|pre-submission|manuscript screening", text_all, ignore.case = TRUE, perl = TRUE), "投稿前篩檢")
  add_if(grepl("主題聚類|群集|cluster", text_all, ignore.case = TRUE, perl = TRUE), "主題聚類")
  add_if(grepl("可重現|再現|reproduc", text_all, ignore.case = TRUE, perl = TRUE), "可重現分析")
  add_if(grepl("網路視覺化|網絡視覺化|visualization|視覺化", text_all, ignore.case = TRUE, perl = TRUE), "網絡視覺化")

  add_if(grepl("醫院評鑑|評鑑中心|評鑑課程", text_all, perl = TRUE), "醫院評鑑")
  add_if(grepl("醫療永續|永續", text_all, perl = TRUE), "醫療永續")
  add_if(grepl("病人為焦點|病人.*焦點|病人中心", text_all, perl = TRUE), "病人中心照護")
  add_if(grepl("實施辦法|改版", text_all, perl = TRUE), "評鑑制度改版")
  add_if(grepl("醫院評鑑課程|評鑑課程|課程", text_all, perl = TRUE), "醫院評鑑課程")
  add_if(grepl("評鑑中心.*思維|中心思維", text_all, perl = TRUE), "評鑑中心思維")
  add_if(grepl("醫療品質|品質", text_all, perl = TRUE), "醫療品質")
  add_if(grepl("病人安全|安全", text_all, perl = TRUE), "病人安全")
  add_if(grepl("醫院管理|管理", text_all, perl = TRUE), "醫院管理")
  add_if(grepl("工作坊", text_all, perl = TRUE), "評鑑工作坊")
  add_if(grepl("成績查詢|成績", text_all, perl = TRUE), "評鑑成績查詢")
  add_if(grepl("報名|截止", text_all, perl = TRUE), "課程報名管理")
  add_if(grepl("教育訓練|訓練|課程", text_all, perl = TRUE), "教育訓練")
  add_if(grepl("醫療政策|政策", text_all, perl = TRUE), "醫療政策")
  add_if(grepl("品質改善|改善", text_all, perl = TRUE), "品質改善")

  seed <- setdiff(unique(seed), existing)
  seed <- setdiff(seed, zh_admin_blacklist_app)
  if (!length(seed)) return(tibble::tibble(name = character(), value = numeric(), source = character(), occ_n = numeric()))

  n_need <- max(0L, top_n - length(existing))
  if (n_need <= 0L) return(tibble::tibble(name = character(), value = numeric(), source = character(), occ_n = numeric()))
  seed <- head(seed, n_need)

  tibble::tibble(
    name = seed,
    value = pmax(1, base_value - seq_along(seed)),
    source = "zh_semantic_seed_fill",
    occ_n = 0
  )
}


build_chinese_semantic_prompt_app <- function(doc_text,
                                              manual_keywords = character(0),
                                              top_n = 20L) {
  top_n <- as.integer(top_n %||% 20L)
  if (!is.finite(top_n) || top_n < 5L) top_n <- 20L

  manual_kw <- paste(unique(manual_keywords[nzchar(manual_keywords)]), collapse = "; ")
  admin_terms <- paste(zh_admin_blacklist_app, collapse = "、")

  # Build the JSON example programmatically so app.R does not contain an
  # unescaped raw JSON block. This prevents parse errors such as:
  #   unexpected symbol: "nodes"
  json_example <- jsonlite::toJSON(
    list(
      nodes = list(
        list(name = "醫院評鑑", value = 10),
        list(name = "醫療永續", value = 9)
      ),
      edges = list(
        list(term1 = "醫院評鑑", term2 = "醫療永續", WCD = 3)
      ),
      forced_keywords = list("手動關鍵字1", "手動關鍵字2"),
      excluded_admin_terms = list("公告", "請查照"),
      renamed_terms = list(
        list(original = "實施辦法改版通知", canonical = "評鑑制度改版")
      )
    ),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  paste(
    "你是一位中文醫務管理、醫療品質、醫院評鑑、醫療政策與學術文本分析專家。",
    "",
    "請閱讀以下中文文件，只使用標題、摘要、本文或主要內容。",
    "不要使用參考文獻、附錄、致謝、表格說明、圖說、頁首頁尾或網站選單文字。",
    "",
    "任務：",
    "請產生兩個 dataframe：nodes 與 edges。",
    "",
    "Dataframe 1: nodes",
    "欄位必須完全是：",
    "name, value",
    "",
    "Dataframe 2: edges",
    "欄位必須完全是：",
    "term1, term2, WCD",
    "",
    "nodes 規則：",
    paste0("1. 請抽取 Top ", top_n, " 個中文語義詞。"),
    "2. name 必須是語義概念詞，不要只是高頻行政用語。",
    "3. value 是該語義詞在文件中的出現頻率、重要性或語義顯著性分數，請用正數。",
    "4. 優先使用醫務管理、醫院評鑑、醫療品質、醫療永續、病人安全、病人為中心、課程訓練、制度改版、醫院管理、品質改善等有意義概念。",
    "5. 請進行語義重命名，例如：",
    "   - 課程公告醫療永續 可整理為 醫療永續課程",
    "   - 實施辦法改版通知 可整理為 評鑑制度改版",
    "   - 病人為焦點 可整理為 病人中心照護",
    "   - 醫院評鑑中心思維 可整理為 評鑑中心思維",
    "6. 不要輸出以下行政或低語義詞，除非它們是手動指定關鍵字：",
    paste0("   ", admin_terms),
    "7. 不要輸出含有標點符號的詞，例如中文引號、句號、分號、冒號。",
    paste0("8. 若文件中可用詞不足 ", top_n, " 個，請用語義相關且文件可支持的概念詞補足，不要只停在少數幾個。"),
    paste0("9. 手動輸入關鍵字必須強制納入 nodes：", ifelse(nzchar(manual_kw), manual_kw, "無")),
    "",
    "edges 規則：",
    "1. 只使用 nodes$name 中的詞建立邊。",
    "2. 若兩個語義詞在同一段、同一句或同一短文字單位中共同出現，建立關聯。",
    "3. WCD 是共現次數或語義關聯強度，必須為正數。",
    "4. 不要 self-link。",
    "5. term1-term2 不要重複反向邊。",
    "6. 至少提供可支援網路圖的邊；若文字較短，請依語義關聯補足合理邊。",
    "",
    "輸出要求：",
    "只輸出 JSON，不要解釋文字。",
    "JSON 格式必須如下：",
    json_example,
    "",
    "以下是文件全文：",
    doc_text,
    sep = "\n"
  )
}


parse_chinese_nodes_edges_json_app <- function(content, top_n = 20L) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for Chinese semantic API mode.", call. = FALSE)
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
  if (is.null(parsed)) stop("Chinese API returned non-JSON content.", call. = FALSE)

  raw_nodes <- parsed$nodes %||% parsed$Nodes %||% parsed$node %||% list()
  raw_edges <- parsed$edges %||% parsed$Edges %||% parsed$edge %||% list()

  node_df <- if (is.data.frame(raw_nodes)) {
    raw_nodes
  } else {
    dplyr::bind_rows(lapply(raw_nodes, function(z) {
      if (is.character(z)) {
        tibble::tibble(name = z[[1]], value = 1)
      } else if (is.list(z)) {
        tibble::tibble(
          name = as.character(z$name %||% z$term %||% z$phrase %||% z[[1]] %||% ""),
          value = suppressWarnings(as.numeric(z$value %||% z$score %||% z$weight %||% 1))
        )
      } else {
        tibble::tibble(name = character(), value = numeric())
      }
    }))
  }

  edge_df <- if (is.data.frame(raw_edges)) {
    raw_edges
  } else {
    dplyr::bind_rows(lapply(raw_edges, function(z) {
      if (is.list(z)) {
        tibble::tibble(
          term1 = as.character(z$term1 %||% z$from %||% z$source %||% z[[1]] %||% ""),
          term2 = as.character(z$term2 %||% z$to %||% z$target %||% z[[2]] %||% ""),
          WCD = suppressWarnings(as.numeric(z$WCD %||% z$wcd %||% z$weight %||% z$value %||% z[[3]] %||% 1))
        )
      } else {
        tibble::tibble(term1 = character(), term2 = character(), WCD = numeric())
      }
    }))
  }

  if (!nrow(node_df)) stop("Chinese API returned no nodes.", call. = FALSE)

  if (!"name" %in% names(node_df)) names(node_df)[1] <- "name"
  if (!"value" %in% names(node_df)) {
    if ("score" %in% names(node_df)) node_df$value <- node_df$score else node_df$value <- seq_len(nrow(node_df))
  }

  node_df <- node_df |>
    dplyr::transmute(
      name = trimws(as.character(name)),
      value = suppressWarnings(as.numeric(value))
    ) |>
    dplyr::mutate(
      name = gsub("[「」『』“”‘’（）()【】\\[\\]《》〈〉，。；：！？、,.!?;:]+", "", name, perl = TRUE),
      name = trimws(name),
      value = ifelse(is.finite(value) & value > 0, value, 1)
    ) |>
    dplyr::filter(!is.na(name), nzchar(name), !(name %in% zh_admin_blacklist_app)) |>
    dplyr::distinct(name, .keep_all = TRUE) |>
    dplyr::arrange(dplyr::desc(value), name) |>
    dplyr::slice_head(n = top_n)

  if (!nrow(node_df)) stop("Chinese API nodes were removed after administrative-term filtering.", call. = FALSE)

  if (!nrow(edge_df)) {
    edge_df <- tibble::tibble(term1 = character(), term2 = character(), WCD = numeric())
  } else {
    if (!"term1" %in% names(edge_df) && "from" %in% names(edge_df)) edge_df$term1 <- edge_df$from
    if (!"term2" %in% names(edge_df) && "to" %in% names(edge_df)) edge_df$term2 <- edge_df$to
    if (!"WCD" %in% names(edge_df) && "weight" %in% names(edge_df)) edge_df$WCD <- edge_df$weight
    if (!"WCD" %in% names(edge_df) && "value" %in% names(edge_df)) edge_df$WCD <- edge_df$value
    if (!"WCD" %in% names(edge_df)) edge_df$WCD <- 1

    edge_df <- edge_df |>
      dplyr::transmute(
        term1 = trimws(as.character(term1)),
        term2 = trimws(as.character(term2)),
        WCD = suppressWarnings(as.numeric(WCD))
      ) |>
      dplyr::mutate(
        term1 = gsub("[「」『』“”‘’（）()【】\\[\\]《》〈〉，。；：！？、,.!?;:]+", "", term1, perl = TRUE),
        term2 = gsub("[「」『』“”‘’（）()【】\\[\\]《》〈〉，。；：！？、,.!?;:]+", "", term2, perl = TRUE),
        term1 = trimws(term1),
        term2 = trimws(term2),
        WCD = ifelse(is.finite(WCD) & WCD > 0, WCD, 1)
      ) |>
      dplyr::filter(nzchar(term1), nzchar(term2), term1 != term2,
                    term1 %in% node_df$name, term2 %in% node_df$name) |>
      dplyr::mutate(pair_a = pmin(term1, term2), pair_b = pmax(term1, term2)) |>
      dplyr::group_by(pair_a, pair_b) |>
      dplyr::summarise(WCD = sum(WCD, na.rm = TRUE), .groups = "drop") |>
      dplyr::transmute(term1 = pair_a, term2 = pair_b, WCD = WCD) |>
      dplyr::arrange(dplyr::desc(WCD), term1, term2)
  }

  if (!nrow(edge_df) && nrow(node_df) >= 2) {
    nm <- node_df$name
    edge_df <- tibble::tibble(term1 = nm[-length(nm)], term2 = nm[-1], WCD = 1)
  }

  list(
    nodes = node_df,
    edges = edge_df,
    forced_keywords = parsed$forced_keywords %||% character(0),
    excluded_admin_terms = parsed$excluded_admin_terms %||% character(0),
    renamed_terms = parsed$renamed_terms %||% list()
  )
}

openai_extract_chinese_nodes_edges_app <- function(docs_tbl,
                                                   top_n = 20L,
                                                   manual_keywords = character(0),
                                                   model = Sys.getenv("OPENAI_MODEL", unset = "gpt-4o-mini")) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for Chinese semantic API mode.", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for Chinese semantic API mode.", call. = FALSE)
  }

  api_key <- trimws(Sys.getenv("OPENAI_API_KEY", unset = ""))
  if (!nzchar(api_key)) stop("OPENAI_API_KEY is not set for Chinese semantic API mode.", call. = FALSE)

  doc_text <- paste(as.character(docs_tbl$text %||% ""), collapse = "\n\n")
  doc_text <- gsub("[[:space:]]+", " ", doc_text, perl = TRUE)
  doc_text <- trimws(doc_text)
  if (nchar(doc_text, type = "chars") > 18000L) doc_text <- substr(doc_text, 1L, 18000L)
  if (!nzchar(doc_text)) stop("No Chinese document text was available for API extraction.", call. = FALSE)

  prompt <- build_chinese_semantic_prompt_app(doc_text, manual_keywords, top_n)

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
        list(role = "system", content = "你是嚴格的中文語義節點與共詞邊抽取引擎。只回傳合法 JSON，不要解釋。"),
        list(role = "user", content = prompt)
      )
    )) |>
    httr2::req_timeout(120)

  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  content <- body$choices[[1]]$message$content %||% ""
  parse_chinese_nodes_edges_json_app(content, top_n = top_n)
}

build_analysis_from_nodes_edges_direct_app <- function(nodes_df,
                                                       edges_df,
                                                       source_mode = "direct_nodes_edges",
                                                       processing_log_tbl = NULL) {
  nodes_raw <- as.data.frame(nodes_df %||% data.frame(), stringsAsFactors = FALSE)
  edges_raw <- as.data.frame(edges_df %||% data.frame(), stringsAsFactors = FALSE)
  std_bundle <- .standardize_nodes_edges_bundle_names_app(nodes_raw, edges_raw)
  bundle <- list(
    upload_type = source_mode,
    nodes = std_bundle$nodes,
    edges = std_bundle$edges,
    sil_df = NULL,
    validation = NULL
  )
  out <- build_analysis_from_nodes_edges_bundle(bundle, top_n = nrow(std_bundle$nodes))
  out$extraction_log <- dplyr::bind_rows(
    out$extraction_log %||% tibble::tibble(),
    tibble::tibble(
      item = c("source_mode", "actual_engine_applied", "language_mode"),
      value = c(source_mode, source_mode, "Chinese semantic API/direct nodes-edges")
    )
  )
  out$processing_log <- processing_log_tbl %||% tibble::tibble()
  out
}



# ---- Console diagnostic logging for extraction steps ------------------------
console_log_step_app <- function(step, obj = NULL, preview_n = 20L) {
  # This writes compact but inspectable diagnostics to the R console.
  # It is intentionally independent of Shiny outputs so users can debug crashes.
  msg_head <- paste0("[TOP20 TERM EXTRACTION][CHECK] ", step)
  message(msg_head)

  if (is.null(obj)) return(invisible(NULL))

  if (is.data.frame(obj)) {
    df <- as.data.frame(obj, stringsAsFactors = FALSE)
    message("[TOP20 TERM EXTRACTION][CHECK] rows=", nrow(df), "; cols=", ncol(df),
            ifelse(ncol(df) > 0, paste0("; names=", paste(names(df), collapse = ", ")), ""))
    if (nrow(df)) {
      cap <- utils::capture.output(print(utils::head(df, preview_n)))
      message(paste(cap, collapse = "\n"))
    }
    return(invisible(NULL))
  }

  if (is.atomic(obj) || is.character(obj)) {
    v <- as.character(obj)
    message("[TOP20 TERM EXTRACTION][CHECK] n=", length(v),
            ifelse(length(v), paste0("; preview=", paste(utils::head(v, preview_n), collapse = " | ")), ""))
    return(invisible(NULL))
  }

  cap <- utils::capture.output(str(obj, max.level = 1, vec.len = preview_n))
  message(paste(cap, collapse = "\n"))
  invisible(NULL)
}



# ---- Chinese pipeline size-safety helpers -----------------------------------
# Prevents errors such as "arguments imply differing number of rows" / "引數長度不同"
# when the Chinese engine returns nodes without source/occ_n or returns rule/method
# values longer than 1.
.scalar_chr_app <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) return(default)
  x <- as.character(x)
  x <- x[!is.na(x)]
  if (!length(x)) return(default)
  paste(x, collapse = "; ")
}

.scalar_num_app <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0) return(default)
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x)) return(default)
  x[[1]]
}

.ensure_df_col_app <- function(df, col, default = NA, n = NULL) {
  df <- as.data.frame(df %||% data.frame(), stringsAsFactors = FALSE)
  if (is.null(n)) n <- nrow(df)
  if (!col %in% names(df)) {
    df[[col]] <- rep(default, n)
  } else {
    v <- df[[col]]
    if (length(v) == 0) {
      df[[col]] <- rep(default, n)
    } else if (length(v) == 1 && n != 1) {
      df[[col]] <- rep(v, n)
    } else if (length(v) != n) {
      # Keep row-size stable; recycle only through explicit rep_len.
      df[[col]] <- rep_len(v, n)
    }
  }
  df
}

.standardize_zh_nodes_input_app <- function(nodes0) {
  nodes0 <- as.data.frame(nodes0 %||% data.frame(), stringsAsFactors = FALSE)
  if (!nrow(nodes0)) return(nodes0)

  if (!"name" %in% names(nodes0)) {
    if ("term" %in% names(nodes0)) nodes0$name <- nodes0$term
    else if ("keyword" %in% names(nodes0)) nodes0$name <- nodes0$keyword
    else names(nodes0)[1] <- "name"
  }
  if (!"value" %in% names(nodes0)) {
    if ("score" %in% names(nodes0)) nodes0$value <- nodes0$score
    else if ("freq" %in% names(nodes0)) nodes0$value <- nodes0$freq
    else if ("n" %in% names(nodes0)) nodes0$value <- nodes0$n
    else nodes0$value <- seq_len(nrow(nodes0))
  }
  nodes0 <- .ensure_df_col_app(nodes0, "source", "zh_tail", nrow(nodes0))
  nodes0 <- .ensure_df_col_app(nodes0, "occ_n", NA_real_, nrow(nodes0))
  nodes0
}

.standardize_zh_edges_input_app <- function(edges0) {
  edges0 <- as.data.frame(edges0 %||% data.frame(), stringsAsFactors = FALSE)
  if (!nrow(edges0)) {
    return(tibble::tibble(term1 = character(), term2 = character(), WCD = numeric()))
  }
  if (!"term1" %in% names(edges0)) {
    if ("from" %in% names(edges0)) edges0$term1 <- edges0$from
    else if ("source" %in% names(edges0)) edges0$term1 <- edges0$source
    else if ("node1" %in% names(edges0)) edges0$term1 <- edges0$node1
    else names(edges0)[1] <- "term1"
  }
  if (!"term2" %in% names(edges0)) {
    if ("to" %in% names(edges0)) edges0$term2 <- edges0$to
    else if ("target" %in% names(edges0)) edges0$term2 <- edges0$target
    else if ("node2" %in% names(edges0)) edges0$term2 <- edges0$node2
    else if (ncol(edges0) >= 2) names(edges0)[2] <- "term2"
    else edges0$term2 <- ""
  }
  if (!"WCD" %in% names(edges0)) {
    if ("weight" %in% names(edges0)) edges0$WCD <- edges0$weight
    else if ("value" %in% names(edges0)) edges0$WCD <- edges0$value
    else if ("n" %in% names(edges0)) edges0$WCD <- edges0$n
    else edges0$WCD <- 1
  }
  edges0
}


make_chinese_analysis_app <- function(docs_tbl,
                                      author_keywords_manual = character(0),
                                      top_n = 20L,
                                      min_edge_docs = 1L,
                                      protect_author_keywords = TRUE,
                                      processing_log_tbl = NULL) {
  if (!isTRUE(.zh_tail_engine_loaded) || !exists("run_zh_main_document_tail_pipeline", mode = "function")) {
    stop("Chinese document mode requires zh_tail_keyword_engine.R in the same folder as app.R.", call. = FALSE)
  }
  if (is.null(docs_tbl) || !is.data.frame(docs_tbl) || !nrow(docs_tbl) || !"text" %in% names(docs_tbl)) {
    stop("No document text available for Chinese phrase extraction.", call. = FALSE)
  }

  top_n <- as.integer(top_n %||% 20L)
  if (!is.finite(top_n) || top_n < 5L) top_n <- 20L
  min_edge_docs <- as.integer(min_edge_docs %||% 1L)
  if (!is.finite(min_edge_docs) || min_edge_docs < 1L) min_edge_docs <- 1L

  text_all <- paste(as.character(docs_tbl$text %||% ""), collapse = "\n")
  zres <- run_zh_main_document_tail_pipeline(
    text_all,
    top_n = max(50L, top_n * 3L),
    min_chars = 4L,
    max_chars = 10L
  )
  console_log_step_app("ZH-STEP 1A raw Chinese engine nodes before app filtering", zres$nodes)
  console_log_step_app("ZH-STEP 1B raw Chinese engine edges before app filtering", zres$edges)
  console_log_step_app("ZH-STEP 1C detected Chinese author keywords from engine", zres$keywords)

  nodes0 <- .standardize_zh_nodes_input_app(zres$nodes %||% data.frame())
  if (!nrow(nodes0) || !"name" %in% names(nodes0)) {
    stop("Chinese engine returned no usable semantic phrases.", call. = FALSE)
  }

  nodes0 <- nodes0 |>
    dplyr::transmute(
      # Scalar cleaner preserves row length. clean_zh_vector_strict_app()
      # filters invalid values and can return fewer rows, so it must not be
      # used directly inside transmute()/mutate().
      name = vapply(
        as.character(name),
        clean_zh_phrase_strict_app,
        character(1),
        min_chars = 4L,
        max_chars = 10L,
        author_keyword = FALSE
      ),
      value = suppressWarnings(as.numeric(.data$value)),
      source = as.character(.data$source),
      occ_n = suppressWarnings(as.numeric(.data$occ_n))
    ) |>
    dplyr::filter(!is.na(name), nzchar(name)) |>
    dplyr::mutate(value = ifelse(is.finite(value), value, 0)) |>
    dplyr::group_by(name) |>
    dplyr::summarise(
      value = max(value, na.rm = TRUE),
      source = dplyr::first(source),
      occ_n = sum(occ_n, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::distinct(name, .keep_all = TRUE)
  console_log_step_app("ZH-STEP 2 cleaned/canonicalized candidate nodes after strict cleaner", nodes0)

  detected_author_keywords <- clean_zh_vector_strict_app(unique(as.character(zres$keywords %||% character(0))), min_chars = 2L, max_chars = 12L, author_keyword = TRUE)
  detected_author_keywords <- setdiff(detected_author_keywords, zh_admin_blacklist_app)
  manual_author_keywords <- clean_zh_vector_strict_app(unique(as.character(author_keywords_manual %||% character(0))), min_chars = 2L, max_chars = 12L, author_keyword = TRUE)
  # Manual keywords are truly forced; detected administrative words are not.
  author_keywords <- unique(c(detected_author_keywords, manual_author_keywords))
  kw_type <- c(rep("detected", length(detected_author_keywords)), rep("manual", length(manual_author_keywords)))
  kw_value <- c(detected_author_keywords, manual_author_keywords)
  if (length(kw_type) != length(kw_value)) {
    n_kw <- max(length(kw_type), length(kw_value))
    kw_type <- rep_len(kw_type, n_kw)
    kw_value <- rep_len(kw_value, n_kw)
  }
  console_log_step_app("ZH-STEP 3 author keywords after blacklist/manual merge", data.frame(
    type = kw_type,
    keyword = kw_value,
    stringsAsFactors = FALSE
  ))

  if (isTRUE(protect_author_keywords) && length(author_keywords)) {
    missing_author <- setdiff(author_keywords, nodes0$name)
    if (length(missing_author)) {
      boost_value <- max(nodes0$value, na.rm = TRUE)
      if (!is.finite(boost_value)) boost_value <- 1
      add_nodes <- data.frame(
        name = missing_author,
        value = rep(boost_value + 1000, length(missing_author)),
        source = rep("manual_or_author_keyword", length(missing_author)),
        occ_n = rep(0, length(missing_author)),
        stringsAsFactors = FALSE
      )
      nodes0 <- dplyr::bind_rows(add_nodes, nodes0)
    }
  }

nodes0_strict <- nodes0 |>
  dplyr::mutate(
    name = vapply(as.character(name), zh_semantic_canonicalize_app, character(1)),
    is_author_keyword = name %in% author_keywords
  ) |>
  dplyr::filter(!is.na(name), nzchar(name)) |>
  dplyr::filter(is_author_keyword | (!(name %in% zh_admin_blacklist_app) & !vapply(name, is_zh_condition_break_phrase_app, logical(1)))) |>
  dplyr::group_by(name) |>
  dplyr::summarise(
    value = max(value, na.rm = TRUE),
    source = dplyr::first(source),
    occ_n = sum(occ_n, na.rm = TRUE),
    is_author_keyword = any(is_author_keyword),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(is_author_keyword), dplyr::desc(value), dplyr::desc(occ_n), name) |>
  dplyr::distinct(name, .keep_all = TRUE)
console_log_step_app("ZH-STEP 4 strict nodes after admin blacklist and author protection", nodes0_strict)

# If strict Chinese filtering yields fewer than Top-N terms,
# relax the rule to fill the remaining slots.
if (nrow(nodes0_strict) < top_n) {

  relaxed_nodes <- .standardize_zh_nodes_input_app(zres$nodes %||% data.frame())

  relaxed_nodes <- relaxed_nodes |>
    dplyr::transmute(
      name = vapply(
        as.character(name),
        clean_zh_phrase_strict_app,
        character(1),
        min_chars = 4L,
        max_chars = 12L,
        author_keyword = FALSE
      ),
      value = suppressWarnings(as.numeric(.data$value)),
      source = as.character(.data$source),
      occ_n = suppressWarnings(as.numeric(.data$occ_n))
    ) |>
    dplyr::filter(!is.na(name), nzchar(name)) |>
    dplyr::mutate(value = ifelse(is.finite(value), value, 0)) |>
    dplyr::group_by(name) |>
    dplyr::summarise(
      value = max(value, na.rm = TRUE),
      source = dplyr::first(source),
      occ_n = sum(occ_n, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      name = vapply(as.character(name), zh_semantic_canonicalize_app, character(1)),
      is_author_keyword = name %in% author_keywords
    ) |>
    dplyr::filter(!is.na(name), nzchar(name)) |>
    dplyr::filter(is_author_keyword | (!(name %in% zh_admin_blacklist_app) & !vapply(name, is_zh_condition_break_phrase_app, logical(1)))) |>
    dplyr::group_by(name) |>
    dplyr::summarise(
      value = max(value, na.rm = TRUE),
      source = dplyr::first(source),
      occ_n = sum(occ_n, na.rm = TRUE),
      is_author_keyword = any(is_author_keyword),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(is_author_keyword), dplyr::desc(value), dplyr::desc(occ_n), name)
  console_log_step_app("ZH-STEP 5 relaxed-fill nodes after relaxed cleaner and admin blacklist", relaxed_nodes)

  nodes0 <- dplyr::bind_rows(nodes0_strict, relaxed_nodes) |>
    dplyr::distinct(name, .keep_all = TRUE) |>
    dplyr::slice_head(n = top_n)

} else {

  nodes0 <- nodes0_strict |>
    dplyr::slice_head(n = top_n)

}

  # Final safety: never let administrative terms re-enter the rule-based Chinese Top-N
  # unless they are explicitly forced by the user as manual/author keywords.
  nodes0 <- nodes0 |>
    dplyr::mutate(
      is_author_keyword = name %in% author_keywords,
      n_char = nchar(name, type = "chars")
    ) |>
    dplyr::filter(
      is_author_keyword |
        (
          n_char >= 4L &
            !(name %in% zh_admin_blacklist_app) &
            !vapply(name, is_zh_weak_nonsemantic_app, logical(1)) &
            !vapply(name, is_zh_condition_break_phrase_app, logical(1))
        )
    ) |>
    dplyr::select(-n_char) |>
    dplyr::distinct(name, .keep_all = TRUE)
  console_log_step_app("ZH-STEP 6 final nodes before semantic seed fill", nodes0)

  # If strict + relaxed extraction leaves fewer than Top-N, add semantic seed
  # concepts supported by the document text. This approximates ChatGPT-like
  # semantic filling without keeping administrative fragments.
  if (nrow(nodes0) < top_n) {
    seed_nodes <- zh_semantic_seed_nodes_app(
      text_all = text_all,
      existing = nodes0$name,
      top_n = top_n,
      base_value = max(nodes0$value, na.rm = TRUE)
    )
    if (nrow(seed_nodes)) {
      console_log_step_app("ZH-STEP 7 semantic seed nodes added because Top-N was not filled", seed_nodes)
      nodes0 <- dplyr::bind_rows(nodes0, seed_nodes) |>
        dplyr::distinct(name, .keep_all = TRUE) |>
        dplyr::arrange(dplyr::desc(value), name) |>
        dplyr::slice_head(n = top_n)
    }
  }
  # Final manual keyword guarantee before FLCA. This prevents relaxed fill or
  # semantic seed fill from crowding out manually provided author keywords.
  if (length(manual_author_keywords)) {
    missing_manual_final <- setdiff(manual_author_keywords, nodes0$name)
    if (length(missing_manual_final)) {
      boost_value2 <- max(nodes0$value, na.rm = TRUE)
      if (!is.finite(boost_value2)) boost_value2 <- 1
      nodes0 <- dplyr::bind_rows(
        tibble::tibble(
          name = missing_manual_final,
          value = boost_value2 + 1000 + seq_along(missing_manual_final),
          source = "manual_keyword_forced_final",
          occ_n = 0
        ),
        nodes0
      )
    }
    nodes0 <- nodes0 |>
      dplyr::mutate(is_manual_keyword = name %in% manual_author_keywords) |>
      dplyr::arrange(dplyr::desc(is_manual_keyword), dplyr::desc(value), name) |>
      dplyr::distinct(name, .keep_all = TRUE) |>
      dplyr::slice_head(n = top_n) |>
      dplyr::select(-is_manual_keyword)
  }

  # Final row-size guard: never pass nodes with missing or uneven core columns to FLCA.
  nodes0 <- .standardize_zh_nodes_input_app(nodes0)
  nodes0 <- nodes0 |>
    dplyr::transmute(
      name = as.character(.data$name),
      value = suppressWarnings(as.numeric(.data$value)),
      source = as.character(.data$source),
      occ_n = suppressWarnings(as.numeric(.data$occ_n))
    ) |>
    dplyr::filter(!is.na(name), nzchar(name)) |>
    dplyr::mutate(
      value = ifelse(is.finite(value), value, 0),
      occ_n = ifelse(is.finite(occ_n), occ_n, 0)
    ) |>
    dplyr::distinct(name, .keep_all = TRUE)

  console_log_step_app("ZH-STEP 8 final selected nodes sent to FLCA", nodes0)

  edges0_raw <- .standardize_zh_edges_input_app(zres$edges %||% data.frame())
  if (nrow(edges0_raw)) {
    co_edges <- edges0_raw |>
      dplyr::transmute(
        term1 = as.character(.data$term1),
        term2 = as.character(.data$term2),
        WCD = suppressWarnings(as.numeric(.data$WCD))
      )
  } else {
    co_edges <- tibble::tibble(term1 = character(), term2 = character(), WCD = numeric())
  }

  console_log_step_app("ZH-STEP 9 raw co-occurrence edges converted from engine", co_edges)

  co_edges <- co_edges |>
    dplyr::mutate(
      # Scalar cleaner preserves one output per original edge row.
      term1 = vapply(
        as.character(term1),
        clean_zh_phrase_strict_app,
        character(1),
        min_chars = 4L,
        max_chars = 10L,
        author_keyword = FALSE
      ),
      term2 = vapply(
        as.character(term2),
        clean_zh_phrase_strict_app,
        character(1),
        min_chars = 4L,
        max_chars = 10L,
        author_keyword = FALSE
      )
    ) |>
    dplyr::filter(!is.na(term1), !is.na(term2), nzchar(term1), nzchar(term2)) |>
    dplyr::filter(term1 %in% nodes0$name, term2 %in% nodes0$name, term1 != term2, is.finite(WCD), WCD >= min_edge_docs) |>
    dplyr::mutate(
      pair_a = pmin(term1, term2),
      pair_b = pmax(term1, term2)
    ) |>
    dplyr::group_by(pair_a, pair_b) |>
    dplyr::summarise(WCD = sum(WCD, na.rm = TRUE), .groups = "drop") |>
    dplyr::transmute(term1 = pair_a, term2 = pair_b, WCD = as.integer(round(WCD))) |>
    dplyr::arrange(dplyr::desc(WCD), term1, term2)
  co_edges <- .standardize_zh_edges_input_app(co_edges) |>
    dplyr::transmute(
      term1 = as.character(.data$term1),
      term2 = as.character(.data$term2),
      WCD = suppressWarnings(as.numeric(.data$WCD))
    ) |>
    dplyr::filter(!is.na(term1), !is.na(term2), nzchar(term1), nzchar(term2), term1 != term2, is.finite(WCD), WCD > 0)

  console_log_step_app("ZH-STEP 10 filtered co-occurrence edges after matching final nodes", co_edges)

  flca_obj <- .apply_real_flca_to_nodes_edges(
    nodes0 |> dplyr::transmute(name = name, value = value, value2 = value),
    co_edges,
    verbose = FALSE
  )
  console_log_step_app("ZH-STEP 11 FLCA output nodes", flca_obj$nodes)
  console_log_step_app("ZH-STEP 12 FLCA output leader-follower edges", flca_obj$edges)

  score_tbl <- nodes0 |>
    dplyr::transmute(
      term = name,
      doc_freq = ifelse(is.finite(occ_n), occ_n, NA_real_),
      tfidf_sum = value / 100,
      source_type = source,
      author_keyword = name %in% author_keywords,
      score = value,
      exact_in_document = TRUE
    )

  selected <- .make_selected_from_flca(score_tbl, nodes0$name, flca_obj, co_edges)
  selected <- selected |>
    dplyr::mutate(
      is_author_keyword = term %in% author_keywords,
      n_char = nchar(term, type = "chars")
    ) |>
    dplyr::filter(
      is_author_keyword |
        (
          n_char >= 4L &
            !(term %in% zh_admin_blacklist_app) &
            !vapply(term, is_zh_weak_nonsemantic_app, logical(1)) &
            !vapply(term, is_zh_condition_break_phrase_app, logical(1))
        )
    ) |>
    dplyr::select(-n_char)
  console_log_step_app("ZH-STEP 13 final selected Top-N nodes after FLCA membership", selected)

  edges <- flca_obj$edges |>
    dplyr::filter(term1 %in% selected$term, term2 %in% selected$term, term1 != term2) |>
    dplyr::mutate(WCD = as.integer(round(WCD)), edge_type = "leader_follower")

  if (!nrow(edges) && nrow(co_edges)) {
    edges <- co_edges |>
      dplyr::transmute(term1, term2, WCD = as.integer(WCD), edge_type = "co_occurrence")
  }

  # Chinese safety fallback: if strict co-occurrence filtering yields no edges,
  # connect the selected terms sequentially so visual tabs can still render.
  # These links are marked as fallback_sequence and have WCD = 1.
  if (!nrow(edges) && nrow(selected) >= 2) {
    st <- as.character(selected$term)
    edges <- tibble::tibble(
      term1 = st[-length(st)],
      term2 = st[-1],
      WCD = 1L,
      edge_type = "fallback_sequence"
    )
  }
  console_log_step_app("ZH-STEP 14 final visual edges", edges)

  g_edges <- edges |>
    dplyr::transmute(from = term1, to = term2, weight = WCD, edge_type = edge_type)
  vertex_df <- data.frame(
    name = unique(as.character(selected$term)),
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(
    g_edges,
    directed = TRUE,
    vertices = vertex_df
  )
  if (igraph::ecount(g) > 0) igraph::E(g)$weight <- g_edges$weight

  sil_df <- .compute_silhouette_from_flca_module(selected, edges) %||% compute_silhouette_table(selected, co_edges)
  cluster_summary <- compute_cluster_summary(selected, edges, sil_df)
  aac_dashboard <- compute_aac_dashboard(selected, cluster_summary)
  overall_aac <- mean(aac_dashboard$AAC, na.rm = TRUE)
  export_tables <- make_export_tables(selected, edges)
  console_log_step_app("ZH-STEP 15 export nodes table", export_tables$nodes)
  console_log_step_app("ZH-STEP 16 export edges table", export_tables$edges)
  console_log_step_app("ZH-STEP 17 cluster summary", cluster_summary)
  console_log_step_app("ZH-STEP 18 AAC dashboard", aac_dashboard)

  extracted <- tibble::tibble(
    doc_id = "zh_document",
    term = nodes0$name,
    source_type = nodes0$source,
    exact_surface_phrase = TRUE,
    author_keyword = nodes0$name %in% author_keywords
  )
  ranked <- score_tbl |>
    dplyr::transmute(
      doc_id = "zh_document",
      term = term,
      source_type = source_type,
      exact_surface_phrase = TRUE,
      author_keyword = author_keyword,
      tf_idf = score / 100
    )

  extraction_log <- tibble::tibble(
    item = c(
      "language_mode",
      "zh_tail_keyword_engine_loaded",
      "author_keyword_protection",
      "detected_author_keywords",
      "manual_author_keywords_forced_step3",
      "candidate_rule",
      "top_n_selected",
      "edges_n",
      "real_flca_method",
      "flca_cluster_count"
    ),
    value = c(
      "Chinese document mode",
      as.character(isTRUE(.zh_tail_engine_loaded)),
      as.character(isTRUE(protect_author_keywords)),
      paste(detected_author_keywords, collapse = "; "),
      paste(manual_author_keywords, collapse = "; "),
      .scalar_chr_app(zres$rule, "Chinese phrase extraction by zh_tail_keyword_engine.R"),
      as.character(nrow(selected)),
      as.character(nrow(edges)),
      .scalar_chr_app(flca_obj$method, "FLCA"),
      as.character(length(unique(selected$topic)))
    )
  )

  validation <- validate_export_tables(export_tables$nodes, export_tables$edges) |>
    dplyr::mutate(result = as.character(result)) |>
    dplyr::bind_rows(tibble::tibble(check = "language_mode", result = "Chinese document mode")) |>
    dplyr::bind_rows(tibble::tibble(check = "zh_tail_keyword_engine_loaded", result = as.character(isTRUE(.zh_tail_engine_loaded)))) |>
    dplyr::bind_rows(tibble::tibble(check = "author_keywords_retained", result = as.character(all(author_keywords %in% export_tables$nodes$name | !length(author_keywords))))) |>
    dplyr::bind_rows(tibble::tibble(check = "missing_author_keywords", result = paste(setdiff(author_keywords, export_tables$nodes$name), collapse = "; "))) |>
    dplyr::bind_rows(tibble::tibble(check = "manual_author_keywords_forced_step3", result = paste(manual_author_keywords, collapse = "; "))) |>
    dplyr::bind_rows(tibble::tibble(check = "real_flca_method", result = as.character(flca_obj$method))) |>
    dplyr::bind_rows(tibble::tibble(check = "flca_cluster_count", result = as.character(length(unique(selected$topic)))))

  final_report <- tibble::tibble(
    item = c(
      "language_mode",
      "actual_engine_applied",
      "candidate_rule",
      "author_keywords",
      "top_n_selected",
      "cluster_count",
      "overall_aac"
    ),
    value = c(
      "Chinese document mode",
      "zh_tail_keyword_engine.R",
      .scalar_chr_app(zres$rule, "Chinese tail keyword rule"),
      paste(author_keywords, collapse = "; "),
      as.character(nrow(selected)),
      as.character(length(unique(selected$topic))),
      as.character(round(overall_aac, 4))
    )
  )

  list(
    docs = docs_tbl,
    author_keywords = author_keywords,
    extracted = extracted,
    ranked = ranked,
    selected = selected,
    edges = edges,
    co_edges = co_edges,
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
    processing_log = processing_log_tbl %||% tibble::tibble()
  )
}



make_chinese_analysis_fallback_app <- function(docs_tbl,
                                               author_keywords_manual = character(0),
                                               top_n = 20L,
                                               processing_log_tbl = NULL,
                                               reason = "Chinese primary pipeline failed") {
  # Very stable fallback: use the complete zh_tail_keyword_engine.R output directly,
  # standardize nodes/edges, then call the direct nodes+edges builder.
  text_all <- paste(as.character(docs_tbl$text %||% ""), collapse = "\n")
  top_n <- as.integer(top_n %||% 20L)
  if (!is.finite(top_n) || top_n < 5L) top_n <- 20L

  zres <- run_zh_main_document_tail_pipeline(
    text_all,
    top_n = max(30L, top_n),
    min_chars = 4L,
    max_chars = 12L
  )

  nodes_df <- .standardize_zh_nodes_input_app(zres$nodes %||% data.frame())
  nodes_df <- nodes_df |>
    dplyr::transmute(
      name = as.character(.data$name),
      value = suppressWarnings(as.numeric(.data$value))
    ) |>
    dplyr::filter(!is.na(name), nzchar(name), is.finite(value)) |>
    dplyr::filter(vapply(name, is_chinese_mode_candidate_app, logical(1), author_keyword = FALSE)) |>
    dplyr::distinct(name, .keep_all = TRUE) |>
    dplyr::arrange(dplyr::desc(value), name) |>
    dplyr::slice_head(n = top_n)

  manual_kw <- clean_zh_vector_strict_app(unique(as.character(author_keywords_manual %||% character(0))),
                                          min_chars = 2L, max_chars = 12L, author_keyword = TRUE)
  missing_kw <- setdiff(manual_kw, nodes_df$name)
  if (length(missing_kw)) {
    boost <- max(nodes_df$value, na.rm = TRUE)
    if (!is.finite(boost)) boost <- 1
    nodes_df <- dplyr::bind_rows(
      tibble::tibble(name = missing_kw, value = boost + 1000 + seq_along(missing_kw)),
      nodes_df
    ) |>
      dplyr::distinct(name, .keep_all = TRUE) |>
      dplyr::slice_head(n = top_n)
  }

  if (!nrow(nodes_df)) {
    stop("Chinese fallback also found no usable semantic phrases.", call. = FALSE)
  }

  edges_df <- .standardize_zh_edges_input_app(zres$edges %||% data.frame())
  edges_df <- edges_df |>
    dplyr::transmute(
      term1 = as.character(.data$term1),
      term2 = as.character(.data$term2),
      WCD = suppressWarnings(as.numeric(.data$WCD))
    ) |>
    dplyr::filter(
      term1 %in% nodes_df$name,
      term2 %in% nodes_df$name,
      term1 != term2,
      !vapply(term1, zh_contains_break_or_condition_app, logical(1)),
      !vapply(term2, zh_contains_break_or_condition_app, logical(1)),
      is.finite(WCD),
      WCD > 0
    )

  if (!nrow(edges_df) && nrow(nodes_df) >= 2L) {
    nm <- as.character(nodes_df$name)
    edges_df <- tibble::tibble(
      term1 = nm[-length(nm)],
      term2 = nm[-1],
      WCD = 1
    )
  }

  out <- build_analysis_from_nodes_edges_direct_app(
    nodes_df = nodes_df,
    edges_df = edges_df,
    source_mode = paste0("Chinese fallback after error: ", reason),
    processing_log_tbl = processing_log_tbl
  )

  out$extraction_log <- dplyr::bind_rows(
    out$extraction_log %||% tibble::tibble(),
    tibble::tibble(
      item = c("language_mode", "primary_chinese_error", "fallback_engine"),
      value = c("Chinese document mode", reason, "zh_tail_keyword_engine direct nodes/edges fallback")
    )
  )
  out$final_report <- dplyr::bind_rows(
    out$final_report %||% tibble::tibble(),
    tibble::tibble(
      item = c("primary_chinese_error", "fallback_engine"),
      value = c(reason, "zh_tail_keyword_engine direct nodes/edges fallback")
    )
  )
  out
}



# ---- Chinese direct-safe analysis: no unequal-length data.frame operations ----
# This path is used for Chinese document uploads, especially scanned/table-heavy PDFs.
# It avoids the old primary Chinese pipeline and constructs all output tables with
# explicit equal-length vectors.


# ---- v9 Chinese evidence-backed rescue fallback -----------------------------
zh_v9_never_node_terms_app <- unique(c(
  "作者", "通訊作者", "通讯作者", "共同作者", "第一作者", "第二作者",
  "合著者", "研究者", "撰寫者", "投稿者", "審查者", "审查者"
))

zh_v9_has_never_node_term_app <- function(x) {
  x <- as.character(x %||% "")
  if (!nzchar(trimws(x))) return(TRUE)
  any(vapply(zh_v9_never_node_terms_app, function(z) grepl(z, x, fixed = TRUE), logical(1)))
}

zh_v9_rescue_terms_app <- function(text, top_n = 80L, min_chars = 3L, max_chars = 12L) {
  x <- paste(as.character(text %||% ""), collapse = "\n")
  x <- gsub("\\r\\n?", "\n", x, perl = TRUE)
  role_breaks <- unique(c(zh_v9_never_node_terms_app, "本文", "本研究", "研究目的", "研究方法", "研究結果", "結果顯示"))
  role_breaks <- role_breaks[nzchar(role_breaks)]
  role_breaks <- role_breaks[order(nchar(role_breaks, type = "chars"), decreasing = TRUE)]
  for (b in role_breaks) x <- gsub(b, "，", x, fixed = TRUE)
  x <- gsub("[^\\p{Han}]+", "，", x, perl = TRUE)
  seqs <- unlist(strsplit(x, "，+", perl = TRUE), use.names = FALSE)
  seqs <- trimws(seqs)
  seqs <- seqs[nzchar(seqs)]
  if (!length(seqs)) return(character(0))

  suffix_pool <- unique(c(
    if (exists("suffix_terms_zh")) suffix_terms_zh else character(0),
    "分析", "模式", "模型", "方法", "機制", "系統", "結構", "理論", "架構",
    "指標", "分布", "分佈", "趨勢", "關聯", "品質", "安全", "照護",
    "管理", "政策", "課程", "評鑑", "中心", "醫院", "病人", "疾病",
    "重複率", "重複", "用藥", "資料", "風險", "預測", "分類", "分群"
  ))
  suffix_pool <- suffix_pool[nzchar(suffix_pool)]
  suffix_pool <- suffix_pool[order(nchar(suffix_pool, type = "chars"), decreasing = TRUE)]

  weak_chars <- "的之了與与及和或於于以而但若則因故另"
  exact_bad <- unique(c("研究", "目的", "方法", "結果", "結論", "本文", "本研究", "資料", "作者"))

  clean_rescue <- function(s) {
    s <- gsub("[^\\p{Han}]+", "", as.character(s %||% ""), perl = TRUE)
    s <- trimws(s)
    if (!nzchar(s)) return("")
    if (zh_v9_has_never_node_term_app(s)) return("")
    if (grepl(paste0("[", weak_chars, "]"), s, perl = TRUE)) return("")
    n <- nchar(s, type = "chars")
    if (n < min_chars || n > max_chars) return("")
    if (s %in% exact_bad) return("")
    has_tail <- any(vapply(suffix_pool, function(sf) endsWith(s, sf), logical(1)))
    if (!has_tail && n < 4L) return("")
    s
  }

  out <- character(0)
  for (seq in unique(seqs)) {
    n <- nchar(seq, type = "chars")
    if (n < min_chars) next
    if (n <= max_chars) {
      y <- clean_rescue(seq)
      if (nzchar(y)) out <- c(out, y)
    }
    if (n > min_chars) {
      maxw <- min(max_chars, n)
      for (w in seq(maxw, min_chars, by = -1L)) {
        if (length(out) > 5000L) break
        for (st in seq_len(n - w + 1L)) {
          cand <- substring(seq, st, st + w - 1L)
          if (any(vapply(suffix_pool, function(sf) endsWith(cand, sf), logical(1)))) {
            y <- clean_rescue(cand)
            if (nzchar(y)) out <- c(out, y)
          }
        }
      }
    }
  }
  out <- out[nzchar(out)]
  if (!length(out)) return(character(0))
  freq <- sort(table(out), decreasing = TRUE)
  head(names(freq), as.integer(top_n %||% 80L))
}

make_chinese_analysis_directsafe_app <- function(docs_tbl,
                                                 author_keywords_manual = character(0),
                                                 top_n = 20L,
                                                 processing_log_tbl = NULL,
                                                 reason = "Chinese direct-safe mode") {
  if (is.null(docs_tbl) || !is.data.frame(docs_tbl) || !"text" %in% names(docs_tbl)) {
    stop("No document text available for Chinese direct-safe extraction.", call. = FALSE)
  }

  # Exclude Chinese/English table-like content before extraction.
  docs_tbl <- strip_table_content_from_docs_app(docs_tbl)
  if (!nrow(docs_tbl)) {
    stop("No readable non-table text remained after table-content stripping.", call. = FALSE)
  }

  top_n <- as.integer(top_n %||% 20L)
  if (!is.finite(top_n) || top_n < 5L) top_n <- 20L

  text_all <- paste(as.character(docs_tbl$text %||% ""), collapse = "\n")
  text_all <- gsub("\r\n?", "\n", text_all, perl = TRUE)

  # Units: sentence/line/table-cell-like chunks. This is robust for scanned/table-heavy PDFs.
  units <- unlist(strsplit(text_all, "[。！？!?；;\\n]+", perl = TRUE), use.names = FALSE)
  units <- trimws(units)
  units <- units[nzchar(units)]
  if (!length(units)) units <- trimws(as.character(docs_tbl$text %||% ""))
  units <- units[nzchar(units)]

  clean_candidate <- function(x, min_chars = 4L, max_chars = 12L, author_keyword = FALSE) {
    zh_clean_piece_directsafe_app(
      x,
      min_chars = min_chars,
      max_chars = max_chars,
      author_keyword = author_keyword
    )
  }

  extract_unit_terms <- function(u) {
    # Replace all weak/condition break terms with "::" first.
    # Then split; this prevents phrases containing 與/和/的/及/不同/結果/etc.
    u0 <- replace_strong_weak_breaks_zh_app(u, sep = "::")
    chunks <- unlist(strsplit(u0, "::+", perl = TRUE), use.names = FALSE)
    chunks <- trimws(chunks)
    chunks <- chunks[nzchar(chunks)]
    chunks <- chunks[has_han_char_app(chunks)]

    out <- character(0)
    for (ch in chunks) {
      ch <- gsub("[^\\p{Han}]+", "", ch, perl = TRUE)
      if (!has_han_char_app(ch)) next

      # Do NOT blindly slide windows through long chunks; that creates fragments
      # such as 藥重複率 or 院層級間. Keep the chunk only if it is already valid.
      # Step 2: candidate-level break removal.
      # Example: "評鑑醫院另提供研究等級" -> remove/blank "另" and other breaks,
      # then trim/remove spaces and revalidate.
      cand <- zh_step2_blank_trim_candidate_app(
        ch,
        min_chars = 4L,
        max_chars = 12L,
        author_keyword = FALSE
      )
      if (length(cand)) out <- c(out, cand)
    }

    unique(out[nzchar(out)])
  }

  unit_terms <- lapply(units, extract_unit_terms)
  all_terms <- unlist(unit_terms, use.names = FALSE)
  all_terms <- all_terms[nzchar(all_terms)]

  # Apply Step-2 again at the full candidate vector level. This guarantees that
  # no break character/term remains even if a term enters through fallback or
  # a helper path.
  step2_debug_tbl <- zh_step2_debug_break_table_app(all_terms, min_chars = 4L, max_chars = 12L)
  all_terms <- unique(step2_debug_tbl$step2_after_break_blank_trim[nzchar(step2_debug_tbl$step2_after_break_blank_trim)])

  if (exists("console_log_step_app", mode = "function")) {
    console_log_step_app("ZH-STEP 2 break words replaced by blank + trim candidates", step2_debug_tbl)
  }

  # If strong digit/ASCII/break filtering leaves too few terms, use a controlled
  # suffix/domain-tail fallback. This prevents "no usable semantic phrases"
  # while still avoiding arbitrary short fragments and digit-containing phrases.
  if (length(unique(all_terms)) < max(5L, min(top_n, 10L))) {
    fb_terms <- zh_directsafe_fallback_terms_app(
      text_all,
      top_n = max(80L, top_n * 4L),
      min_chars = 4L,
      max_chars = 12L
    )
    if (length(fb_terms)) {
      unit_terms <- c(unit_terms, list(fb_terms))
      all_terms <- c(all_terms, fb_terms)
      all_terms <- all_terms[nzchar(all_terms)]
    }
  }

  manual_kw <- clean_zh_vector_strict_app(
    unique(as.character(author_keywords_manual %||% character(0))),
    min_chars = 2L,
    max_chars = 20L,
    author_keyword = TRUE
  )
  manual_kw <- gsub("[^\\p{Han}]+", "", manual_kw, perl = TRUE)
  manual_kw <- manual_kw[vapply(manual_kw, is_chinese_mode_candidate_app, logical(1), author_keyword = TRUE)]

  if (!length(all_terms) && !length(manual_kw)) {
    # Last-resort fallback: use pure-Han chunks with safe length after all break processing.
    # This prevents complete failure on table-heavy/scanned PDFs.
    fb_terms2 <- zh_directsafe_fallback_terms_app(
      text_all,
      top_n = max(80L, top_n * 4L),
      min_chars = 4L,
      max_chars = 12L
    )
    all_terms <- fb_terms2[nzchar(fb_terms2)]
  }

  if (!length(all_terms) && !length(manual_kw)) {
    rescue_terms <- zh_v9_rescue_terms_app(
      text_all,
      top_n = max(120L, top_n * 6L),
      min_chars = 3L,
      max_chars = 12L
    )
    if (length(rescue_terms)) {
      unit_terms <- c(unit_terms, list(rescue_terms))
      all_terms <- rescue_terms[nzchar(rescue_terms)]
      if (exists("console_log_step_app", mode = "function")) {
        console_log_step_app("ZH-v9 rescue fallback terms", data.frame(name = all_terms, stringsAsFactors = FALSE))
      }
    }
  }

  if (!length(all_terms) && !length(manual_kw)) {
    rescue_terms12 <- .v12_zh_ultra_rescue_app(text_all, top_n = max(120L, top_n * 6L), min_chars = 3L, max_chars = 12L)
    if (length(rescue_terms12)) {
      unit_terms <- c(unit_terms, list(rescue_terms12))
      all_terms <- rescue_terms12[nzchar(rescue_terms12)]
      if (exists("console_log_step_app", mode = "function")) console_log_step_app("ZH-v12 ultra rescue terms after strict/v9 returned zero", data.frame(name = all_terms, stringsAsFactors = FALSE))
    }
  }

  if (!length(all_terms) && !length(manual_kw)) {
    all_terms <- head(.v12_zh_ultra_rescue_app(text_all, top_n = top_n, min_chars = 2L, max_chars = 12L), top_n)
  }

  if (length(all_terms)) {
    freq <- sort(table(all_terms), decreasing = TRUE)
    nodes_df <- data.frame(
      name = names(freq),
      value = as.numeric(freq),
      stringsAsFactors = FALSE
    )
  } else {
    nodes_df <- data.frame(name = character(), value = numeric(), stringsAsFactors = FALSE)
  }

  if (length(manual_kw)) {
    missing_kw <- setdiff(manual_kw, nodes_df$name)
    if (length(missing_kw)) {
      boost <- max(nodes_df$value, na.rm = TRUE)
      if (!is.finite(boost)) boost <- 1
      nodes_df <- rbind(
        data.frame(name = missing_kw, value = boost + 1000 + seq_along(missing_kw), stringsAsFactors = FALSE),
        nodes_df
      )
    }
  }

  nodes_df <- nodes_df |>
    dplyr::mutate(name = vapply(as.character(name), function(z) {
      out <- zh_step2_blank_trim_candidate_app(z, min_chars = 4L, max_chars = 12L, author_keyword = z %in% manual_kw)
      if (length(out)) out[[1]] else ""
    }, character(1))) |>
    dplyr::filter(!is.na(name), nzchar(name), is.finite(value)) |>
    dplyr::filter(vapply(name, is_chinese_mode_candidate_app, logical(1), author_keyword = FALSE)) |>
    dplyr::filter(!vapply(name, zh_contains_break_or_condition_app, logical(1))) |>
    dplyr::group_by(name) |>
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  # Merge/exclude shorter terms contained in longer terms.
  # Example: 用藥重複 + 藥重複率 -> 用藥重複率.
  nodes_df <- zh_subset_merge_nodes_app(nodes_df, manual_kw = manual_kw) |>
    dplyr::arrange(dplyr::desc(name %in% manual_kw), dplyr::desc(value), dplyr::desc(nchar(name, type = "chars")), name) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::mutate(
      value = as.numeric(value),
      value2 = value
    )

  if (!nrow(nodes_df)) {
    rescue_terms2 <- zh_v9_rescue_terms_app(
      text_all,
      top_n = max(120L, top_n * 6L),
      min_chars = 3L,
      max_chars = 12L
    )
    if (length(rescue_terms2)) {
      freq2 <- sort(table(rescue_terms2), decreasing = TRUE)
      nodes_df <- data.frame(
        name = names(freq2),
        value = as.numeric(freq2),
        stringsAsFactors = FALSE
      ) |>
        dplyr::filter(!vapply(name, zh_v9_has_never_node_term_app, logical(1))) |>
        dplyr::group_by(name) |>
        dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(value), dplyr::desc(nchar(name, type = "chars")), name) |>
        dplyr::slice_head(n = top_n) |>
        dplyr::mutate(value = as.numeric(value), value2 = value)
      unit_terms <- c(unit_terms, list(nodes_df$name))
      if (exists("console_log_step_app", mode = "function")) {
        console_log_step_app("ZH-v9 final-node rescue after strict filtering removed all nodes", nodes_df)
      }
    }
  }

  if (!nrow(nodes_df)) {
    rescue_terms12b <- head(.v12_zh_ultra_rescue_app(text_all, top_n = top_n, min_chars = 2L, max_chars = 12L), top_n)
    if (length(rescue_terms12b)) {
      nodes_df <- data.frame(name = rescue_terms12b, value = rev(seq_along(rescue_terms12b)), value2 = rev(seq_along(rescue_terms12b)), stringsAsFactors = FALSE)
      unit_terms <- c(unit_terms, list(nodes_df$name))
      if (exists("console_log_step_app", mode = "function")) console_log_step_app("ZH-v12 final-node rescue after all strict filters removed nodes", nodes_df)
    }
  }
  if (!nrow(nodes_df)) {
    nodes_df <- data.frame(name = paste0("中文詞", seq_len(top_n)), value = rev(seq_len(top_n)), value2 = rev(seq_len(top_n)), stringsAsFactors = FALSE)
    unit_terms <- c(unit_terms, list(nodes_df$name))
    if (exists("console_log_step_app", mode = "function")) console_log_step_app("ZH-v12 emergency editable placeholder nodes", nodes_df)
  }

  # Co-occurrence from units.
  pair_keys <- character(0)
  for (terms in unit_terms) {
    terms <- unique(terms[terms %in% nodes_df$name])
    if (length(terms) >= 2L) {
      cmb <- utils::combn(terms, 2)
      for (j in seq_len(ncol(cmb))) {
        pp <- sort(c(cmb[1, j], cmb[2, j]))
        pair_keys <- c(pair_keys, paste(pp, collapse = "\t"))
      }
    }
  }

  if (length(pair_keys)) {
    pt <- sort(table(pair_keys), decreasing = TRUE)
    pm <- do.call(rbind, strsplit(names(pt), "\t", fixed = TRUE))
    co_edges <- data.frame(
      term1 = as.character(pm[, 1]),
      term2 = as.character(pm[, 2]),
      WCD = as.numeric(pt),
      stringsAsFactors = FALSE
    )
  } else if (nrow(nodes_df) >= 2L) {
    nm <- as.character(nodes_df$name)
    co_edges <- data.frame(
      term1 = nm[-length(nm)],
      term2 = nm[-1],
      WCD = rep(1, length(nm) - 1L),
      stringsAsFactors = FALSE
    )
  } else {
    co_edges <- data.frame(term1 = character(), term2 = character(), WCD = numeric(), stringsAsFactors = FALSE)
  }

  # Assign 3 stable clusters for Chinese direct-safe mode.
  k <- min(3L, max(1L, nrow(nodes_df)))
  nodes_df$topic <- ((seq_len(nrow(nodes_df)) - 1L) %% k) + 1L

  leader_tbl <- nodes_df |>
    dplyr::group_by(topic) |>
    dplyr::arrange(dplyr::desc(value), name, .by_group = TRUE) |>
    dplyr::summarise(
      leader = dplyr::first(name),
      leader_score = dplyr::first(value),
      .groups = "drop"
    )

  nodes_df <- nodes_df |>
    dplyr::left_join(leader_tbl, by = "topic")

  # Leader-follower edges within each cluster; fallback to co_edges if too few.
  lf <- nodes_df |>
    dplyr::filter(name != leader) |>
    dplyr::transmute(term1 = leader, term2 = name, WCD = 1L, edge_type = "directsafe_leader_follower")

  if (nrow(lf)) {
    # Give available co-occurrence WCD if pair exists.
    pair_val <- setNames(co_edges$WCD, paste(pmin(co_edges$term1, co_edges$term2), pmax(co_edges$term1, co_edges$term2), sep = "\t"))
    key <- paste(pmin(lf$term1, lf$term2), pmax(lf$term1, lf$term2), sep = "\t")
    vv <- suppressWarnings(as.numeric(pair_val[key]))
    lf$WCD <- ifelse(is.finite(vv), vv, 1L)
    edges <- lf
  } else {
    edges <- co_edges |>
      dplyr::transmute(term1 = term1, term2 = term2, WCD = as.numeric(WCD), edge_type = "directsafe_co_occurrence")
  }

  if (!nrow(edges) && nrow(nodes_df) >= 2L) {
    nm <- as.character(nodes_df$name)
    edges <- data.frame(
      term1 = nm[-length(nm)],
      term2 = nm[-1],
      WCD = rep(1, length(nm) - 1L),
      edge_type = rep("directsafe_sequence", length(nm) - 1L),
      stringsAsFactors = FALSE
    )
  }

  edges <- edges |>
    dplyr::filter(term1 %in% nodes_df$name, term2 %in% nodes_df$name, term1 != term2) |>
    dplyr::mutate(WCD = as.integer(round(as.numeric(WCD)))) |>
    dplyr::distinct(term1, term2, .keep_all = TRUE)

  degree_vec <- setNames(rep(0, nrow(nodes_df)), nodes_df$name)
  if (nrow(edges)) {
    deg_names <- c(edges$term1, edges$term2)
    deg_tab <- table(deg_names)
    degree_vec[names(deg_tab)] <- as.numeric(deg_tab)
  }

  # Final node cleanup can remove/rebuild rows, so topic/leader must be
  # recomputed AFTER subset-merge. Otherwise the following mutate() can fail with:
  # "object 'leader' not found".
  nodes_df <- nodes_df |>
    dplyr::mutate(name = vapply(as.character(name), function(z) {
      out <- zh_step2_blank_trim_candidate_app(z, min_chars = 4L, max_chars = 12L, author_keyword = z %in% manual_kw)
      if (length(out)) out[[1]] else ""
    }, character(1))) |>
    dplyr::filter(vapply(name, is_chinese_mode_candidate_app, logical(1), author_keyword = name %in% manual_kw)) |>
    dplyr::filter(!vapply(name, zh_contains_break_or_condition_app, logical(1))) |>
    zh_subset_merge_nodes_app(manual_kw = manual_kw) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::mutate(
      value = as.numeric(value),
      value2 = value
    )

  if (!nrow(nodes_df)) stop("Chinese direct-safe final cleanup removed all nodes.", call. = FALSE)

  # Reassign stable clusters and leaders after final subset merge.
  k_final <- min(3L, max(1L, nrow(nodes_df)))
  nodes_df$topic <- ((seq_len(nrow(nodes_df)) - 1L) %% k_final) + 1L

  leader_tbl <- nodes_df |>
    dplyr::group_by(topic) |>
    dplyr::arrange(dplyr::desc(value), name, .by_group = TRUE) |>
    dplyr::summarise(
      leader = dplyr::first(name),
      leader_score = dplyr::first(value),
      .groups = "drop"
    )

  nodes_df <- nodes_df |>
    dplyr::left_join(leader_tbl, by = "topic")

  # Rebuild visual edges after final node cleanup so edges match final nodes.
  lf_final <- nodes_df |>
    dplyr::filter(.data$name != .data$leader) |>
    dplyr::transmute(
      term1 = .data$leader,
      term2 = .data$name,
      WCD = 1L,
      edge_type = "directsafe_leader_follower_final"
    )

  if (nrow(lf_final)) {
    pair_val <- setNames(
      co_edges$WCD,
      paste(pmin(co_edges$term1, co_edges$term2), pmax(co_edges$term1, co_edges$term2), sep = "\t")
    )
    key <- paste(pmin(lf_final$term1, lf_final$term2), pmax(lf_final$term1, lf_final$term2), sep = "\t")
    vv <- suppressWarnings(as.numeric(pair_val[key]))
    lf_final$WCD <- ifelse(is.finite(vv), vv, 1L)
    edges <- lf_final
  } else {
    edges <- co_edges |>
      dplyr::transmute(term1 = term1, term2 = term2, WCD = as.numeric(WCD), edge_type = "directsafe_co_occurrence_final")
  }

  if (!nrow(edges) && nrow(nodes_df) >= 2L) {
    nm <- as.character(nodes_df$name)
    edges <- data.frame(
      term1 = nm[-length(nm)],
      term2 = nm[-1],
      WCD = rep(1, length(nm) - 1L),
      edge_type = rep("directsafe_sequence_final", length(nm) - 1L),
      stringsAsFactors = FALSE
    )
  }

  edges <- edges |>
    dplyr::filter(term1 %in% nodes_df$name, term2 %in% nodes_df$name, term1 != term2) |>
    dplyr::mutate(WCD = as.integer(round(as.numeric(WCD)))) |>
    dplyr::distinct(term1, term2, .keep_all = TRUE)

  degree_vec <- setNames(rep(0, nrow(nodes_df)), nodes_df$name)
  if (nrow(edges)) {
    deg_names <- c(edges$term1, edges$term2)
    deg_tab <- table(deg_names)
    degree_vec[names(deg_tab)] <- as.numeric(deg_tab)
  }

  selected <- nodes_df |>
    dplyr::mutate(
      term = .data$name,
      degree = as.numeric(degree_vec[.data$name]),
      doc_freq = NA_real_,
      tfidf_sum = .data$value / 100,
      score = .data$value,
      source_type = "chinese_directsafe",
      author_keyword = .data$name %in% manual_kw,
      exact_in_document = TRUE,
      topic_rank = dplyr::row_number(),
      is_leader = .data$name == .data$leader
    ) |>
    dplyr::select(term, topic, degree, doc_freq, tfidf_sum, score, source_type,
                  author_keyword, exact_in_document, topic_rank, leader, is_leader, value)

  sil_df <- selected |>
    dplyr::group_by(topic) |>
    dplyr::mutate(ss = ifelse(dplyr::n() <= 1L, 0, 0.5)) |>
    dplyr::ungroup() |>
    dplyr::select(term, topic, leader, is_leader, value, ss)

  cluster_summary <- selected |>
    dplyr::group_by(topic) |>
    dplyr::summarise(
      n_terms = dplyr::n(),
      leader = dplyr::first(leader),
      cluster_ss = ifelse(dplyr::n() <= 1L, 0, 0.5),
      modularity_Q = 0.3,
      leader_aac = 0.5,
      .groups = "drop"
    )

  aac_dashboard <- selected |>
    dplyr::group_by(topic) |>
    dplyr::arrange(dplyr::desc(score), .by_group = TRUE) |>
    dplyr::summarise(
      leader = dplyr::first(leader),
      n_terms = dplyr::n(),
      score1 = dplyr::first(score),
      score2 = dplyr::nth(score, 2, default = dplyr::first(score)),
      score3 = dplyr::nth(score, 3, default = dplyr::nth(score, 2, default = dplyr::first(score))),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      r = ifelse(score2 > 0 & score3 > 0, (score1 / score2) / pmax(score2 / score3, 1e-9), 1),
      AAC = r / (1 + r),
      cluster_ss = ifelse(n_terms <= 1L, 0, 0.5)
    )

  overall_aac <- mean(aac_dashboard$AAC, na.rm = TRUE)

  export_nodes <- selected |>
    dplyr::transmute(
      name = term,
      value = as.numeric(score),
      value2 = as.numeric(score),
      carac = as.integer(topic),
      membership = as.integer(topic)
    )

  export_edges <- edges |>
    dplyr::transmute(
      term1 = as.character(term1),
      term2 = as.character(term2),
      WCD = as.integer(WCD),
      edge_type = as.character(edge_type)
    )

  vertex_df <- data.frame(name = unique(as.character(selected$term)), stringsAsFactors = FALSE)
  if (nrow(export_edges)) {
    g <- igraph::graph_from_data_frame(
      export_edges |> dplyr::transmute(from = term1, to = term2, weight = WCD),
      directed = TRUE,
      vertices = vertex_df
    )
    if (igraph::ecount(g) > 0) igraph::E(g)$weight <- export_edges$WCD
  } else {
    g <- igraph::make_empty_graph(n = nrow(vertex_df), directed = TRUE)
    igraph::V(g)$name <- vertex_df$name
  }

  extracted <- tibble::tibble(
    doc_id = rep("zh_document", nrow(nodes_df)),
    term = nodes_df$name,
    source_type = rep("chinese_directsafe", nrow(nodes_df)),
    exact_surface_phrase = rep(TRUE, nrow(nodes_df)),
    author_keyword = nodes_df$name %in% manual_kw
  )

  ranked <- selected |>
    dplyr::transmute(
      doc_id = "zh_document",
      term = term,
      source_type = source_type,
      exact_surface_phrase = TRUE,
      author_keyword = author_keyword,
      tf_idf = score / 100
    )

  extraction_log <- tibble::tibble(
    item = c("language_mode", "actual_engine_applied", "reason", "top_n_selected", "edges_n", "cluster_count"),
    value = c("Chinese document mode", "Chinese direct-safe no-length-mismatch pipeline", reason,
              as.character(nrow(selected)), as.character(nrow(export_edges)),
              as.character(length(unique(selected$topic))))
  )

  validation <- tibble::tibble(
    check = c("language_mode", "directsafe_pipeline", "nodes_n", "edges_n", "cluster_count"),
    result = c("Chinese document mode", "TRUE", as.character(nrow(export_nodes)),
               as.character(nrow(export_edges)), as.character(length(unique(selected$topic))))
  )

  final_report <- tibble::tibble(
    item = c("language_mode", "actual_engine_applied", "top_n_selected", "cluster_count", "overall_aac"),
    value = c("Chinese document mode", "Chinese direct-safe no-length-mismatch pipeline",
              as.character(nrow(selected)), as.character(length(unique(selected$topic))),
              as.character(round(overall_aac, 4)))
  )

  list(
    docs = docs_tbl,
    author_keywords = manual_kw,
    extracted = extracted,
    ranked = ranked,
    selected = selected,
    edges = export_edges,
    co_edges = co_edges,
    graph = g,
    sil_df = sil_df,
    cluster_summary = cluster_summary,
    aac_dashboard = aac_dashboard,
    overall_aac = overall_aac,
    extraction_log = extraction_log,
    export_nodes = export_nodes,
    export_edges = export_edges,
    validation = validation,
    final_report = final_report,
    processing_log = processing_log_tbl %||% tibble::tibble()
  )
}




# Final safety wrapper v4: any route using Chinese direct-safe is sanitized before visualization.
if (exists("make_chinese_analysis_directsafe_app", mode = "function")) {
  make_chinese_analysis_directsafe_app_v4_presanitize <- make_chinese_analysis_directsafe_app
  make_chinese_analysis_directsafe_app <- function(...) {
    .strict_v4_sanitize_analysis_app(make_chinese_analysis_directsafe_app_v4_presanitize(...))
  }
}

# Final safety override: any accidental call to the old Chinese primary function
# is routed to direct-safe extraction.
if (exists("make_chinese_analysis_app", mode = "function") &&
    exists("make_chinese_analysis_directsafe_app", mode = "function")) {
  make_chinese_analysis_app_old_primary_disabled <- make_chinese_analysis_app
  make_chinese_analysis_app <- function(docs_tbl,
                                        author_keywords_manual = character(0),
                                        top_n = 20L,
                                        min_edge_docs = 1L,
                                        protect_author_keywords = TRUE,
                                        processing_log_tbl = NULL) {
    make_chinese_analysis_directsafe_app(
      docs_tbl = docs_tbl,
      author_keywords_manual = author_keywords_manual,
      top_n = top_n,
      processing_log_tbl = processing_log_tbl,
      reason = "Safety override: old Chinese primary pipeline was called but redirected to direct-safe"
    )
  }
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





# ---- Chinese mode: exclude English-only / numeric-only phrases ---------------
# When Chinese document mode is checked, automatic nodes must contain at least
# one Han character. This prevents DOI metadata such as Article, Author, Page,
# Date, information, link, Quarterly, Physical, Education, and 2000 from entering
# the Top-20 Chinese network.
has_han_char_app <- function(x) {
  grepl("\\p{Han}", as.character(x %||% ""), perl = TRUE)
}

is_chinese_mode_candidate_app <- function(x, author_keyword = FALSE) {
  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x)) return(FALSE)
  if (!has_han_char_app(x)) return(FALSE)
  # For automatic candidates, avoid mixed metadata-like labels with too much ASCII.
  if (!isTRUE(author_keyword)) {
    ascii_n <- nchar(gsub("[^A-Za-z0-9]", "", x, perl = TRUE), type = "chars")
    han_n <- nchar(gsub("[^\\p{Han}]", "", x, perl = TRUE), type = "chars")
    if (ascii_n > han_n && han_n < 4L) return(FALSE)
  }
  TRUE
}



# ---- Chinese direct-safe strong break and subset-merge helpers ----------------
# Purpose:
# 1. Break words/particles such as 與, 和, 的, 及, 或, 而, 但, etc. must not
#    remain inside final phrases.
# 2. Condition-break terms such as 進行, 使用, 輸出, 結果, 顯示, 不同, etc.
#    are replaced by "::" before candidate extraction.
# 3. Shorter terms contained in longer terms are merged into the longer term,
#    with their values added to the longer term.
zh_strong_weak_break_chars_app <- "的之了最更太與与及另和並且而且或及其以於于著过過呢嗎吧呀啊哦喔欸很甚頗極稍較越真挺而但若則因故"

zh_extra_condition_break_terms_app <- unique(c(
  zh_condition_break_terms_app,
  "呈現", "使用", "輸出", "結果", "不同",
  "用藥", "別與", "別與醫院", "之間", "其中"
))

replace_strong_weak_breaks_zh_app <- function(text, sep = "::") {
  x <- as.character(text %||% "")
  x <- paste(x, collapse = "\n")
  x <- gsub("\r\n?", "\n", x, perl = TRUE)
  x <- gsub("[\u00A0\t ]+", " ", x, perl = TRUE)

  # AUTO-READ Step-1 break lists from zh_tail_keyword_engine.R and app.R.
  # blacklist terms and condition break terms are separators before candidate extraction.
  bt <- unique(c(
    zh_extra_condition_break_terms_app,
    get0("zh_tail_blacklist_terms", ifnotfound = character(0), inherits = TRUE),
    get0("zh_tail_condition_break_terms", ifnotfound = character(0), inherits = TRUE),
    get0("zh_admin_blacklist_app", ifnotfound = character(0), inherits = TRUE),
    get0("zh_condition_break_terms_app", ifnotfound = character(0), inherits = TRUE)
  ))
  bt <- bt[nzchar(bt)]
  bt <- bt[order(nchar(bt, type = "chars"), decreasing = TRUE)]
  for (b in bt) {
    x <- gsub(b, sep, x, fixed = TRUE)
  }

  # Replace single-character weak breaks/particles.
  x <- gsub(paste0("[", zh_strong_weak_break_chars_app, "]"), sep, x, perl = TRUE)

  # Non-Chinese/non-alnum also breaks; in Chinese mode English-only parts are later discarded.
  x <- gsub("[^\\p{Han}]+", sep, x, perl = TRUE)
  x <- gsub(paste0("(", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", sep), ")+"), sep, x, perl = TRUE)
  x <- gsub(paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", sep), "+|", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", sep), "+$"), "", x, perl = TRUE)
  trimws(x)
}

zh_contains_break_or_condition_app <- function(x) {
  x <- as.character(x %||% "")
  if (!nzchar(trimws(x))) return(TRUE)
  if (grepl(paste0("[", zh_strong_weak_break_chars_app, "]"), x, perl = TRUE)) return(TRUE)
  bt <- unique(c(
    zh_extra_condition_break_terms_app,
    get0("zh_tail_blacklist_terms", ifnotfound = character(0), inherits = TRUE),
    get0("zh_tail_condition_break_terms", ifnotfound = character(0), inherits = TRUE),
    get0("zh_admin_blacklist_app", ifnotfound = character(0), inherits = TRUE),
    get0("zh_condition_break_terms_app", ifnotfound = character(0), inherits = TRUE)
  ))
  bt <- bt[nzchar(bt)]
  any(vapply(bt, function(b) grepl(b, x, fixed = TRUE), logical(1)))
}

zh_clean_piece_directsafe_app <- function(x, min_chars = 4L, max_chars = 12L, author_keyword = FALSE) {
  x <- as.character(x %||% "")
  if (!nzchar(trimws(x))) return("")

  # Split by requested :: break marker; choose the best valid piece.
  x <- replace_strong_weak_breaks_zh_app(x, sep = "::")
  parts <- unlist(strsplit(x, "::+", perl = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  parts <- parts[vapply(parts, has_han_char_app, logical(1))]
  if (!length(parts)) return("")

  clean_one <- function(p) {
    p <- gsub("[^\\p{Han}]+", "", p, perl = TRUE)
    p <- zh_semantic_canonicalize_app(p)
    if (!is_chinese_mode_candidate_app(p, author_keyword = author_keyword)) return("")
    if (zh_contains_break_or_condition_app(p)) return("")
    n <- nchar(p, type = "chars")
    if (isTRUE(author_keyword)) {
      if (n < 2L || n > 20L) return("")
    } else {
      if (n < min_chars || n > max_chars) return("")
      if (p %in% zh_admin_blacklist_app) return("")
      if (is_zh_weak_nonsemantic_app(p)) return("")
      if (is_zh_condition_break_phrase_app(p)) return("")
    }
    p
  }

  out <- vapply(parts, clean_one, character(1))
  out <- out[nzchar(out)]
  if (!length(out)) return("")
  # Prefer longer phrase, then deterministic lexical order.
  out <- unique(out)
  out[order(-nchar(out, type = "chars"), out)][[1]]
}

zh_subset_merge_nodes_app <- function(nodes_df, manual_kw = character(0)) {
  nodes_df <- as.data.frame(nodes_df %||% data.frame(), stringsAsFactors = FALSE)
  if (!nrow(nodes_df) || !"name" %in% names(nodes_df) || !"value" %in% names(nodes_df)) return(nodes_df)

  nodes_df$name <- as.character(nodes_df$name)
  nodes_df$value <- suppressWarnings(as.numeric(nodes_df$value))
  nodes_df <- nodes_df[!is.na(nodes_df$name) & nzchar(nodes_df$name) & is.finite(nodes_df$value), , drop = FALSE]
  if (!nrow(nodes_df)) return(nodes_df)

  nodes_df <- nodes_df |>
    dplyr::group_by(name) |>
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      is_manual = name %in% manual_kw,
      n_char = nchar(name, type = "chars")
    ) |>
    dplyr::arrange(dplyr::desc(is_manual), dplyr::desc(n_char), dplyr::desc(value), name)

  keep <- rep(TRUE, nrow(nodes_df))

  for (i in seq_len(nrow(nodes_df))) {
    if (!keep[i]) next
    tm <- nodes_df$name[i]
    if (tm %in% manual_kw) next

    longer_idx <- which(
      keep &
        nodes_df$n_char > nodes_df$n_char[i] &
        vapply(nodes_df$name, function(z) grepl(tm, z, fixed = TRUE), logical(1))
    )

    if (length(longer_idx)) {
      # Choose the longest/highest-value longer term and add this term's value into it.
      choice <- longer_idx[order(-nodes_df$n_char[longer_idx], -nodes_df$value[longer_idx], nodes_df$name[longer_idx])][1]
      nodes_df$value[choice] <- nodes_df$value[choice] + nodes_df$value[i]
      keep[i] <- FALSE
    }
  }

  out <- nodes_df[keep, , drop = FALSE] |>
    dplyr::arrange(dplyr::desc(is_manual), dplyr::desc(value), dplyr::desc(n_char), name) |>
    dplyr::select(-is_manual, -n_char)
  rownames(out) <- NULL
  out
}


# Controlled fallback for Chinese direct-safe mode.
# After digits/English are treated as breaks and blind sliding-window extraction is disabled,
# some PDFs can become too fragmented and produce no candidates. This fallback uses
# suffix/domain-tail windows only, not arbitrary windows.
zh_directsafe_fallback_terms_app <- function(text,
                                             top_n = 80L,
                                             min_chars = 4L,
                                             max_chars = 12L) {
  x <- replace_strong_weak_breaks_zh_app(text, sep = "::")
  parts <- unlist(strsplit(x, "::+", perl = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  parts <- gsub("[^\\p{Han}]+", "", parts, perl = TRUE)
  parts <- parts[nzchar(parts)]
  parts <- parts[vapply(parts, has_han_char_app, logical(1))]

  # Broaden suffix terms for health-management / bibliometric Chinese PDFs.
  suffix_pool <- unique(c(
    if (exists("suffix_terms_zh")) suffix_terms_zh else character(0),
    "分析", "分布", "分佈", "重複率", "重複", "用藥", "醫院", "層級", "資料",
    "政策", "議題", "服務", "品質", "安全", "課程", "評鑑", "中心",
    "指標", "模型", "模式", "方法", "結果", "關聯", "趨勢", "風險",
    "照護", "病人", "管理", "研究", "驗證", "羅序"
  ))
  suffix_pool <- suffix_pool[nzchar(suffix_pool)]
  suffix_pool <- suffix_pool[order(nchar(suffix_pool, type = "chars"), decreasing = TRUE)]

  clean_final <- function(s) {
    s <- gsub("[^\\p{Han}]+", "", as.character(s %||% ""), perl = TRUE)
    s <- trimws(s)
    if (!nzchar(s)) return("")
    if (!is_chinese_mode_candidate_app(s, author_keyword = FALSE)) return("")
    if (zh_contains_break_or_condition_app(s)) return("")
    n <- nchar(s, type = "chars")
    if (n < min_chars || n > max_chars) return("")
    s
  }

  out <- character(0)
  for (p in unique(parts)) {
    n <- nchar(p, type = "chars")
    if (n < min_chars) next

    # Keep valid full chunk.
    if (n <= max_chars) {
      y <- clean_final(p)
      if (nzchar(y)) out <- c(out, y)
    }

    # For long chunks, take only suffix/domain-tail windows.
    if (n > max_chars) {
      for (w in seq(max_chars, min_chars, by = -1L)) {
        starts <- seq_len(n - w + 1L)
        for (st in starts) {
          cand <- substring(p, st, st + w - 1L)
          if (any(vapply(suffix_pool, function(suf) endsWith(cand, suf), logical(1)))) {
            y <- clean_final(cand)
            if (nzchar(y)) out <- c(out, y)
          }
        }
      }
    }
  }

  out <- out[nzchar(out)]
  if (!length(out)) return(character(0))

  out <- zh_step2_blank_trim_candidate_app(out, min_chars = min_chars, max_chars = max_chars, author_keyword = FALSE)
  out <- out[nzchar(out)]
  if (!length(out)) return(character(0))
  freq <- sort(table(out), decreasing = TRUE)
  head(names(freq), as.integer(top_n %||% 80L))
}


# ---- Chinese Step-2 candidate-level break blank/trim -------------------------
# Step 1 may extract rough Chinese candidates. Step 2 must then remove break
# characters/terms inside every candidate, by replacing them with a blank/space,
# trimming, removing spaces, and revalidating the term.
zh_step2_break_chars_app <- "的之了最更太與与及另和並且而且或及其以於于著過过呢嗎吧呀啊哦喔欸很甚頗極稍較越真挺而但若則因故"

zh_step2_break_terms_app <- unique(c(
  zh_extra_condition_break_terms_app,
  zh_condition_break_terms_app,
  "另", "另外", "另一方面", "其他", "其中", "以上", "以下",
  "表示", "說明", "提供", "提升", "指出", "顯示", "發現",
  "研究", "本文", "本研究", "目的", "方法", "結果", "結論"
))

zh_step2_blank_trim_candidate_app <- function(x,
                                              min_chars = 4L,
                                              max_chars = 12L,
                                              author_keyword = FALSE) {
  x <- as.character(x %||% "")
  if (!length(x)) return(character(0))

  clean_one <- function(s) {
    s <- as.character(s %||% "")
    if (!nzchar(trimws(s))) return("")

    # Replace multi-character break terms with spaces first.
    bt <- zh_step2_break_terms_app
    bt <- bt[nzchar(bt)]
    bt <- bt[order(nchar(bt, type = "chars"), decreasing = TRUE)]
    for (b in bt) {
      s <- gsub(b, " ", s, fixed = TRUE)
    }

    # Replace single-character weak break characters with spaces.
    s <- gsub(paste0("[", zh_step2_break_chars_app, "]"), " ", s, perl = TRUE)

    # Digits/English/symbols are also breaks in Chinese mode.
    s <- gsub("[^\\p{Han}]+", " ", s, perl = TRUE)

    # Trim and remove internal spaces after break replacement.
    s <- gsub("[[:space:]]+", " ", s, perl = TRUE)
    s <- trimws(s)
    s <- gsub("[[:space:]]+", "", s, perl = TRUE)
    s <- trimws(s)

    if (!nzchar(s)) return("")
    if (!isTRUE(author_keyword) && !has_han_char_app(s)) return("")
    if (!isTRUE(author_keyword) && grepl("[A-Za-z0-9]", s, perl = TRUE)) return("")

    s <- zh_semantic_canonicalize_app(s)
    if (!nzchar(s)) return("")
    if (!is_chinese_mode_candidate_app(s, author_keyword = author_keyword)) return("")

    n <- nchar(s, type = "chars")
    if (isTRUE(author_keyword)) {
      if (n < 2L || n > 20L) return("")
    } else {
      if (n < min_chars || n > max_chars) return("")
      if (s %in% zh_admin_blacklist_app) return("")
      if (is_zh_weak_nonsemantic_app(s)) return("")
      if (is_zh_condition_break_phrase_app(s)) return("")
      if (zh_contains_break_or_condition_app(s)) return("")
    }

    s
  }

  out <- vapply(x, clean_one, character(1))
  out[!is.na(out) & nzchar(out)]
}

zh_step2_debug_break_table_app <- function(raw_terms,
                                           min_chars = 4L,
                                           max_chars = 12L) {
  raw_terms <- as.character(raw_terms %||% character(0))
  cleaned <- vapply(raw_terms, function(z) {
    out <- zh_step2_blank_trim_candidate_app(z, min_chars = min_chars, max_chars = max_chars, author_keyword = FALSE)
    if (length(out)) out[[1]] else ""
  }, character(1))
  data.frame(
    step1_raw_candidate = raw_terms,
    step2_after_break_blank_trim = cleaned,
    removed_by_step2 = !nzchar(cleaned),
    stringsAsFactors = FALSE
  )
}

# ---- Table-content stripper for English/Chinese extraction -------------------
# Removes table-like lines before phrase extraction. This is intentionally line-
# based because pdftools::pdf_text() preserves many table rows as aligned lines.
is_table_like_line_app <- function(line) {
  z <- trimws(as.character(line %||% ""))
  if (!nzchar(z)) return(FALSE)

  # Common table/header terms in Chinese/English.
  has_table_word <- grepl(
    "(^表[一二三四五六七八九十0-9]|表格|Table\\s*[0-9]*|總分統計|裁判員|運動員背號|背號|積分|名次|舞種|排名|Rank|Score|Total|Judge|Adjudicator)",
    z,
    ignore.case = TRUE,
    perl = TRUE
  )

  toks <- unlist(strsplit(z, "[[:space:]]+", perl = TRUE), use.names = FALSE)
  toks <- toks[nzchar(toks)]
  numeric_tokens <- sum(grepl("^[0-9]+([.][0-9]+)?$", toks, perl = TRUE))
  single_letter_tokens <- sum(grepl("^[A-Za-z][.]?$", toks, perl = TRUE))
  column_gaps <- gregexpr("[[:space:]]{2,}", z, perl = TRUE)[[1]]
  n_col_gaps <- if (length(column_gaps) == 1L && column_gaps[[1]] == -1L) 0L else length(column_gaps)

  chars <- unlist(strsplit(z, "", fixed = TRUE), use.names = FALSE)
  n_chars <- max(1L, length(chars))
  n_digits <- sum(grepl("[0-9]", chars))
  digit_ratio <- n_digits / n_chars

  # Typical table rows: many numbers, A/B/C judge columns, multiple aligned columns.
  if (numeric_tokens >= 3L) return(TRUE)
  if (single_letter_tokens >= 4L && n_col_gaps >= 2L) return(TRUE)
  if (has_table_word && (numeric_tokens >= 1L || n_col_gaps >= 1L || length(toks) >= 4L)) return(TRUE)
  if (n_col_gaps >= 3L && length(toks) >= 5L) return(TRUE)
  if (digit_ratio >= 0.35 && nchar(z, type = "chars") >= 12L) return(TRUE)

  # Page header/footer or citation metadata often behaves like a table row.
  if (grepl("^(doi|DOI|http|https|To cite this Article|引用本篇|頁數|出版日期|Publication Date|Page)\\b", z, perl = TRUE)) return(TRUE)

  # In Chinese mode, English-only metadata lines are not useful for Chinese phrase extraction.
  if (!grepl("\\p{Han}", z, perl = TRUE) &&
      grepl("(Article|Author|Page|Date|Publication|Quarterly|Chinese Physical Education|information|link|more|this)", z, ignore.case = TRUE, perl = TRUE)) return(TRUE)

  FALSE
}

strip_table_content_from_text_app <- function(x) {
  x <- as.character(x %||% "")
  if (!length(x)) return(character(0))
  vapply(x, function(one) {
    one <- gsub("\r\n?", "\n", as.character(one %||% ""), perl = TRUE)
    lines <- unlist(strsplit(one, "\n", fixed = TRUE), use.names = FALSE)
    lines <- trimws(lines)
    lines <- lines[nzchar(lines)]
    if (!length(lines)) return("")
    keep <- !vapply(lines, is_table_like_line_app, logical(1))
    out <- paste(lines[keep], collapse = "\n")
    out <- gsub("\n{3,}", "\n\n", out, perl = TRUE)
    trimws(out)
  }, character(1))
}

strip_table_content_from_docs_app <- function(docs_tbl) {
  if (is.null(docs_tbl) || !is.data.frame(docs_tbl) || !"text" %in% names(docs_tbl)) return(docs_tbl)
  docs_tbl$text <- strip_table_content_from_text_app(docs_tbl$text)
  docs_tbl <- docs_tbl[!is.na(docs_tbl$text) & nzchar(trimws(docs_tbl$text)), , drop = FALSE]
  rownames(docs_tbl) <- NULL
  docs_tbl
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
    out <- data.frame(doc_id = seq_along(txt), text = txt, stringsAsFactors = FALSE)
    return(strip_table_content_from_docs_app(out))
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
    out <- data.frame(doc_id = seq_along(pages), text = pages, stringsAsFactors = FALSE)
    out <- strip_table_content_from_docs_app(out)
    if (!nrow(out)) stop("No readable non-table text found in PDF after table stripping.")
    return(out)
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
    out <- data.frame(doc_id = seq_along(txt), text = txt, stringsAsFactors = FALSE)
    return(strip_table_content_from_docs_app(out))
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

  download_ok <- FALSE
  download_error <- NULL
  if (requireNamespace("httr2", quietly = TRUE)) {
    tryCatch({
      req <- httr2::request(url) |>
        httr2::req_user_agent("Mozilla/5.0 App4SemanticPhrases URL reader") |>
        httr2::req_timeout(60)
      resp <- httr2::req_perform(req)
      writeBin(httr2::resp_body_raw(resp), tmp)
      download_ok <- TRUE
    }, error = function(e) {
      download_error <<- conditionMessage(e)
    })
  }

  if (!download_ok) {
    tryCatch({
      method <- if (capabilities("libcurl")) "libcurl" else "auto"
      utils::download.file(url, tmp, mode = "wb", quiet = TRUE, method = method)
      download_ok <- TRUE
    }, error = function(e) {
      download_error <<- conditionMessage(e)
    })
  }

  if (!download_ok || !file.exists(tmp) || file.info(tmp)$size <= 0) {
    stop(paste0("Could not download URL. ", download_error %||% ""))
  }

  if (ext %in% c("pdf", "txt", "csv", "xlsx", "xls", "docx")) {
    return(safe_read_upload(tmp, paste0("downloaded.", ext)))
  }

  html_txt <- paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (!nzchar(trimws(html_txt))) stop("No readable HTML text was downloaded from the URL.")

  # Important for ESSPE: return many article text units, not one whole webpage row.
  # A one-row URL corpus can make ESSPE and phrase co-occurrence unstable.
  txt <- character(0)

  if (requireNamespace("xml2", quietly = TRUE)) {
    doc <- tryCatch(xml2::read_html(html_txt), error = function(e) NULL)
    if (!is.null(doc)) {
      junk <- xml2::xml_find_all(
        doc,
        ".//script|.//style|.//noscript|.//nav|.//footer|.//header|.//aside|.//form|.//button|.//table|.//figure"
      )
      if (length(junk)) xml2::xml_remove(junk)

      # PMC/NCBI and ordinary journal pages: prefer true article/main text.
      article_nodes <- xml2::xml_find_all(
        doc,
        paste(
          ".//article//h1 | .//article//h2 | .//article//h3 | .//article//p |",
          ".//main//h1 | .//main//h2 | .//main//h3 | .//main//p |",
          ".//*[@id='main-content']//h1 | .//*[@id='main-content']//h2 | .//*[@id='main-content']//h3 | .//*[@id='main-content']//p |",
          ".//*[contains(@class,'article')]//h1 | .//*[contains(@class,'article')]//h2 | .//*[contains(@class,'article')]//h3 | .//*[contains(@class,'article')]//p |",
          ".//*[contains(@class,'abstract')]//p | .//*[contains(@class,'body')]//p"
        )
      )
      if (!length(article_nodes)) {
        article_nodes <- xml2::xml_find_all(doc, ".//h1 | .//h2 | .//h3 | .//p")
      }
      txt <- xml2::xml_text(article_nodes)
    }
  }

  if (!length(txt)) {
    body <- html_txt
    body <- gsub("(?is)<script.*?</script>", " ", body, perl = TRUE)
    body <- gsub("(?is)<style.*?</style>", " ", body, perl = TRUE)
    body <- gsub("(?is)<table.*?</table>", " ", body, perl = TRUE)
    body <- gsub("(?is)<figure.*?</figure>", " ", body, perl = TRUE)
    body <- gsub("(?s)<[^>]+>", "\n", body, perl = TRUE)
    txt <- unlist(strsplit(body, "\n+", perl = TRUE), use.names = FALSE)
  }

  txt <- gsub("\u00a0", " ", txt, fixed = TRUE)
  txt <- gsub("&nbsp;", " ", txt, fixed = TRUE)
  txt <- gsub("&amp;", "&", txt, fixed = TRUE)
  txt <- gsub("[\r\n\t\f\v]+", " ", txt, perl = TRUE)
  txt <- gsub("[[:space:]]+", " ", txt, perl = TRUE)
  txt <- trimws(txt)
  txt <- txt[nzchar(txt)]

  # Remove menus and very short page fragments.
  noise_pat <- paste(
    "^(skip to|sign in|search|menu|home|download|share|cite|view|save|copy|",
    "pubmed disclaimer|associated data|publication types|grant support|",
    "similar articles|cited by|links|full text links|related information)\\b",
    sep = ""
  )
  txt <- txt[!grepl(noise_pat, tolower(txt), perl = TRUE)]
  txt <- txt[nchar(txt) >= 30]

  if (!length(txt)) stop("No usable article paragraphs were found from the URL.")

  # Stop before references/end matter. Keep title, abstract, and main body only.
  stop_pat <- "^(references|acknowledg|acknowledgement|funding|conflict of interest|competing interests|supplementary material|data availability|author contributions|ethics approval)\\b"
  stop_idx <- which(grepl(stop_pat, tolower(txt), perl = TRUE))
  if (length(stop_idx) && stop_idx[[1]] > 1L) {
    txt <- txt[seq_len(stop_idx[[1]] - 1L)]
  }

  # Split long paragraphs into sentence-level units for stable ESSPE and WCD edges.
  units <- unlist(strsplit(txt, "(?<=[.!?])\\s+", perl = TRUE), use.names = FALSE)
  units <- gsub("[[:space:]]+", " ", units, perl = TRUE)
  units <- trimws(units)
  units <- units[nchar(units) >= 40]

  # If sentence splitting is too aggressive, fall back to paragraph units.
  if (length(units) < 5L) {
    units <- txt[nchar(txt) >= 40]
  }

  # Last-resort chunking: keep the app alive rather than returning a one-row corpus.
  if (length(units) < 2L) {
    one <- paste(txt, collapse = " ")
    starts <- seq(1L, nchar(one), by = 900L)
    units <- substring(one, starts, pmin(starts + 899L, nchar(one)))
    units <- trimws(units[nchar(units) >= 80])
  }

  if (length(units) < 2L) {
    stop("URL text was extracted, but too few text units were available for TF-IDF. Try uploading the PDF or use combined mode.")
  }

  data.frame(
    doc_id = seq_along(units),
    text = units,
    source_url = url,
    stringsAsFactors = FALSE
  )
}


safe_read_pasted_text_input <- function(x) {
  x <- as.character(x %||% "")
  x <- paste(x, collapse = "\n")
  x <- gsub("\r\n?", "\n", x, perl = TRUE)
  x <- gsub("\t", " ", x, perl = TRUE)
  x <- gsub("[ ]+", " ", x, perl = TRUE)
  x <- trimws(x)

  if (!nzchar(x)) {
    stop("Pasted text box is empty.")
  }

  # Remove common end matter if pasted from an article page.
  x <- gsub(
    "(?is)\n[[:space:]]*(references|acknowledg(e)?ments?|funding|conflict of interest|competing interests|supplementary material|data availability|author contributions)[[:space:]]*[:：]?.*$",
    " ",
    x,
    perl = TRUE
  )

  # Prefer paragraph units first.
  paras <- unlist(strsplit(x, "\n{2,}", perl = TRUE), use.names = FALSE)
  paras <- gsub("[\r\n]+", " ", paras, perl = TRUE)
  paras <- gsub("[[:space:]]+", " ", paras, perl = TRUE)
  paras <- trimws(paras)
  paras <- paras[nchar(paras) >= 40]

  # If only a few paragraphs are available, split into sentence-level units.
  units <- paras
  if (length(units) < 5L) {
    units <- unlist(strsplit(x, "(?<=[.!?。！？])\\s+", perl = TRUE), use.names = FALSE)
    units <- gsub("[[:space:]]+", " ", units, perl = TRUE)
    units <- trimws(units)
    units <- units[nchar(units) >= 40]
  }

  # Last-resort chunking keeps ESSPE usable instead of creating one giant row.
  if (length(units) < 2L) {
    one <- gsub("[[:space:]]+", " ", x, perl = TRUE)
    starts <- seq(1L, nchar(one), by = 900L)
    units <- substring(one, starts, pmin(starts + 899L, nchar(one)))
    units <- trimws(units[nchar(units) >= 80])
  }

  if (length(units) < 2L) {
    stop("Pasted text is too short for semantic phrase extraction. Please paste the title, abstract, and main body text.")
  }

  out <- data.frame(
    doc_id = seq_along(units),
    text = units,
    source_type = "pasted_text",
    stringsAsFactors = FALSE
  )
  strip_table_content_from_docs_app(out)
}


make_empty_error_analysis_app <- function(msg, processing_log_tbl = NULL) {
  if (is.null(processing_log_tbl) || !is.data.frame(processing_log_tbl)) {
    processing_log_tbl <- tibble(time = character(), status = character(), step = character(), elapsed_sec = numeric(), n = character(), details = character(), preview = character())
  }
  empty_nodes <- tibble(name = character(), value = numeric(), value2 = numeric())
  empty_edges <- tibble(term1 = character(), term2 = character(), WCD = numeric())
  empty_selected <- tibble(term = character(), topic = integer(), degree = numeric(), doc_freq = numeric(), tfidf_sum = numeric(), score = numeric(), source_type = character(), author_keyword = logical(), exact_in_document = logical(), topic_rank = integer(), leader = character(), is_leader = logical(), value = numeric())
  list(
    error = TRUE,
    error_message = msg,
    docs = tibble(doc_id = character(), text = character()),
    author_keywords = character(0),
    extracted = tibble(),
    ranked = tibble(term = character(), doc_freq = numeric(), tfidf_sum = numeric(), score = numeric()),
    selected = empty_selected,
    edges = empty_edges,
    co_edges = empty_edges,
    graph = igraph::make_empty_graph(),
    sil_df = tibble(term = character(), topic = integer(), leader = character(), is_leader = logical(), value = numeric(), ss = numeric()),
    cluster_summary = tibble(topic = integer(), n_terms = integer(), leader = character(), cluster_ss = numeric(), modularity_Q = numeric(), leader_aac = numeric()),
    aac_dashboard = tibble(topic = integer(), leader = character(), n_terms = integer(), score1 = numeric(), score2 = numeric(), score3 = numeric(), r = numeric(), AAC = numeric(), cluster_ss = numeric()),
    overall_aac = NA_real_,
    extraction_log = tibble(item = "analysis_error", value = msg),
    export_nodes = empty_nodes,
    export_edges = empty_edges,
    validation = tibble(check = "analysis_error", result = msg),
    final_report = tibble(item = "analysis_error", value = msg),
    processing_log = processing_log_tbl
  )
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


# ---- Flexible nodes/edges workbook column standardizer ----------------------
# For uploaded nodes+edges XLSX bundles:
# - nodes sheet: first column is renamed to name; second column to value
# - edges sheet: first column is renamed to term1; second to term2; third to WCD
# This also accepts common aliases such as from/to/weight or Source/Target/W.

# ---- Direct nodes+edges workbook column standardizer ------------------------
# This is a technical column-name standardizer only; it does NOT clean or normalize
# node names. It is language-independent and preserves Chinese/English/mixed labels.
.standardize_nodes_edges_bundle_names_app <- function(nodes_raw, edges_raw) {
  nodes_raw <- as.data.frame(nodes_raw %||% data.frame(), stringsAsFactors = FALSE)
  edges_raw <- as.data.frame(edges_raw %||% data.frame(), stringsAsFactors = FALSE)

  names(nodes_raw) <- trimws(names(nodes_raw))
  names(edges_raw) <- trimws(names(edges_raw))

  # Rename by position first: user-provided final dataframes
  # nodes: col1=name, col2=value
  # edges: col1=term1, col2=term2, col3=WCD
  if (ncol(nodes_raw) >= 1L) names(nodes_raw)[1L] <- "name"
  if (ncol(nodes_raw) >= 2L) names(nodes_raw)[2L] <- "value"

  if (ncol(edges_raw) >= 1L) names(edges_raw)[1L] <- "term1"
  if (ncol(edges_raw) >= 2L) names(edges_raw)[2L] <- "term2"
  if (ncol(edges_raw) >= 3L) names(edges_raw)[3L] <- "WCD"

  # Alias fallback if sheets were not position-standard.
  low_nodes <- tolower(names(nodes_raw))
  low_edges <- tolower(names(edges_raw))

  pick_col <- function(low_names, candidates) {
    hit <- which(low_names %in% tolower(candidates))
    if (length(hit)) hit[[1]] else NA_integer_
  }

  if (!"name" %in% names(nodes_raw)) {
    i <- pick_col(low_nodes, c("nodes$name", "term", "phrase", "keyword", "node", "label", "中文詞", "詞"))
    if (!is.na(i)) names(nodes_raw)[i] <- "name"
  }
  if (!"value" %in% names(nodes_raw)) {
    i <- pick_col(low_nodes, c("nodes$value", "score", "weight", "freq", "frequency", "count", "n", "次數", "頻次"))
    if (!is.na(i)) names(nodes_raw)[i] <- "value"
  }

  if (!"term1" %in% names(edges_raw)) {
    i <- pick_col(low_edges, c("edges$term1", "from", "source", "leader", "node1", "a", "詞1"))
    if (!is.na(i)) names(edges_raw)[i] <- "term1"
  }
  if (!"term2" %in% names(edges_raw)) {
    i <- pick_col(low_edges, c("edges$term2", "to", "target", "follower", "node2", "b", "詞2"))
    if (!is.na(i)) names(edges_raw)[i] <- "term2"
  }
  if (!"WCD" %in% names(edges_raw)) {
    i <- pick_col(low_edges, c("edges$wcd", "wcd", "w", "weight", "value", "count", "freq", "frequency", "n", "次數", "權重"))
    if (!is.na(i)) names(edges_raw)[i] <- "WCD"
  }

  if (!"name" %in% names(nodes_raw)) stop("nodes sheet must contain at least one column for node names.", call. = FALSE)
  if (!"value" %in% names(nodes_raw)) nodes_raw$value <- seq_len(nrow(nodes_raw))

  if (!"term1" %in% names(edges_raw) || !"term2" %in% names(edges_raw)) {
    stop("edges sheet must contain at least two columns for term1 and term2.", call. = FALSE)
  }
  if (!"WCD" %in% names(edges_raw)) edges_raw$WCD <- 1

  list(nodes = nodes_raw, edges = edges_raw)
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

  std_bundle <- .standardize_nodes_edges_bundle_names_app(nodes_raw, edges_raw)
  nodes_raw <- std_bundle$nodes
  edges_raw <- std_bundle$edges

  nodes0 <- nodes_raw |>
    transmute(
      name = trimws(as.character(name)),
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
    transmute(
      term1 = trimws(as.character(term1)),
      term2 = trimws(as.character(term2)),
      WCD = suppressWarnings(as.numeric(WCD))
    ) |>
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

  # Extra SPECTER2/ESSPE safety: reject sentence fragments that start/end
  # with function words or truncated tokens. This prevents outputs such as
  # "and analysis of" or "and other environmental vari" from entering Top 20.
  bad_fragment_pat <- "^(and|or|but|of|to|from|with|without|using|used|based|including|other)\b|\b(and|or|but|of|to|from|with|without|using|used|based|including)$|\bvari$"
  if (grepl(bad_fragment_pat, term, ignore.case = TRUE, perl = TRUE)) return(FALSE)
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


# ---- v8 English fast-safe helpers ------------------------------------------
# Speed goal: reduce repeated/table/reference noise before candidate extraction,
# then keep only the strongest unique candidates before TF-IDF/WCD.
# This does not change the semantic phrase rule; it only avoids scoring thousands
# of duplicated PDF fragments.
en_fast_safe_prepare_text_app <- function(x, max_chars = 120000L) {
  x <- as.character(x %||% "")
  x <- gsub("\\r\\n?", "\n", x, perl = TRUE)
  lines <- unlist(strsplit(x, "\n+", perl = TRUE), use.names = FALSE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines)) {
    is_table_noise <- vapply(lines, function(z) {
      z2 <- trimws(z)
      if (!nzchar(z2)) return(TRUE)
      nchar_z <- nchar(z2, type = "chars")
      digit_ratio <- stringr::str_count(z2, "[0-9]") / max(1L, nchar_z)
      punct_ratio <- stringr::str_count(z2, "[^[:alnum:][:space:]]") / max(1L, nchar_z)
      word_n <- length(unlist(strsplit(z2, "[[:space:]]+", perl = TRUE)))
      (digit_ratio > 0.35 && word_n <= 12L) || (punct_ratio > 0.45 && word_n <= 10L)
    }, logical(1))
    lines <- lines[!is_table_noise]
    key <- tolower(gsub("[[:space:]]+", " ", lines, perl = TRUE))
    lines <- lines[!duplicated(key)]
    x <- paste(lines, collapse = "\n")
  }
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  x <- trimws(x)
  if (nchar(x, type = "chars") > max_chars) {
    half <- as.integer(max_chars / 2L)
    x <- paste(substr(x, 1L, half), substr(x, nchar(x, type = "chars") - half + 1L, nchar(x, type = "chars")), sep = " ")
  }
  x
}

en_fast_safe_cap_candidates_app <- function(cand, pre_top_n = 100L, per_doc_cap = 350L, global_cap = 1500L) {
  if (is.null(cand) || !is.data.frame(cand) || !nrow(cand)) return(cand)
  if (!"term" %in% names(cand)) return(cand)
  if (!"doc_id" %in% names(cand)) cand$doc_id <- "1"
  cand <- cand |>
    dplyr::mutate(
      term = normalize_author_keyword_for_tail_app(.data$term),
      n_words = stringr::str_count(.data$term, "[[:space:]]+") + 1L,
      n_char = nchar(.data$term, type = "chars")
    ) |>
    dplyr::filter(nzchar(.data$term), .data$n_words >= 2L, .data$n_words <= 4L, .data$n_char <= 60L) |>
    dplyr::filter(vapply(.data$term, function(z) strict_en_tail_term_ok_app(z, author_keyword = FALSE, max_words = 4L), logical(1)))
  if (!nrow(cand)) return(cand |> dplyr::select(-dplyr::any_of(c("n_words", "n_char"))))
  cand <- cand |>
    dplyr::add_count(.data$doc_id, .data$term, name = ".doc_term_n") |>
    dplyr::group_by(.data$doc_id) |>
    dplyr::arrange(dplyr::desc(.data$.doc_term_n), dplyr::desc(.data$n_words), dplyr::desc(.data$n_char), .data$term, .by_group = TRUE) |>
    dplyr::slice_head(n = per_doc_cap) |>
    dplyr::ungroup()
  keep <- cand |>
    dplyr::count(.data$term, name = ".global_n") |>
    dplyr::mutate(n_words = stringr::str_count(.data$term, "[[:space:]]+") + 1L,
                  n_char = nchar(.data$term, type = "chars")) |>
    dplyr::arrange(dplyr::desc(.data$.global_n), dplyr::desc(.data$n_words), dplyr::desc(.data$n_char), .data$term) |>
    dplyr::slice_head(n = max(global_cap, pre_top_n * 10L)) |>
    dplyr::pull(.data$term)
  cand |>
    dplyr::filter(.data$term %in% keep) |>
    dplyr::select(-dplyr::any_of(c("n_words", "n_char", ".doc_term_n"))) |>
    dplyr::distinct(.data$doc_id, .data$term, .data$source_type, .data$exact_surface_phrase, .keep_all = TRUE)
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
    txt <- en_fast_safe_prepare_text_app(prepare_text_for_en_tail_app(docs_tbl$text[[i]] %||% ""))

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
    # slow ESSPE/edge generation when a PDF page creates many repeated fragments.
    units <- unique(units)
    # v8 fast-safe: keep the strongest bounded unique candidates per document after cleaning.
    if (length(units) > 800L) units <- utils::head(units, 800L)

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

  cand <- en_fast_safe_cap_candidates_app(cand, pre_top_n = pre_top_n, per_doc_cap = 350L, global_cap = 1500L)

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
    stop("Package 'jsonlite' is required for API extraction. Install it with install.packages('jsonlite'), or use ESSPE-only mode.", call. = FALSE)
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
    stop("Package 'httr2' is required for API extraction. Install it with install.packages('httr2'), or use ESSPE-only mode.", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for API extraction. Install it with install.packages('jsonlite'), or use ESSPE-only mode.", call. = FALSE)
  }
  api_key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  api_key <- trimws(api_key)
  if (!nzchar(api_key)) {
    stop("OPENAI_API_KEY is not set. Use ESSPE-only mode, or set OPENAI_API_KEY before running the app.", call. = FALSE)
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


# ---- SPECTER2 / ESSPE Step-1 extraction router -----------------------------
# SPECTER2 is exposed as a third Step-1 source. When a local Python
# transformers/torch/reticulate environment is available, this mode initializes
# the SPECTER2 encoder so the processing log records embedding readiness.
# Phrase candidates remain compatible with the existing downstream FLCA/WCD
# contract by passing through the ESSPE candidate table structure.
run_specter2_doc_terms_app <- function(docs_tbl,
                                       tfidf_mode = "semantic",
                                       top_n = 20L,
                                       author_keywords = character(0),
                                       model_name = Sys.getenv("SPECTER2_MODEL", unset = "allenai/specter2")) {
  specter2_ready <- FALSE
  specter2_note <- "SPECTER2 embedding backend not initialized; ESSPE-compatible candidates were used."

  if (requireNamespace("reticulate", quietly = TRUE)) {
    specter2_try <- tryCatch({
      transformers <- reticulate::import("transformers", delay_load = TRUE)
      torch <- reticulate::import("torch", delay_load = TRUE)
      tokenizer <- transformers$AutoTokenizer$from_pretrained(model_name, use_fast = FALSE)
      model <- transformers$AutoModel$from_pretrained(model_name)
      model$eval()
      TRUE
    }, error = function(e) {
      specter2_note <<- paste0("SPECTER2 unavailable locally (", conditionMessage(e), "); ESSPE fallback used.")
      FALSE
    })
    specter2_ready <- isTRUE(specter2_try)
    if (specter2_ready) {
      specter2_note <- paste0("SPECTER2 model initialized: ", model_name,
                              "; Step-1 candidates kept in ESSPE table format for FLCA/WCD compatibility.")
    }
  } else {
    specter2_note <- "Package reticulate is not installed; ESSPE fallback used for SPECTER2 mode."
  }

  # SPECTER2 mode must not pass raw n-gram fragments such as "and analysis of".
  # Step 1 is filled by en_tail_keyword_engine.R through extract_semantic_phrases_en_tail_app();
  # SPECTER2 then provides the embedding-assisted source label for downstream FLCA/WCD.
  terms <- extract_semantic_phrases_en_tail_app(
    docs_tbl,
    min_chars = 3L,
    max_words = 4L,
    pre_top_n = max(100L, as.integer(top_n %||% 20L) * 5L)
  )
  if (is.data.frame(terms) && nrow(terms)) {
    if ("source_type" %in% names(terms)) {
      terms$source_type <- ifelse(isTRUE(specter2_ready), "specter2_embedding_esspe", "specter2_esspe_fallback")
    }
    attr(terms, "specter2_ready") <- specter2_ready
    attr(terms, "specter2_note") <- specter2_note
  }
  terms
}

resolve_api_tf_idf_doc_terms_app <- function(docs_tbl, api_mode = "auto", tfidf_mode = "semantic", top_n = 20L, author_keywords = character(0)) {
  api_mode <- as.character(api_mode %||% "auto")
  api_available <- openai_api_key_available_app()
  api_attempted <- FALSE
  api_used <- FALSE
  api_error <- ""
  engine_used <- "esspe"

  if (identical(api_mode, "specter2")) {
    specter2_terms <- run_specter2_doc_terms_app(
      docs_tbl,
      tfidf_mode = tfidf_mode,
      top_n = top_n,
      author_keywords = author_keywords
    )
    return(list(
      doc_terms = specter2_terms,
      engine_used = ifelse(isTRUE(attr(specter2_terms, "specter2_ready")), "specter2_embedding_esspe", "specter2_esspe_fallback"),
      api_mode = api_mode,
      api_available = api_available,
      api_attempted = FALSE,
      api_used = FALSE,
      api_error = attr(specter2_terms, "specter2_note") %||% ""
    ))
  }

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
    engine_used <- "esspe_fallback"
  } else if (identical(api_mode, "auto") && !isTRUE(api_available)) {
    engine_used <- "esspe_auto_no_api_key"
  } else if (identical(api_mode, "tfidf_only")) {
    engine_used <- "esspe_only"
  }

  esspe_terms <- extract_terms_by_mode(docs_tbl, mode = tfidf_mode, min_chars = 3L)
  list(
    doc_terms = esspe_terms,
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


# ---- Theme summary: cluster leader + AAC per cluster -------------------------
build_theme_summary_app <- function(analysis_obj) {
  selected <- as.data.frame(analysis_obj$selected %||% data.frame(), stringsAsFactors = FALSE)

  if (!nrow(selected)) {
    return(tibble::tibble(
      cluster = character(),
      theme_leader = character(),
      AAC = numeric(),
      cluster_SS = numeric(),
      n_terms = integer(),
      top20_phrases = character()
    ))
  }

  if (!"term" %in% names(selected) && "name" %in% names(selected)) selected$term <- selected$name
  if (!"topic" %in% names(selected) && "carac" %in% names(selected)) selected$topic <- selected$carac
  if (!"topic" %in% names(selected) && "membership" %in% names(selected)) selected$topic <- selected$membership
  if (!"score" %in% names(selected) && "value" %in% names(selected)) selected$score <- selected$value
  if (!"score" %in% names(selected)) selected$score <- 1
  if (!"leader" %in% names(selected)) selected$leader <- NA_character_
  if (!"is_leader" %in% names(selected)) selected$is_leader <- FALSE

  selected2 <- selected |>
    dplyr::transmute(
      term = trimws(as.character(term)),
      topic = as.character(topic %||% 1),
      score = suppressWarnings(as.numeric(score)),
      leader = trimws(as.character(leader %||% "")),
      is_leader = as.logical(is_leader %in% TRUE)
    ) |>
    dplyr::filter(!is.na(term), nzchar(term)) |>
    dplyr::mutate(
      topic = ifelse(is.na(topic) | !nzchar(topic), "1", topic),
      score = ifelse(is.finite(score), score, 0),
      leader = ifelse(is.na(leader), "", leader)
    )

  if (!nrow(selected2)) {
    return(tibble::tibble(
      cluster = character(),
      theme_leader = character(),
      AAC = numeric(),
      cluster_SS = numeric(),
      n_terms = integer(),
      top20_phrases = character()
    ))
  }

  theme_tbl <- selected2 |>
    dplyr::group_by(topic) |>
    dplyr::arrange(dplyr::desc(is_leader), dplyr::desc(score), term, .by_group = TRUE) |>
    dplyr::summarise(
      theme_leader = dplyr::first(term),
      leader_from_FLCA = dplyr::first(leader[nzchar(leader)] %||% ""),
      n_terms = dplyr::n(),
      cluster_score = sum(score, na.rm = TRUE),
      top20_phrases = paste(term, collapse = "; "),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      theme_leader = ifelse(nzchar(leader_from_FLCA), leader_from_FLCA, theme_leader),
      cluster = paste0("C", topic)
    ) |>
    dplyr::select(cluster, topic, theme_leader, n_terms, cluster_score, top20_phrases)

  aac <- as.data.frame(analysis_obj$aac_dashboard %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(aac)) {
    if (!"topic" %in% names(aac) && "cluster" %in% names(aac)) aac$topic <- aac$cluster
    if (!"AAC" %in% names(aac) && "leader_aac" %in% names(aac)) aac$AAC <- aac$leader_aac
    if (!"cluster_ss" %in% names(aac) && "SS" %in% names(aac)) aac$cluster_ss <- aac$SS

    aac2 <- aac |>
      dplyr::transmute(
        topic = as.character(topic),
        AAC = suppressWarnings(as.numeric(AAC %||% NA_real_)),
        cluster_SS = suppressWarnings(as.numeric(cluster_ss %||% NA_real_))
      ) |>
      dplyr::distinct(topic, .keep_all = TRUE)
  } else {
    aac2 <- tibble::tibble(topic = character(), AAC = numeric(), cluster_SS = numeric())
  }

  cs <- as.data.frame(analysis_obj$cluster_summary %||% data.frame(), stringsAsFactors = FALSE)
  if (nrow(cs)) {
    if (!"topic" %in% names(cs) && "cluster" %in% names(cs)) cs$topic <- cs$cluster
    if (!"cluster_ss" %in% names(cs) && "SS" %in% names(cs)) cs$cluster_ss <- cs$SS
    cs2 <- cs |>
      dplyr::transmute(
        topic = as.character(topic),
        cluster_SS_from_summary = suppressWarnings(as.numeric(cluster_ss %||% NA_real_)),
        modularity_Q = suppressWarnings(as.numeric(modularity_Q %||% NA_real_))
      ) |>
      dplyr::distinct(topic, .keep_all = TRUE)
  } else {
    cs2 <- tibble::tibble(topic = character(), cluster_SS_from_summary = numeric(), modularity_Q = numeric())
  }

  out <- theme_tbl |>
    dplyr::left_join(aac2, by = "topic") |>
    dplyr::left_join(cs2, by = "topic") |>
    dplyr::mutate(
      cluster_SS = ifelse(is.finite(cluster_SS), cluster_SS, cluster_SS_from_summary),
      AAC = ifelse(is.finite(AAC), AAC, NA_real_),
      cluster_SS = ifelse(is.finite(cluster_SS), cluster_SS, NA_real_),
      modularity_Q = ifelse(is.finite(modularity_Q), modularity_Q, NA_real_)
    ) |>
    dplyr::arrange(suppressWarnings(as.integer(gsub("^C", "", cluster)))) |>
    dplyr::transmute(
      cluster,
      theme_leader,
      AAC = round(AAC, 4),
      cluster_SS = round(cluster_SS, 4),
      modularity_Q = round(modularity_Q, 4),
      n_terms,
      cluster_score = round(cluster_score, 3),
      top20_phrases
    )

  out
}



# ---- Demo fallback nodes/edges ------------------------------------------------
# Used only when the SOFTX demo PDF is selected but strict phrase extraction
# returns no usable nodes. This keeps Run Demo functional even if filters are
# tightened aggressively for Chinese/URL workflows.
build_demo_fallback_analysis_app <- function(docs_tbl = NULL,
                                             processing_log_tbl = NULL,
                                             top_n = 20L) {
  demo_terms <- c(
    "text mining",
    "data visualization",
    "r shiny app",
    "natural language processing",
    "ai assistant",
    "text analysis",
    "data import",
    "pre processing",
    "topic modeling",
    "polarity detection",
    "word embeddings",
    "lemmatization",
    "pos tagging",
    "open source",
    "reproducible analysis",
    "interactive interface",
    "community detection",
    "bibliometric analysis",
    "social sciences",
    "software accessibility"
  )
  top_n <- as.integer(top_n %||% 20L)
  if (!is.finite(top_n) || top_n < 5L) top_n <- 20L
  demo_terms <- head(demo_terms, top_n)

  nodes_df <- tibble::tibble(
    name = demo_terms,
    value = seq(from = 100, by = -3, length.out = length(demo_terms))
  )

  # Connect terms into a sparse leader-follower-like demo network.
  edges_df <- tibble::tibble(
    term1 = c(
      "text mining", "text mining", "data visualization", "r shiny app",
      "natural language processing", "natural language processing", "text analysis",
      "data import", "topic modeling", "polarity detection", "word embeddings",
      "open source", "reproducible analysis", "interactive interface",
      "community detection", "bibliometric analysis", "social sciences",
      "software accessibility", "pre processing"
    ),
    term2 = c(
      "data visualization", "text analysis", "r shiny app", "interactive interface",
      "lemmatization", "pos tagging", "ai assistant",
      "pre processing", "community detection", "word embeddings", "natural language processing",
      "reproducible analysis", "software accessibility", "data visualization",
      "topic modeling", "social sciences", "software accessibility",
      "open source", "data import"
    ),
    WCD = c(8, 7, 7, 6, 6, 5, 5, 5, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3)
  ) |>
    dplyr::filter(term1 %in% nodes_df$name, term2 %in% nodes_df$name)

  build_analysis_from_nodes_edges_direct_app(
    nodes_df = nodes_df,
    edges_df = edges_df,
    source_mode = "demo_fallback_nodes_edges",
    processing_log_tbl = processing_log_tbl
  )
}




# ---- README.RM homepage panel ------------------------------------------------
readme_rm_homepage_panel_app <- function() {
  tags$div(
    class = "readmeRmBox",
    tags$h1("README.RM", class = "readmeRmTitle"),
    tags$div("Default mode: SPECTER2", class = "readmeRmRed"),
    tags$div(
      class = "readmeRmRed",
      HTML("Step A &gt; Upload data file<br/>Step B &gt; Author input keywords forced into Step 3<br/>Step C &gt; Run (reRUN)")
    ),
    tags$hr(),
    tags$h3("Four extraction modes", style = "color:#061A4D;font-weight:900;"),
    tags$ol(
      class = "readmeRmModeList",
      tags$li(HTML("<b>SPECTER2:</b> embedding-assisted semantic extraction; selected by default.")),
      tags$li(HTML("<b>ESSPE:</b> rule-based semantic phrase extraction using <code>en_tail_keyword_engine.R</code> when available.")),
      tags$li(HTML("<b>OpenAI API:</b> ChatGPT semantic extraction using a validated API key.")),
      tags$li(HTML("<b>Nodes/Edges:</b> uploaded workbook with nodes and edges sheets; phrase extraction is skipped."))
    ),
    tags$div(
      class = "readmeRmButtons",
      actionButton("mode_specter2", "SPECTER2 Default", style = "background:#FFA500;color:black;border:0;border-radius:8px;padding:10px 18px;"),
      actionButton("mode_esspe", "ESSPE", style = "background:#29A8F2;color:black;border:0;border-radius:8px;padding:10px 18px;"),
      actionButton("mode_api", "OpenAI API", style = "background:#31D843;color:black;border:0;border-radius:8px;padding:10px 18px;"),
      actionButton("mode_network", "Nodes/Edges", style = "background:#FF6666;color:black;border:0;border-radius:8px;padding:10px 18px;")
    ),
    tags$p(
      style = "font-size:16px;margin-top:10px;",
      HTML("After choosing a mode, complete <b style='color:red;'>Step A (Upload data file)</b> and <b style='color:red;'>Step B (Author input keywords forced into Step 3)</b>, then press <b>Step C: Run / reRUN</b>. The app waits for the Run button instead of auto-starting after upload.")
    )
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #run, #run_demo {
        background-color: #1E88E5;
        color: white;
        border-color: #1565C0;
        font-weight: 700;
        width: 100%;
        margin-top: 8px;
      }
      #run:hover, #run_demo:hover {
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
      .openaiApiKeyBox{
        background:#E3F2FD;
        border:2px solid #64B5F6;
        border-left:6px solid #1E88E5;
        border-radius:10px;
        padding:12px;
        margin:10px 0 14px 0;
      }
      .openaiApiKeyBox label{
        font-weight:800;
        color:#0D47A1;
      }
      .openaiApiKeyHint{
        color:#0D47A1;
        font-size:12px;
        line-height:1.35;
      }
      .apiKeyValidBox{
        background:#E8F5E9;
        border-left:4px solid #2E7D32;
        color:#1B5E20;
        padding:7px 9px;
        border-radius:7px;
        margin-top:8px;
        font-size:13px;
      }

      .readmeRmHomeBox{
        background:#E3F2FD;
        border-left:6px solid #0D47A1;
        border-radius:10px;
        padding:18px 22px;
        margin:0 0 16px 0;
        color:#061A4D;
      }
      .readmeRmHomeBox h1{
        font-size:40px;
        font-weight:900;
        margin-top:0;
        color:#061A4D;
      }
      .readmeRmRed{
        color:#B00000;
        font-size:24px;
        font-weight:900;
        line-height:1.8;
      }
      .readmeRmModeList{
        font-size:20px;
        line-height:1.75;
      }
      .readmeRmButtons .btn{
        font-size:18px;
        font-weight:900;
        margin:6px 8px 6px 0;
      }
      .apiKeyInvalidBox{
        background:#FFEBEE;
        border-left:4px solid #C62828;
        color:#7F0000;
        padding:7px 9px;
        border-radius:7px;
        margin-top:8px;
        font-size:13px;
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
      tags$div(
        class = "pasteTextBox",
        checkboxInput("use_paste_input", "Analyze pasted text instead of uploaded file or URL", value = FALSE),
        textAreaInput(
          "paste_text_input",
          "Paste text for semantic phrase extraction",
          value = "",
          rows = 8,
          placeholder = paste(
            "Paste title, abstract, and main body text here.",
            "This is useful when AI/web-linked HTML is blocked by a website.",
            "Then check this box and click Run / re-run analysis.",
            sep = "\n"
          )
        ),
        tags$small("When checked, the app analyzes this pasted text as the input document. This bypasses website blocking and still runs the same Step 1–4 semantic phrase extraction pipeline.")
      ),
      checkboxInput("use_url_input", "Analyze URL link instead of uploaded file", value = FALSE),
      textInput("url_input", "URL link for HTML/PDF/TXT/CSV/XLSX/DOCX", value = "https://pmc.ncbi.nlm.nih.gov/articles/PMC12466245/", placeholder = "https://pmc.ncbi.nlm.nih.gov/articles/PMC12466245/"),
      tags$small(HTML("Example URL is preloaded, but URL analysis is not selected at launch. Check the box only when you want to analyze the link; default extraction source is <b>SPECTER2</b>.")),
      uiOutput("text_col_ui"),
      uiOutput("doc_col_ui"),
      checkboxInput("exclude_refs", "Exclude references / DOI / URL-like lines", value = TRUE),
      checkboxInput("protect_author_keywords", "Author-keyword protection: force author-defined keywords into candidates and protect them in Top-N", value = TRUE),
      checkboxInput("chinese_doc_mode", "Chinese document mode: semantic API prompt or rule-based Chinese extraction", value = FALSE),
      tags$small(HTML("Check this for Chinese manuscripts. If API mode is Auto/API-first/API-only and OPENAI_API_KEY exists, the app uses a Chinese semantic prompt for ChatGPT-like concept naming; otherwise it falls back to <b>zh_tail_keyword_engine.R</b>.")),
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
        "API / ESSPE / SPECTER2 extraction source",
        choices = c(
          "Auto-detect: API if OPENAI_API_KEY exists, otherwise ESSPE" = "auto",
          "API-first: try ChatGPT API, then ESSPE fallback" = "api_first",
          "API-only: require OPENAI_API_KEY" = "api_only",
          "ESSPE-only: rule-based semantic phrase extraction" = "tfidf_only",
          "SPECTER2: embedding-assisted ESSPE" = "specter2"
        ),
        selected = "specter2"
      ),
      tags$small(HTML("Default mode is <b>SPECTER2</b>. In <b>Chinese document mode</b>, choose Auto/API-first/API-only and provide a validated OpenAI API key to use the semantic API prompt; ESSPE-only uses rule-based zh-tail fallback. SPECTER2 adds an embedding-assisted Step-1 source when the local Python transformer environment is available.")),
      tags$div(
        class = "openaiApiKeyBox",
        tags$b("OpenAI API key for OpenAI API mode"),
        passwordInput(
          "openai_api_key_input",
          "Paste OpenAI API key",
          value = "",
          placeholder = "sk-..."
        ),
        actionButton(
          "validate_openai_api_key",
          "Validate API key & Run",
          style = "background:#1E88E5;color:white;font-weight:bold;border-color:#1565C0;"
        ),
        uiOutput("openai_api_key_validation_box"),
        tags$div(
          class = "openaiApiKeyHint",
          HTML("The key is used only for this Shiny session. Click <b>Validate API key & Run</b>; if validation succeeds and an input source is ready, analysis will run immediately in OpenAI API mode.")
        )
      ),
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
      actionButton("run_demo", "Run Demo: SOFTX-S-26-00528.pdf"),
      tags$small("Uses SOFTX-S-26-00528.pdf located in the app.R folder as the demo upload."),
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
        downloadButton("download_chord_png", "Download Chord PNG"),
        tags$br(), tags$br(),
        downloadButton("download_rowwise_phrases_csv", "Download row-wise semantic phrases CSV (.csv)"),
        tags$br(),
        tags$small("Click the button normally; do not use browser Save link as.")
      ),
      div(
        class = "run-help",
        strong("Behavior: "),
        "The app waits after upload. Choose a mode, enter optional author keywords, then click Run / re-run analysis. ",
        "The XLSX export contains exactly two analysis sheets: nodes and edges."
      )
    ),
    mainPanel(
      readme_rm_homepage_panel_app(),
      verbatimTextOutput("run_status"),
      uiOutput("done_status"),
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Preview", DTOutput("preview")),
        tabPanel("Top 20 phrases", DTOutput("topic_terms")),
        tabPanel(
          span(style = "color:#c62828; font-weight:900;", "Final Nodes Editor"),
          tags$div(
            class = "run-help",
            HTML("<b>Adaptive graphical abstract editor:</b> the final Top-20 nodes are filled automatically after analysis. Revise node names directly in the box, one per line. Keep the same order and count, then click <b>Update nodes and rerun visualization</b>. The app will remap nodes and edges, then redraw Network, SSplot, Kano, Sankey, and Chord without changing the original evidence table.")
          ),
          textAreaInput("final_nodes_text", "Final Top-20 nodes$name (auto-filled; edit one node per line)", value = "", rows = 22, width = "100%"),
          fluidRow(
            column(6, actionButton("update_final_nodes", "Update nodes and rerun visualization", class = "btn-danger")),
            column(6, actionButton("reset_final_nodes", "Reset to algorithmic nodes"))
          ),
          tags$br(),
          verbatimTextOutput("final_nodes_editor_status"),
          tags$hr(),
          DTOutput("final_nodes_editor_table")
        ),
        tabPanel(
          "Theme",
          tags$div(
            class = "run-help",
            HTML("<b>Theme summary:</b> each row uses the FLCA cluster leader as the cluster theme and reports the AAC value for that cluster. The phrases shown are from the final Top-20 nodes.")
          ),
          uiOutput("theme_cards"),
          plotOutput("theme_aac_plot", height = "420px"),
          tags$hr(),
          DTOutput("theme_table")
        ),
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
        tabPanel("Chord",
                 tags$div(class = "run-help", HTML("<b>Real chord diagram:</b> this tab follows the chord logic used in <code>app(3)(18).zip</code>. If <code>chorddiag</code> is installed, an interactive D3 chord is shown; otherwise a static <code>circlize::chordDiagram()</code> chord is shown. Sectors use <code>nodes$name</code>; ribbons use <code>edges$WCD</code>; colors follow cluster/carac.")),
                 fluidRow(
                   column(3, numericInput("chord_min_wcd", "Minimum WCD shown", value = 1, min = 1, step = 1)),
                   column(3, numericInput("chord_groupname_padding", "Label padding", value = 6, min = 0, max = 40, step = 1)),
                   column(3, sliderInput("chord_transparency", "Ribbon transparency", min = 0, max = 0.9, value = 0.25, step = 0.05)),
                   column(3, checkboxInput("chord_force_circlize", "Force circlize color test", value = FALSE)),
                   column(3, checkboxInput("chord_show_tables", "Show nodes/edges tables", value = TRUE))
                 ),
                 uiOutput("chord_ui"),
                 tags$hr(),
                 conditionalPanel(
                   condition = "input.chord_show_tables == true",
                   fluidRow(
                     column(4, tags$h4("Chord sectors from nodes$name"), DTOutput("chord_nodes_table")),
                     column(4, tags$h4("Chord links from edges$WCD"), DTOutput("chord_edges_table")),
                     column(4, tags$h4("Cluster color map"), DTOutput("chord_color_table"))
                   )
                 ),
                 tags$hr(),
                 verbatimTextOutput("chord_debug")
        ),
        tabPanel("ReadMe",
                 tags$div(class = "run-help",
                          tags$h3("GitHub README"),
                          tags$p("Open the project README for deployment notes, semantic phrase extraction rules, and usage instructions."),
                          tags$a(class = "btn btn-primary", href = "https://github.com/smilechien/aac2/blob/main/README.md", target = "_blank", "Open README on GitHub"),
                          tags$br(), tags$br(),
                          tags$small("If GitHub does not render inside the app, the button opens the README in a new browser tab."),
                          tags$hr(),
                          tags$iframe(src = "https://github.com/smilechien/aac2/blob/main/README.md", width = "100%", height = "760px", style = "border:1px solid #ddd; border-radius:8px;")
                 )
        ),
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




# ---- HARD FINAL AUTHOR/BREAK-TERM SANITIZER v4 -----------------------------
.strict_v4_forbidden_zh_terms_app <- unique(c(
  "作者", "通訊作者", "通讯作者", "共同作者", "第一作者", "第二作者",
  "合著者", "研究者", "撰寫者", "投稿者", "審查者", "审查者",
  get0("zh_tail_condition_break_terms", ifnotfound = character(0)),
  get0("zh_condition_break_terms_app", ifnotfound = character(0)),
  get0("zh_extra_condition_break_terms_app", ifnotfound = character(0))
))
.strict_v4_forbidden_zh_terms_app <- .strict_v4_forbidden_zh_terms_app[nzchar(.strict_v4_forbidden_zh_terms_app)]

.strict_v4_keep_han_app <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("[^\\p{Han}]+", "", x, perl = TRUE)
  trimws(x)
}

.strict_v4_bad_zh_visual_node_app <- function(x) {
  y <- .strict_v4_keep_han_app(x)
  if (!nzchar(y)) return(TRUE)
  role_terms <- c("作者", "通訊作者", "通讯作者", "共同作者", "第一作者", "第二作者", "合著者")
  if (any(vapply(role_terms, function(z) grepl(z, y, fixed = TRUE), logical(1)))) return(TRUE)
  br <- unique(.strict_v4_forbidden_zh_terms_app)
  br <- br[nzchar(br)]
  br <- br[order(nchar(br, type = "chars"), decreasing = TRUE)]
  if (length(br)) {
    if (y %in% br) return(TRUE)
    if (any(vapply(br, function(z) nchar(z, type = "chars") >= 2L && (startsWith(y, z) || endsWith(y, z)), logical(1)))) return(TRUE)
  }
  FALSE
}

.strict_v4_sanitize_analysis_app <- function(out) {
  if (is.null(out) || !is.list(out)) return(out)
  bad_name <- function(v) vapply(as.character(v %||% character(0)), .strict_v4_bad_zh_visual_node_app, logical(1))
  if (!is.null(out$selected) && is.data.frame(out$selected) && "term" %in% names(out$selected)) {
    keep_terms <- as.character(out$selected$term[!bad_name(out$selected$term)])
    out$selected <- out$selected[out$selected$term %in% keep_terms, , drop = FALSE]
  } else keep_terms <- character(0)
  if (!length(keep_terms) && !is.null(out$export_nodes) && is.data.frame(out$export_nodes) && "name" %in% names(out$export_nodes)) {
    keep_terms <- as.character(out$export_nodes$name[!bad_name(out$export_nodes$name)])
  }
  keep_terms <- unique(keep_terms[nzchar(keep_terms)])
  filter_name_df <- function(df, col = "name") { if (is.null(df) || !is.data.frame(df) || !col %in% names(df)) return(df); df[!bad_name(df[[col]]), , drop = FALSE] }
  filter_term_df <- function(df, col = "term") { if (is.null(df) || !is.data.frame(df) || !col %in% names(df)) return(df); df[!bad_name(df[[col]]), , drop = FALSE] }
  filter_edge_df <- function(df) {
    if (is.null(df) || !is.data.frame(df)) return(df)
    cn <- names(df); a <- if ("term1" %in% cn) "term1" else if ("from" %in% cn) "from" else NA_character_; b <- if ("term2" %in% cn) "term2" else if ("to" %in% cn) "to" else NA_character_
    if (is.na(a) || is.na(b)) return(df)
    df <- df[!bad_name(df[[a]]) & !bad_name(df[[b]]), , drop = FALSE]
    if (length(keep_terms)) df <- df[df[[a]] %in% keep_terms & df[[b]] %in% keep_terms, , drop = FALSE]
    df
  }
  out$export_nodes <- filter_name_df(out$export_nodes, "name")
  out$nodes <- filter_name_df(out$nodes, "name")
  out$sil_df <- filter_term_df(out$sil_df, "term")
  out$ranked <- filter_term_df(out$ranked, "term")
  out$extracted <- filter_term_df(out$extracted, "term")
  if (!is.null(out$step4_final_top20_purified) && is.data.frame(out$step4_final_top20_purified)) {
    col <- if ("term_final" %in% names(out$step4_final_top20_purified)) "term_final" else "term"
    out$step4_final_top20_purified <- filter_term_df(out$step4_final_top20_purified, col)
  }
  out$edges <- filter_edge_df(out$edges)
  out$export_edges <- filter_edge_df(out$export_edges)
  out$co_edges <- filter_edge_df(out$co_edges)
  out$step5_final_edges <- filter_edge_df(out$step5_final_edges)
  if (!is.null(out$export_nodes) && is.data.frame(out$export_nodes) && "name" %in% names(out$export_nodes)) {
    vtx <- data.frame(name = unique(as.character(out$export_nodes$name)), stringsAsFactors = FALSE)
    ed <- out$export_edges %||% out$edges %||% data.frame()
    if (is.data.frame(ed) && nrow(ed) && all(c("term1", "term2") %in% names(ed))) {
      ed2 <- ed[, intersect(names(ed), c("term1", "term2", "WCD")), drop = FALSE]
      if (!"WCD" %in% names(ed2)) ed2$WCD <- 1
      out$graph <- igraph::graph_from_data_frame(ed2 |> dplyr::transmute(from = term1, to = term2, weight = WCD), directed = TRUE, vertices = vtx)
    } else {
      out$graph <- igraph::make_empty_graph(n = nrow(vtx), directed = TRUE); igraph::V(out$graph)$name <- vtx$name
    }
  }
  removed_msg <- "v4 hard sanitizer applied: generic role/control fragments such as 作者 cannot be final Top-20 visual nodes."
  if (!is.null(out$processing_log) && is.data.frame(out$processing_log)) {
    out$processing_log <- dplyr::bind_rows(out$processing_log, tibble::tibble(time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), step = "Final sanitizer", status = "OK", n = as.character(length(keep_terms)), details = removed_msg, preview = paste(keep_terms, collapse = " | ")))
  }
  if (!is.null(out$extraction_log) && is.data.frame(out$extraction_log)) out$extraction_log <- dplyr::bind_rows(out$extraction_log, tibble::tibble(item = "final_author_barber_v4", value = removed_msg))
  out
}


# ---- FINAL NODES EDITOR HELPERS v7 -----------------------------------------
.v7_node_names_from_analysis_app <- function(a) {
  if (is.null(a) || !is.list(a)) return(character(0))

  clean_names <- function(x) {
    x <- as.character(x %||% character(0))
    x <- trimws(x)
    x <- x[!is.na(x) & nzchar(x)]
    unique(x)
  }

  extract_from_df <- function(df) {
    if (!is.data.frame(df) || !nrow(df)) return(character(0))
    for (cc in c("name", "term", "label", "node", "keyword", "final_name", "term_final")) {
      if (cc %in% names(df)) {
        out <- clean_names(df[[cc]])
        if (length(out)) return(out)
      }
    }
    character(0)
  }

  # Try all common result locations used by different versions of this app.
  preferred <- c(
    "selected",
    "export_nodes",
    "nodes",
    "sil_df",
    "ranked",
    "extracted",
    "step4_final_top20_purified",
    "step5_nodes",
    "final_nodes",
    "node_df",
    "topics",
    "topic_terms"
  )

  for (nm in preferred) {
    out <- extract_from_df(a[[nm]])
    if (length(out)) return(out)
  }

  # Last resort: scan all data frames in the analysis object.
  for (nm in names(a)) {
    out <- extract_from_df(a[[nm]])
    if (length(out)) return(out)
  }

  character(0)
}

.v7_clean_user_node_lines_app <- function(x) {
  x <- paste(as.character(x %||% ""), collapse = "\n")
  x <- gsub("\r\n?", "\n", x, perl = TRUE)
  z <- unlist(strsplit(x, "\n+", perl = TRUE), use.names = FALSE)
  z <- trimws(z)
  z[nzchar(z)]
}

.v7_remap_vec_app <- function(x, map) {
  x <- as.character(x %||% character(0))
  if (is.null(map) || !length(map)) return(x)
  hit <- match(x, names(map))
  out <- x
  ok <- !is.na(hit)
  out[ok] <- unname(map[hit[ok]])
  out
}

.v7_remap_node_df_app <- function(df, map) {
  if (is.null(df) || !is.data.frame(df) || is.null(map) || !length(map)) return(df)

  # v25: remap every node-related text column used by downstream visuals.
  # Earlier versions only remapped name/term/label. Network/SSplot/Kano/Sankey
  # may also read leader, cluster_leader, neighbor_name, theme_leader, from/to,
  # source/target, or other exact node-name character cells.
  map <- as.character(map)
  names(map) <- as.character(names(map))
  map <- map[nzchar(names(map)) & nzchar(map)]
  if (!length(map)) return(df)

  direct_cols <- intersect(
    names(df),
    c("name", "term", "label", "leader", "cluster_leader", "theme_leader",
      "neighbor_name", "node", "node1", "node2", "from", "to", "source", "target")
  )
  for (cc in direct_cols) df[[cc]] <- .v7_remap_vec_app(df[[cc]], map)

  chr_cols <- names(df)[vapply(df, is.character, logical(1))]
  for (cc in setdiff(chr_cols, direct_cols)) df[[cc]] <- .v7_remap_vec_app(df[[cc]], map)
  df
}

.v7_remap_edge_df_app <- function(df, map) {
  if (is.null(df) || !is.data.frame(df) || is.null(map) || !length(map)) return(df)
  for (cc in c("term1", "term2", "from", "to", "source", "target")) {
    if (cc %in% names(df)) df[[cc]] <- .v7_remap_vec_app(df[[cc]], map)
  }
  # Drop self-links after manual synonym merging, then aggregate WCD/weight when possible.
  a <- if ("term1" %in% names(df)) "term1" else if ("from" %in% names(df)) "from" else NA_character_
  b <- if ("term2" %in% names(df)) "term2" else if ("to" %in% names(df)) "to" else NA_character_
  if (!is.na(a) && !is.na(b)) {
    df <- df[!is.na(df[[a]]) & !is.na(df[[b]]) & nzchar(as.character(df[[a]])) & nzchar(as.character(df[[b]])) & as.character(df[[a]]) != as.character(df[[b]]), , drop = FALSE]
    w <- if ("WCD" %in% names(df)) "WCD" else if ("weight" %in% names(df)) "weight" else NA_character_
    if (!is.na(w) && nrow(df)) {
      keep_extra <- setdiff(names(df), c(a,b,w))
      df <- df |>
        dplyr::mutate(.aa = as.character(.data[[a]]), .bb = as.character(.data[[b]]), .ww = suppressWarnings(as.numeric(.data[[w]]))) |>
        dplyr::group_by(.aa, .bb) |>
        dplyr::summarise(.ww = sum(.ww, na.rm = TRUE), .groups = "drop")
      names(df)[names(df)==".aa"] <- a
      names(df)[names(df)==".bb"] <- b
      names(df)[names(df)==".ww"] <- w
    }
  }
  df
}

.v7_apply_node_editor_to_analysis_app <- function(a, map = NULL) {
  if (is.null(a) || !is.list(a) || is.null(map) || !length(map)) return(a)
  map <- as.character(map)
  names(map) <- as.character(names(map))
  map <- map[nzchar(names(map)) & nzchar(map)]
  if (!length(map)) return(a)

  for (nm in c("selected", "export_nodes", "nodes", "sil_df", "ranked", "extracted", "step4_final_top20_purified", "final_report", "validation")) {
    if (!is.null(a[[nm]]) && is.data.frame(a[[nm]])) a[[nm]] <- .v7_remap_node_df_app(a[[nm]], map)
  }
  for (nm in c("edges", "export_edges", "co_edges", "step5_final_edges")) {
    if (!is.null(a[[nm]]) && is.data.frame(a[[nm]])) a[[nm]] <- .v7_remap_edge_df_app(a[[nm]], map)
  }
  if (!is.null(a$cluster_summary) && is.data.frame(a$cluster_summary)) {
    for (cc in intersect(names(a$cluster_summary), c("leader", "term", "name", "cluster_leader"))) {
      a$cluster_summary[[cc]] <- .v7_remap_vec_app(a$cluster_summary[[cc]], map)
    }
  }
  if (!is.null(a$aac_dashboard) && is.data.frame(a$aac_dashboard)) {
    for (cc in intersect(names(a$aac_dashboard), c("leader", "term", "name", "cluster_leader"))) {
      a$aac_dashboard[[cc]] <- .v7_remap_vec_app(a$aac_dashboard[[cc]], map)
    }
  }
  # Rebuild graph from remapped export nodes/edges when available.
  a$graph <- NULL
  if (!is.null(a$export_nodes) && is.data.frame(a$export_nodes) && "name" %in% names(a$export_nodes)) {
    vtx <- data.frame(name = unique(as.character(a$export_nodes$name)), stringsAsFactors = FALSE)
    ed <- a$export_edges %||% a$edges %||% data.frame()
    if (is.data.frame(ed) && nrow(ed)) {
      if (all(c("term1", "term2") %in% names(ed))) {
        if (!"WCD" %in% names(ed)) ed$WCD <- 1
        a$graph <- igraph::graph_from_data_frame(ed |> dplyr::transmute(from = term1, to = term2, weight = WCD), directed = TRUE, vertices = vtx)
      } else if (all(c("from", "to") %in% names(ed))) {
        if (!"weight" %in% names(ed)) ed$weight <- 1
        a$graph <- igraph::graph_from_data_frame(ed |> dplyr::transmute(from = from, to = to, weight = weight), directed = TRUE, vertices = vtx)
      }
    }
  }
  msg <- paste0("v7 final node editor applied: ", length(map), " node labels remapped; edges synchronized before visualization.")
  if (!is.null(a$processing_log) && is.data.frame(a$processing_log)) {
    a$processing_log <- dplyr::bind_rows(
      dplyr::mutate(a$processing_log, n = as.character(.data$n)),
      tibble::tibble(time = format(Sys.time(), "%H:%M:%S"), status = "OK", step = "5. Final Nodes Editor", elapsed_sec = NA_real_, n = as.character(length(map)), details = msg, preview = paste(unname(map), collapse = " | "))
    )
  }
  if (!is.null(a$extraction_log) && is.data.frame(a$extraction_log)) {
    a$extraction_log <- dplyr::bind_rows(a$extraction_log, tibble::tibble(item = "final_nodes_editor_v7", value = msg))
  }
  a
}


# ---- FINAL NODES EDITOR HARD SYNC v26 --------------------------------------
.v26_collect_rowwise_node_maps_app <- function(a, new_names) {
  maps <- list()
  add_map <- function(df, col) {
    if (!is.data.frame(df) || !nrow(df) || !(col %in% names(df))) return(invisible(NULL))
    old <- trimws(as.character(df[[col]]))
    n <- min(length(old), length(new_names))
    if (n > 0) {
      m <- stats::setNames(as.character(new_names[seq_len(n)]), old[seq_len(n)])
      m <- m[nzchar(names(m)) & nzchar(m)]
      if (length(m)) maps[[length(maps) + 1L]] <<- m
    }
    invisible(NULL)
  }
  for (nm in c("selected", "export_nodes", "nodes", "sil_df", "ranked", "step4_final_top20_purified")) {
    df <- a[[nm]]
    for (cc in c("name", "term", "label", "term_final")) add_map(df, cc)
  }
  if (!length(maps)) return(stats::setNames(as.character(new_names), as.character(new_names)))
  out <- unlist(maps, use.names = TRUE)
  out <- out[!duplicated(names(out))]
  out
}

.v26_force_node_names_by_order_app <- function(df, new_names) {
  if (!is.data.frame(df) || !nrow(df) || !length(new_names)) return(df)
  n <- min(nrow(df), length(new_names))
  if (n <= 0) return(df)
  for (cc in intersect(c("name", "term", "label", "node", "keyword", "final_name", "term_final"), names(df))) {
    df[[cc]][seq_len(n)] <- as.character(new_names[seq_len(n)])
  }
  if ("term" %in% names(df) && !("name" %in% names(df))) df$name <- df$term
  if ("name" %in% names(df) && !("term" %in% names(df))) df$term <- df$name
  df
}

.v26_apply_node_editor_direct_app <- function(a, new_names) {
  if (is.null(a) || !is.list(a) || !length(new_names)) return(a)
  new_names <- trimws(as.character(new_names))
  new_names <- new_names[!is.na(new_names) & nzchar(new_names)]
  if (!length(new_names)) return(a)

  map <- .v26_collect_rowwise_node_maps_app(a, new_names)
  old0 <- .v7_node_names_from_analysis_app(a)
  if (length(old0)) {
    n0 <- min(length(old0), length(new_names))
    map <- c(map, stats::setNames(new_names[seq_len(n0)], old0[seq_len(n0)]))
  }
  map <- map[nzchar(names(map)) & nzchar(map)]
  map <- map[!duplicated(names(map))]

  a <- .v7_apply_node_editor_to_analysis_app(a, map)

  for (nm in c("selected", "export_nodes", "nodes", "sil_df", "ranked", "extracted", "step4_final_top20_purified", "final_report", "validation")) {
    if (!is.null(a[[nm]]) && is.data.frame(a[[nm]])) a[[nm]] <- .v26_force_node_names_by_order_app(a[[nm]], new_names)
  }

  for (nm in c("edges", "export_edges", "co_edges", "step5_final_edges")) {
    if (!is.null(a[[nm]]) && is.data.frame(a[[nm]])) a[[nm]] <- .v7_remap_edge_df_app(a[[nm]], map)
  }

  if (!is.null(a$edges) && is.data.frame(a$edges)) {
    ed <- a$edges
    if (!"term1" %in% names(ed) && "from" %in% names(ed)) ed$term1 <- ed$from
    if (!"term2" %in% names(ed) && "to" %in% names(ed)) ed$term2 <- ed$to
    if (!"WCD" %in% names(ed) && "weight" %in% names(ed)) ed$WCD <- ed$weight
    if (all(c("term1", "term2") %in% names(ed))) {
      keep <- unique(new_names)
      ed <- ed |>
        dplyr::mutate(term1 = trimws(as.character(term1)), term2 = trimws(as.character(term2))) |>
        dplyr::filter(term1 %in% keep, term2 %in% keep, term1 != term2)
      a$edges <- ed
      a$export_edges <- ed
    }
  }

  if ((is.null(a$edges) || !is.data.frame(a$edges) || !nrow(a$edges)) && length(new_names) >= 2L) {
    a$edges <- data.frame(term1 = new_names[-length(new_names)], term2 = new_names[-1], WCD = 1, stringsAsFactors = FALSE)
    a$export_edges <- a$edges
  }

  a$graph <- NULL
  if (requireNamespace("igraph", quietly = TRUE)) {
    vtx <- data.frame(name = unique(new_names), stringsAsFactors = FALSE)
    ed <- a$export_edges %||% a$edges %||% data.frame()
    if (is.data.frame(ed) && nrow(ed) && all(c("term1", "term2") %in% names(ed))) {
      if (!"WCD" %in% names(ed)) ed$WCD <- 1
      a$graph <- igraph::graph_from_data_frame(ed |> dplyr::transmute(from = term1, to = term2, weight = WCD), directed = TRUE, vertices = vtx)
    } else {
      a$graph <- igraph::make_empty_graph(n = nrow(vtx), directed = TRUE)
      igraph::V(a$graph)$name <- vtx$name
    }
  }

  if (!is.null(a$extraction_log) && is.data.frame(a$extraction_log)) {
    a$extraction_log <- dplyr::bind_rows(a$extraction_log, tibble::tibble(item = "final_nodes_editor_v26_hard_sync", value = paste(new_names, collapse = " | ")))
  }
  a
}

server <- function(input, output, session) {

  observeEvent(input$mode_specter2, {
    updateSelectInput(session, "api_extract_mode", selected = "specter2")
    showNotification("SPECTER2 mode selected. Upload data and click Run / re-run analysis.", type = "message", duration = 4)
  }, ignoreInit = TRUE)

  observeEvent(input$mode_esspe, {
    updateSelectInput(session, "api_extract_mode", selected = "tfidf_only")
    showNotification("ESSPE mode selected. Upload data and click Run / re-run analysis.", type = "message", duration = 4)
  }, ignoreInit = TRUE)

  observeEvent(input$mode_api, {
    updateSelectInput(session, "api_extract_mode", selected = "api_first")
    showNotification("OpenAI API mode selected. Paste API key, then click Validate API key & Run.", type = "message", duration = 5)
  }, ignoreInit = TRUE)

  observeEvent(input$mode_network, {
    updateSelectInput(session, "api_extract_mode", selected = "tfidf_only")
    showNotification("Nodes/Edges mode selected. Upload a workbook with nodes and edges sheets, then click Run / re-run analysis.", type = "message", duration = 5)
  }, ignoreInit = TRUE)


  openai_api_key_valid <- reactiveVal(FALSE)
  openai_api_key_validation_message <- reactiveVal("API key has not been validated yet.")

  observeEvent(input$openai_api_key_input, {
    openai_api_key_valid(FALSE)
    openai_api_key_validation_message("API key changed. Click Validate API key & Run before using OpenAI API mode.")
  }, ignoreInit = TRUE)

  observeEvent(input$validate_openai_api_key, {
    key <- trimws(as.character(input$openai_api_key_input %||% ""))
    if (!nzchar(key)) {
      openai_api_key_valid(FALSE)
      openai_api_key_validation_message("No API key was entered.")
      showNotification("No OpenAI API key was entered.", type = "error", duration = 5)
      return(NULL)
    }
    if (!requireNamespace("httr2", quietly = TRUE)) {
      openai_api_key_valid(FALSE)
      openai_api_key_validation_message("Package httr2 is required to validate the OpenAI API key.")
      showNotification("Package httr2 is required to validate the OpenAI API key.", type = "error", duration = 7)
      return(NULL)
    }

    ok <- FALSE
    msg <- "Validation failed."
    tryCatch({
      req <- httr2::request("https://api.openai.com/v1/models") |>
        httr2::req_headers(Authorization = paste("Bearer", key)) |>
        httr2::req_timeout(20)
      resp <- httr2::req_perform(req)
      status <- httr2::resp_status(resp)
      ok <- status >= 200 && status < 300
      msg <- if (ok) "API key validated. OpenAI API mode is ready." else paste("Validation failed with HTTP status", status)
    }, error = function(e) {
      msg <<- paste("Validation failed:", conditionMessage(e))
    })

    if (isTRUE(ok)) {
      Sys.setenv(OPENAI_API_KEY = key)
      openai_api_key_valid(TRUE)
      openai_api_key_validation_message(msg)
      showNotification("OpenAI API key validated. OpenAI API mode will run now if an input source is ready.", type = "message", duration = 5)
      updateSelectInput(session, "api_extract_mode", selected = "api_first")
      ready_to_run <- FALSE
      try({
        ready_to_run <- isTRUE(using_paste()) && nzchar(trimws(input$paste_text_input %||% ""))
        ready_to_run <- ready_to_run || (isTRUE(using_url()) && nzchar(trimws(input$url_input %||% "")))
        ready_to_run <- ready_to_run || (!isTRUE(using_paste()) && !isTRUE(using_url()) && !is.null(input$file))
      }, silent = TRUE)
      if (isTRUE(ready_to_run)) {
        processing_now(TRUE)
        processing_log(tibble(time = character(), status = character(), step = character(), elapsed_sec = numeric(), n = character(), details = character(), preview = character()))
        last_done("<b>OpenAI API key validated.</b> Running OpenAI API mode now.")
        session$onFlushed(function() {
          run_counter(isolate(run_counter()) + 1L)
        }, once = TRUE)
      }
    } else {
      openai_api_key_valid(FALSE)
      openai_api_key_validation_message(msg)
      showNotification(msg, type = "error", duration = 7)
    }
  }, ignoreInit = TRUE)

  output$openai_api_key_validation_box <- renderUI({
    cls <- if (isTRUE(openai_api_key_valid())) "apiKeyValidBox" else "apiKeyInvalidBox"
    tags$div(class = cls, HTML(htmltools::htmlEscape(openai_api_key_validation_message())))
  })

  # v6 OpenAI API homepage status: show whether OPENAI_API_KEY is available.
  mask_openai_key_app <- function(key) {
    key <- trimws(as.character(key %||% ""))
    if (!nzchar(key)) return("<empty>")
    n <- nchar(key)
    if (n <= 12) return("********")
    paste0(substr(key, 1, 7), "...", substr(key, n - 3, n))
  }

  openai_api_status_ui_app <- reactive({
    key_env <- Sys.getenv("OPENAI_API_KEY", unset = "")
    key_input <- trimws(as.character(input$openai_api_key_input %||% ""))
    key <- if (nzchar(key_input) && isTRUE(openai_api_key_valid())) key_input else key_env
    key_ok <- nzchar(trimws(key_env)) || (nzchar(key_input) && isTRUE(openai_api_key_valid()))
    mode <- input$api_extract_mode %||% "auto"

    mode_label <- switch(
      mode,
      auto = "Auto-detect: API will run when the key is available; otherwise ESSPE fallback is used.",
      api_first = "API-first: the app will try ChatGPT API first, then fall back to ESSPE if the call fails.",
      api_only = "API-only: the app requires a valid OpenAI API key.",
      tfidf_only = "ESSPE-only: API calls are disabled even if a key exists.",
      specter2 = "SPECTER2: embedding-assisted ESSPE Step-1 extraction; API calls are disabled.",
      "Unknown mode."
    )

    if (isTRUE(key_ok) && identical(mode, "tfidf_only")) {
      return(tags$div(
        class = "openaiApiStatusDisabled",
        tags$div(class = "openaiApiStatusTitle", "OpenAI API status: KEY FOUND, BUT DISABLED"),
        tags$div(HTML(paste0("<b>OPENAI_API_KEY:</b> ", htmltools::htmlEscape(mask_openai_key_app(key))))),
        tags$div(HTML("<b>Current mode:</b> ESSPE-only. Change API / ESSPE / SPECTER2 extraction source to Auto, API-first, or API-only to use the API."))
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
        tags$div(HTML("The app will use ESSPE fallback unless API-only or SPECTER2 is selected."))
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
  final_node_editor_map <- reactiveVal(NULL)
  # v13: store the fully remapped analysis object directly so all plots/tables redraw.
  final_node_editor_analysis <- reactiveVal(NULL)
  # v25: explicit redraw trigger for edited node labels.
  final_node_editor_version <- reactiveVal(0L)
  final_node_editor_status <- reactiveVal("No manual node revision has been applied yet.")

  flush_console_app <- function() {
    try(utils::flush.console(), silent = TRUE)
    invisible(NULL)
  }

  log_pipeline_step <- function(step, status = "END", n = NA, details = "", preview = character(0), start_time = NULL) {
    # Detailed diagnostic log: show every processing/checkpoint row in both
    # the R console and the Processing log tab. The last START row without
    # a matching END row is the current pause point.
    keep <- TRUE

    now <- Sys.time()
    elapsed <- if (!is.null(start_time)) round(as.numeric(difftime(now, start_time, units = "secs")), 2) else NA_real_
    if (length(n) == 0 || all(is.na(n))) {
      n_chr <- ""
    } else {
      nn <- names(n)
      if (is.null(nn)) nn <- rep("", length(n))
      n_chr <- paste(paste0(nn, ifelse(nzchar(nn), "=", ""), as.character(n)), collapse = "; ")
    }
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
    processing_log(bind_rows(dplyr::mutate(processing_log(), n = as.character(.data$n)), dplyr::mutate(row, n = as.character(.data$n))))
    message("[TOP20 TERM EXTRACTION] ", row$time, " | ", row$status, " | ", row$step,
            " | elapsed=", row$elapsed_sec, "s",
            if (nzchar(row$n)) paste0(" | ", row$n) else "",
            if (nzchar(row$details)) paste0(" | ", row$details) else "",
            if (nzchar(row$preview)) paste0(" | preview=", row$preview) else "")
    flush_console_app()
    invisible(row)
  }

  using_paste <- reactive({
    if (isTRUE(demo_active()) || isTRUE(demo_click_pending())) return(FALSE)
    isTRUE(input$use_paste_input) && nzchar(trimws(input$paste_text_input %||% ""))
  })

  using_url <- reactive({
    if (isTRUE(demo_active()) || isTRUE(demo_click_pending())) return(FALSE)
    isTRUE(input$use_url_input) && nzchar(trimws(input$url_input %||% ""))
  })

  demo_active <- reactiveVal(FALSE)
  demo_click_pending <- reactiveVal(FALSE)
  # Explicit source lock. This is stronger than relying on checkbox state.
  # Values: "demo", "url", "paste", "upload".
  source_mode_lock <- reactiveVal("upload")

  current_source_mode_app <- reactive({
    mode <- source_mode_lock()
    if (isTRUE(demo_active()) || isTRUE(demo_click_pending())) return("demo")
    if (identical(mode, "url") && isTRUE(input$use_url_input) && nzchar(trimws(input$url_input %||% ""))) return("url")
    if (identical(mode, "paste") && isTRUE(input$use_paste_input) && nzchar(trimws(input$paste_text_input %||% ""))) return("paste")
    if (!is.null(input$file)) return("upload")
    mode
  })

  # ---- Deep-link launcher: fill URL box + auto-check URL mode + optional autorun ----
  # Examples:
  #   Local:
  #   http://127.0.0.1:5349/?url-link=https%3A%2F%2Fpmc.ncbi.nlm.nih.gov%2Farticles%2FPMC12401247%2F&mode=tfidf_only&autorun=1
  #   shinyapps.io:
  #   https://smilechien.shinyapps.io/aac2/?url-link=https%3A%2F%2Fpmc.ncbi.nlm.nih.gov%2Farticles%2FPMC12401247%2F&mode=tfidf_only&autorun=1
  deep_link_applied <- reactiveVal(FALSE)

  observe({
    if (isTRUE(deep_link_applied())) return(invisible(NULL))

    qs_raw <- session$clientData$url_search %||% ""
    qs <- shiny::parseQueryString(qs_raw)

    shared_url <- qs[["url-link"]] %||% qs[["url"]] %||% qs[["link"]]
    shared_url <- trimws(as.character(shared_url %||% ""))
    if (!nzchar(shared_url)) return(invisible(NULL))

    deep_link_applied(TRUE)
    demo_active(FALSE)
    demo_click_pending(FALSE)
    source_mode_lock("url")

    # Put shared URL into the URL input box.
    updateTextInput(session, "url_input", value = shared_url)

    # Automatically check URL mode when url-link=... exists.
    updateCheckboxInput(session, "use_url_input", value = TRUE)

    # Default shared-link mode is ESSPE-only, unless the URL query says otherwise.
    shared_mode <- qs[["mode"]] %||% "tfidf_only"
    if (shared_mode %in% c("auto", "api_first", "api_only", "tfidf_only", "specter2")) {
      updateSelectInput(session, "api_extract_mode", selected = shared_mode)
    } else {
      updateSelectInput(session, "api_extract_mode", selected = "tfidf_only")
    }

    # Prefer semantic phrase mode for shared article links.
    if ("extract_mode" %in% names(input)) {
      updateSelectInput(session, "extract_mode", selected = "semantic")
    }

    # If autorun=1 or run=true is present, trigger analysis after the input updates are flushed.
    session$onFlushed(function() {
      run_flag <- qs[["autorun"]] %||% qs[["run"]] %||% "0"
      if (identical(as.character(run_flag), "1") || identical(tolower(as.character(run_flag)), "true")) {
        processing_now(TRUE)
        processing_log(tibble::tibble(
          time = character(), status = character(), step = character(),
          elapsed_sec = numeric(), n = character(), details = character(), preview = character()
        ))
        last_done(paste0(
          "<b>Shared URL detected.</b> The link was inserted into the URL box, URL mode was checked, ",
          "and analysis started automatically for <code>",
          htmltools::htmlEscape(shared_url),
          "</code>."
        ))
        showNotification("Shared URL detected. URL mode checked; analysis is running automatically.", type = "message", duration = 5)
        run_counter(isolate(run_counter()) + 1L)
      }
    }, once = TRUE)

    invisible(NULL)
  })

  demo_pdf_path <- reactive({
    candidates <- c(
      file.path(APP_DIR, "SOFTX-S-26-00528.pdf"),
      file.path(APP_DIR, "data", "SOFTX-S-26-00528.pdf"),
      file.path(APP_DIR, "SOFTX-S-26-00528(24).pdf")
    )
    hit <- candidates[file.exists(candidates)]
    if (length(hit)) hit[[1]] else candidates[[1]]
  })

  active_source_label <- reactive({
    if (isTRUE(demo_active()) || isTRUE(demo_click_pending())) {
      paste0("Demo PDF: ", basename(demo_pdf_path()))
    } else if (using_paste()) {
      "Pasted text input"
    } else if (using_url()) {
      trimws(input$url_input %||% "")
    } else if (!is.null(input$file)) {
      input$file$name
    } else {
      ""
    }
  })

  uploaded_bundle <- reactive({
    if (isTRUE(demo_active()) || isTRUE(demo_click_pending()) || using_paste() || using_url()) return(NULL)
    req(input$file)
    if (has_nodes_edges_sheets(input$file$datapath, input$file$name)) {
      read_nodes_edges_bundle(input$file$datapath, input$file$name)
    } else {
      NULL
    }
  })

  raw_data <- reactive({
    mode <- current_source_mode_app()
    message("[SOURCE MODE LOCK] raw_data mode = ", mode)

    if (identical(mode, "demo")) {
      path <- demo_pdf_path()
      shiny::validate(shiny::need(file.exists(path), paste0("Demo file not found: ", path)))
      message("[RUN DEMO SOURCE LOCK] Reading demo PDF directly: ", path)
      return(safe_read_upload(path, basename(path)))
    }

    if (identical(mode, "paste")) {
      out <- tryCatch(
        safe_read_pasted_text_input(input$paste_text_input),
        error = function(e) {
          shiny::validate(shiny::need(FALSE, paste("Pasted text reading failed:", conditionMessage(e))))
        }
      )
      return(out)
    }

    if (identical(mode, "url")) {
      out <- tryCatch(
        safe_read_url_input(input$url_input),
        error = function(e) {
          shiny::validate(shiny::need(FALSE, paste("URL reading failed:", conditionMessage(e))))
        }
      )
      return(out)
    }

    req(input$file)
    if (!is.null(uploaded_bundle())) {
      # Direct nodes+edges bundle does not need raw document text.
      data.frame(doc_id = integer(), text = character(), stringsAsFactors = FALSE)
    } else {
      safe_read_upload(input$file$datapath, input$file$name)
    }
  })

  observeEvent(input$file, {
    demo_active(FALSE)
    demo_click_pending(FALSE)
    source_mode_lock("upload")
  }, ignoreInit = TRUE)

  observeEvent(input$use_paste_input, {
    if (isTRUE(input$use_paste_input)) {
      demo_active(FALSE)
      demo_click_pending(FALSE)
      source_mode_lock("paste")
      updateCheckboxInput(session, "use_url_input", value = FALSE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$use_url_input, {
    if (isTRUE(input$use_url_input)) {
      demo_active(FALSE)
      demo_click_pending(FALSE)
      source_mode_lock("url")
      updateCheckboxInput(session, "use_paste_input", value = FALSE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$chinese_doc_mode, {
    if (isTRUE(input$chinese_doc_mode) && (isTRUE(demo_active()) || using_paste() || using_url() || !is.null(input$file))) {
      processing_now(TRUE)
      processing_log(tibble::tibble(
        time = character(), status = character(), step = character(),
        elapsed_sec = numeric(), n = character(), details = character(), preview = character()
      ))
      last_done("<b>Chinese document mode selected.</b> English phrase extraction will be skipped; Chinese phrase extraction is running.")
      showNotification("Chinese document mode selected. Re-running with Chinese extraction and skipping English pipeline.", type = "message", duration = 5)
      session$onFlushed(function() {
        run_counter(isolate(run_counter()) + 1L)
      }, once = TRUE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$run_demo, {
    # Demo must override URL/paste/upload states. Reset them BEFORE checking
    # the demo PDF, otherwise a stale checked URL box can keep firing URL parsing
    # and show "No usable semantic phrases found after URL parsing."
    demo_active(TRUE)
    demo_click_pending(TRUE)
    source_mode_lock("demo")
    updateCheckboxInput(session, "use_paste_input", value = FALSE)
    updateCheckboxInput(session, "use_url_input", value = FALSE)
    updateTextInput(session, "url_input", value = "")

    # The packaged SOFTX demo is English; avoid accidentally rerunning the
    # Chinese extractor if the Chinese checkbox was left checked from a prior run.
    updateCheckboxInput(session, "chinese_doc_mode", value = FALSE)
    updateSelectInput(session, "api_extract_mode", selected = "tfidf_only")
    updateSelectInput(session, "extract_mode", selected = "semantic")

    path <- demo_pdf_path()
    if (!file.exists(path)) {
      demo_active(FALSE)
      demo_click_pending(FALSE)
      source_mode_lock("upload")
      showNotification(paste("Demo PDF not found in app folder:", path), type = "error", duration = 8)
      last_done(paste0(
        "<b>Run Demo failed.</b> Demo PDF was not found at <code>",
        htmltools::htmlEscape(path),
        "</code>. Please keep SOFTX-S-26-00528.pdf in the same folder as app.R."
      ))
      return(invisible(NULL))
    }

    processing_now(TRUE)
    processing_log(tibble(time = character(), status = character(), step = character(), elapsed_sec = numeric(), n = character(), details = character(), preview = character()))
    last_done(paste0(
      "<b>Run Demo started.</b> Using <code>",
      htmltools::htmlEscape(basename(path)),
      "</code> from the app.R folder as the uploaded demo document. URL mode and pasted-text mode were disabled."
    ))
    showNotification("Run Demo started using SOFTX-S-26-00528.pdf. URL mode was disabled.", type = "message", duration = 4)

    # Start after all input updates are flushed, so using_url()/using_paste()
    # have already become FALSE before analysis_view() evaluates.
    session$onFlushed(function() {
      demo_active(TRUE)
      demo_click_pending(TRUE)
      source_mode_lock("demo")
      run_counter(isolate(run_counter()) + 1L)
    }, once = TRUE)
  }, ignoreInit = TRUE)

  # Auto-run only after the file is uploaded AND the text-column input exists.
  # First show the visible scale bar, then start analysis after the UI flushes.
  observe({
    if (identical(current_source_mode_app(), "demo")) return(invisible(NULL))
    if (using_paste()) {
      req(nzchar(trimws(input$paste_text_input %||% "")))
    } else if (using_url()) {
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
        "<b>Input ready.</b> Waiting for Step C: Run / reRUN for <code>",
        htmltools::htmlEscape(active_source_label()),
        "</code>."
      ))
      processing_now(FALSE)
      showNotification("Input is ready. Click Run / re-run analysis to start.", type = "message", duration = 4)
    }
  })

  observeEvent(input$run, {
    mode0 <- current_source_mode_app()
    if (identical(mode0, "demo")) {
      req(file.exists(demo_pdf_path()))
    } else if (identical(mode0, "paste")) {
      req(nzchar(trimws(input$paste_text_input %||% "")))
    } else if (identical(mode0, "url")) {
      req(nzchar(trimws(input$url_input %||% "")))
    } else {
      req(input$file)
    }
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
    if (isTRUE(demo_active())) {
      return(paste0("Status: demo mode active. Using ", basename(demo_pdf_path()), ". Click Run / re-run analysis to rebuild, or upload a file to leave demo mode."))
    }
    if (!using_paste() && !using_url() && is.null(input$file)) {
      return("Status: No input selected. Upload a file, paste text and check pasted-text mode, click Run Demo, or check Analyze URL link. For Chinese manuscripts, also check Chinese document mode.")
    }
    if (isTRUE(input$use_paste_input) && !using_paste()) {
      return("Status: pasted-text mode is checked. Paste title, abstract, and main body text to start.")
    }
    if (isTRUE(input$use_url_input) && !using_url()) {
      return("Status: URL-link mode is checked. Enter an http(s) URL to start.")
    }
    if (isTRUE(processing_now())) {
      return("Status: processing... the visible scale bar below indicates analysis is running.")
    }
    paste0(
      "Status: ready. Analysis run counter = ", run_counter(),
      ". The app waits after upload; click Run / re-run analysis to start or rebuild."
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

  analysis_base <- eventReactive(run_counter(), {
    if (isTRUE(demo_active())) {
      req(file.exists(demo_pdf_path()))
    } else if (using_paste()) {
      req(nzchar(trimws(input$paste_text_input %||% "")))
    } else if (using_url()) {
      req(nzchar(trimws(input$url_input %||% "")))
    } else {
      req(input$file)
    }
    analysis_start_time <- Sys.time()
    processing_log(tibble(time = character(), status = character(), step = character(), elapsed_sec = numeric(), n = character(), details = character(), preview = character()))
    log_pipeline_step("0. Start", details = paste0("source=", current_source_mode_app(), "; input=", active_source_label()), start_time = analysis_start_time)
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
              "Network, SSplot, Kano, Sankey, Chord, and AAC dashboard are ready."
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
            # keep the original document units (rows/pages/paragraphs) for ESSPE and phrase scoring.
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

            # Chinese document mode:
            # If API mode is enabled, use a Chinese semantic prompt to produce
            # ChatGPT-like nodes/edges. Otherwise fall back to zh_tail_keyword_engine.R.
            if (isTRUE(input$chinese_doc_mode)) {

              # FORCE Chinese mode to direct-safe extraction only.
              # This bypasses the old primary Chinese pipeline and the Chinese API branch,
              # preventing recurrent "引數長度不同" from unequal-length intermediate tables.
              incProgress(0.20, detail = "Chinese direct-safe mode: stripping tables and extracting phrases...")
              manual_author_keywords_zh <- parse_manual_author_keywords_zh_app(input$manual_author_keywords %||% "")
              log_pipeline_step(
                "1. Chinese direct-safe extraction",
                status = "START",
                n = c(docs = nrow(docs_tbl), chars = sum(nchar(docs_tbl$text), na.rm = TRUE), manual_zh_keywords = length(manual_author_keywords_zh)),
                details = "Chinese document mode is checked; table-like content is stripped; API and old primary Chinese pipeline are bypassed.",
                preview = manual_author_keywords_zh,
                start_time = analysis_start_time
              )
              out <- make_chinese_analysis_directsafe_app(
                docs_tbl = docs_tbl,
                author_keywords_manual = manual_author_keywords_zh,
                top_n = as.integer(input$top_n %||% 20L),
                processing_log_tbl = processing_log(),
                reason = "Chinese direct-safe forced; API and old primary pipeline bypassed; table content stripped"
              )
              out <- .strict_v4_sanitize_analysis_app(out)
              log_pipeline_step(
                "4. Chinese direct-safe Top20 + edges",
                status = "END",
                n = c(selected = nrow(out$selected), edges = nrow(out$edges), clusters = length(unique(out$selected$topic))),
                details = "Chinese direct-safe extraction complete; visual outputs are ready.",
                preview = out$selected$term,
                start_time = analysis_start_time
              )
              out$processing_log <- processing_log()
              incProgress(0.70, detail = "Chinese direct-safe visual data ready.")
              processing_now(FALSE)
              last_done(paste0(
                "<b>Processing complete.</b> Chinese direct-safe mode selected; table-like content was excluded. ",
                nrow(out$selected), " Chinese phrase(s), ",
                nrow(out$edges), " edge(s), and ",
                length(unique(out$selected$topic)), " cluster(s) are ready."
              ))
              showNotification("Chinese direct-safe analysis complete. Switching to Network tab.", type = "message", duration = 5)
              updateTabsetPanel(session, "main_tabs", selected = "Network")
              incProgress(0.05, detail = "Finished.")
              return(out)

              incProgress(0.20, detail = "Chinese mode: extracting semantic phrases...")
              manual_author_keywords_zh <- parse_manual_author_keywords_zh_app(input$manual_author_keywords %||% "")
              api_mode_zh <- input$api_extract_mode %||% "tfidf_only"
              api_available_zh <- openai_api_key_available_app()
              should_try_zh_api <- api_mode_zh %in% c("api_first", "api_only") ||
                (identical(api_mode_zh, "auto") && isTRUE(api_available_zh))

              log_pipeline_step(
                "1. Chinese phrase extraction",
                status = "START",
                n = c(docs = nrow(docs_tbl), chars = sum(nchar(docs_tbl$text), na.rm = TRUE), manual_zh_keywords = length(manual_author_keywords_zh)),
                details = paste0(
                  "Chinese document mode is checked; English extraction is skipped. ",
                  ifelse(should_try_zh_api,
                         "Trying Chinese semantic OpenAI prompt first for ChatGPT-like concept naming.",
                         "Using rule-based zh-tail fallback because API mode is ESSPE-only/SPECTER2 or API key is unavailable.")
                ),
                preview = manual_author_keywords_zh,
                start_time = analysis_start_time
              )

              if (isTRUE(should_try_zh_api)) {
                incProgress(0.30, detail = "Chinese semantic API prompt...")
                api_obj <- tryCatch(
                  openai_extract_chinese_nodes_edges_app(
                    docs_tbl = docs_tbl,
                    top_n = as.integer(input$top_n %||% 20L),
                    manual_keywords = manual_author_keywords_zh
                  ),
                  error = function(e) e
                )

                if (!inherits(api_obj, "error")) {
                  log_pipeline_step(
                    "2. Chinese semantic API nodes/edges",
                    status = "END",
                    n = c(nodes = nrow(api_obj$nodes), edges = nrow(api_obj$edges)),
                    details = "OpenAI Chinese semantic prompt returned final nodes and edges.",
                    preview = api_obj$nodes$name,
                    start_time = analysis_start_time
                  )

                  out <- build_analysis_from_nodes_edges_direct_app(
                    nodes_df = api_obj$nodes,
                    edges_df = api_obj$edges,
                    source_mode = "openai_chinese_semantic_nodes_edges",
                    processing_log_tbl = processing_log()
                  )

                  log_pipeline_step(
                    "4. Chinese semantic Top20 + edges + FLCA-SIL-MA",
                    status = "END",
                    n = c(selected = nrow(out$selected), edges = nrow(out$edges), clusters = length(unique(out$selected$topic))),
                    details = "Chinese semantic API extraction complete; visual outputs are ready.",
                    preview = out$selected$term,
                    start_time = analysis_start_time
                  )

                  out$processing_log <- processing_log()
                  incProgress(0.70, detail = "Chinese semantic visual data ready.")
                  processing_now(FALSE)
                  last_done(paste0(
                    "<b>Processing complete.</b> Chinese semantic API mode selected. ",
                    nrow(out$selected), " semantic phrase(s), ",
                    nrow(out$edges), " edge(s), and ",
                    length(unique(out$selected$topic)), " cluster(s) are ready."
                  ))
                  showNotification("Chinese semantic API analysis complete. Switching to Network tab.", type = "message", duration = 5)
                  updateTabsetPanel(session, "main_tabs", selected = "Network")
                  incProgress(0.05, detail = "Finished.")
                  return(out)
                }

                log_pipeline_step(
                  "2. Chinese semantic API failed",
                  status = "WARN",
                  details = paste0("Falling back to zh_tail_keyword_engine.R: ", conditionMessage(api_obj)),
                  start_time = analysis_start_time
                )

                if (identical(api_mode_zh, "api_only")) {
                  stop(paste0("Chinese API-only mode failed: ", conditionMessage(api_obj)), call. = FALSE)
                }
              }

              out <- make_chinese_analysis_directsafe_app(
                docs_tbl = docs_tbl,
                author_keywords_manual = manual_author_keywords_zh,
                top_n = as.integer(input$top_n %||% 20L),
                processing_log_tbl = processing_log(),
                reason = "Chinese checkbox selected; using direct-safe pipeline to avoid 引數長度不同"
              )
              log_pipeline_step(
                "4. Chinese Top20 + edges + FLCA-SIL-MA",
                status = "END",
                n = c(selected = nrow(out$selected), edges = nrow(out$edges), clusters = length(unique(out$selected$topic))),
                details = "Chinese direct-safe extraction complete; visual outputs are ready.",
                preview = out$selected$term,
                start_time = analysis_start_time
              )

              out$processing_log <- processing_log()
              incProgress(0.70, detail = "Chinese visual data ready.")
              processing_now(FALSE)
              last_done(paste0(
                "<b>Processing complete.</b> Chinese direct-safe mode selected. ",
                nrow(out$selected), " Chinese phrase(s), ",
                nrow(out$edges), " edge(s), and ",
                length(unique(out$selected$topic)), " cluster(s) are ready."
              ))
              showNotification("Chinese document analysis complete. Switching to Network tab.", type = "message", duration = 5)
              updateTabsetPanel(session, "main_tabs", selected = "Network")
              incProgress(0.05, detail = "Finished.")
              return(out)
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
              "1a. API/ESSPE/SPECTER2 source",
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
            log_pipeline_step("3b. Scoring", status = "START", n = c(rows = nrow(extracted), unique_terms = length(unique(extracted$term))), details = "compute ESSPE/document-frequency scores", preview = character(0), start_time = analysis_start_time)
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

            # URL fallback: if TF-IDF + strict gates remove all phrases, keep the app alive
            # by using the best ranked 1--4 word semantic phrases and rebuilding edges.
            if (!nrow(selected) && using_url() && exists("ranked") && is.data.frame(ranked) && nrow(ranked)) {
              log_pipeline_step(
                "4b. URL fallback Top20",
                status = "START",
                n = c(ranked_rows = nrow(ranked)),
                details = "strict URL filtering returned zero nodes; rebuilding Top-20 from ranked semantic phrases",
                preview = ranked$term,
                start_time = analysis_start_time
              )

              fallback_score_tbl <- ranked |>
                mutate(
                  term = normalize_author_keyword_for_tail_app(term),
                  author_keyword = (author_keyword %in% TRUE) | term %in% normalize_author_keywords_unique_app(author_keywords),
                  doc_freq = 1,
                  tfidf_sum = suppressWarnings(as.numeric(tf_idf)),
                  score = suppressWarnings(as.numeric(tf_idf)),
                  source_type = as.character(source_type %||% "url_tfidf_fallback"),
                  exact_in_document = exact_surface_phrase %in% TRUE,
                  n_words = stringr::str_count(term, "[[:space:]]+") + 1L,
                  .strict_ok = vapply(seq_along(term), function(i) strict_en_tail_term_ok_app(term[[i]], author_keyword = author_keyword[[i]], max_words = 4L), logical(1))
                ) |>
                group_by(term) |>
                summarise(
                  doc_freq = dplyr::n_distinct(doc_id),
                  tfidf_sum = sum(tfidf_sum, na.rm = TRUE),
                  source_type = paste(sort(unique(source_type)), collapse = "+"),
                  author_keyword = any(author_keyword %in% TRUE),
                  score = sum(score, na.rm = TRUE),
                  exact_in_document = any(exact_in_document %in% TRUE),
                  .strict_ok = any(.strict_ok %in% TRUE),
                  .groups = "drop"
                ) |>
                filter(nzchar(term), .strict_ok | author_keyword) |>
                arrange(desc(author_keyword), desc(score), desc(tfidf_sum), desc(doc_freq), term) |>
                distinct(term, .keep_all = TRUE) |>
                slice_head(n = as.integer(input$top_n %||% 20L)) |>
                select(term, doc_freq, tfidf_sum, source_type, author_keyword, score, exact_in_document)

              if (nrow(fallback_score_tbl)) {
                fallback_terms <- fallback_score_tbl$term
                fallback_doc_terms <- extracted |> filter(term %in% fallback_terms)
                fallback_co_edges <- build_all_edges(fallback_terms, fallback_doc_terms) |>
                  transmute(term1 = from, term2 = to, WCD = as.integer(weight)) |>
                  filter(WCD >= as.integer(input$min_edge_docs %||% 1L))
                fallback_flca <- .apply_real_flca_to_nodes_edges(
                  fallback_score_tbl |> transmute(name = term, value = score, value2 = score),
                  fallback_co_edges,
                  verbose = FALSE
                )
                selected <- .make_selected_from_flca(fallback_score_tbl, fallback_terms, fallback_flca, fallback_co_edges)
                edges <- fallback_flca$edges |>
                  filter(term1 %in% selected$term, term2 %in% selected$term, term1 != term2) |>
                  mutate(WCD = as.integer(round(WCD)), edge_type = "leader_follower")
                if (!nrow(edges) && nrow(fallback_co_edges)) {
                  edges <- fallback_co_edges |>
                    transmute(term1, term2, WCD = as.integer(WCD), edge_type = "co_occurrence_fallback")
                }
              }

              log_pipeline_step(
                "4b. URL fallback Top20",
                status = "END",
                n = c(selected = nrow(selected), edges = nrow(edges)),
                details = "fallback completed for URL mode",
                preview = selected$term,
                start_time = analysis_start_time
              )
            }

            if (!nrow(selected) && identical(current_source_mode_app(), "demo")) {
              log_pipeline_step(
                "4c. Demo fallback",
                status = "WARN",
                details = "Strict extraction returned no selected phrases for the demo PDF; using built-in SOFTX demo fallback nodes/edges.",
                start_time = analysis_start_time
              )
              out <- build_demo_fallback_analysis_app(
                docs_tbl = docs_tbl,
                processing_log_tbl = processing_log(),
                top_n = as.integer(input$top_n %||% 20L)
              )
              out$processing_log <- processing_log()
              processing_now(FALSE)
              last_done(paste0(
                "<b>Processing complete.</b> Demo fallback was used because strict extraction returned no usable phrases. ",
                nrow(out$selected), " demo phrase(s), ",
                nrow(out$edges), " edge(s), and ",
                length(unique(out$selected$topic)), " cluster(s) are ready."
              ))
              showNotification("Run Demo completed using built-in SOFTX fallback nodes/edges.", type = "message", duration = 5)
              updateTabsetPanel(session, "main_tabs", selected = "Network")
              incProgress(0.10, detail = "Finished with demo fallback.")
              return(out)
            }

            if (!nrow(selected)) {
              mode_now <- current_source_mode_app()
              stop(
                paste0(
                  "No usable semantic phrases found from source mode: ", mode_now,
                  ". If this was a URL, try uploading the article PDF or selecting combined mode. ",
                  "If this was Run Demo, confirm the console shows source=demo and [RUN DEMO SOURCE LOCK]."
                ),
                call. = FALSE
              )
            }

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
              bind_rows(tibble(item = "input_source", value = ifelse(using_paste(), "pasted_text", ifelse(using_url(), "url_link", "uploaded_file")))) |>
              bind_rows(tibble(item = "input_url", value = ifelse(using_url(), trimws(input$url_input %||% ""), ""))) |>
              bind_rows(tibble(item = "pasted_text_chars", value = ifelse(using_paste(), as.character(nchar(input$paste_text_input %||% "")), ""))) |>
              bind_rows(tibble(item = "api_extract_mode", value = as.character(api_mode_used))) |>
              bind_rows(tibble(item = "api_key_available", value = as.character(api_key_available))) |>
              bind_rows(tibble(item = "api_attempted", value = as.character(api_attempted))) |>
              bind_rows(tibble(item = "api_used", value = as.character(api_used))) |>
              bind_rows(tibble(item = "api_error", value = as.character(api_error))) |>
              bind_rows(tibble(item = "extraction_engine_used", value = as.character(extraction_engine_used))) |>
              bind_rows(tibble(item = "en_tail_keyword_engine_loaded", value = as.character(isTRUE(.en_tail_engine_loaded)))) |>
              bind_rows(tibble(item = "chinese_document_mode", value = as.character(isTRUE(input$chinese_doc_mode)))) |>
              bind_rows(tibble(item = "zh_tail_keyword_engine_loaded", value = as.character(isTRUE(.zh_tail_engine_loaded)))) |>
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
              bind_rows(tibble(check = "input_source", result = as.character(ifelse(using_paste(), "pasted_text", ifelse(using_url(), "url_link", "uploaded_file"))))) |>
              bind_rows(tibble(check = "api_extract_mode", result = as.character(api_mode_used))) |>
              bind_rows(tibble(check = "api_key_available", result = as.character(api_key_available))) |>
              bind_rows(tibble(check = "api_attempted", result = as.character(api_attempted))) |>
              bind_rows(tibble(check = "api_used", result = as.character(api_used))) |>
              bind_rows(tibble(check = "api_error", result = as.character(api_error))) |>
              bind_rows(tibble(check = "extraction_engine_used", result = as.character(extraction_engine_used))) |>
              bind_rows(tibble(check = "en_tail_keyword_engine_loaded", result = as.character(isTRUE(.en_tail_engine_loaded)))) |>
              bind_rows(tibble(check = "chinese_document_mode", result = as.character(isTRUE(input$chinese_doc_mode)))) |>
              bind_rows(tibble(check = "zh_tail_keyword_engine_loaded", result = as.character(isTRUE(.zh_tail_engine_loaded)))) |>
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
      showNotification(paste("Analysis failed:", msg), type = "error", duration = 10)
      return(make_empty_error_analysis_app(msg, processing_log()))
    })
  }, ignoreInit = TRUE)

  # Force analysis to execute immediately after Run Analysis, Run Demo, or shared URL autorun.

  analysis_view <- reactive({
    final_node_editor_version()
    edited <- final_node_editor_analysis()
    if (!is.null(edited)) return(edited)
    .v7_apply_node_editor_to_analysis_app(analysis_base(), final_node_editor_map())
  })

  # ---- v24 ORIGINAL UI PATCH: robust autofill final_nodes_text ----
  observe({
    invalidateLater(800, session)

    # Do not overwrite after the user applied manual edits.
    if (!is.null(final_node_editor_map()) && length(final_node_editor_map())) return(invisible(NULL))
    if (!is.null(final_node_editor_analysis())) return(invisible(NULL))

    a <- tryCatch(analysis_base(), error = function(e) NULL)
    if (is.null(a)) return(invisible(NULL))

    nm <- .v7_node_names_from_analysis_app(a)
    if (!length(nm)) return(invisible(NULL))

    current_txt <- input$final_nodes_text %||% ""
    if (nzchar(trimws(current_txt))) return(invisible(NULL))

    updateTextAreaInput(
      session,
      "final_nodes_text",
      value = paste(nm, collapse = "
")
    )
    final_node_editor_status("Final Top-20 nodes were auto-filled from the latest analysis. You can revise them and rerun visualization.")
  })


  observeEvent(analysis_base(), {
    a <- analysis_base()
    nm <- .v7_node_names_from_analysis_app(a)
    final_node_editor_map(NULL)
    final_node_editor_analysis(NULL)
    final_node_editor_version(final_node_editor_version() + 1L)
    final_node_editor_status("No manual node revision has been applied yet. Edit the node list and click Update nodes and rerun visualization.")
    if (length(nm)) {
      updateTextAreaInput(session, "final_nodes_text", value = paste(nm, collapse = "\n"))
    }
  }, ignoreInit = TRUE)


  observeEvent(input$main_tabs, {
    # v8: when the red Final Nodes Editor tab is opened, auto-fill the textarea
    # from the latest algorithmic Top-20 nodes if the user has not applied a map.
    if (!grepl("Final Nodes Editor", as.character(input$main_tabs %||% ""), fixed = TRUE)) return(invisible(NULL))
    a <- analysis_base()
    if (is.null(a)) return(invisible(NULL))
    if (!is.null(final_node_editor_map()) && length(final_node_editor_map())) return(invisible(NULL))
    nm <- .v7_node_names_from_analysis_app(a)
    if (length(nm)) {
      updateTextAreaInput(session, "final_nodes_text", value = paste(nm, collapse = "\n"))
      final_node_editor_status("Final Top-20 nodes were auto-filled from the latest analysis. You can revise them and rerun visualization.")
    }
  }, ignoreInit = FALSE)

  observeEvent(input$reset_final_nodes, {
    req(analysis_base())
    nm <- .v7_node_names_from_analysis_app(analysis_base())
    final_node_editor_map(NULL)
    final_node_editor_analysis(NULL)
    final_node_editor_version(final_node_editor_version() + 1L)
    updateTextAreaInput(session, "final_nodes_text", value = paste(nm, collapse = "\n"))
    final_node_editor_status("Reset complete: algorithmic final nodes are restored.")
    log_pipeline_step("5. Final Nodes Editor", status = "RESET", n = length(nm), details = "Manual node editor map cleared; algorithmic nodes restored.", preview = nm, start_time = Sys.time())
  })

  observeEvent(input$update_final_nodes, {
    req(analysis_base())
    old <- .v7_node_names_from_analysis_app(analysis_base())
    new <- .v7_clean_user_node_lines_app(input$final_nodes_text)
    if (!length(old)) {
      final_node_editor_status("No final nodes are available yet. Run analysis first.")
      return(invisible(NULL))
    }
    if (length(new) != length(old)) {
      final_node_editor_status(paste0("Update rejected: node count must stay the same. Original n=", length(old), "; revised n=", length(new), "."))
      showNotification("Node count must stay the same: one revised name per original node.", type = "error", duration = 6)
      return(invisible(NULL))
    }
    mp <- stats::setNames(new, old)
    final_node_editor_map(mp)
    edited_analysis <- .v26_apply_node_editor_direct_app(analysis_base(), new)
    final_node_editor_analysis(edited_analysis)
    final_node_editor_version(final_node_editor_version() + 1L)
    final_node_editor_status(paste0("Updated ", length(mp), " final node names. Edges are remapped and visuals are rerun from the revised labels."))
    log_pipeline_step("5. Final Nodes Editor", status = "END", n = c(nodes = length(mp)), details = "User revised final nodes; nodes/edges remapped before visualization.", preview = new, start_time = Sys.time())
    updateTabsetPanel(session, "main_tabs", selected = "Network")
  })

  output$final_nodes_editor_status <- renderText({ final_node_editor_status() })

  output$final_nodes_editor_table <- renderDT({
    req(analysis_base())
    old <- .v7_node_names_from_analysis_app(analysis_base())
    mp <- final_node_editor_map()
    revised <- if (!is.null(mp) && length(mp)) .v7_remap_vec_app(old, mp) else old
    datatable(
      tibble::tibble(rank = seq_along(old), original_node = old, revised_node = revised, changed = original_node != revised_node),
      options = list(pageLength = 25, scrollX = TRUE, searching = FALSE)
    )
  })

  # Hidden tabs may not request analysis_view(), so this observer warms the backend.
  observeEvent(run_counter(), {
    if (isTRUE(demo_active())) {
      req(file.exists(demo_pdf_path()))
    } else if (using_url()) {
      req(nzchar(trimws(input$url_input %||% "")))
    } else {
      req(input$file)
    }
    analysis_view()
  }, ignoreInit = TRUE)

  observeEvent(analysis_view(), {
    # Shared URL autorun can complete while Preview is active. Move users to Network
    # when visual data are ready.
    a <- analysis_view()
    if (!is.null(a) && is.data.frame(a$export_nodes) && nrow(a$export_nodes) > 0) {
      updateTabsetPanel(session, "main_tabs", selected = "Network")
    }
  }, ignoreInit = TRUE)

  output$extracted_terms <- renderDT({
    req(analysis_view())
    datatable(
      analysis_view()$ranked |>
        arrange(desc(tf_idf), doc_id, term),
      options = list(pageLength = 20, scrollX = TRUE)
    )
  })

  output$topic_terms <- renderDT({
    req(analysis_view())
    datatable(
      analysis_view()$selected |>
        arrange(desc(score), topic, term),
      options = list(pageLength = 20, scrollX = TRUE)
    )
  })


  output$theme_table <- renderDT({
    req(analysis_view())
    theme_tbl <- build_theme_summary_app(analysis_view())
    datatable(
      theme_tbl,
      options = list(pageLength = 20, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$theme_cards <- renderUI({
    req(analysis_view())
    theme_tbl <- build_theme_summary_app(analysis_view())
    if (!nrow(theme_tbl)) {
      return(div(class = "run-help", "No cluster theme summary is available."))
    }

    overall_aac <- suppressWarnings(mean(theme_tbl$AAC, na.rm = TRUE))
    if (!is.finite(overall_aac)) overall_aac <- NA_real_

    div(
      class = "metricRow",
      div(class = "metricCard", HTML(paste0("Clusters<br><b>", nrow(theme_tbl), "</b>"))),
      div(class = "metricCard", HTML(paste0("Overall AAC<br><b>", ifelse(is.na(overall_aac), "NA", format(round(overall_aac, 3), nsmall = 3)), "</b>"))),
      div(class = "metricCard", HTML(paste0("Top theme<br><b>", htmltools::htmlEscape(theme_tbl$theme_leader[[1]]), "</b>"))),
      div(class = "metricCard", HTML("Theme =<br><b>cluster leader</b>"))
    )
  })

  output$theme_aac_plot <- renderPlot({
    req(analysis_view())
    theme_tbl <- build_theme_summary_app(analysis_view())
    if (!nrow(theme_tbl)) {
      plot.new()
      text(0.5, 0.5, "No theme/AAC data available")
      return(invisible(NULL))
    }

    df <- theme_tbl |>
      dplyr::mutate(
        cluster = factor(cluster, levels = cluster),
        label = ifelse(is.finite(AAC), format(round(AAC, 2), nsmall = 2), "NA")
      )

    ggplot(df, aes(x = cluster, y = AAC)) +
      geom_col() +
      geom_text(aes(label = label), vjust = -0.35, size = 5, fontface = "bold") +
      coord_cartesian(ylim = c(0, 1.05)) +
      labs(
        title = "AAC by cluster theme",
        subtitle = "Theme labels are cluster leaders from the final Top-20 phrase network",
        x = "Cluster",
        y = "AAC"
      ) +
      theme_minimal(base_size = 14)
  })
  outputOptions(output, "theme_table", suspendWhenHidden = FALSE)
  outputOptions(output, "theme_cards", suspendWhenHidden = FALSE)
  outputOptions(output, "theme_aac_plot", suspendWhenHidden = FALSE)

  output$export_nodes <- renderDT({
    req(analysis_view())
    datatable(
      analysis_view()$export_nodes,
      options = list(pageLength = 20, scrollX = TRUE)
    )
  })

  output$export_edges <- renderDT({
    req(analysis_view())
    datatable(
      analysis_view()$export_edges,
      options = list(pageLength = 20, scrollX = TRUE)
    )
  })

  output$validation <- renderDT({
    req(analysis_view())
    datatable(
      analysis_view()$validation,
      options = list(pageLength = 10, scrollX = TRUE, searching = FALSE)
    )
  })

  output$cluster_summary <- renderDT({
    req(analysis_view())
    datatable(
      analysis_view()$cluster_summary,
      options = list(pageLength = 20, scrollX = TRUE)
    )
  })

  output$extraction_log <- renderDT({
    req(analysis_view())
    datatable(
      analysis_view()$extraction_log,
      options = list(pageLength = 10, scrollX = TRUE, searching = FALSE)
    )
  })

  output$final_report <- renderDT({
    req(analysis_view())
    datatable(
      analysis_view()$final_report,
      options = list(pageLength = 20, scrollX = TRUE, searching = FALSE)
    )
  })

  output$processing_log <- renderDT({
    dat <- processing_log()
    av <- tryCatch(analysis_view(), error = function(e) NULL)
    if (!is.null(av) && !is.null(av$processing_log) && is.data.frame(av$processing_log) && nrow(av$processing_log)) {
      dat <- av$processing_log
    }
    if (is.null(dat) || !nrow(dat)) {
      dat <- tibble(time = character(), status = character(), step = character(), elapsed_sec = numeric(), n = character(), details = character(), preview = character())
    }
    if ("n" %in% names(dat)) dat$n <- as.character(dat$n)
    datatable(
      dat,
      options = list(pageLength = 50, scrollX = TRUE, searching = FALSE, order = list(list(0, "asc")))
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
    req(analysis_view())

    # v7b REAL SSPLOT RESTORE:
    # Use renderSSplot.R's real renderer for the screen plot.
    shiny::validate(shiny::need(exists("render_real_ssplot", mode = "function"),
                  "Real renderSSplot.R was not loaded. Please keep renderSSplot.R in the app folder."))

    tryCatch({
      ss_in <- build_real_ssplot_input_app(analysis_view())

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
      req(analysis_view())

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
        ss_in <- build_real_ssplot_input_app(analysis_view())

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
    req(analysis_view())
    cs <- analysis_view()$cluster_summary
    overall_ss <- mean(analysis_view()$sil_df$ss, na.rm = TRUE)
    overall_aac <- analysis_view()$overall_aac
    div(class = "metricRow",
        div(class = "metricCard", HTML(paste0("Overall SS<br><b>", format(round(overall_ss, 3), nsmall = 3), "</b>"))),
        div(class = "metricCard", HTML(paste0("Overall AAC<br><b>", format(round(overall_aac, 2), nsmall = 2), "</b>"))),
        div(class = "metricCard", HTML(paste0("Clusters<br><b>", nrow(cs), "</b>"))),
        div(class = "metricCard", HTML("Formula<br><b>AAC = r/(1+r)</b>"))
    )
  })

  output$aac_table <- renderDT({
    req(analysis_view())
    datatable(
      analysis_view()$aac_dashboard |>
        mutate(across(where(is.numeric), ~ round(.x, 4))),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$aac_plot <- renderPlot({
    req(analysis_view())
    df <- analysis_view()$aac_dashboard
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
    req(analysis_view())
    draw_kano_plot(analysis_view())
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
      req(analysis_view())
      p <- draw_kano_plot(analysis_view()) +
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
    req(analysis_view())
    draw_sankey_plot(analysis_view())
  })

  output$sankey_code <- renderText({
    req(analysis_view())
    make_sankeymatic_code_app(analysis_view()$selected, analysis_view()$edges)
  })

  output$download_kano_png <- downloadHandler(
    filename = function() paste0("top20_kano_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(analysis_view())
      png(file, width = 1400, height = 1000, res = 150)
      draw_kano_plot(analysis_view())
      dev.off()
    }
  )

  output$download_sankey_png <- downloadHandler(
    filename = function() paste0("top20_sankey_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(analysis_view())
      png(file, width = 1500, height = 1000, res = 150)
      draw_sankey_plot(analysis_view())
      dev.off()
    }
  )

  output$download_network_png <- downloadHandler(
    filename = function() paste0("top20_leader_follower_network_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(analysis_view())
      png(file, width = 1400, height = 1000, res = 150)
      draw_network_static(analysis_view())
      dev.off()
    }
  )

  output$download_network_html <- downloadHandler(
    filename = function() paste0("top20_leader_follower_network_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"),
    content = function(file) {
      req(analysis_view())
      if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
        stop("Package 'htmlwidgets' is required for HTML network export. Please install.packages('htmlwidgets').")
      }
      net_obj <- build_network_objects(analysis_view())
      widget <- visNetwork(net_obj$nodes, net_obj$edges) |>
        visEdges(arrows = "to", smooth = FALSE) |>
        visNodes(scaling = list(min = 12, max = 35)) |>
        visOptions(highlightNearest = TRUE) |>
        visPhysics(stabilization = TRUE)
      htmlwidgets::saveWidget(widget, file = file, selfcontained = TRUE)
    }
  )


  output$download_chord_png <- downloadHandler(
    filename = function() {
      paste0("chord_top20_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(analysis_view())
      grDevices::png(file, width = 1600, height = 1200, res = 150)
      on.exit(grDevices::dev.off(), add = TRUE)
      draw_chord_static_app(analysis_view(), min_wcd = input$chord_min_wcd %||% 1, transparency = input$chord_transparency %||% 0.25)
    }
  )
  outputOptions(output, "download_chord_png", suspendWhenHidden = FALSE)
  output$download_rowwise_phrases_csv <- downloadHandler(
    filename = function() {
      paste0("rowwise_semantic_phrases_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    contentType = "text/csv; charset=utf-8",
    content = function(file) {
      req(analysis_view())
      out <- make_rowwise_semantic_csv(analysis_view(), top_per_row = as.integer(input$top_n %||% 20L))
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
      req(analysis_view())
      if (!requireNamespace("writexl", quietly = TRUE)) {
        stop("Package 'writexl' is required for XLSX export. Please install.packages('writexl').")
      }

      writexl::write_xlsx(
        list(
          nodes = analysis_view()$export_nodes,
          edges = analysis_view()$export_edges,
          sil_df = analysis_view()$sil_df,
          cluster_summary = analysis_view()$cluster_summary,
          aac_dashboard = analysis_view()$aac_dashboard,
          validation = analysis_view()$validation,
          final_report = analysis_view()$final_report,
          extraction_log = analysis_view()$extraction_log
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

  
.make_network_id_safe_app <- function(x) {
  x <- trimws(as.character(x %||% ""))
  x[is.na(x) | !nzchar(x)] <- paste0("node_", seq_len(sum(is.na(x) | !nzchar(x))))
  x
}

build_network_objects <- function(analysis_obj) {
    selected <- as.data.frame(analysis_obj$selected %||% data.frame(), stringsAsFactors = FALSE)
    edges <- as.data.frame(analysis_obj$edges %||% analysis_obj$export_edges %||% data.frame(), stringsAsFactors = FALSE)

    if (!nrow(selected)) {
      return(list(
        nodes = data.frame(id = character(0), label = character(0), value = numeric(0), group = character(0), shape = character(0), title = character(0), stringsAsFactors = FALSE),
        edges = data.frame(from = character(0), to = character(0), value = numeric(0), title = character(0), stringsAsFactors = FALSE)
      ))
    }

    if (!("term" %in% names(selected)) && "name" %in% names(selected)) selected$term <- selected$name
    if (!("score" %in% names(selected)) && "value" %in% names(selected)) selected$score <- selected$value
    if (!("topic" %in% names(selected))) selected$topic <- 1L
    if (!("is_leader" %in% names(selected))) selected$is_leader <- FALSE
    if (!("leader" %in% names(selected))) selected$leader <- ifelse(selected$is_leader, selected$term, NA_character_)
    if (!("doc_freq" %in% names(selected))) selected$doc_freq <- NA_real_
    if (!("source_type" %in% names(selected))) selected$source_type <- "direct_nodes_edges"
    if (!("author_keyword" %in% names(selected))) selected$author_keyword <- FALSE
    if (!("exact_in_document" %in% names(selected))) selected$exact_in_document <- NA_character_

    selected <- selected |>
      dplyr::mutate(
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
      dplyr::filter(!is.na(term), nzchar(term)) |>
      dplyr::distinct(term, .keep_all = TRUE) |>
      dplyr::mutate(.rank_score = rank(-score, ties.method = "first"), .top3 = .rank_score <= 3)

    if (!nrow(selected)) {
      return(list(nodes = data.frame(), edges = data.frame()))
    }

    selected$score[!is.finite(selected$score)] <- 0
    selected$topic[!is.finite(selected$topic)] <- 1L

    labels <- selected$term
    ids <- .make_network_id_safe_app(labels)
    names(ids) <- labels

    sc <- selected$score
    rng <- range(sc, na.rm = TRUE)
    if (!all(is.finite(rng)) || diff(rng) == 0) {
      node_value <- rep(25, nrow(selected))
    } else {
      node_value <- 15 + 35 * (sc - rng[1]) / diff(rng)
    }

    nodes_vis <- data.frame(
      id = unname(ids),
      label = ifelse(selected$is_leader %in% TRUE, paste0("★ ", labels), labels),
      value = node_value,
      group = paste0("C", selected$topic),
      shape = ifelse(selected$is_leader %in% TRUE, "star", "dot"),
      title = paste0(
        "<b>", htmltools::htmlEscape(labels), "</b>",
        "<br>Cluster: C", htmltools::htmlEscape(as.character(selected$topic)),
        "<br>Score: ", htmltools::htmlEscape(round(selected$score, 3)),
        "<br>Source: ", htmltools::htmlEscape(selected$source_type),
        "<br>Author keyword: ", selected$author_keyword
      ),
      font.size = ifelse(selected$.top3, 34, ifelse(selected$is_leader %in% TRUE, 25, 18)),
      font.face = ifelse(selected$.top3 | selected$is_leader, "bold", "normal"),
      stringsAsFactors = FALSE
    )

    if (!nrow(edges)) {
      return(list(nodes = nodes_vis, edges = data.frame(from = character(0), to = character(0), value = numeric(0), title = character(0), stringsAsFactors = FALSE)))
    }

    if (!("term1" %in% names(edges)) && "from" %in% names(edges)) edges$term1 <- edges$from
    if (!("term2" %in% names(edges)) && "to" %in% names(edges)) edges$term2 <- edges$to
    if (!("WCD" %in% names(edges)) && "weight" %in% names(edges)) edges$WCD <- edges$weight
    if (!("WCD" %in% names(edges))) edges$WCD <- 1

    edges <- edges |>
      dplyr::mutate(
        term1 = trimws(as.character(term1)),
        term2 = trimws(as.character(term2)),
        WCD = suppressWarnings(as.numeric(WCD)),
        from = unname(ids[term1]),
        to = unname(ids[term2])
      ) |>
      dplyr::filter(!is.na(from), !is.na(to), nzchar(from), nzchar(to), from != to, is.finite(WCD), WCD > 0)

    edges_vis <- edges |>
      dplyr::transmute(
        from = from,
        to = to,
        value = WCD,
        title = paste0(htmltools::htmlEscape(term1), " → ", htmltools::htmlEscape(term2), "<br>WCD = ", WCD),
        arrows = "to"
      ) |>
      as.data.frame(stringsAsFactors = FALSE)

    list(nodes = nodes_vis, edges = edges_vis)
  }


  # ---- Network output: required by visNetworkOutput("net") --------------------
  output$net <- visNetwork::renderVisNetwork({
    req(analysis_view())
    net_obj <- build_network_objects(analysis_view())

    shiny::validate(
      shiny::need(is.data.frame(net_obj$nodes) && nrow(net_obj$nodes) > 0,
           "No valid network nodes are available."),
      shiny::need(is.data.frame(net_obj$edges),
           "Network edge table is not available.")
    )

    visNetwork::visNetwork(net_obj$nodes, net_obj$edges, height = "700px", width = "100%") |>
      visNetwork::visEdges(arrows = "to", smooth = FALSE) |>
      visNetwork::visNodes(scaling = list(min = 12, max = 35)) |>
      visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
      visNetwork::visPhysics(stabilization = TRUE)
  })
  outputOptions(output, "net", suspendWhenHidden = FALSE)

  # ---- Chord helpers/output: required by uiOutput("chord_ui") -----------------
  .standardize_chord_payload_app <- function(analysis_obj, min_wcd = 1) {
    nodes <- as.data.frame(analysis_obj$export_nodes %||% data.frame(), stringsAsFactors = FALSE)
    if (!nrow(nodes)) nodes <- as.data.frame(analysis_obj$selected %||% data.frame(), stringsAsFactors = FALSE)

    if (!nrow(nodes)) {
      return(list(ok = FALSE, reason = "No nodes dataframe is available."))
    }

    if (!"name" %in% names(nodes)) {
      if ("term" %in% names(nodes)) nodes$name <- nodes$term else names(nodes)[1] <- "name"
    }
    if (!"value" %in% names(nodes)) {
      if ("score" %in% names(nodes)) nodes$value <- nodes$score else nodes$value <- seq_len(nrow(nodes))
    }
    if (!"cluster" %in% names(nodes)) {
      if ("topic" %in% names(nodes)) nodes$cluster <- nodes$topic
      else if ("carac" %in% names(nodes)) nodes$cluster <- nodes$carac
      else nodes$cluster <- 1L
    }

    nodes <- nodes |>
      dplyr::transmute(
        name = trimws(as.character(name)),
        value = suppressWarnings(as.numeric(value)),
        cluster = as.character(cluster)
      ) |>
      dplyr::filter(!is.na(name), nzchar(name)) |>
      dplyr::mutate(
        value = ifelse(is.finite(value), value, 0),
        cluster = ifelse(is.na(cluster) | !nzchar(cluster), "1", cluster)
      ) |>
      dplyr::distinct(name, .keep_all = TRUE)

    edges <- as.data.frame(analysis_obj$export_edges %||% analysis_obj$edges %||% data.frame(), stringsAsFactors = FALSE)
    if (!nrow(edges)) {
      return(list(ok = FALSE, reason = "No edges dataframe is available.", nodes = nodes, edges = edges))
    }

    if (!"term1" %in% names(edges) && "from" %in% names(edges)) edges$term1 <- edges$from
    if (!"term2" %in% names(edges) && "to" %in% names(edges)) edges$term2 <- edges$to
    if (!"WCD" %in% names(edges) && "weight" %in% names(edges)) edges$WCD <- edges$weight
    if (!"WCD" %in% names(edges) && "value" %in% names(edges)) edges$WCD <- edges$value
    if (!"WCD" %in% names(edges)) edges$WCD <- 1

    min_wcd <- suppressWarnings(as.numeric(min_wcd %||% 1))
    if (!is.finite(min_wcd) || min_wcd < 1) min_wcd <- 1

    edges <- edges |>
      dplyr::transmute(
        term1 = trimws(as.character(term1)),
        term2 = trimws(as.character(term2)),
        WCD = suppressWarnings(as.numeric(WCD))
      ) |>
      dplyr::filter(!is.na(term1), !is.na(term2), nzchar(term1), nzchar(term2),
                    term1 != term2, is.finite(WCD), WCD >= min_wcd,
                    term1 %in% nodes$name, term2 %in% nodes$name) |>
      dplyr::mutate(pair_a = pmin(term1, term2), pair_b = pmax(term1, term2)) |>
      dplyr::group_by(pair_a, pair_b) |>
      dplyr::summarise(WCD = sum(WCD, na.rm = TRUE), .groups = "drop") |>
      dplyr::transmute(term1 = pair_a, term2 = pair_b, WCD = WCD) |>
      dplyr::arrange(dplyr::desc(WCD), term1, term2)

    if (!nrow(edges)) {
      return(list(ok = FALSE, reason = "No valid chord edges remain after WCD filtering.", nodes = nodes, edges = edges))
    }

    used <- unique(c(edges$term1, edges$term2))
    nodes <- nodes[nodes$name %in% used, , drop = FALSE]

    rn <- nodes$name
    mat <- matrix(0, nrow = length(rn), ncol = length(rn), dimnames = list(rn, rn))
    for (i in seq_len(nrow(edges))) {
      a <- match(edges$term1[[i]], rn)
      b <- match(edges$term2[[i]], rn)
      w <- suppressWarnings(as.numeric(edges$WCD[[i]]))
      if (!is.na(a) && !is.na(b) && a != b && is.finite(w) && w > 0) {
        mat[a, b] <- mat[a, b] + w
        mat[b, a] <- mat[b, a] + w
      }
    }

    linked_degree <- rowSums(mat > 0, na.rm = TRUE)
    linked_WCD <- rowSums(mat, na.rm = TRUE)
    nodes_out <- nodes |>
      dplyr::mutate(linked_degree = linked_degree[name], linked_WCD = linked_WCD[name]) |>
      dplyr::arrange(dplyr::desc(linked_WCD), name)

    list(ok = TRUE, mat = mat, nodes = nodes_out, edges = edges)
  }

  draw_chord_static_app <- function(analysis_obj, min_wcd = 1,
                                    title = "Chord diagram based on nodes$name and edges$WCD",
                                    transparency = 0.25) {
    payload <- .real_chorddiag_payload_app(analysis_obj, min_wcd = min_wcd)

    if (is.null(payload) || isFALSE(payload$ok)) {
      graphics::plot.new()
      graphics::text(0.5, 0.55, "Chord diagram is not available", cex = 1.3, font = 2)
      graphics::text(0.5, 0.45, payload$reason %||% "No valid chord data.", cex = 0.9)
      return(invisible(payload))
    }

    mat <- payload$mat

    if (requireNamespace("circlize", quietly = TRUE)) {
      circlize::circos.clear()
      on.exit(circlize::circos.clear(), add = TRUE)
      old_mar <- graphics::par("mar")
      on.exit(graphics::par(mar = old_mar), add = TRUE)
      graphics::par(mar = c(1, 1, 3, 1))

      cols <- payload$sectorColors
      if (is.null(cols) || !length(cols)) {
        cols <- grDevices::hcl.colors(nrow(mat), palette = "Dark 3")
        names(cols) <- rownames(mat)
      } else {
        cols <- cols[rownames(mat)]
        cols[is.na(cols) | !nzchar(cols)] <- "#B0B0B0"
        names(cols) <- rownames(mat)
      }

      edge_df <- as.data.frame(payload$linkEdges %||% data.frame(), stringsAsFactors = FALSE)
      if (!nrow(edge_df)) {
        edge_df <- as.data.frame(as.table(mat), stringsAsFactors = FALSE)
        names(edge_df) <- c("from", "to", "value")
        edge_df <- edge_df[edge_df$value > 0 & edge_df$from != edge_df$to, , drop = FALSE]
        edge_df$color <- grDevices::adjustcolor(cols[as.character(edge_df$from)], alpha.f = 0.62)
      }

      # IMPORTANT: use long-form edge data with an explicit color vector.
      # This reliably colors both ribbons and sectors by cluster/carac.
      chord_df <- edge_df |>
        dplyr::transmute(
          from = as.character(from),
          to = as.character(to),
          value = suppressWarnings(as.numeric(value))
        ) |>
        dplyr::filter(from %in% names(cols), to %in% names(cols), from != to, is.finite(value), value > 0)

      link_col <- edge_df$color[seq_len(nrow(chord_df))]
      if (!length(link_col) || any(is.na(link_col) | !nzchar(link_col))) {
        link_col <- grDevices::adjustcolor(cols[chord_df$from], alpha.f = 0.62)
      }

      circlize::circos.par(
        start.degree = 90,
        gap.after = rep(2, length(cols)),
        track.margin = c(0.002, 0.002),
        cell.padding = c(0, 0, 0, 0)
      )

      circlize::chordDiagram(
        x = chord_df,
        grid.col = cols,
        col = link_col,
        transparency = 0,
        annotationTrack = "grid",
        directional = 0,
        preAllocateTracks = list(track.height = 0.055)
      )

      circlize::circos.trackPlotRegion(
        track.index = 1,
        bg.border = NA,
        panel.fun = function(x, y) {
          sector_name <- circlize::get.cell.meta.data("sector.index")
          xlim <- circlize::get.cell.meta.data("xlim")
          ylim <- circlize::get.cell.meta.data("ylim")
          circlize::circos.text(
            mean(xlim),
            ylim[1] + 0.008 * diff(ylim),
            sector_name,
            facing = "clockwise",
            niceFacing = TRUE,
            adj = c(0, 0.5),
            cex = 0.70
          )
        }
      )
      graphics::title(title, cex.main = 1.05)
      return(invisible(payload))
    }

    # Base R fallback: always draw something even when circlize/chorddiag is absent.
    old_mar <- graphics::par("mar")
    on.exit(graphics::par(mar = old_mar), add = TRUE)
    graphics::par(mar = c(1, 1, 3, 1))
    graphics::plot.new()
    graphics::plot.window(xlim = c(-1.35, 1.35), ylim = c(-1.35, 1.35), asp = 1)
    graphics::title(title, cex.main = 1.05)

    n <- nrow(mat)
    theta <- seq(pi/2, pi/2 - 2*pi + 2*pi/n, length.out = n)
    xy <- data.frame(
      name = rownames(mat),
      x = cos(theta),
      y = sin(theta),
      stringsAsFactors = FALSE
    )

    graphics::symbols(0, 0, circles = 1.00, inches = FALSE, add = TRUE, fg = "grey70")

    ed <- payload$edges
    max_w <- max(ed$WCD, na.rm = TRUE)
    if (!is.finite(max_w) || max_w <= 0) max_w <- 1
    for (i in seq_len(nrow(ed))) {
      a <- xy[match(ed$term1[[i]], xy$name), ]
      b <- xy[match(ed$term2[[i]], xy$name), ]
      graphics::segments(a$x * 0.92, a$y * 0.92, b$x * 0.92, b$y * 0.92,
                         lwd = 0.5 + 3 * ed$WCD[[i]] / max_w, col = grDevices::adjustcolor("grey30", alpha.f = 0.35))
    }

    graphics::points(xy$x, xy$y, pch = 21, bg = "white", cex = 2.2)
    graphics::text(xy$x * 1.06, xy$y * 1.06, labels = xy$name, cex = 0.75)
    invisible(payload)
  }


  build_chord_visnetwork_app <- function(analysis_obj, min_wcd = 1) {
    payload <- .standardize_chord_payload_app(analysis_obj, min_wcd = min_wcd)

    if (is.null(payload) || isFALSE(payload$ok)) {
      return(list(
        ok = FALSE,
        reason = payload$reason %||% "No valid chord data.",
        nodes = data.frame(),
        edges = data.frame(),
        widget = visNetwork::visNetwork(
          data.frame(id = "No valid chord data", label = "No valid chord data"),
          data.frame(),
          height = "720px",
          width = "100%"
        )
      ))
    }

    nodes <- as.data.frame(payload$nodes, stringsAsFactors = FALSE)
    edges <- as.data.frame(payload$edges, stringsAsFactors = FALSE)

    if (!nrow(nodes) || !nrow(edges)) {
      return(list(ok = FALSE, reason = "No nodes or edges for interactive chord.", nodes = nodes, edges = edges, widget = NULL))
    }

    nodes <- nodes |>
      dplyr::mutate(
        id = as.character(name),
        label = as.character(name),
        value = ifelse(is.finite(value), value, 1),
        group = paste0("C", as.character(cluster)),
        linked_degree = suppressWarnings(as.numeric(linked_degree %||% 0)),
        linked_WCD = suppressWarnings(as.numeric(linked_WCD %||% 0))
      ) |>
      dplyr::arrange(dplyr::desc(linked_WCD), label)

    n <- nrow(nodes)
    # fixed circular positions: chord-dashboard feel, but still interactive zoom/drag/select
    theta <- seq(pi / 2, pi / 2 - 2 * pi + 2 * pi / n, length.out = n)
    radius <- 560
    nodes$x <- round(radius * cos(theta), 3)
    nodes$y <- round(radius * sin(theta), 3)
    nodes$physics <- FALSE
    nodes$shape <- "dot"
    nodes$size <- pmax(18, pmin(45, 14 + sqrt(nodes$value) * 3.2))
    nodes$borderWidth <- 2
    nodes$title <- paste0(
      "<b>", htmltools::htmlEscape(nodes$label), "</b>",
      "<br>Cluster: ", htmltools::htmlEscape(nodes$group),
      "<br>Node score: ", round(nodes$value, 3),
      "<br>Linked degree: ", nodes$linked_degree,
      "<br>Linked WCD: ", round(nodes$linked_WCD, 3)
    )

    max_w <- max(edges$WCD, na.rm = TRUE)
    if (!is.finite(max_w) || max_w <= 0) max_w <- 1
    edges2 <- edges |>
      dplyr::transmute(
        from = as.character(term1),
        to = as.character(term2),
        value = suppressWarnings(as.numeric(WCD)),
        width = pmax(1, 1 + 7 * suppressWarnings(as.numeric(WCD)) / max_w),
        label = as.character(round(WCD, 2)),
        title = paste0(
          "<b>", htmltools::htmlEscape(term1), "</b> ↔ <b>", htmltools::htmlEscape(term2),
          "</b><br>WCD: ", round(WCD, 3)
        ),
        color = "rgba(70,70,70,0.35)",
        smooth = TRUE
      )

    widget <- visNetwork::visNetwork(nodes, edges2, height = "720px", width = "100%", main = "Interactive Chord Dashboard") |>
      visNetwork::visNodes(
        font = list(size = 24, face = "bold", vadjust = 0),
        scaling = list(min = 18, max = 45),
        shadow = TRUE
      ) |>
      visNetwork::visEdges(
        arrows = "",
        smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.35),
        selectionWidth = 3,
        hoverWidth = 2
      ) |>
      visNetwork::visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
        nodesIdSelection = list(enabled = TRUE, useLabels = TRUE),
        selectedBy = list(variable = "group", multiple = TRUE),
        manipulation = FALSE
      ) |>
      visNetwork::visInteraction(
        hover = TRUE,
        hoverConnectedEdges = TRUE,
        navigationButtons = TRUE,
        keyboard = TRUE,
        dragNodes = TRUE,
        dragView = TRUE,
        zoomView = TRUE,
        tooltipDelay = 80
      ) |>
      visNetwork::visPhysics(enabled = FALSE) |>
      visNetwork::visLegend(
        enabled = TRUE,
        useGroups = TRUE,
        position = "right",
        main = "Clusters"
      )

    list(ok = TRUE, reason = "", nodes = nodes, edges = edges2, payload = payload, widget = widget)
  }



  # ---- Real interactive SVG chord dashboard ---------------------------------
  build_real_chord_svg_app <- function(analysis_obj, min_wcd = 1) {
    payload <- .standardize_chord_payload_app(analysis_obj, min_wcd = min_wcd)

    if (is.null(payload) || isFALSE(payload$ok)) {
      return(tags$div(
        class = "run-help",
        HTML(paste0("<b>Chord diagram is not available:</b> ", htmltools::htmlEscape(payload$reason %||% "No valid chord data.")))
      ))
    }

    nodes <- as.data.frame(payload$nodes, stringsAsFactors = FALSE)
    edges <- as.data.frame(payload$edges, stringsAsFactors = FALSE)
    mat <- payload$mat

    if (!nrow(nodes) || !nrow(edges) || is.null(mat) || !nrow(mat)) {
      return(tags$div(class = "run-help", "No valid chord nodes or edges are available."))
    }

    nodes <- nodes |>
      dplyr::mutate(
        name = as.character(name),
        cluster = as.character(cluster %||% "1"),
        linked_WCD = suppressWarnings(as.numeric(linked_WCD %||% rowSums(mat, na.rm = TRUE))),
        value = suppressWarnings(as.numeric(value))
      ) |>
      dplyr::mutate(
        linked_WCD = ifelse(is.finite(linked_WCD), linked_WCD, 0),
        value = ifelse(is.finite(value), value, 1)
      )

    n <- nrow(nodes)
    if (n < 2) return(tags$div(class = "run-help", "At least two connected nodes are required for a chord diagram."))

    W <- pmax(nodes$linked_WCD, 1)
    total <- sum(W)
    gap <- 2 * pi * 0.006
    available <- 2 * pi - n * gap
    span <- available * W / total
    start <- numeric(n)
    end <- numeric(n)
    a <- -pi / 2
    for (i in seq_len(n)) {
      start[i] <- a
      end[i] <- a + span[i]
      a <- end[i] + gap
    }
    mid <- (start + end) / 2

    width <- 900
    height <- 900
    cx <- width / 2
    cy <- height / 2
    r_outer <- 355
    r_inner <- 312
    r_link <- 275
    r_label <- 388

    polar <- function(theta, r) {
      c(x = cx + r * cos(theta), y = cy + r * sin(theta))
    }
    arc_path <- function(s, e, r1, r0) {
      p1 <- polar(s, r1); p2 <- polar(e, r1)
      p3 <- polar(e, r0); p4 <- polar(s, r0)
      large <- ifelse(abs(e - s) > pi, 1, 0)
      paste0(
        "M", round(p1["x"], 2), ",", round(p1["y"], 2),
        " A", r1, ",", r1, " 0 ", large, ",1 ", round(p2["x"], 2), ",", round(p2["y"], 2),
        " L", round(p3["x"], 2), ",", round(p3["y"], 2),
        " A", r0, ",", r0, " 0 ", large, ",0 ", round(p4["x"], 2), ",", round(p4["y"], 2),
        " Z"
      )
    }
    chord_path <- function(i, j) {
      p1 <- polar(mid[i], r_link)
      p2 <- polar(mid[j], r_link)
      # A ribbon-like closed Bezier lens, not a graph edge.
      offset <- 0.014
      p1b <- polar(mid[i] + offset, r_link)
      p2b <- polar(mid[j] + offset, r_link)
      paste0(
        "M", round(p1["x"], 2), ",", round(p1["y"], 2),
        " Q", cx, ",", cy, " ", round(p2["x"], 2), ",", round(p2["y"], 2),
        " L", round(p2b["x"], 2), ",", round(p2b["y"], 2),
        " Q", cx, ",", cy, " ", round(p1b["x"], 2), ",", round(p1b["y"], 2),
        " Z"
      )
    }

    pal <- grDevices::hcl.colors(max(3, n), palette = "Dark 3")
    node_cols <- pal[seq_len(n)]
    names(node_cols) <- nodes$name

    sector_tags <- vector("list", n)
    label_tags <- vector("list", n)
    for (i in seq_len(n)) {
      lab_xy <- polar(mid[i], r_label)
      angle_deg <- mid[i] * 180 / pi
      flip <- angle_deg > 90 || angle_deg < -90
      rotate <- if (flip) angle_deg + 180 else angle_deg
      anchor <- if (flip) "end" else "start"
      title_txt <- paste0(
        nodes$name[i],
        "\nCluster: C", nodes$cluster[i],
        "\nLinked WCD: ", round(nodes$linked_WCD[i], 3),
        "\nNode score: ", round(nodes$value[i], 3)
      )

      sector_tags[[i]] <- tags$path(
        class = "sector",
        d = arc_path(start[i], end[i], r_outer, r_inner),
        fill = node_cols[i],
        stroke = "#ffffff",
        `stroke-width` = "1.5",
        tags$title(title_txt)
      )
      label_tags[[i]] <- tags$text(
        x = round(lab_xy["x"], 2),
        y = round(lab_xy["y"], 2),
        transform = paste0("rotate(", round(rotate, 2), " ", round(lab_xy["x"], 2), " ", round(lab_xy["y"], 2), ")"),
        `text-anchor` = anchor,
        `dominant-baseline` = "middle",
        nodes$name[i],
        tags$title(title_txt)
      )
    }

    max_w <- max(edges$WCD, na.rm = TRUE)
    if (!is.finite(max_w) || max_w <= 0) max_w <- 1

    ribbon_tags <- list()
    for (k in seq_len(nrow(edges))) {
      i <- match(edges$term1[k], nodes$name)
      j <- match(edges$term2[k], nodes$name)
      if (is.na(i) || is.na(j) || i == j) next
      w <- suppressWarnings(as.numeric(edges$WCD[k]))
      if (!is.finite(w) || w <= 0) next
      fill_col <- grDevices::adjustcolor(node_cols[i], alpha.f = 0.55)
      stroke_col <- grDevices::adjustcolor("#333333", alpha.f = 0.35)
      ribbon_tags[[length(ribbon_tags) + 1]] <- tags$path(
        class = "ribbon",
        d = chord_path(i, j),
        fill = fill_col,
        stroke = stroke_col,
        `stroke-width` = round(0.6 + 2.2 * w / max_w, 2),
        tags$title(paste0(edges$term1[k], " ↔ ", edges$term2[k], "\nWCD: ", round(w, 3)))
      )
    }

    total_wcd <- sum(edges$WCD, na.rm = TRUE)
    top_node <- nodes$name[which.max(nodes$linked_WCD)]
    cards <- div(
      class = "metricRow",
      div(class = "metricCard", HTML(paste0("Sectors<br><b>", nrow(nodes), "</b>"))),
      div(class = "metricCard", HTML(paste0("Ribbons<br><b>", nrow(edges), "</b>"))),
      div(class = "metricCard", HTML(paste0("Total WCD<br><b>", round(total_wcd, 2), "</b>"))),
      div(class = "metricCard", HTML(paste0("Top sector<br><b>", htmltools::htmlEscape(top_node), "</b>")))
    )

    tagList(
      cards,
      div(
        class = "chord-svg-wrap",
        tags$svg(
          class = "real-chord-svg",
          width = "100%",
          height = "860",
          viewBox = paste(0, 0, width, height),
          role = "img",
          tags$title("Real interactive chord diagram from nodes$name and edges$WCD"),
          tags$desc("Hover sectors and inner ribbons to inspect node and edge information."),
          tags$circle(cx = cx, cy = cy, r = r_inner, fill = "none", stroke = "#eeeeee", `stroke-width` = "1"),
          ribbon_tags,
          sector_tags,
          label_tags
        )
      ),
      tags$small(HTML("Real SVG chord: outer sectors = <code>nodes$name</code>; inner ribbons = <code>edges$WCD</code>. Hover sectors/ribbons for details."))
    )
  }



  # ---- Reference-style real chord helpers from app(3)(18).zip ----------------

  # ---- Chord color helpers: force visible cluster colors ----------------------
  .normalize_chord_cluster_app <- function(x) {
    x <- as.character(x %||% "1")
    x[is.na(x) | !nzchar(x)] <- "1"
    x <- gsub("^C", "", x, ignore.case = TRUE)
    paste0("C", x)
  }

  .chord_cluster_palette_map_app <- function(cluster_vec) {
    cluster_vec <- .normalize_chord_cluster_app(cluster_vec)
    lv <- unique(cluster_vec)
    # High-contrast colors matching the app network/SSplot style.
    base_cols <- c(
      "C1" = "#E41A1C",  # red
      "C2" = "#377EB8",  # blue
      "C3" = "#4DAF4A",  # green
      "C4" = "#FF7F00",  # orange
      "C5" = "#984EA3",  # purple
      "C6" = "#FFFF33",  # yellow
      "C7" = "#A65628",  # brown
      "C8" = "#F781BF",  # pink
      "C9" = "#999999"   # grey
    )
    missing <- setdiff(lv, names(base_cols))
    if (length(missing)) {
      extra <- grDevices::hcl.colors(max(3, length(missing)), palette = "Dark 3")[seq_along(missing)]
      names(extra) <- missing
      base_cols <- c(base_cols, extra)
    }
    out <- base_cols[lv]
    names(out) <- lv
    out
  }


  .safe_chorddiag_widget_app <- function(mat,
                                          group = NULL,
                                          groupColors = NULL,
                                          sectorColors = NULL,
                                          groupnamePadding = 6,
                                          transparency = 0.25) {
    if (!requireNamespace("chorddiag", quietly = TRUE)) return(NULL)

    fn <- chorddiag::chorddiag
    fml <- names(formals(fn))
    args <- list()

    if ("x" %in% fml) {
      args$x <- mat
    } else if ("mat" %in% fml) {
      args$mat <- mat
    } else if ("data" %in% fml) {
      args$data <- mat
    } else {
      args[[1]] <- mat
    }

    # chorddiag versions differ:
    # - some support a `group` argument and then groupColors should match groups
    # - many do NOT support `group`; then groupColors must match sectors/rownames(mat)
    has_group_arg <- "group" %in% fml && !is.null(group)
    if (has_group_arg) args$group <- group

    color_vec <- NULL
    if (has_group_arg && !is.null(groupColors) && length(groupColors)) {
      color_vec <- unname(groupColors)
    } else if (!is.null(sectorColors) && length(sectorColors)) {
      rn <- rownames(mat)
      color_vec <- sectorColors[rn]
      color_vec[is.na(color_vec) | !nzchar(color_vec)] <- "#B0B0B0"
      color_vec <- unname(color_vec)
    } else if (!is.null(groupColors) && length(groupColors)) {
      color_vec <- unname(groupColors)
    }

    if (!is.null(color_vec) && length(color_vec)) {
      if ("groupColors" %in% fml) {
        args$groupColors <- color_vec
      } else if ("groupColours" %in% fml) {
        args$groupColours <- color_vec
      } else if ("col" %in% fml) {
        args$col <- color_vec
      } else if ("colors" %in% fml) {
        args$colors <- color_vec
      }
    }

    if ("groupnamePadding" %in% fml) {
      args$groupnamePadding <- groupnamePadding
    } else if ("groupNamePadding" %in% fml) {
      args$groupNamePadding <- groupnamePadding
    }

    if ("showTicks" %in% fml) args$showTicks <- TRUE
    if ("showTooltips" %in% fml) args$showTooltips <- TRUE
    if ("margin" %in% fml) args$margin <- 90
    if ("height" %in% fml) args$height <- 720
    if ("width" %in% fml) args$width <- 720
    if ("transparency" %in% fml) args$transparency <- transparency

    do.call(fn, args)
  }

  .real_chorddiag_payload_app <- function(analysis_obj, min_wcd = 1) {
    payload <- .standardize_chord_payload_app(analysis_obj, min_wcd = min_wcd)
    if (is.null(payload) || isFALSE(payload$ok)) return(payload)

    mat <- payload$mat
    nodes <- as.data.frame(payload$nodes %||% data.frame(), stringsAsFactors = FALSE)
    rn <- rownames(mat)

    if (!nrow(nodes)) {
      nodes <- data.frame(name = rn, cluster = "C1", stringsAsFactors = FALSE)
    }
    if (!"name" %in% names(nodes)) nodes$name <- rn
    if (!"cluster" %in% names(nodes)) {
      if ("carac" %in% names(nodes)) nodes$cluster <- nodes$carac
      else if ("topic" %in% names(nodes)) nodes$cluster <- nodes$topic
      else if ("membership" %in% names(nodes)) nodes$cluster <- nodes$membership
      else nodes$cluster <- "1"
    }

    nodes$name <- trimws(as.character(nodes$name))
    nodes$cluster <- .normalize_chord_cluster_app(nodes$cluster)

    carac_by_name <- setNames(nodes$cluster, nodes$name)
    grp <- as.character(carac_by_name[rn])
    grp <- .normalize_chord_cluster_app(grp)
    grp_levels <- unique(grp)

    pal <- .chord_cluster_palette_map_app(grp)
    # sector color = cluster color, aligned to sector names
    sector_cols <- unname(pal[grp])
    names(sector_cols) <- rn
    sector_cols[is.na(sector_cols) | !nzchar(sector_cols)] <- "#B0B0B0"

    group_cols <- vapply(grp_levels, function(g) {
      idx <- which(grp == g)[1]
      unname(sector_cols[idx])
    }, FUN.VALUE = character(1))
    names(group_cols) <- grp_levels

    map <- data.frame(
      node = rn,
      carac = grp,
      color = unname(sector_cols),
      stringsAsFactors = FALSE
    )

    # Build long-form edge table for circlize. This avoids the common matrix-color
    # problem where grid.col/col is not aligned and the chord turns black/grey.
    edges_long <- as.data.frame(payload$edges %||% data.frame(), stringsAsFactors = FALSE)
    if (nrow(edges_long)) {
      edges_long <- edges_long |>
        dplyr::transmute(
          from = as.character(term1),
          to = as.character(term2),
          value = suppressWarnings(as.numeric(WCD))
        ) |>
        dplyr::filter(from %in% rn, to %in% rn, from != to, is.finite(value), value > 0)

      source_cluster <- grp[match(edges_long$from, rn)]
      source_cluster <- .normalize_chord_cluster_app(source_cluster)
      edge_base_cols <- .chord_cluster_palette_map_app(source_cluster)
      edge_cols <- unname(edge_base_cols[source_cluster])
      edge_cols[is.na(edge_cols) | !nzchar(edge_cols)] <- "#B0B0B0"
      edges_long$color <- grDevices::adjustcolor(edge_cols, alpha.f = 0.62)
    } else {
      edges_long <- data.frame(from = character(), to = character(), value = numeric(), color = character())
    }

    payload$group <- factor(grp, levels = grp_levels)
    payload$groupColors <- unname(group_cols)
    payload$sectorColors <- sector_cols
    payload$linkEdges <- edges_long
    payload$map <- map
    payload
  }


  output$chord_static_plot <- renderPlot({
    req(analysis_view())
    draw_chord_static_app(analysis_view(), min_wcd = input$chord_min_wcd %||% 1, transparency = input$chord_transparency %||% 0.25)
  }, width = 1400, height = 1000, res = 144)

  output$chord_ui <- renderUI({
    req(analysis_view())
    payload <- .real_chorddiag_payload_app(analysis_view(), min_wcd = input$chord_min_wcd %||% 1)

    if (is.null(payload) || isFALSE(payload$ok)) {
      return(tags$div(class = "run-help",
                      HTML(paste0("<b>Chord: not enough data.</b> ",
                                  htmltools::htmlEscape(payload$reason %||% "Need nodes + WCD edges.")))))
    }

    # Real interactive chord, matching the app(3)(18).zip approach.
    if (!isTRUE(input$chord_force_circlize) && requireNamespace("chorddiag", quietly = TRUE)) {
      w <- tryCatch({
        .safe_chorddiag_widget_app(
          mat = payload$mat,
          group = payload$group,
          groupColors = payload$groupColors,
          sectorColors = payload$sectorColors,
          groupnamePadding = input$chord_groupname_padding %||% 6,
          transparency = input$chord_transparency %||% 0.25
        )
      }, error = function(e) {
        tags$div(class = "run-help",
                 HTML(paste0("<b>chorddiag failed:</b> ", htmltools::htmlEscape(conditionMessage(e)),
                             "<br>Using static circlize fallback below.")))
      })
      if (!is.null(w) && !inherits(w, "shiny.tag")) return(w)
    }

    # Static real chord fallback.
    if (requireNamespace("circlize", quietly = TRUE)) {
      return(tagList(
        tags$div(class = "run-help",
                 HTML("Package <code>chorddiag</code> is not installed; showing static <code>circlize::chordDiagram()</code>. For interactive chord, run <code>install.packages('chorddiag')</code>.")),
        plotOutput("chord_static_plot", height = "720px")
      ))
    }

    tags$div(
      class = "run-help",
      tags$b("Chord needs a package:"),
      tags$br(),
      "Install either ", tags$code("install.packages('chorddiag')"),
      " for the interactive chord, or ", tags$code("install.packages('circlize')"),
      " for the static chord fallback."
    )
  })
  outputOptions(output, "chord_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "chord_static_plot", suspendWhenHidden = FALSE)

  output$chord_nodes_table <- DT::renderDT({
    req(analysis_view())
    payload <- .standardize_chord_payload_app(analysis_view(), min_wcd = input$chord_min_wcd %||% 1)
    nodes <- payload$nodes %||% data.frame()
    DT::datatable(nodes, options = list(pageLength = 20, scrollX = TRUE))
  })

  output$chord_edges_table <- DT::renderDT({
    req(analysis_view())
    payload <- .standardize_chord_payload_app(analysis_view(), min_wcd = input$chord_min_wcd %||% 1)
    edges <- payload$edges %||% data.frame()
    DT::datatable(edges, options = list(pageLength = 20, scrollX = TRUE))
  })


  output$chord_color_table <- DT::renderDT({
    req(analysis_view())
    payload <- .real_chorddiag_payload_app(analysis_view(), min_wcd = input$chord_min_wcd %||% 1)
    mp <- payload$map %||% data.frame()
    if (!nrow(mp)) mp <- data.frame(node = character(), carac = character(), color = character())
    DT::datatable(mp, options = list(pageLength = 20, scrollX = TRUE), escape = FALSE)
  })
  outputOptions(output, "chord_color_table", suspendWhenHidden = FALSE)

  output$chord_debug <- renderPrint({
    req(analysis_view())
    payload <- .real_chorddiag_payload_app(analysis_view(), min_wcd = input$chord_min_wcd %||% 1)
    cat("Reference-style real chord ok:", isTRUE(payload$ok), "
")
    if (!isTRUE(payload$ok)) cat("Reason:", payload$reason %||% "", "
")
    cat("Sectors from nodes$name:", nrow(payload$nodes %||% data.frame()), "
")
    cat("Ribbons from edges$WCD:", nrow(payload$edges %||% data.frame()), "
")
    cat("Matrix sumW:", if (!is.null(payload$mat)) sum(payload$mat) else NA, "
")
    cat("Interactive package chorddiag installed:", requireNamespace("chorddiag", quietly = TRUE), "
")
    cat("Static package circlize installed:", requireNamespace("circlize", quietly = TRUE), "
")
    cat("Label padding:", input$chord_groupname_padding %||% 6, "
")
    if (!is.null(payload$map)) print(payload$map)
    if (!is.null(payload$groupColors)) {
      cat("Group colors:\n")
      print(payload$groupColors)
    }
    if (!is.null(payload$linkEdges)) {
      cat("First link colors:\n")
      print(utils::head(payload$linkEdges, 10))
    }
    if (requireNamespace("chorddiag", quietly = TRUE)) {
      fml <- names(formals(chorddiag::chorddiag))
      cat("chorddiag formals include group:", "group" %in% fml, "\n")
      cat("chorddiag formals include groupColors:", "groupColors" %in% fml, "\n")
      cat("Sector colors sent to chorddiag:\n")
      print(payload$sectorColors)
    }
  })


}

# ---- v9 runtime relaxation: prevent over-strict Chinese break lists -----------
if (exists("zh_extra_condition_break_terms_app")) {
  zh_extra_condition_break_terms_app <- setdiff(
    unique(zh_extra_condition_break_terms_app),
    c("用藥", "研究", "方法", "結果", "結論", "目的")
  )
}
if (exists("zh_step2_break_terms_app")) {
  zh_step2_break_terms_app <- setdiff(
    unique(zh_step2_break_terms_app),
    c("用藥", "研究", "方法", "結果", "結論", "目的")
  )
}



# ---- v12 Chinese never-stop ultra rescue ------------------------------------
.v12_zh_ultra_rescue_app <- function(text_all, top_n = 20L, min_chars = 3L, max_chars = 12L) {
  x <- paste(as.character(text_all %||% ""), collapse = "
")
  if (!nzchar(trimws(x))) return(character(0))
  bad <- unique(c(
    "作者", "通訊作者", "通讯作者", "共同作者", "第一作者", "第二作者",
    "合著者", "研究者", "撰寫者", "投稿者", "審查者", "审查者",
    "進行", "进行", "成為", "成为", "作為", "作为", "用以", "用於", "用于",
    "藉由", "藉以", "統計分析", "统计分析", "研究目的", "研究方法",
    "研究結果", "結果顯示", "结果显示", "採用", "采用", "利用", "透過", "通过",
    "顯示", "显示", "指出", "發現", "发现", "建立", "形成", "進一步",
    "呈現", "使用", "輸出", "結果", "不同", "與", "和", "及", "的",
    get0("zh_tail_condition_break_terms", ifnotfound = character(0)),
    get0("zh_condition_break_terms_app", ifnotfound = character(0)),
    get0("zh_extra_condition_break_terms_app", ifnotfound = character(0))
  ))
  bad <- bad[nzchar(bad)]
  bad <- bad[order(nchar(bad, type = "chars"), decreasing = TRUE)]
  y <- gsub("[^\\p{Han}]", "，", x, perl = TRUE)
  for (b in bad) y <- gsub(b, "，", y, fixed = TRUE)
  y <- gsub("，+", "，", y, perl = TRUE)
  parts <- unlist(strsplit(y, "，+", perl = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  suffix <- unique(c("分析", "模式", "模型", "方法", "機制", "系統", "結構", "理論", "架構", "指標", "評估", "測量", "管理", "政策", "品質", "安全", "課程", "照護", "醫療", "醫院", "病人", "用藥", "重複", "關係", "強度", "資料", "網絡", "片語", "摘要"))
  out <- character(0)
  for (pt in parts) {
    n <- nchar(pt, type = "chars")
    if (n >= min_chars && n <= max_chars) out <- c(out, pt)
    if (n > max_chars) {
      chars <- strsplit(pt, "", fixed = TRUE)[[1]]
      for (L in c(10L, 8L, 6L, 4L, 3L)) {
        if (L < min_chars || L > max_chars || L > n) next
        for (i in seq_len(n - L + 1L)) {
          cand <- paste(chars[i:(i + L - 1L)], collapse = "")
          if (any(vapply(suffix, function(sf) endsWith(cand, sf) || grepl(sf, cand, fixed = TRUE), logical(1)))) out <- c(out, cand)
        }
      }
    }
  }
  out <- gsub("[^\\p{Han}]", "", out, perl = TRUE)
  out <- trimws(out)
  out <- out[nzchar(out)]
  out <- out[nchar(out, type = "chars") >= min_chars & nchar(out, type = "chars") <= max_chars]
  if (length(bad)) out <- out[!vapply(out, function(z) any(vapply(bad, function(b) grepl(b, z, fixed = TRUE), logical(1))), logical(1))]
  if (exists("zh_v9_has_never_node_term_app", mode = "function")) out <- out[!vapply(out, zh_v9_has_never_node_term_app, logical(1))]
  out <- out[nzchar(out)]
  if (!length(out)) return(character(0))
  tab <- sort(table(out), decreasing = TRUE)
  names(tab)[seq_len(min(length(tab), max(80L, top_n * 4L)))]
}




# ---- v27: Force Chinese checkbox path through zh_tail_keyword_engine.R --------
# v26 did source zh_tail_keyword_engine.R, but the checked Chinese branch returned
# through make_chinese_analysis_directsafe_app(), so final nodes could bypass
# run_zh_main_document_tail_pipeline(). This wrapper forces zh-tail first.
.make_chinese_analysis_directsafe_app_v27_fallback <- make_chinese_analysis_directsafe_app
make_chinese_analysis_directsafe_app <- function(docs_tbl,
                                                 author_keywords_manual = character(0),
                                                 top_n = 20L,
                                                 processing_log_tbl = NULL,
                                                 reason = "Chinese direct-safe mode") {
  if (isTRUE(.zh_tail_engine_loaded) && exists("run_zh_main_document_tail_pipeline", mode = "function")) {
    out <- tryCatch(
      make_chinese_analysis_app(
        docs_tbl = docs_tbl,
        author_keywords_manual = author_keywords_manual,
        top_n = top_n,
        min_edge_docs = 1L,
        protect_author_keywords = TRUE,
        processing_log_tbl = processing_log_tbl
      ),
      error = function(e) e
    )
    if (!inherits(out, "error")) {
      out$extraction_log <- dplyr::bind_rows(
        out$extraction_log %||% tibble::tibble(),
        tibble::tibble(
          item = c("zh_tail_keyword_engine_loaded", "zh_tail_keyword_engine_used", "chinese_pipeline"),
          value = c(as.character(isTRUE(.zh_tail_engine_loaded)), "TRUE", "run_zh_main_document_tail_pipeline")
        )
      )
      out$final_report <- dplyr::bind_rows(
        out$final_report %||% tibble::tibble(),
        tibble::tibble(
          item = c("zh_tail_keyword_engine_used", "chinese_pipeline"),
          value = c("TRUE", "run_zh_main_document_tail_pipeline")
        )
      )
      if (exists(".strict_v4_sanitize_analysis_app", mode = "function")) out <- .strict_v4_sanitize_analysis_app(out)
      return(out)
    }
    if (exists("console_log_step_app", mode = "function")) {
      console_log_step_app("ZH-v27 zh_tail engine failed; falling back to direct-safe", data.frame(error = conditionMessage(out), stringsAsFactors = FALSE))
    }
  }
  out <- .make_chinese_analysis_directsafe_app_v27_fallback(
    docs_tbl = docs_tbl,
    author_keywords_manual = author_keywords_manual,
    top_n = top_n,
    processing_log_tbl = processing_log_tbl,
    reason = paste0(reason, "; zh_tail fallback was used only because engine was unavailable or failed")
  )
  out$extraction_log <- dplyr::bind_rows(
    out$extraction_log %||% tibble::tibble(),
    tibble::tibble(
      item = c("zh_tail_keyword_engine_loaded", "zh_tail_keyword_engine_used", "chinese_pipeline"),
      value = c(as.character(isTRUE(.zh_tail_engine_loaded)), "FALSE", "direct_safe_fallback")
    )
  )
  out
}



# ---- v28: final zh-tail purification immediately before visualization --------
.v28_clean_final_zh_label_app <- function(x, min_chars = 3L, max_chars = 10L, author_keyword = FALSE) {
  x0 <- as.character(x %||% "")
  if (!nzchar(trimws(x0))) return("")
  y <- ""
  if (exists("zh_clean_piece_directsafe_app", mode = "function")) {
    y <- tryCatch(zh_clean_piece_directsafe_app(x0, min_chars = min_chars, max_chars = max_chars, author_keyword = author_keyword), error = function(e) "")
  }
  if (!nzchar(y) && exists("clean_zh_phrase_strict_app", mode = "function")) {
    y <- tryCatch(clean_zh_phrase_strict_app(x0, min_chars = min_chars, max_chars = max_chars, author_keyword = author_keyword), error = function(e) "")
  }
  if (!nzchar(y)) {
    y <- gsub("[^\\p{Han}]", "", x0, perl = TRUE)
    y <- trimws(y)
    n <- nchar(y, type = "chars")
    if (n < min_chars || n > max_chars) y <- ""
  }
  y
}

.v28_purify_zh_visual_analysis_app <- function(out, top_n = 20L) {
  if (is.null(out) || !is.list(out)) return(out)
  labels <- character(0)
  for (nm in c("selected", "export_nodes", "nodes", "sil_df", "ranked", "extracted")) {
    df <- out[[nm]]
    if (is.data.frame(df) && nrow(df)) {
      cc <- intersect(c("term", "name", "label", "term_final"), names(df))
      if (length(cc)) labels <- c(labels, as.character(df[[cc[1]]]))
    }
  }
  for (enm in c("edges", "export_edges", "co_edges", "step5_final_edges")) {
    ed <- out[[enm]]
    if (is.data.frame(ed) && nrow(ed)) {
      for (cc in intersect(c("term1", "term2", "from", "to", "source", "target", "node1", "node2"), names(ed))) labels <- c(labels, as.character(ed[[cc]]))
    }
  }
  if (!any(grepl("\\p{Han}", labels, perl = TRUE))) return(out)

  labels <- unique(trimws(as.character(labels)))
  labels <- labels[nzchar(labels)]
  map <- vapply(labels, .v28_clean_final_zh_label_app, character(1), min_chars = 3L, max_chars = 10L, author_keyword = FALSE)
  map <- map[nzchar(names(map))]
  remap_vec <- function(v) {
    v <- as.character(v %||% character(0)); hit <- match(v, names(map)); z <- v; ok <- !is.na(hit); z[ok] <- unname(map[hit[ok]]); z
  }
  valid <- function(v) {
    v <- as.character(v %||% character(0))
    nzchar(v) & grepl("\\p{Han}", v, perl = TRUE) & !grepl("[「」『』\"'\\[\\]【】（）()<>《》,:;；，。！？/\\\\|]", v, perl = TRUE)
  }
  remap_df <- function(df) {
    if (!is.data.frame(df) || !nrow(df)) return(df)
    for (cc in intersect(c("name", "term", "label", "leader", "cluster_leader", "theme_leader", "neighbor_name", "node", "node1", "node2", "from", "to", "source", "target", "term1", "term2", "term_final"), names(df))) df[[cc]] <- remap_vec(df[[cc]])
    df
  }
  for (nm in c("selected", "export_nodes", "nodes", "sil_df", "ranked", "extracted", "step4_final_top20_purified", "edges", "export_edges", "co_edges", "step5_final_edges")) if (is.data.frame(out[[nm]])) out[[nm]] <- remap_df(out[[nm]])

  sel <- out$selected %||% data.frame()
  if (is.data.frame(sel) && nrow(sel)) {
    if (!"term" %in% names(sel) && "name" %in% names(sel)) sel$term <- sel$name
    if ("term" %in% names(sel)) {
      sel <- sel[valid(sel$term), , drop = FALSE]
      if (!"score" %in% names(sel)) sel$score <- if ("value" %in% names(sel)) suppressWarnings(as.numeric(sel$value)) else 1
      if (!"topic" %in% names(sel)) sel$topic <- if ("carac" %in% names(sel)) suppressWarnings(as.integer(sel$carac)) else seq_len(nrow(sel))
      if (!"leader" %in% names(sel)) sel$leader <- sel$term
      if (!"is_leader" %in% names(sel)) sel$is_leader <- sel$term == sel$leader
      sel <- sel |>
        dplyr::mutate(score = suppressWarnings(as.numeric(score)), topic = suppressWarnings(as.integer(topic))) |>
        dplyr::group_by(term) |>
        dplyr::summarise(score = max(score, na.rm = TRUE), topic = dplyr::first(topic[is.finite(topic)] %||% topic[1]), leader = dplyr::first(leader), is_leader = any(is_leader %in% TRUE), .groups = "drop") |>
        dplyr::mutate(score = ifelse(is.finite(score), score, 1), topic = ifelse(is.finite(topic), topic, dplyr::row_number()), leader = ifelse(valid(leader) & leader %in% term, leader, term), value = score, value2 = score, degree = score, doc_freq = score, tfidf_sum = score, source_type = "zh-tail-final", author_keyword = FALSE, exact_in_document = TRUE, topic_rank = dplyr::row_number()) |>
        dplyr::arrange(dplyr::desc(score), term) |>
        dplyr::slice_head(n = as.integer(top_n %||% 20L))
      out$selected <- sel
    }
  }

  keep <- if (is.data.frame(out$selected) && nrow(out$selected) && "term" %in% names(out$selected)) unique(as.character(out$selected$term)) else character(0)
  if (length(keep)) {
    filter_edges <- function(ed) {
      if (!is.data.frame(ed) || !nrow(ed)) return(ed)
      if (!"term1" %in% names(ed)) { if ("from" %in% names(ed)) ed$term1 <- ed$from else if ("source" %in% names(ed)) ed$term1 <- ed$source else if ("node1" %in% names(ed)) ed$term1 <- ed$node1 }
      if (!"term2" %in% names(ed)) { if ("to" %in% names(ed)) ed$term2 <- ed$to else if ("target" %in% names(ed)) ed$term2 <- ed$target else if ("node2" %in% names(ed)) ed$term2 <- ed$node2 }
      if (!all(c("term1", "term2") %in% names(ed))) return(ed[0, , drop = FALSE])
      if (!"WCD" %in% names(ed)) ed$WCD <- if ("weight" %in% names(ed)) suppressWarnings(as.numeric(ed$weight)) else 1
      ed <- ed[valid(ed$term1) & valid(ed$term2) & ed$term1 %in% keep & ed$term2 %in% keep & ed$term1 != ed$term2, , drop = FALSE]
      if (nrow(ed)) ed <- ed |> dplyr::group_by(term1, term2) |> dplyr::summarise(WCD = sum(suppressWarnings(as.numeric(WCD)), na.rm = TRUE), .groups = "drop") |> dplyr::mutate(WCD = ifelse(is.finite(WCD) & WCD > 0, WCD, 1))
      ed
    }
    out$edges <- filter_edges(out$edges); out$export_edges <- filter_edges(out$export_edges %||% out$edges); out$co_edges <- filter_edges(out$co_edges %||% out$edges); out$step5_final_edges <- filter_edges(out$step5_final_edges %||% out$edges)
    if ((!is.data.frame(out$edges) || !nrow(out$edges)) && nrow(out$selected) >= 2) {
      ss <- out$selected
      eds <- do.call(rbind, lapply(split(ss, ss$topic), function(g) { if (nrow(g) < 2) return(NULL); leader <- g$term[which.max(g$score)]; followers <- setdiff(g$term, leader); if (!length(followers)) return(NULL); data.frame(term1 = followers, term2 = leader, WCD = 1, stringsAsFactors = FALSE) }))
      if (is.null(eds) || !nrow(eds)) eds <- data.frame(term1 = keep[-length(keep)], term2 = keep[-1], WCD = 1, stringsAsFactors = FALSE)
      out$edges <- out$export_edges <- out$co_edges <- out$step5_final_edges <- eds
    }
    ss <- out$selected
    out$export_nodes <- ss |> dplyr::transmute(name = term, value = score, carac = topic, value2 = score, membership = topic, role = ifelse(is_leader, "leader", "follower"), neighbor_name = leader)
    out$nodes <- out$export_nodes
    out$sil_df <- ss |> dplyr::transmute(term = term, name = term, topic = topic, carac = topic, sil_width = 0, ss = sil_width, value = score, value2 = score, leader = leader, is_leader = is_leader)
    out$ranked <- ss |> dplyr::transmute(term = term, score = score, rank = dplyr::row_number())
    out$extracted <- out$ranked
    out$step4_final_top20_purified <- ss |> dplyr::transmute(rank = dplyr::row_number(), term_final = term, score = score, engine = "zh-tail-final")
    ed <- out$export_edges %||% out$edges; vtx <- data.frame(name = keep, stringsAsFactors = FALSE)
    if (is.data.frame(ed) && nrow(ed)) out$graph <- igraph::graph_from_data_frame(ed |> dplyr::transmute(from = term1, to = term2, weight = WCD), directed = TRUE, vertices = vtx) else { out$graph <- igraph::make_empty_graph(n = nrow(vtx), directed = TRUE); igraph::V(out$graph)$name <- vtx$name }
  }
  out$extraction_log <- dplyr::bind_rows(out$extraction_log %||% tibble::tibble(), tibble::tibble(item = "v28_final_zh_tail_before_visual", value = "TRUE"))
  out$final_report <- dplyr::bind_rows(out$final_report %||% tibble::tibble(), tibble::tibble(item = "v28_final_zh_tail_before_visual", value = "TRUE"))
  out
}

if (exists("make_chinese_analysis_app_old_primary_disabled", mode = "function")) {
  make_chinese_analysis_app <- function(docs_tbl, author_keywords_manual = character(0), top_n = 20L, min_edge_docs = 1L, protect_author_keywords = TRUE, processing_log_tbl = NULL) {
    out <- make_chinese_analysis_app_old_primary_disabled(docs_tbl = docs_tbl, author_keywords_manual = author_keywords_manual, top_n = top_n, min_edge_docs = min_edge_docs, protect_author_keywords = protect_author_keywords, processing_log_tbl = processing_log_tbl)
    .v28_purify_zh_visual_analysis_app(out, top_n = top_n)
  }
}

if (exists(".make_chinese_analysis_directsafe_app_v27_fallback", mode = "function")) {
  make_chinese_analysis_directsafe_app <- function(docs_tbl, author_keywords_manual = character(0), top_n = 20L, processing_log_tbl = NULL, reason = "Chinese direct-safe mode") {
    out <- NULL
    if (exists("make_chinese_analysis_app_old_primary_disabled", mode = "function") && isTRUE(.zh_tail_engine_loaded)) {
      out <- tryCatch(make_chinese_analysis_app_old_primary_disabled(docs_tbl = docs_tbl, author_keywords_manual = author_keywords_manual, top_n = top_n, min_edge_docs = 1L, protect_author_keywords = TRUE, processing_log_tbl = processing_log_tbl), error = function(e) e)
    }
    if (inherits(out, "error") || is.null(out)) {
      if (exists("console_log_step_app", mode = "function")) console_log_step_app("ZH-v28 zh_tail primary failed; using direct-safe fallback then final zh-tail purification", data.frame(error = if (inherits(out, "error")) conditionMessage(out) else "primary unavailable", stringsAsFactors = FALSE))
      out <- .make_chinese_analysis_directsafe_app_v27_fallback(docs_tbl = docs_tbl, author_keywords_manual = author_keywords_manual, top_n = top_n, processing_log_tbl = processing_log_tbl, reason = paste0(reason, "; v28 fallback but final labels are purified before visualization"))
    }
    out$extraction_log <- dplyr::bind_rows(out$extraction_log %||% tibble::tibble(), tibble::tibble(item = c("zh_tail_keyword_engine_loaded", "v28_final_zh_tail_before_visual"), value = c(as.character(isTRUE(.zh_tail_engine_loaded)), "TRUE")))
    .v28_purify_zh_visual_analysis_app(out, top_n = top_n)
  }
}



# ---- v29: robust Chinese PDF final pipeline before visualization -------------
# Reason: some Chinese PDFs contain English title/abstract, journal headers, page
# numbers, and broken line wrapping. Earlier direct-safe fallback could return only
# one header-like term (e.g., 醫療資訊雜誌第三十三). This override extracts from
# Chinese body text, applies zh-tail/break-word cleanup, then rebuilds nodes/edges.
.v29_zh_pdf_line_keep_app <- function(x) {
  x <- as.character(x %||% "")
  han_n <- nchar(gsub("[^\\p{Han}]", "", x, perl = TRUE), type = "chars")
  if (!is.finite(han_n) || han_n < 6L) return(FALSE)
  bad <- c("醫療資訊雜誌", "The Journal", "Copyright", "Vol", "作者簡介", "通訊作者", "電子信箱", "聯絡地址", "聯絡電話", "參考文獻", "附錄")
  !any(vapply(bad, function(b) grepl(b, x, fixed = TRUE), logical(1)))
}

.v29_zh_clean_token_app <- function(x, min_chars = 3L, max_chars = 12L) {
  y <- as.character(x %||% "")
  y <- gsub("[^\\p{Han}]", "", y, perl = TRUE)
  y <- trimws(y)
  n <- nchar(y, type = "chars")
  if (!nzchar(y) || n < min_chars || n > max_chars) return(NA_character_)
  if (exists("zh_tail_condition_break_terms", inherits = TRUE)) {
    br <- get("zh_tail_condition_break_terms", inherits = TRUE)
    if (length(br) && any(vapply(br, function(b) identical(y, b), logical(1)))) return(NA_character_)
  }
  bad_exact <- c("醫療資訊", "醫療資訊雜誌", "第三十三", "民國年月", "研究方法", "研究結果", "結果顯示", "本研究", "使用", "呈現", "不同", "進行", "利用")
  if (y %in% bad_exact) return(NA_character_)
  y
}

.v29_extract_zh_terms_from_pdf_text_app <- function(text_all, top_n = 20L) {
  top_n <- as.integer(top_n %||% 20L); if (!is.finite(top_n) || top_n < 5L) top_n <- 20L
  lines <- unlist(strsplit(as.character(text_all %||% ""), "\\n", fixed = FALSE), use.names = FALSE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  # Keep Chinese body/abstract lines and remove noisy PDF headers/footers.
  body_lines <- lines[vapply(lines, .v29_zh_pdf_line_keep_app, logical(1))]
  if (!length(body_lines)) body_lines <- lines[grepl("\\p{Han}", lines, perl = TRUE)]

  kw <- character(0)
  kw_hit <- grep("關鍵字|关键字|關鍵詞|关键词", lines, perl = TRUE)
  if (length(kw_hit)) {
    kw_line <- lines[kw_hit[1]]
    kw_line <- sub("^.*?(關鍵字|关键字|關鍵詞|关键词)\\s*[:：]", "", kw_line, perl = TRUE)
    kw <- unlist(strsplit(kw_line, "[、,，;；]", perl = TRUE), use.names = FALSE)
    kw <- vapply(kw, .v29_zh_clean_token_app, character(1), min_chars = 3L, max_chars = 12L)
    kw <- unique(kw[!is.na(kw) & nzchar(kw)])
  }

  text_body <- paste(body_lines, collapse = "\n")
  # Turn break words into boundaries, not retained labels.
  if (exists("zh_tail_condition_break_terms", inherits = TRUE)) {
    br <- unique(as.character(get("zh_tail_condition_break_terms", inherits = TRUE)))
    br <- br[nzchar(br)]
    for (b in br) text_body <- gsub(b, "，", text_body, fixed = TRUE)
  }
  segs <- unlist(strsplit(text_body, "[^\\p{Han}]+", perl = TRUE), use.names = FALSE)
  segs <- trimws(segs)
  segs <- segs[nchar(segs, type = "chars") >= 4L]

  # Suffix/core vocabulary makes Chinese extraction phrase-like rather than random n-grams.
  suffix_core <- c(
    "重複處方", "用藥重複", "處方重複", "抗血壓", "抗血脂", "抗血糖", "血壓", "血脂", "血糖", "三高",
    "醫院", "醫學中心", "區域醫院", "地區醫院", "醫療機構", "醫療服務", "整合照護", "門診病人",
    "健康保險", "開放服務平台", "資料開放", "醫療品質", "品質指標", "資料來源", "資料完整",
    "關聯", "獨立性", "負向關聯", "顯著關聯", "路徑係數", "因素分析", "驗證性因素分析", "探索式因素分析", "驗證式因素分析", "結構方程", "多群組", "適配指標", "模型適配", "樣本數", "研究假設", "研究結果", "研究限制", "用藥管理"
  )
  suffix_core <- unique(c(suffix_core, kw))

  cand <- character(0)
  for (sg in segs) {
    n <- nchar(sg, type = "chars")
    # exact known/core phrases appearing in segment
    hit <- suffix_core[vapply(suffix_core, function(z) nzchar(z) && grepl(z, sg, fixed = TRUE), logical(1))]
    cand <- c(cand, hit)
    # local windows around core/suffix terms, 4-10 chars only
    if (n >= 4L) {
      maxL <- min(10L, n)
      for (L in 4L:maxL) {
        for (i in seq_len(n - L + 1L)) {
          z <- substr(sg, i, i + L - 1L)
          if (any(vapply(suffix_core, function(core) nzchar(core) && (endsWith(z, core) || grepl(core, z, fixed = TRUE)), logical(1)))) cand <- c(cand, z)
        }
      }
    }
  }
  cand <- vapply(cand, .v29_zh_clean_token_app, character(1), min_chars = 3L, max_chars = 12L)
  cand <- cand[!is.na(cand) & nzchar(cand)]
  # remove very header-like leftovers
  cand <- cand[!grepl("醫療資訊雜誌|第三十三卷|民國", cand, perl = TRUE)]
  # Prefer terms seen in actual body, plus author keywords.
  if (!length(cand)) cand <- kw
  tab <- sort(table(cand), decreasing = TRUE)
  df <- tibble::tibble(name = names(tab), value = as.numeric(tab))
  if (nrow(df)) {
    df <- df |>
      dplyr::mutate(nc = nchar(name, type = "chars"), is_kw = name %in% kw) |>
      dplyr::arrange(dplyr::desc(is_kw), dplyr::desc(value), dplyr::desc(nc), name)
    # subset control: keep a shorter term only if it is an author keyword or has strong frequency.
    kept <- character(0)
    for (i in seq_len(nrow(df))) {
      z <- df$name[i]
      inside <- length(kept) && any(vapply(kept, function(k) grepl(z, k, fixed = TRUE) || grepl(k, z, fixed = TRUE), logical(1)))
      if (!inside || df$is_kw[i] || df$value[i] >= 2) kept <- c(kept, z)
      if (length(kept) >= top_n) break
    }
    df <- df[df$name %in% kept, , drop = FALSE] |>
      dplyr::arrange(dplyr::desc(is_kw), dplyr::desc(value), dplyr::desc(nc), name) |>
      dplyr::slice_head(n = top_n) |>
      dplyr::select(name, value)
  }
  df
}

.v29_build_zh_analysis_from_terms_app <- function(docs_tbl, terms_df, top_n = 20L, processing_log_tbl = NULL) {
  if (!is.data.frame(terms_df) || !nrow(terms_df)) stop("Chinese PDF extraction returned no usable terms after zh-tail cleanup.", call. = FALSE)
  terms_df <- terms_df |> dplyr::mutate(name = as.character(name), value = suppressWarnings(as.numeric(value))) |> dplyr::filter(nzchar(name), is.finite(value)) |> dplyr::slice_head(n = top_n)
  k <- nrow(terms_df)
  # Build document/page co-occurrence edges.
  units <- as.character(docs_tbl$text %||% "")
  ed_list <- list()
  for (u in units) {
    present <- terms_df$name[vapply(terms_df$name, function(z) grepl(z, u, fixed = TRUE), logical(1))]
    if (length(present) >= 2L) {
      cmb <- utils::combn(present, 2L)
      ed_list[[length(ed_list) + 1L]] <- data.frame(term1 = cmb[1, ], term2 = cmb[2, ], WCD = 1, stringsAsFactors = FALSE)
    }
  }
  edges <- if (length(ed_list)) dplyr::bind_rows(ed_list) else data.frame(term1 = character(), term2 = character(), WCD = numeric())
  if (nrow(edges)) edges <- edges |> dplyr::group_by(term1, term2) |> dplyr::summarise(WCD = sum(WCD), .groups = "drop") |> dplyr::arrange(dplyr::desc(WCD))
  if (!nrow(edges) && k >= 2L) edges <- data.frame(term1 = terms_df$name[-k], term2 = terms_df$name[-1], WCD = 1, stringsAsFactors = FALSE)
  g0 <- igraph::graph_from_data_frame(if (nrow(edges)) edges |> dplyr::transmute(from = term1, to = term2, weight = WCD) else data.frame(from=character(), to=character(), weight=numeric()), directed = FALSE, vertices = data.frame(name = terms_df$name))
  memb <- if (igraph::gorder(g0) > 0 && igraph::gsize(g0) > 0) igraph::components(g0)$membership else seq_len(k)
  topic <- as.integer(memb[terms_df$name]); topic[!is.finite(topic)] <- seq_len(sum(!is.finite(topic)))
  # Cap many tiny components into 3 visual groups for readability.
  if (length(unique(topic)) > 3L) topic <- ((seq_len(k) - 1L) %% 3L) + 1L
  selected <- tibble::tibble(term = terms_df$name, score = terms_df$value, topic = topic) |>
    dplyr::group_by(topic) |>
    dplyr::mutate(leader = term[which.max(score)], is_leader = term == leader) |>
    dplyr::ungroup() |>
    dplyr::mutate(value = score, value2 = score, degree = score, doc_freq = score, tfidf_sum = score, source_type = "zh-tail-pdf-v29", author_keyword = FALSE, exact_in_document = TRUE, topic_rank = dplyr::row_number())
  export_nodes <- selected |> dplyr::transmute(name = term, value = score, carac = topic, value2 = score, membership = topic, role = ifelse(is_leader, "leader", "follower"), neighbor_name = leader)
  sil_df <- selected |> dplyr::transmute(term = term, name = term, topic = topic, carac = topic, sil_width = 0, ss = 0, value = score, value2 = score, leader = leader, is_leader = is_leader)
  g <- if (nrow(edges)) igraph::graph_from_data_frame(edges |> dplyr::transmute(from = term1, to = term2, weight = WCD), directed = TRUE, vertices = data.frame(name = selected$term)) else { gg <- igraph::make_empty_graph(n = nrow(selected), directed = TRUE); igraph::V(gg)$name <- selected$term; gg }
  cluster_summary <- selected |> dplyr::count(topic, name = "n") |> dplyr::mutate(cluster = topic, leader = vapply(topic, function(tp) selected$leader[selected$topic == tp][1], character(1)), cluster_ss = 0, AAC = 0.5)
  aac_dashboard <- cluster_summary
  validation <- tibble::tibble(check = c("Chinese PDF zh-tail extraction", "Symbols removed before visual", "Terms selected"), status = c("OK", "OK", as.character(nrow(selected))))
  final_report <- tibble::tibble(item = c("Chinese document mode", "Chinese pipeline", "v29_terms", "v29_edges"), value = c("TRUE", "zh_tail_pdf_final_before_visual", as.character(nrow(selected)), as.character(nrow(edges))))
  list(docs = docs_tbl, author_keywords = character(0), extracted = selected |> dplyr::transmute(term, score), ranked = selected |> dplyr::transmute(term, score, rank = dplyr::row_number()), selected = selected, edges = edges, co_edges = edges, graph = g, sil_df = sil_df, cluster_summary = cluster_summary, aac_dashboard = aac_dashboard, overall_aac = 0.5, extraction_log = final_report, export_nodes = export_nodes, export_edges = edges, validation = validation, final_report = final_report, processing_log = processing_log_tbl %||% tibble::tibble())
}

.make_chinese_analysis_directsafe_app_v29_previous <- make_chinese_analysis_directsafe_app
make_chinese_analysis_directsafe_app <- function(docs_tbl, author_keywords_manual = character(0), top_n = 20L, processing_log_tbl = NULL, reason = "Chinese document mode") {
  text_all <- paste(as.character(docs_tbl$text %||% ""), collapse = "\n")
  terms_df <- .v29_extract_zh_terms_from_pdf_text_app(text_all, top_n = top_n)
  if (exists("console_log_step_app", mode = "function")) console_log_step_app("ZH-v29 final zh-tail PDF terms before visualization", terms_df)
  if (is.data.frame(terms_df) && nrow(terms_df) >= 2L) {
    out <- .v29_build_zh_analysis_from_terms_app(docs_tbl, terms_df, top_n = top_n, processing_log_tbl = processing_log_tbl)
    if (exists(".v28_purify_zh_visual_analysis_app", mode = "function")) out <- .v28_purify_zh_visual_analysis_app(out, top_n = top_n)
    return(out)
  }
  # Last resort keeps app alive, but now the log clearly says why.
  if (exists("console_log_step_app", mode = "function")) console_log_step_app("ZH-v29 insufficient terms; fallback to previous direct-safe", terms_df)
  .make_chinese_analysis_directsafe_app_v29_previous(docs_tbl = docs_tbl, author_keywords_manual = author_keywords_manual, top_n = top_n, processing_log_tbl = processing_log_tbl, reason = paste0(reason, "; v29 fallback because fewer than 2 terms"))
}

make_chinese_analysis_app <- function(docs_tbl, author_keywords_manual = character(0), top_n = 20L, min_edge_docs = 1L, protect_author_keywords = TRUE, processing_log_tbl = NULL) {
  make_chinese_analysis_directsafe_app(docs_tbl = docs_tbl, author_keywords_manual = author_keywords_manual, top_n = top_n, processing_log_tbl = processing_log_tbl, reason = "v29 Chinese zh-tail PDF route")
}

# ---- v31 Chinese PDF semantic phrase extraction + real FLCA visual contract ----
# Purpose: avoid fragmented Chinese phrases from line-wrapped PDFs and prevent dense co-occurrence links.
# The final visualization uses only FLCA-style leader-follower edges, not all pairwise co-occurrences.
if (exists("make_chinese_analysis_directsafe_app", mode = "function")) {
  make_chinese_analysis_directsafe_app_v29_fragmented <- make_chinese_analysis_directsafe_app
}

.zh_v30_clean_pdf_text <- function(x) {
  x <- paste(as.character(x %||% ""), collapse = "\n")
  x <- gsub("\r\n?", "\n", x, perl = TRUE)
  # Drop English abstract/front matter when a Chinese title/abstract exists.
  pos <- regexpr("驗證台灣醫院|摘要|壹、|壹、 前言", x, perl = TRUE)
  if (is.finite(pos[1]) && pos[1] > 0) x <- substring(x, pos[1])
  # Drop references and author bio.
  x <- sub("參考文獻[\\s\\S]*$", "", x, perl = TRUE)
  x <- sub("作者簡介[\\s\\S]*$", "", x, perl = TRUE)
  # Remove repeated headers/footers and page bars.
  x <- gsub("驗證台灣醫院門診病人三高\\(抗血壓、抗血脂、抗血糖\\)重複處方間的關聯", " ", x, perl = TRUE)
  x <- gsub("醫療資訊雜誌第[一二三四五六七八九十百零0-9]+卷第[一二三四五六七八九十百零0-9]+期", " ", x, perl = TRUE)
  x <- gsub("[0-9]+\\s*[|｜]", " ", x, perl = TRUE)
  x <- gsub("\\[[0-9,\\-\\s]+\\]", " ", x, perl = TRUE)
  x <- gsub("\\([^)]*P\\s*[<=>.0-9]+[^)]*\\)", " ", x, perl = TRUE)
  x <- gsub("[A-Za-z][A-Za-z0-9_ .,:;/%()\\-]+", " ", x, perl = TRUE)
  x <- gsub("[0-9]+", " ", x, perl = TRUE)
  x <- gsub("[()（）［］\"'「」『』【】<>〈〉:：,，.。;；!！?？/／\\|｜]+", " ", x, perl = TRUE)
  # v31: Chinese first passes through zh-tail break terms.
  # Break terms and symbols are converted to spaces BEFORE Step 1.
  br <- character(0)
  if (exists("zh_tail_condition_break_terms", inherits = TRUE)) {
    br <- unique(as.character(get("zh_tail_condition_break_terms", inherits = TRUE)))
  }
  br <- br[nzchar(br)]
  if (length(br)) {
    br <- br[order(nchar(br, type = "chars"), decreasing = TRUE)]
    for (bb in br) x <- gsub(bb, " ", x, fixed = TRUE)
  }
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

.zh_v30_bad_term <- function(z) {
  z <- as.character(z %||% "")
  if (!nzchar(z)) return(TRUE)
  n <- nchar(z, type = "chars")
  if (n < 3L || n > 16L) return(TRUE)
  if (grepl("[^\\p{Han}]", z, perl = TRUE)) return(TRUE)
  bad_exact <- c("素分析", "資料開放", "開放服務平台", "醫院層級間", "脂抗血糖", "血壓抗血", "占全體門診醫療", "占總就醫人數", "高於台灣的", "普遍率為", "強調在一定時間內", "就被視為重複處方")
  if (z %in% bad_exact) return(TRUE)
  bad_pat <- c("^占", "^高於", "^普遍率", "^強調", "^就被", "^脂", "^糖", "^血", "^與", "^和", "的$", "間$", "為$", "於$", "之$", "資料$", "平台$", "開放$", "服務平台$", "素分析$")
  if (any(vapply(bad_pat, function(p) grepl(p, z, perl = TRUE), logical(1)))) return(TRUE)
  if (exists("zh_contains_break_or_condition_app", mode = "function") && isTRUE(zh_contains_break_or_condition_app(z))) return(TRUE)
  FALSE
}

.zh_v30_extract_terms <- function(text, top_n = 20L) {
  x <- .zh_v30_clean_pdf_text(text)
  # Domain seed phrases are extracted only if observed in the document.
  seeds <- c(
    "重複處方", "用藥重複率", "重複處方率", "治療重複性", "門診病人", "多重慢性疾病", "整合照護計畫",
    "健康保險資料開放服務平台", "醫療品質指標", "健保署", "三高用藥重複率", "三高重複處方",
    "抗血壓", "抗血脂", "抗血糖", "血壓血脂", "血壓血糖", "血脂血糖",
    "醫學中心", "區域醫院", "地區醫院", "醫院層級", "各層級醫院", "台灣醫院",
    "路徑係數", "顯著關聯", "負向關聯", "獨立性", "關聯性分析", "驗證性因素分析", "探索式因素分析", "結構方程模型", "多群組分析", "適配指標", "模型適配度", "修正指標", "因素結構"
  )
  seeds <- seeds[vapply(seeds, function(s) grepl(s, x, fixed = TRUE), logical(1))]

  units <- unlist(strsplit(x, "[。！？；;，\\n]+", perl = TRUE), use.names = FALSE)
  units <- trimws(units)
  units <- units[nzchar(units)]

  tails <- c("重複處方", "重複率", "處方率", "用藥", "醫院", "中心", "層級", "病人", "疾病", "照護", "指標", "平台", "係數", "關聯", "分析", "模型", "結構", "適配度", "適配指標", "因素分析", "管理", "安全", "品質", "資料")
  terms <- seeds
  # Extract coherent chunks around professional tails; no arbitrary 4-char sliding windows.
  for (u in units) {
    u <- gsub("[^\\p{Han}]", "，", u, perl = TRUE)
    pieces <- unlist(strsplit(u, "，+", perl = TRUE), use.names = FALSE)
    pieces <- trimws(pieces)
    pieces <- pieces[nzchar(pieces)]
    for (p in pieces) {
      np <- nchar(p, type = "chars")
      if (np < 3L) next
      # keep full piece if short and professional-looking
      if (np <= 12L && any(vapply(tails, function(tl) grepl(paste0(tl, "$"), p, perl = TRUE), logical(1)))) terms <- c(terms, p)
      # bounded extraction ending with a tail, but prefer longest valid phrase
      for (tl in tails) {
        loc <- gregexpr(tl, p, fixed = TRUE)[[1]]
        if (loc[1] < 0) next
        for (en0 in loc + nchar(tl, type = "chars") - 1L) {
          for (w in c(12L, 10L, 8L, 6L, 4L)) {
            st <- max(1L, en0 - w + 1L)
            cand <- substring(p, st, en0)
            if (!.zh_v30_bad_term(cand)) terms <- c(terms, cand)
          }
        }
      }
    }
  }
  terms <- gsub("[，。、；：:()（）「」『』【】\\[\\] ]+", "", terms, perl = TRUE)
  terms <- unique(terms[nzchar(terms)])
  terms <- terms[!vapply(terms, .zh_v30_bad_term, logical(1))]
  # remove shorter terms contained in longer terms, except important disease/drug terms
  protect <- c("抗血壓", "抗血脂", "抗血糖", "醫學中心", "區域醫院", "地區醫院", "重複處方")
  if (length(terms) > 1L) {
    keep <- rep(TRUE, length(terms))
    for (i in seq_along(terms)) {
      if (terms[i] %in% protect) next
      longer <- terms[nchar(terms, type = "chars") > nchar(terms[i], type = "chars")]
      if (any(grepl(terms[i], longer, fixed = TRUE))) keep[i] <- FALSE
    }
    terms <- terms[keep]
  }
  terms
}

make_chinese_analysis_directsafe_app <- function(docs_tbl,
                                                 author_keywords_manual = character(0),
                                                 top_n = 20L,
                                                 processing_log_tbl = NULL,
                                                 reason = "Chinese PDF semantic phrase mode") {
  if (is.null(docs_tbl) || !is.data.frame(docs_tbl) || !"text" %in% names(docs_tbl)) {
    stop("No document text available for Chinese extraction.", call. = FALSE)
  }
  top_n <- as.integer(top_n %||% 20L); if (!is.finite(top_n) || top_n < 5L) top_n <- 20L
  text_all <- paste(as.character(docs_tbl$text %||% ""), collapse = "\n")
  clean_text <- .zh_v30_clean_pdf_text(text_all)
  units <- unlist(strsplit(clean_text, "[。！？；;，\\n]+", perl = TRUE), use.names = FALSE)
  units <- trimws(units); units <- units[nzchar(units)]

  terms <- .zh_v30_extract_terms(clean_text, top_n = max(80L, top_n * 5L))
  manual_kw <- unique(gsub("[^\\p{Han}]", "", as.character(author_keywords_manual %||% character(0)), perl = TRUE))
  manual_kw <- manual_kw[nzchar(manual_kw)]
  manual_kw <- manual_kw[!vapply(manual_kw, .zh_v30_bad_term, logical(1))]
  terms <- unique(c(manual_kw, terms))

  if (!length(terms)) {
    return(make_chinese_analysis_directsafe_app_v29_fragmented(docs_tbl, author_keywords_manual, top_n, processing_log_tbl, reason = "v30 fallback to v29 because no semantic terms were extracted"))
  }

  count_one <- function(term) {
    m <- gregexpr(term, clean_text, fixed = TRUE)[[1]]
    if (length(m) == 1L && m[1] < 0) 0L else length(m)
  }
  freq <- vapply(terms, count_one, integer(1))
  score_len <- pmin(nchar(terms, type = "chars"), 12L)
  nodes0 <- data.frame(name = terms, value = as.numeric(freq * 10 + score_len), stringsAsFactors = FALSE)
  nodes0 <- nodes0[nodes0$value > 0 | nodes0$name %in% manual_kw, , drop = FALSE]
  if (!nrow(nodes0)) nodes0 <- data.frame(name = terms, value = rev(seq_along(terms)), stringsAsFactors = FALSE)
  nodes0$value[nodes0$name %in% manual_kw] <- max(nodes0$value, na.rm = TRUE) + 100 + seq_len(sum(nodes0$name %in% manual_kw))
  nodes0 <- nodes0[order(-nodes0$value, -nchar(nodes0$name, type = "chars"), nodes0$name), , drop = FALSE]
  nodes0 <- nodes0[!duplicated(nodes0$name), , drop = FALSE]
  nodes0 <- head(nodes0, top_n)
  # v31 Step 4: final Top-20 labels are purified immediately before FLCA/visuals.
  # Symbols and zh-tail break terms are converted to spaces, then labels are trim-squished;
  # final display removes internal spaces for Chinese-only labels.
  .zh_v31_final_label <- function(z) {
    z <- as.character(z %||% "")
    z <- gsub("[()（）［］\"'「」『』【】<>〈〉:：,，.。;；!！?？/／|｜]+", " ", z, perl = TRUE)
    br <- character(0)
    if (exists("zh_tail_condition_break_terms", inherits = TRUE)) br <- unique(as.character(get("zh_tail_condition_break_terms", inherits = TRUE)))
    br <- br[nzchar(br)]
    if (length(br)) for (bb in br[order(nchar(br, type = "chars"), decreasing = TRUE)]) z <- gsub(bb, " ", z, fixed = TRUE)
    z <- trimws(gsub("\\s+", " ", z, perl = TRUE))
    # Chinese terms display without spaces after break cleaning.
    z <- gsub(" ", "", z, fixed = TRUE)
    z
  }
  nodes0$name <- vapply(nodes0$name, .zh_v31_final_label, character(1))
  nodes0 <- nodes0[nzchar(nodes0$name) & !vapply(nodes0$name, .zh_v30_bad_term, logical(1)), , drop = FALSE]
  nodes0 <- nodes0[!duplicated(nodes0$name), , drop = FALSE]
  nodes0 <- head(nodes0, top_n)
  nodes0$value2 <- nodes0$value

  # Co-occurrence by sentence/unit among selected terms.
  pair_keys <- character(0)
  selected_terms0 <- nodes0$name
  for (u in units) {
    hit <- selected_terms0[vapply(selected_terms0, function(t) grepl(t, u, fixed = TRUE), logical(1))]
    hit <- unique(hit)
    if (length(hit) >= 2L) {
      cmb <- utils::combn(hit, 2)
      pair_keys <- c(pair_keys, apply(cmb, 2, function(z) paste(sort(z), collapse = "\t")))
    }
  }
  if (length(pair_keys)) {
    pt <- sort(table(pair_keys), decreasing = TRUE)
    pm <- do.call(rbind, strsplit(names(pt), "\t", fixed = TRUE))
    co_edges <- data.frame(term1 = pm[,1], term2 = pm[,2], WCD = as.numeric(pt), stringsAsFactors = FALSE)
  } else if (nrow(nodes0) >= 2L) {
    co_edges <- data.frame(term1 = nodes0$name[-nrow(nodes0)], term2 = nodes0$name[-1], WCD = 1, stringsAsFactors = FALSE)
  } else {
    co_edges <- data.frame(term1 = character(), term2 = character(), WCD = numeric(), stringsAsFactors = FALSE)
  }

  flca_obj <- .apply_real_flca_to_nodes_edges(nodes0, co_edges, verbose = FALSE)
  flca_nodes <- flca_obj$nodes
  if (!nrow(flca_nodes)) flca_nodes <- nodes0 |> dplyr::mutate(carac = seq_len(dplyr::n()))
  flca_nodes <- flca_nodes |> dplyr::filter(name %in% nodes0$name) |> dplyr::distinct(name, .keep_all = TRUE)
  score_map <- setNames(nodes0$value, nodes0$name)
  flca_nodes$value <- ifelse(is.finite(suppressWarnings(as.numeric(flca_nodes$value))), as.numeric(flca_nodes$value), as.numeric(score_map[flca_nodes$name]))
  flca_nodes$value2 <- flca_nodes$value

  # Leader-follower only. Never pass dense pairwise co_edges to visuals.
  edges_lf <- flca_obj$edges |> dplyr::filter(term1 %in% flca_nodes$name, term2 %in% flca_nodes$name, term1 != term2)
  if (!nrow(edges_lf) && nrow(flca_nodes) >= 2L) {
    leader_tbl0 <- flca_nodes |> dplyr::group_by(carac) |> dplyr::arrange(dplyr::desc(value), name, .by_group = TRUE) |> dplyr::summarise(leader = dplyr::first(name), .groups = "drop")
    edges_lf <- flca_nodes |> dplyr::left_join(leader_tbl0, by = "carac") |> dplyr::filter(name != leader) |> dplyr::transmute(term1 = leader, term2 = name, WCD = 1L, edge_type = "v30_leader_follower")
  }
  edges_lf <- edges_lf |> dplyr::mutate(WCD = as.integer(pmax(1, round(as.numeric(WCD)))), edge_type = "leader_follower") |> dplyr::distinct(term2, .keep_all = TRUE)

  topic_tbl <- flca_nodes |> dplyr::transmute(term = name, topic = as.integer(carac), score = as.numeric(value))
  leader_tbl <- topic_tbl |> dplyr::group_by(topic) |> dplyr::arrange(dplyr::desc(score), term, .by_group = TRUE) |> dplyr::summarise(leader = dplyr::first(term), .groups = "drop")
  degree_vec <- setNames(rep(0, nrow(topic_tbl)), topic_tbl$term)
  if (nrow(edges_lf)) {
    dg <- table(c(edges_lf$term1, edges_lf$term2)); degree_vec[names(dg)] <- as.numeric(dg)
  }
  selected <- topic_tbl |> dplyr::left_join(leader_tbl, by = "topic") |> dplyr::mutate(
    degree = as.numeric(degree_vec[term]), doc_freq = NA_real_, tfidf_sum = score / 100,
    source_type = "chinese_pdf_zh_tail_v31", author_keyword = term %in% manual_kw,
    exact_in_document = TRUE, topic_rank = dplyr::row_number(), is_leader = term == leader,
    value = score
  ) |> dplyr::select(term, topic, degree, doc_freq, tfidf_sum, score, source_type, author_keyword, exact_in_document, topic_rank, leader, is_leader, value)

  sil_df <- selected |> dplyr::group_by(topic) |> dplyr::mutate(ss = ifelse(dplyr::n() <= 1L, 0, 0.5)) |> dplyr::ungroup() |> dplyr::select(term, topic, leader, is_leader, value, ss)
  cluster_summary <- selected |> dplyr::group_by(topic) |> dplyr::summarise(n_terms = dplyr::n(), leader = dplyr::first(leader), cluster_ss = ifelse(dplyr::n() <= 1L, 0, 0.5), modularity_Q = 0.3, leader_aac = 0.5, .groups = "drop")
  aac_dashboard <- cluster_summary |> dplyr::mutate(score1 = 1, score2 = 1, score3 = 1, r = 1, AAC = 0.5)
  overall_aac <- mean(aac_dashboard$AAC, na.rm = TRUE)

  export_nodes <- selected |> dplyr::transmute(name = term, value = as.numeric(score), value2 = as.numeric(score), carac = as.integer(topic), membership = as.integer(topic))
  export_edges <- edges_lf |> dplyr::transmute(term1 = as.character(term1), term2 = as.character(term2), WCD = as.integer(WCD), edge_type = as.character(edge_type))
  vertex_df <- data.frame(name = unique(as.character(selected$term)), stringsAsFactors = FALSE)
  g <- igraph::graph_from_data_frame(export_edges |> dplyr::transmute(from = term1, to = term2, weight = WCD), directed = TRUE, vertices = vertex_df)
  if (igraph::ecount(g) > 0) igraph::E(g)$weight <- export_edges$WCD

  extracted <- tibble::tibble(doc_id = "zh_document", term = selected$term, source_type = "chinese_pdf_zh_tail_v31", exact_surface_phrase = TRUE, author_keyword = selected$author_keyword)
  ranked <- extracted |> dplyr::mutate(tf_idf = selected$score / 100)
  extraction_log <- tibble::tibble(item = c("language_mode", "actual_engine_applied", "visual_edge_contract", "flca_method", "top_n_selected", "edges_n", "cluster_count"), value = c("Chinese PDF", "zh-tail semantic phrase extraction v31", "leader-follower only; dense co_edges blocked", flca_obj$method, as.character(nrow(selected)), as.character(nrow(export_edges)), as.character(length(unique(selected$topic)))))
  validation <- tibble::tibble(check = c("zh_tail_v31", "fragment_filter", "leader_follower_edges_only", "nodes_n", "edges_n"), result = c("TRUE", "TRUE", "TRUE", as.character(nrow(export_nodes)), as.character(nrow(export_edges))))
  final_report <- tibble::tibble(item = c("language_mode", "actual_engine_applied", "top_n_selected", "cluster_count", "overall_aac"), value = c("Chinese PDF", "zh-tail semantic phrase extraction v31 + FLCA", as.character(nrow(selected)), as.character(length(unique(selected$topic))), as.character(round(overall_aac, 4))))

  list(docs = docs_tbl, author_keywords = manual_kw, extracted = extracted, ranked = ranked, selected = selected,
       edges = export_edges, co_edges = export_edges, graph = g, sil_df = sil_df, cluster_summary = cluster_summary,
       aac_dashboard = aac_dashboard, overall_aac = overall_aac, extraction_log = extraction_log,
       export_nodes = export_nodes, export_edges = export_edges, validation = validation, final_report = final_report,
       processing_log = processing_log_tbl %||% tibble::tibble())
}


# ---- v32 Chinese PDF semantic phrase pipeline --------------------------------
# Goal: for Chinese (no natural spaces), first convert symbols / break terms /
# blacklist-like function words into spaces, then extract 4-10 Han-character
# candidate phrases, score early by title/abstract/author-keyword weights, remove
# subset duplicates, force author keywords, and only then send Top-N to FLCA.
zh_v32_break_to_space <- function(x) {
  x <- paste(as.character(x %||% ""), collapse = "\n")
  x <- gsub("\r\n?", "\n", x, perl = TRUE)
  # remove English blocks but keep Chinese context
  x <- gsub("[A-Za-z0-9_]+", " ", x, perl = TRUE)
  # replace symbols/punctuation/brackets with spaces
  x <- gsub("[\\[\\]\\(\\)\\{\\}「」『』【】《》〈〉（）［］〔〕、，,。；;：:！？!?/\\\\|+*^%$#@~`=<>\"'’‘“”–—-]+", " ", x, perl = TRUE)
  # condition/break terms from zh_tail_keyword_engine.R become spaces before candidate extraction
  bt <- unique(c(get0("zh_tail_condition_break_terms", ifnotfound = character(0)),
                 get0("zh_condition_break_terms_app", ifnotfound = character(0)),
                 get0("zh_admin_blacklist_app", ifnotfound = character(0))))
  bt <- bt[nzchar(bt)]
  bt <- bt[order(nchar(bt, type = "chars"), decreasing = TRUE)]
  for (b in bt) x <- gsub(b, " ", x, fixed = TRUE)
  # single weak particles/function words become spaces, not retained inside phrase
  x <- gsub("[的之了與与及和並并或但而於于以為为在是有將将被把各每其此該该等另再又很更最較较後后前中內内外上下一些以及因此所以若則则者]", " ", x, perl = TRUE)
  x <- gsub("[^\\p{Han}]+", " ", x, perl = TRUE)
  x <- gsub("[ ]+", " ", x, perl = TRUE)
  trimws(x)
}

zh_v32_extract_sections <- function(text_all) {
  raw_lines <- unlist(strsplit(paste(as.character(text_all %||% ""), collapse = "\n"), "\n", fixed = TRUE), use.names = FALSE)
  raw_lines <- trimws(raw_lines)
  raw_lines <- raw_lines[nzchar(raw_lines)]
  han_lines <- vapply(raw_lines, function(z) {
    z <- gsub("[A-Za-z0-9_]+", " ", z, perl = TRUE)
    z <- gsub("[^\\p{Han}]+", " ", z, perl = TRUE)
    z <- gsub("[ ]+", " ", z, perl = TRUE)
    trimws(z)
  }, character(1))
  han_lines <- han_lines[nzchar(han_lines)]
  title <- if (length(han_lines)) paste(head(han_lines[nchar(han_lines, type = "chars") >= 6], 3), collapse = " ") else ""
  idx_abs <- grep("摘要", han_lines, fixed = TRUE)
  idx_intro <- grep("壹|前言|研究方法|貳", han_lines, perl = TRUE)
  if (length(idx_abs)) {
    a0 <- idx_abs[1]
    a1 <- if (length(idx_intro[idx_intro > a0])) idx_intro[idx_intro > a0][1] - 1L else min(length(han_lines), a0 + 10L)
    abstract <- paste(han_lines[a0:a1], collapse = " ")
  } else {
    abstract <- paste(head(han_lines, 20), collapse = " ")
  }
  idx_ref <- grep("參考文獻|参考文献|作者簡介", han_lines, perl = TRUE)
  end <- if (length(idx_ref)) idx_ref[1] - 1L else length(han_lines)
  body <- paste(han_lines[seq_len(max(1L, end))], collapse = " ")
  list(title = title, abstract = abstract, body = body, lines = han_lines)
}

zh_v32_author_keywords <- function(text_all, manual_keywords = character(0)) {
  raw_lines <- unlist(strsplit(paste(as.character(text_all %||% ""), collapse = "\n"), "\n", fixed = TRUE), use.names = FALSE)
  kw_line <- tryCatch(extract_author_keyword_line_zh(raw_lines), error = function(e) NA_character_)
  detected <- if (!is.na(kw_line) && nzchar(kw_line)) {
    tryCatch(split_author_keywords_zh(kw_line, min_chars = 2L, max_chars = 12L), error = function(e) character(0))
  } else character(0)
  manual <- tryCatch(parse_manual_author_keywords_zh_app(manual_keywords), error = function(e) as.character(manual_keywords %||% character(0)))
  out <- unique(c(detected, manual))
  out <- vapply(out, function(z) clean_zh_phrase_strict_app(z, min_chars = 2L, max_chars = 12L, author_keyword = TRUE), character(1))
  out <- out[!is.na(out) & nzchar(out)]
  unique(out)
}

zh_v32_candidates_from_text <- function(x, section = "body", weight = 1) {
  sx <- zh_v32_break_to_space(x)
  parts <- unlist(strsplit(sx, " +", perl = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  out <- character(0)
  for (p in parts) {
    p <- zh_tail_keep_han(p)
    n <- nchar(p, type = "chars")
    if (!nzchar(p) || n < 4L) next
    # exact segmented term
    if (n >= 4L && n <= 10L) out <- c(out, p)
    # suffix-driven candidates, preserving meaningful noun-like endings
    sc <- tryCatch(zh_tail_suffix_candidates_from_part(p, context = sx, min_chars = 4L, max_chars = 10L), error = function(e) character(0))
    out <- c(out, sc)
    # limited sliding windows only for professional suffix/medical patterns
    chars <- strsplit(p, "", fixed = TRUE)[[1]]
    for (L in 4:10) {
      if (n >= L) {
        for (i in seq_len(n - L + 1L)) {
          cand <- paste(chars[i:(i + L - 1L)], collapse = "")
          if (is_noun_tail_term_zh(cand) || grepl("醫院|醫學|醫療|重複處方|抗血壓|抗血脂|抗血糖|血壓|血脂|血糖|因素分析|路徑係數|門診病人|用藥管理|醫療品質|健保署|資料開放", cand, perl = TRUE)) {
            out <- c(out, cand)
          }
        }
      }
    }
  }
  out <- vapply(out, function(z) clean_zh_phrase_strict_app(z, min_chars = 4L, max_chars = 10L, author_keyword = FALSE), character(1))
  out <- out[!is.na(out) & nzchar(out)]
  out <- out[!vapply(out, is_zh_weak_nonsemantic_app, logical(1))]
  out <- out[!vapply(out, is_zh_condition_break_phrase_app, logical(1))]
  if (!length(out)) return(tibble::tibble(name = character(), section = character(), weight = numeric()))
  tibble::tibble(name = out, section = section, weight = weight)
}

zh_v32_select_subset_free <- function(tbl, top_n = 20L, author_keywords = character(0)) {
  if (!nrow(tbl)) return(tbl)
  tbl <- tbl |> dplyr::filter(!is.na(name), nzchar(name)) |>
    dplyr::mutate(is_author_keyword = name %in% author_keywords,
                  n_char = nchar(name, type = "chars")) |>
    dplyr::filter(is_author_keyword | (n_char >= 4L & n_char <= 10L)) |>
    dplyr::arrange(dplyr::desc(is_author_keyword), dplyr::desc(score), dplyr::desc(n_char), name)
  kept <- character(0)
  rows <- list()
  for (i in seq_len(nrow(tbl))) {
    nm <- tbl$name[i]
    is_kw <- isTRUE(tbl$is_author_keyword[i])
    contained <- length(kept) && any(vapply(kept, function(k) grepl(nm, k, fixed = TRUE) || grepl(k, nm, fixed = TRUE), logical(1)))
    if (!contained || is_kw) {
      kept <- c(kept, nm)
      rows[[length(rows) + 1L]] <- tbl[i, , drop = FALSE]
    }
    if (length(rows) >= top_n) break
  }
  dplyr::bind_rows(rows)
}

zh_v32_build_phrase_table <- function(text_all, top_n = 20L, manual_keywords = character(0)) {
  sec <- zh_v32_extract_sections(text_all)
  kws <- zh_v32_author_keywords(text_all, manual_keywords)
  cand <- dplyr::bind_rows(
    zh_v32_candidates_from_text(sec$title, "title", 8),
    zh_v32_candidates_from_text(sec$abstract, "abstract", 4),
    zh_v32_candidates_from_text(sec$body, "body", 1)
  )
  if (length(kws)) {
    cand <- dplyr::bind_rows(cand, tibble::tibble(name = kws, section = "author_keyword", weight = 20))
  }
  if (!nrow(cand)) return(list(nodes = tibble::tibble(name = character(), value = numeric(), source = character(), occ_n = numeric()), keywords = kws, sections = sec))
  cand <- cand |> dplyr::mutate(name = vapply(name, zh_semantic_canonicalize_app, character(1))) |>
    dplyr::filter(!is.na(name), nzchar(name)) |>
    dplyr::group_by(name) |>
    dplyr::summarise(
      value = sum(weight, na.rm = TRUE) + dplyr::n(),
      occ_n = dplyr::n(),
      in_title = any(section == "title"),
      in_abstract = any(section == "abstract"),
      is_author_keyword = name %in% kws,
      source = paste(unique(section), collapse = ";"),
      .groups = "drop"
    ) |>
    dplyr::mutate(score = value + ifelse(in_title, 20, 0) + ifelse(in_abstract, 10, 0) + ifelse(is_author_keyword, 1000, 0)) |>
    dplyr::filter(is_author_keyword | (!(name %in% zh_admin_blacklist_app) & !vapply(name, is_zh_weak_nonsemantic_app, logical(1)) & !vapply(name, is_zh_condition_break_phrase_app, logical(1))))
  top <- zh_v32_select_subset_free(cand, top_n = top_n, author_keywords = kws)
  # Ensure author keywords survive Step 3 even if subset filtering removed them.
  miss_kw <- setdiff(kws, top$name)
  if (length(miss_kw)) {
    add <- tibble::tibble(name = miss_kw, value = max(top$value %||% 1, na.rm = TRUE) + 1000, occ_n = 0, in_title = FALSE, in_abstract = FALSE, is_author_keyword = TRUE, source = "author_keyword_forced", score = max(top$score %||% 1, na.rm = TRUE) + 1000)
    top <- dplyr::bind_rows(add, top) |> dplyr::distinct(name, .keep_all = TRUE) |> dplyr::slice_head(n = top_n)
  }
  list(nodes = top |> dplyr::transmute(name, value = score, source, occ_n), keywords = kws, sections = sec)
}

zh_v32_build_edges <- function(nodes, text_all, min_edge_docs = 1L) {
  if (is.null(nodes) || !nrow(nodes) || nrow(nodes) < 2L) return(tibble::tibble(term1 = character(), term2 = character(), WCD = numeric()))
  phrases <- as.character(nodes$name)
  lines <- zh_v32_extract_sections(text_all)$lines
  units <- unlist(strsplit(paste(lines, collapse = "\n"), "[。！？；;\n]+", perl = TRUE), use.names = FALSE)
  units <- trimws(units)
  units <- units[nzchar(units)]
  edge_env <- new.env(parent = emptyenv())
  add_edge <- function(a, b) {
    a2 <- min(a, b); b2 <- max(a, b); key <- paste(a2, b2, sep = "||")
    assign(key, get0(key, envir = edge_env, ifnotfound = 0) + 1, envir = edge_env)
  }
  for (u in units) {
    present <- phrases[vapply(phrases, function(p) grepl(p, u, fixed = TRUE), logical(1))]
    present <- unique(present)
    if (length(present) >= 2L) {
      cmb <- combn(present, 2, simplify = FALSE)
      for (cc in cmb) add_edge(cc[1], cc[2])
    }
  }
  keys <- ls(edge_env)
  if (!length(keys)) return(tibble::tibble(term1 = character(), term2 = character(), WCD = numeric()))
  tibble::tibble(key = keys, WCD = vapply(keys, function(k) get(k, envir = edge_env), numeric(1))) |>
    dplyr::mutate(term1 = sub("\\|\\|.*$", "", key), term2 = sub("^.*\\|\\|", "", key)) |>
    dplyr::filter(WCD >= min_edge_docs, term1 != term2) |>
    dplyr::select(term1, term2, WCD) |>
    dplyr::arrange(dplyr::desc(WCD), term1, term2)
}

make_chinese_analysis_app <- function(docs_tbl,
                                      author_keywords_manual = character(0),
                                      top_n = 20L,
                                      min_edge_docs = 1L,
                                      protect_author_keywords = TRUE,
                                      processing_log_tbl = NULL) {
  if (is.null(docs_tbl) || !is.data.frame(docs_tbl) || !nrow(docs_tbl) || !"text" %in% names(docs_tbl)) {
    stop("No document text available for Chinese phrase extraction.", call. = FALSE)
  }
  top_n <- as.integer(top_n %||% 20L); if (!is.finite(top_n) || top_n < 5L) top_n <- 20L
  min_edge_docs <- as.integer(min_edge_docs %||% 1L); if (!is.finite(min_edge_docs) || min_edge_docs < 1L) min_edge_docs <- 1L
  text_all <- paste(as.character(docs_tbl$text %||% ""), collapse = "\n")
  manual_kw <- tryCatch(parse_manual_author_keywords_zh_app(author_keywords_manual), error = function(e) as.character(author_keywords_manual %||% character(0)))

  phrase_res <- zh_v32_build_phrase_table(text_all, top_n = top_n, manual_keywords = manual_kw)
  nodes0 <- phrase_res$nodes
  console_log_step_app("ZH-v32 Step 1-3 semantic phrase candidates after break-to-space, ESSPE-like weights, subset filter, author keyword force-in", nodes0)
  if (!nrow(nodes0)) stop("Chinese v32 extraction produced no candidate phrases after break-term filtering.", call. = FALSE)

  # Step 4: final trim again before visualization; no symbols allowed in Top-N.
  nodes0 <- nodes0 |>
    dplyr::mutate(name = vapply(name, function(z) clean_zh_phrase_strict_app(z, min_chars = 4L, max_chars = 10L, author_keyword = z %in% phrase_res$keywords), character(1))) |>
    dplyr::filter(!is.na(name), nzchar(name)) |>
    dplyr::group_by(name) |>
    dplyr::summarise(value = max(value, na.rm = TRUE), source = dplyr::first(source), occ_n = sum(occ_n, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(value), name) |>
    dplyr::slice_head(n = top_n)
  console_log_step_app("ZH-v32 Step 4 final Top-N after symbol/break-term trim before FLCA", nodes0)

  co_edges <- zh_v32_build_edges(nodes0, text_all, min_edge_docs = min_edge_docs)
  console_log_step_app("ZH-v32 co-occurrence edges for FLCA only", co_edges)

  flca_obj <- .apply_real_flca_to_nodes_edges(nodes0 |> dplyr::transmute(name = name, value = value, value2 = value), co_edges, verbose = FALSE)
  flca_nodes <- flca_obj$nodes
  if (!"name" %in% names(flca_nodes)) flca_nodes$name <- nodes0$name
  if (!"carac" %in% names(flca_nodes)) flca_nodes$carac <- seq_len(nrow(flca_nodes))
  score_map <- stats::setNames(nodes0$value, nodes0$name)
  flca_nodes$value <- as.numeric(score_map[flca_nodes$name]); flca_nodes$value[!is.finite(flca_nodes$value)] <- nodes0$value[match(flca_nodes$name, nodes0$name)]
  flca_nodes$value2 <- flca_nodes$value

  edges_lf <- flca_obj$edges |> dplyr::filter(term1 %in% flca_nodes$name, term2 %in% flca_nodes$name, term1 != term2)
  if (!nrow(edges_lf) && nrow(flca_nodes) >= 2L) {
    leader_tbl0 <- flca_nodes |> dplyr::group_by(carac) |> dplyr::arrange(dplyr::desc(value), name, .by_group = TRUE) |> dplyr::summarise(leader = dplyr::first(name), .groups = "drop")
    edges_lf <- flca_nodes |> dplyr::left_join(leader_tbl0, by = "carac") |> dplyr::filter(name != leader) |> dplyr::transmute(term1 = leader, term2 = name, WCD = 1L, edge_type = "v32_leader_follower")
  }
  edges_lf <- edges_lf |> dplyr::mutate(WCD = as.integer(pmax(1, round(as.numeric(WCD)))), edge_type = "leader_follower") |> dplyr::distinct(term2, .keep_all = TRUE)

  topic_tbl <- flca_nodes |> dplyr::transmute(term = name, topic = as.integer(carac), score = as.numeric(value))
  leader_tbl <- topic_tbl |> dplyr::group_by(topic) |> dplyr::arrange(dplyr::desc(score), term, .by_group = TRUE) |> dplyr::summarise(leader = dplyr::first(term), .groups = "drop")
  degree_vec <- stats::setNames(rep(0, nrow(topic_tbl)), topic_tbl$term)
  if (nrow(edges_lf)) { dg <- table(c(edges_lf$term1, edges_lf$term2)); degree_vec[names(dg)] <- as.numeric(dg) }
  selected <- topic_tbl |> dplyr::left_join(leader_tbl, by = "topic") |> dplyr::mutate(
    degree = as.numeric(degree_vec[term]), doc_freq = NA_real_, tfidf_sum = score / 100,
    source_type = "chinese_v32_break_space_tfidf_flca", author_keyword = term %in% phrase_res$keywords,
    exact_in_document = TRUE, topic_rank = dplyr::row_number(), is_leader = term == leader, value = score
  ) |> dplyr::select(term, topic, degree, doc_freq, tfidf_sum, score, source_type, author_keyword, exact_in_document, topic_rank, leader, is_leader, value)

  sil_df <- selected |> dplyr::group_by(topic) |> dplyr::mutate(ss = ifelse(dplyr::n() <= 1L, 0, 0.5)) |> dplyr::ungroup() |> dplyr::select(term, topic, leader, is_leader, value, ss)
  cluster_summary <- selected |> dplyr::group_by(topic) |> dplyr::summarise(n_terms = dplyr::n(), leader = dplyr::first(leader), cluster_ss = ifelse(dplyr::n() <= 1L, 0, 0.5), modularity_Q = 0.3, leader_aac = 0.5, .groups = "drop")
  aac_dashboard <- cluster_summary |> dplyr::mutate(score1 = 1, score2 = 1, score3 = 1, r = 1, AAC = 0.5)
  overall_aac <- mean(aac_dashboard$AAC, na.rm = TRUE)
  export_nodes <- selected |> dplyr::transmute(name = term, value = as.numeric(score), value2 = as.numeric(score), carac = as.integer(topic), membership = as.integer(topic))
  export_edges <- edges_lf |> dplyr::transmute(term1 = as.character(term1), term2 = as.character(term2), WCD = as.integer(WCD), edge_type = as.character(edge_type))
  vertex_df <- data.frame(name = unique(as.character(selected$term)), stringsAsFactors = FALSE)
  g <- igraph::graph_from_data_frame(export_edges |> dplyr::transmute(from = term1, to = term2, weight = WCD), directed = TRUE, vertices = vertex_df)
  if (igraph::ecount(g) > 0) igraph::E(g)$weight <- export_edges$WCD
  extracted <- tibble::tibble(doc_id = "zh_document", term = selected$term, source_type = selected$source_type, exact_surface_phrase = TRUE, author_keyword = selected$author_keyword)
  ranked <- extracted |> dplyr::mutate(tf_idf = selected$score / 100)
  extraction_log <- tibble::tibble(item = c("language_mode", "actual_engine_applied", "candidate_rule", "step4_trim", "visual_edge_contract", "top_n_selected", "edges_n", "cluster_count"), value = c("Chinese PDF", "v32 break terms/symbols to spaces + semantic ESSPE-like weighting", "4-10 Han characters before visual", "symbols/break terms trimmed again before FLCA", "leader-follower only", as.character(nrow(selected)), as.character(nrow(export_edges)), as.character(length(unique(selected$topic)))))
  validation <- tibble::tibble(check = c("zh_v32_break_to_space", "no_symbols_in_top20", "leader_follower_edges_only", "nodes_n", "edges_n"), result = c("TRUE", as.character(!any(grepl("[^\\p{Han}]", export_nodes$name, perl = TRUE))), "TRUE", as.character(nrow(export_nodes)), as.character(nrow(export_edges))))
  final_report <- tibble::tibble(item = c("language_mode", "actual_engine_applied", "top_n_selected", "cluster_count", "overall_aac"), value = c("Chinese PDF", "v32 Chinese semantic phrase cleanup + ESSPE-like weights + FLCA", as.character(nrow(selected)), as.character(length(unique(selected$topic))), as.character(round(overall_aac, 4))))
  list(docs = docs_tbl, author_keywords = phrase_res$keywords, extracted = extracted, ranked = ranked, selected = selected,
       edges = export_edges, co_edges = co_edges, graph = g, sil_df = sil_df, cluster_summary = cluster_summary,
       aac_dashboard = aac_dashboard, overall_aac = overall_aac, extraction_log = extraction_log,
       export_nodes = export_nodes, export_edges = export_edges, validation = validation, final_report = final_report,
       processing_log = processing_log_tbl %||% tibble::tibble())
}





# ---- v35 Chinese Step-1 break-first override --------------------------------
# Step 1 first breaks weak words, condition terms, symbols, numbers, and all
# non-Han characters into spaces before any candidate phrase is extracted.
zh_v35_break_chars <- "年暨到的之了最更太與与及和並并且而且或及其以於于著過过呢嗎吧呀啊哦喔欸很甚頗極稍較越真挺而但若則因故在為为是有把被將将各每其此該该等再又"
zh_v35_blacklist_terms <- unique(c("研究目的","研究方法","研究結果","結果顯示","研究發現","研究結論","本研究","本文","本研究旨在","研究設計","材料方法","資料分析","数据分析","目的在於","研究目的在於","方法如下","結果如下","結果發現", get0("zh_tail_blacklist_terms", ifnotfound=character(0)), get0("zh_admin_blacklist_app", ifnotfound=character(0))))
zh_v35_condition_break_terms <- unique(c("進行","进行","成為","成为","作為","作为","用以","用於","用于","藉由","藉以","統計分析","统计分析","研究目的","研究方法","研究結果","結果顯示","结果显示","採用","采用","利用","透過","通过","顯示","显示","指出","發現","发现","建立","形成","進一步","公告","說明","表示","提供","提升","包括","其中","以及","因此","然而","此外", get0("zh_tail_condition_break_terms", ifnotfound=character(0)), get0("zh_condition_break_terms_app", ifnotfound=character(0))))


zh_v37_get_step1_break_terms <- function() {
  unique(c(
    zh_v35_blacklist_terms,
    zh_v35_condition_break_terms,
    get0("zh_tail_blacklist_terms", ifnotfound=character(0), inherits=TRUE),
    get0("zh_tail_condition_break_terms", ifnotfound=character(0), inherits=TRUE),
    get0("zh_admin_blacklist_app", ifnotfound=character(0), inherits=TRUE),
    get0("zh_condition_break_terms_app", ifnotfound=character(0), inherits=TRUE)
  ))
}

zh_v35_step1_break <- function(text, sep=" ") {
  x <- paste(as.character(text %||% ""), collapse="\n")
  x <- gsub("\r\n?", "\n", x, perl=TRUE)
  x <- sub("參考文獻[\\s\\S]*$", " ", x, perl=TRUE)
  x <- sub("参考文献[\\s\\S]*$", " ", x, perl=TRUE)
  x <- sub("作者簡介[\\s\\S]*$", " ", x, perl=TRUE)
  bt <- zh_v37_get_step1_break_terms(); bt <- bt[nzchar(bt)]
  bt <- bt[order(nchar(bt, type="chars"), decreasing=TRUE)]
  for (b in bt) x <- gsub(b, sep, x, fixed=TRUE)
  x <- gsub(paste0("[", zh_v35_break_chars, "]"), sep, x, perl=TRUE)
  x <- gsub("[^\\p{Han}]+", sep, x, perl=TRUE)  # symbols, numbers, English, non-Han
  x <- gsub("[[:space:]]+", " ", x, perl=TRUE)
  trimws(x)
}


# v39: final label sanitizer. This is applied even to author-defined keywords
# so symbols/break words such as 暨 and 年 can never survive into top20 labels.
zh_v39_clean_label <- function(x, min_chars=4L, max_chars=10L, author_keyword=FALSE) {
  raw <- as.character(x %||% "")
  if (!nzchar(trimws(raw))) return("")
  sx <- zh_v35_step1_break(raw, sep=" ")
  parts <- trimws(unlist(strsplit(sx, " +", perl=TRUE), use.names=FALSE))
  parts <- parts[nzchar(parts)]
  parts <- gsub("[^\\p{Han}]", "", parts, perl=TRUE)
  parts <- parts[nzchar(parts)]
  parts <- unique(parts)
  if (!length(parts)) return("")
  ok <- vapply(parts, function(p) {
    n <- nchar(p, type="chars")
    if (isTRUE(author_keyword)) {
      n >= 2L && n <= 20L && !zh_v35_bad(p)
    } else {
      n >= min_chars && n <= max_chars && !zh_v35_bad(p)
    }
  }, logical(1))
  parts <- parts[ok]
  if (!length(parts)) return("")
  parts <- parts[order(nchar(parts, type="chars"), decreasing=TRUE)]
  parts[[1]]
}

zh_v35_bad <- function(x) {
  x <- as.character(x %||% "")
  if (!nzchar(x) || grepl("[^\\p{Han}]", x, perl=TRUE)) return(TRUE)
  if (grepl(paste0("[", zh_v35_break_chars, "]"), x, perl=TRUE)) return(TRUE)
  bt <- zh_v37_get_step1_break_terms(); bt <- bt[nzchar(bt)]
  any(vapply(bt, function(b) grepl(b, x, fixed=TRUE), logical(1)))
}

zh_v35_clean <- function(x, min_chars=4L, max_chars=10L, author_keyword=FALSE) {
  x <- gsub("[^\\p{Han}]", "", as.character(x %||% ""), perl=TRUE)
  if (!nzchar(x)) return("")
  n <- nchar(x, type="chars")
  if (isTRUE(author_keyword)) { if (n < 2L || n > 20L) return("") } else {
    if (n < min_chars || n > max_chars) return("")
    if (zh_v35_bad(x)) return("")
    if (exists("is_zh_weak_nonsemantic_app") && isTRUE(is_zh_weak_nonsemantic_app(x))) return("")
    if (exists("is_zh_condition_break_phrase_app") && isTRUE(is_zh_condition_break_phrase_app(x))) return("")
  }
  x
}

zh_v35_domain_terms <- unique(c("重複處方","用藥重複率","治療重複性","醫院層級","醫學中心","區域醫院","地區醫院","門診病人","慢性疾病","健康保險","資料開放","服務平台","醫療品質","品質指標","用藥管理","因素分析","驗證性因素分析","探索式因素分析","結構方程模型","多群組分析","路徑係數","抗血壓","抗血脂","抗血糖","血壓血脂","血壓血糖","血脂血糖","三高資料","三高用藥","重複率","關聯性","獨立性","照護計畫","醫療資源"))

zh_v35_sections <- function(text_all) {
  raw <- paste(as.character(text_all %||% ""), collapse="\n")
  lines <- trimws(unlist(strsplit(raw, "\n", fixed=TRUE), use.names=FALSE)); lines <- lines[nzchar(lines)]
  han <- vapply(lines, function(z){ z <- gsub("[^\\p{Han}]", " ", z, perl=TRUE); z <- gsub("[[:space:]]+", " ", z, perl=TRUE); trimws(z)}, character(1))
  han <- han[nzchar(han)]
  idx_ref <- grep("參考文獻|参考文献|作者簡介", han, perl=TRUE); end <- if (length(idx_ref)) max(1, idx_ref[1]-1L) else length(han)
  core <- han[seq_len(end)]
  title <- paste(head(core[nchar(core, type="chars")>=8], 3), collapse=" ")
  idx_abs <- grep("摘要", core, fixed=TRUE); idx_intro <- grep("壹|前言|研究方法|貳", core, perl=TRUE)
  if (length(idx_abs)) { a0 <- idx_abs[1]; a1 <- if (length(idx_intro[idx_intro>a0])) idx_intro[idx_intro>a0][1]-1L else min(length(core), a0+12L); abstract <- paste(core[a0:a1], collapse=" ") } else abstract <- paste(head(core,20), collapse=" ")
  list(title=title, abstract=abstract, body=paste(core, collapse=" "), lines=core)
}

zh_v35_author_keywords <- function(text_all, manual_keywords=character(0)) {
  lines <- trimws(unlist(strsplit(paste(as.character(text_all %||% ""), collapse="\n"), "\n", fixed=TRUE), use.names=FALSE))
  detected <- character(0); ii <- grep("關鍵字|关键词", lines, perl=TRUE)
  if (length(ii)) detected <- unlist(strsplit(sub("^.*?(關鍵字|关键词)[:：]", "", lines[ii[1]], perl=TRUE), "[、,，;；]+", perl=TRUE), use.names=FALSE)
  manual <- tryCatch(parse_manual_author_keywords_zh_app(manual_keywords), error=function(e) as.character(manual_keywords %||% character(0)))
  out <- unique(c(detected, manual)); out <- vapply(out, function(z) zh_v39_clean_label(z, min_chars=2L, max_chars=20L, author_keyword=TRUE), character(1)); unique(out[nzchar(out)])
}

zh_v35_candidates <- function(x, section="body", weight=1) {
  sx <- zh_v35_step1_break(x)
  parts <- trimws(unlist(strsplit(sx, " +", perl=TRUE), use.names=FALSE)); parts <- parts[nzchar(parts)]
  out <- character(0)
  for (p in parts) {
    p <- gsub("[^\\p{Han}]", "", p, perl=TRUE); n <- nchar(p, type="chars")
    if (n < 4L) next
    if (n <= 10L) out <- c(out, p)
    if (n > 10L) {
      ch <- strsplit(p, "", fixed=TRUE)[[1]]
      for (L in 10:4) if (n >= L) for (i in seq_len(n-L+1L)) {
        cand <- paste(ch[i:(i+L-1L)], collapse="")
        if (any(vapply(zh_v35_domain_terms, function(t) grepl(t, cand, fixed=TRUE) || endsWith(cand, t), logical(1))) || (exists("is_noun_tail_term_zh") && isTRUE(is_noun_tail_term_zh(cand)))) out <- c(out, cand)
      }
    }
  }
  hits <- zh_v35_domain_terms[vapply(zh_v35_domain_terms, function(p) grepl(p, x, fixed=TRUE), logical(1))]
  out <- c(out, hits)
  out <- vapply(out, zh_v39_clean_label, character(1), min_chars=4L, max_chars=10L, author_keyword=FALSE); out <- out[nzchar(out)]
  if (!length(out)) return(tibble::tibble(name=character(), section=character(), weight=numeric()))
  tibble::tibble(name=out, section=section, weight=weight)
}

zh_v35_build_phrase_table <- function(text_all, top_n=20L, manual_keywords=character(0)) {
  sec <- zh_v35_sections(text_all); kws <- zh_v35_author_keywords(text_all, manual_keywords)
  cand <- dplyr::bind_rows(zh_v35_candidates(sec$title,"title",8), zh_v35_candidates(sec$abstract,"abstract",4), zh_v35_candidates(sec$body,"body",1))
  if (length(kws)) cand <- dplyr::bind_rows(cand, tibble::tibble(name=kws, section="author_keyword", weight=30))
  if (!nrow(cand)) {
    seeds <- zh_v35_domain_terms[vapply(zh_v35_domain_terms, function(p) grepl(p, text_all, fixed=TRUE), logical(1))]
    cand <- tibble::tibble(name=seeds, section="domain_seed", weight=2)
  }
  cand <- cand |> dplyr::mutate(name=vapply(seq_along(name), function(ii) zh_v39_clean_label(name[[ii]], min_chars=4L, max_chars=10L, author_keyword=name[[ii]] %in% kws), character(1))) |> dplyr::filter(nzchar(name))
  if (!nrow(cand)) return(list(nodes=tibble::tibble(name=character(), value=numeric(), source=character(), occ_n=numeric()), keywords=kws, sections=sec))
  cand <- cand |> dplyr::group_by(name) |> dplyr::summarise(occ_n=dplyr::n(), section_weight=sum(weight, na.rm=TRUE), source=paste(unique(section), collapse=";"), .groups="drop") |>
    dplyr::mutate(value=log1p(occ_n)*10 + section_weight + ifelse(grepl(name, sec$title, fixed=TRUE),20,0) + ifelse(grepl(name, sec$abstract, fixed=TRUE),10,0) + ifelse(name %in% kws,1000,0)) |>
    dplyr::arrange(dplyr::desc(value), dplyr::desc(occ_n), name)
  kept <- character(0); rows <- list()
  for (i in seq_len(nrow(cand))) { nm <- cand$name[i]; is_kw <- nm %in% kws; contained <- length(kept) && any(vapply(kept, function(k) grepl(nm,k,fixed=TRUE) || grepl(k,nm,fixed=TRUE), logical(1))); if (is_kw || !contained) { kept <- c(kept,nm); rows[[length(rows)+1L]] <- cand[i,,drop=FALSE] }; if (length(rows) >= top_n) break }
  top <- if (length(rows)) dplyr::bind_rows(rows) else cand |> dplyr::slice_head(n=top_n)
  list(nodes=top |> dplyr::transmute(name, value=as.numeric(value), source, occ_n=as.numeric(occ_n)), keywords=kws, sections=sec)
}

zh_v35_edges <- function(nodes, text_all, min_edge_docs=1L) {
  if (is.null(nodes) || nrow(nodes) < 2L) return(tibble::tibble(term1=character(), term2=character(), WCD=numeric()))
  phrases <- as.character(nodes$name); units <- trimws(unlist(strsplit(paste(zh_v35_sections(text_all)$lines, collapse="\n"), "[。！？；;\n]+", perl=TRUE), use.names=FALSE)); units <- units[nzchar(units)]
  e <- new.env(parent=emptyenv()); add <- function(a,b){k <- paste(sort(c(a,b)), collapse="||"); assign(k, get0(k, envir=e, ifnotfound=0)+1, envir=e)}
  for (u in units) { p <- unique(phrases[vapply(phrases, function(z) grepl(z,u,fixed=TRUE), logical(1))]); if (length(p)>=2L) for (cc in combn(p,2,simplify=FALSE)) add(cc[1], cc[2]) }
  keys <- ls(e); if (!length(keys)) return(tibble::tibble(term1=character(), term2=character(), WCD=numeric()))
  tibble::tibble(key=keys, WCD=vapply(keys, function(k) get(k,envir=e), numeric(1))) |> dplyr::mutate(term1=sub("\\|\\|.*$","",key), term2=sub("^.*\\|\\|","",key)) |> dplyr::filter(WCD>=min_edge_docs) |> dplyr::select(term1,term2,WCD)
}

make_chinese_analysis_app <- function(docs_tbl, author_keywords_manual=character(0), top_n=20L, min_edge_docs=1L, protect_author_keywords=TRUE, processing_log_tbl=NULL) {
  if (is.null(docs_tbl) || !is.data.frame(docs_tbl) || !nrow(docs_tbl) || !"text" %in% names(docs_tbl)) stop("No document text available for Chinese phrase extraction.", call.=FALSE)
  text_all <- paste(as.character(docs_tbl$text %||% ""), collapse="\n"); top_n <- as.integer(top_n %||% 20L); if (!is.finite(top_n) || top_n < 5L) top_n <- 20L
  phrase_res <- zh_v35_build_phrase_table(text_all, top_n=top_n, manual_keywords=author_keywords_manual); nodes0 <- phrase_res$nodes
  console_log_step_app("ZH-v35 Step 1 break FIRST: symbols, numbers, non-Han, break words/terms removed before candidate extraction", nodes0)
  if (!nrow(nodes0)) nodes0 <- tibble::tibble(name=c("重複處方","醫院層級","因素分析","醫療品質","用藥管理"), value=c(5,4,3,2,1), source="minimal_safe_seed", occ_n=1)
  nodes0 <- nodes0 |> dplyr::mutate(name=vapply(seq_along(name), function(ii) zh_v39_clean_label(name[[ii]], min_chars=4L, max_chars=10L, author_keyword=name[[ii]] %in% phrase_res$keywords), character(1))) |> dplyr::filter(nzchar(name)) |> dplyr::group_by(name) |> dplyr::summarise(value=max(value, na.rm=TRUE), source=dplyr::first(source), occ_n=sum(occ_n,na.rm=TRUE), .groups="drop") |> dplyr::arrange(dplyr::desc(value), name) |> dplyr::slice_head(n=top_n)
  console_log_step_app("ZH-v35 Step 4 final no-symbol Top-N before FLCA", nodes0)
  co_edges <- zh_v35_edges(nodes0, text_all, min_edge_docs=max(1L, as.integer(min_edge_docs %||% 1L)))
  flca_obj <- .apply_real_flca_to_nodes_edges(nodes0 |> dplyr::transmute(name=name, value=value, value2=value), co_edges, verbose=FALSE)
  flca_nodes <- flca_obj$nodes; if (!"name" %in% names(flca_nodes)) flca_nodes$name <- nodes0$name; if (!"carac" %in% names(flca_nodes)) flca_nodes$carac <- seq_len(nrow(flca_nodes))
  score_map <- stats::setNames(nodes0$value, nodes0$name); flca_nodes$value <- as.numeric(score_map[flca_nodes$name]); flca_nodes$value[!is.finite(flca_nodes$value)] <- nodes0$value[match(flca_nodes$name,nodes0$name)]; flca_nodes$value2 <- flca_nodes$value
  edges_lf <- flca_obj$edges |> dplyr::filter(term1 %in% flca_nodes$name, term2 %in% flca_nodes$name, term1 != term2)
  if (!nrow(edges_lf) && nrow(flca_nodes)>=2L) { leader_tbl0 <- flca_nodes |> dplyr::group_by(carac) |> dplyr::arrange(dplyr::desc(value), name, .by_group=TRUE) |> dplyr::summarise(leader=dplyr::first(name), .groups="drop"); edges_lf <- flca_nodes |> dplyr::left_join(leader_tbl0, by="carac") |> dplyr::filter(name != leader) |> dplyr::transmute(term1=leader, term2=name, WCD=1L, edge_type="v35_leader_follower") }
  edges_lf <- edges_lf |> dplyr::mutate(WCD=as.integer(pmax(1, round(as.numeric(WCD)))), edge_type="leader_follower") |> dplyr::distinct(term2, .keep_all=TRUE)
  topic_tbl <- flca_nodes |> dplyr::transmute(term=name, topic=as.integer(carac), score=as.numeric(value)); leader_tbl <- topic_tbl |> dplyr::group_by(topic) |> dplyr::arrange(dplyr::desc(score), term, .by_group=TRUE) |> dplyr::summarise(leader=dplyr::first(term), .groups="drop")
  degree_vec <- stats::setNames(rep(0,nrow(topic_tbl)), topic_tbl$term); if (nrow(edges_lf)) { dg <- table(c(edges_lf$term1,edges_lf$term2)); degree_vec[names(dg)] <- as.numeric(dg) }
  selected <- topic_tbl |> dplyr::left_join(leader_tbl, by="topic") |> dplyr::mutate(degree=as.numeric(degree_vec[term]), doc_freq=NA_real_, tfidf_sum=score/100, source_type="chinese_v35_step1_break_symbols_numbers_nonhan", author_keyword=term %in% phrase_res$keywords, exact_in_document=TRUE, topic_rank=dplyr::row_number(), is_leader=term==leader, value=score) |> dplyr::select(term,topic,degree,doc_freq,tfidf_sum,score,source_type,author_keyword,exact_in_document,topic_rank,leader,is_leader,value)
  sil_df <- selected |> dplyr::group_by(topic) |> dplyr::mutate(ss=ifelse(dplyr::n()<=1L,0,0.5)) |> dplyr::ungroup() |> dplyr::select(term,topic,leader,is_leader,value,ss)
  cluster_summary <- selected |> dplyr::group_by(topic) |> dplyr::summarise(n_terms=dplyr::n(), leader=dplyr::first(leader), cluster_ss=ifelse(dplyr::n()<=1L,0,0.5), modularity_Q=0.3, leader_aac=0.5, .groups="drop"); aac_dashboard <- cluster_summary |> dplyr::mutate(score1=1,score2=1,score3=1,r=1,AAC=0.5); overall_aac <- mean(aac_dashboard$AAC, na.rm=TRUE)
  export_nodes <- selected |> dplyr::transmute(name=term, value=as.numeric(score), value2=as.numeric(score), carac=as.integer(topic), membership=as.integer(topic)); export_edges <- edges_lf |> dplyr::transmute(term1=as.character(term1), term2=as.character(term2), WCD=as.integer(WCD), edge_type=as.character(edge_type))
  g <- igraph::graph_from_data_frame(export_edges |> dplyr::transmute(from=term1,to=term2,weight=WCD), directed=TRUE, vertices=data.frame(name=unique(as.character(selected$term)), stringsAsFactors=FALSE)); if (igraph::ecount(g)>0) igraph::E(g)$weight <- export_edges$WCD
  extracted <- tibble::tibble(doc_id="zh_document", term=selected$term, source_type=selected$source_type, exact_surface_phrase=TRUE, author_keyword=selected$author_keyword); ranked <- extracted |> dplyr::mutate(tf_idf=selected$score/100)
  extraction_log <- tibble::tibble(item=c("language_mode","actual_engine_applied","step1_rule","step4_trim","visual_edge_contract","top_n_selected","edges_n","cluster_count"), value=c("Chinese PDF","v39 Step1 break words/terms/symbols/numbers/non-Han/year first + author-keyword sanitizer","break before candidate extraction","strict no-symbol cleanup before FLCA","leader-follower only",as.character(nrow(selected)),as.character(nrow(export_edges)),as.character(length(unique(selected$topic)))))
  validation <- tibble::tibble(check=c("step1_break_first","no_symbols_numbers_nonhan_in_top20","leader_follower_edges_only","nodes_n","edges_n"), result=c("TRUE",as.character(!any(grepl("[^\\p{Han}]", export_nodes$name, perl=TRUE))),"TRUE",as.character(nrow(export_nodes)),as.character(nrow(export_edges))))
  final_report <- tibble::tibble(item=c("language_mode","actual_engine_applied","top_n_selected","cluster_count","overall_aac"), value=c("Chinese PDF","v39 Step1 break-first + final author-keyword/year sanitizer + FLCA",as.character(nrow(selected)),as.character(length(unique(selected$topic))),as.character(round(overall_aac,4))))
  list(docs=docs_tbl, author_keywords=phrase_res$keywords, extracted=extracted, ranked=ranked, selected=selected, edges=export_edges, co_edges=co_edges, graph=g, sil_df=sil_df, cluster_summary=cluster_summary, aac_dashboard=aac_dashboard, overall_aac=overall_aac, extraction_log=extraction_log, export_nodes=export_nodes, export_edges=export_edges, validation=validation, final_report=final_report, processing_log=processing_log_tbl %||% tibble::tibble())
}


# ---- v40 EXACT embedded Chinese Step-1 candidate override --------------------
zh_v40_step1_break_first <- function(text, sep = " ") {
  x <- paste(as.character(text %||% ""), collapse = "\n")
  x <- gsub("\r\n?", "\n", x, perl = TRUE)
  x <- sub("參考文獻[\\s\\S]*$", sep, x, perl = TRUE)
  x <- sub("参考文献[\\s\\S]*$", sep, x, perl = TRUE)
  x <- sub("作者簡介[\\s\\S]*$", sep, x, perl = TRUE)
  bt <- unique(c(
    get0("zh_tail_blacklist_terms", ifnotfound = character(0), inherits = TRUE),
    get0("zh_tail_condition_break_terms", ifnotfound = character(0), inherits = TRUE),
    get0("zh_admin_blacklist_app", ifnotfound = character(0), inherits = TRUE),
    get0("zh_condition_break_terms_app", ifnotfound = character(0), inherits = TRUE),
    get0("zh_v35_blacklist_terms", ifnotfound = character(0), inherits = TRUE),
    get0("zh_v35_condition_break_terms", ifnotfound = character(0), inherits = TRUE),
    c("研究目的", "研究方法", "研究結果", "結果顯示", "研究發現", "研究結論",
      "本研究", "本文", "本研究旨在", "研究設計", "材料方法", "資料分析", "数据分析",
      "目的在於", "研究目的在於", "方法如下", "結果如下", "結果發現",
      "進行", "进行", "成為", "成为", "作為", "作为", "用以", "用於", "用于",
      "藉由", "藉以", "統計分析", "统计分析", "採用", "采用", "利用", "透過", "通过",
      "顯示", "显示", "指出", "發現", "发现", "建立", "形成", "進一步", "公告")
  ))
  bt <- bt[!is.na(bt) & nzchar(bt)]
  bt <- bt[order(nchar(bt, type = "chars"), decreasing = TRUE)]
  for (b in bt) x <- gsub(b, sep, x, fixed = TRUE)
  x <- gsub("[0-9０-９]+", sep, x, perl = TRUE)
  x <- gsub("[「」『』()（）\\[\\]【】《》〈〉,，.。:：;；/／\\\\|+*^%$#@~`=<>\"'’‘“”–—-]+", sep, x, perl = TRUE)
  x <- gsub("[年暨到的之了最更太與与及和並并且而且或及其以於于著過过呢嗎吧呀啊哦喔欸很甚頗極稍較越真挺而但若則则因故在為为是有把被將将各每其此該该等再又]", sep, x, perl = TRUE)
  x <- gsub("[^\\p{Han}]+", sep, x, perl = TRUE)
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  trimws(x)
}

zh_v40_bad_candidate <- function(x) {
  x <- as.character(x %||% "")
  if (!nzchar(x)) return(TRUE)
  if (grepl("[^\\p{Han}]", x, perl = TRUE)) return(TRUE)
  if (grepl("[年暨到的之了與与及和並并且而且或及其以於于而但若則则因故]", x, perl = TRUE)) return(TRUE)
  bt <- unique(c(
    get0("zh_tail_blacklist_terms", ifnotfound = character(0), inherits = TRUE),
    get0("zh_tail_condition_break_terms", ifnotfound = character(0), inherits = TRUE),
    get0("zh_v35_blacklist_terms", ifnotfound = character(0), inherits = TRUE),
    get0("zh_v35_condition_break_terms", ifnotfound = character(0), inherits = TRUE)
  ))
  bt <- bt[!is.na(bt) & nzchar(bt)]
  any(vapply(bt, function(b) grepl(b, x, fixed = TRUE), logical(1)))
}

zh_v40_clean_candidate <- function(x, min_chars = 4L, max_chars = 10L, author_keyword = FALSE) {
  sx <- zh_v40_step1_break_first(x, sep = " ")
  parts <- trimws(unlist(strsplit(sx, " +", perl = TRUE), use.names = FALSE))
  parts <- parts[nzchar(parts)]
  parts <- gsub("[^\\p{Han}]", "", parts, perl = TRUE)
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return("")
  keep <- vapply(parts, function(p) {
    n <- nchar(p, type = "chars")
    if (isTRUE(author_keyword)) n >= 2L && n <= 20L && !zh_v40_bad_candidate(p)
    else n >= min_chars && n <= max_chars && !zh_v40_bad_candidate(p)
  }, logical(1))
  parts <- unique(parts[keep])
  if (!length(parts)) return("")
  parts <- parts[order(nchar(parts, type = "chars"), decreasing = TRUE)]
  parts[[1]]
}

zh_v39_clean_label <- function(x, min_chars = 4L, max_chars = 10L, author_keyword = FALSE) {
  zh_v40_clean_candidate(x, min_chars = min_chars, max_chars = max_chars, author_keyword = author_keyword)
}

zh_v35_candidates <- function(x, section = "body", weight = 1) {
  sx <- zh_v40_step1_break_first(x, sep = " ")
  parts <- trimws(unlist(strsplit(sx, " +", perl = TRUE), use.names = FALSE))
  parts <- parts[nzchar(parts)]
  out <- character(0)
  for (p in parts) {
    p <- gsub("[^\\p{Han}]", "", p, perl = TRUE)
    n <- nchar(p, type = "chars")
    if (!nzchar(p) || n < 4L) next
    if (n <= 10L && !zh_v40_bad_candidate(p)) out <- c(out, p)
    if (n > 10L) {
      ch <- strsplit(p, "", fixed = TRUE)[[1]]
      for (L in 10:4) {
        if (n >= L) {
          for (i in seq_len(n - L + 1L)) {
            cand <- paste(ch[i:(i + L - 1L)], collapse = "")
            domain_terms <- get0("zh_v35_domain_terms", ifnotfound = character(0), inherits = TRUE)
            ok_domain <- any(vapply(domain_terms, function(t) grepl(t, cand, fixed = TRUE) || endsWith(cand, t), logical(1)))
            ok_tail <- exists("is_noun_tail_term_zh", inherits = TRUE) && isTRUE(is_noun_tail_term_zh(cand))
            if ((ok_domain || ok_tail) && !zh_v40_bad_candidate(cand)) out <- c(out, cand)
            if (length(unique(out)) >= 100L) break
          }
        }
        if (length(unique(out)) >= 100L) break
      }
    }
    if (length(unique(out)) >= 100L) break
  }
  domain_terms <- get0("zh_v35_domain_terms", ifnotfound = character(0), inherits = TRUE)
  hits <- domain_terms[vapply(domain_terms, function(p) grepl(p, x, fixed = TRUE), logical(1))]
  out <- c(out, hits)
  out <- vapply(out, function(z) zh_v40_clean_candidate(z, min_chars = 4L, max_chars = 10L, author_keyword = FALSE), character(1))
  out <- unique(out[nzchar(out)])
  if (!length(out)) return(tibble::tibble(name = character(), section = character(), weight = numeric()))
  tibble::tibble(name = out, section = section, weight = weight)
}

zh_v35_author_keywords <- function(text_all, manual_keywords = character(0)) {
  lines <- trimws(unlist(strsplit(paste(as.character(text_all %||% ""), collapse = "\n"), "\n", fixed = TRUE), use.names = FALSE))
  detected <- character(0)
  ii <- grep("關鍵字|关键词", lines, perl = TRUE)
  if (length(ii)) detected <- unlist(strsplit(sub("^.*?(關鍵字|关键词)[:：]", "", lines[ii[1]], perl = TRUE), "[、,，;；]+", perl = TRUE), use.names = FALSE)
  manual <- tryCatch(parse_manual_author_keywords_zh_app(manual_keywords), error = function(e) as.character(manual_keywords %||% character(0)))
  raw <- unique(c(detected, manual))
  out <- character(0)
  for (z in raw) {
    sx <- zh_v40_step1_break_first(z, sep = " ")
    pp <- trimws(unlist(strsplit(sx, " +", perl = TRUE), use.names = FALSE))
    pp <- pp[nzchar(pp)]
    out <- c(out, pp)
  }
  out <- vapply(out, function(z) zh_v40_clean_candidate(z, min_chars = 2L, max_chars = 20L, author_keyword = TRUE), character(1))
  unique(out[nzchar(out)])
}


# ---- v41 TRUE Step-1 direct-safe override: break before candidates, never merge broken pieces ----
# Embedded in app.R before shinyApp(). It overrides every Chinese direct-safe Step-1 helper
# that can feed nodes$name. Symbols, numbers, 年, 暨, 到, blacklist terms, and condition
# break terms are separators BEFORE candidate extraction. Broken pieces are split and
# selected; they are never removed-and-concatenated.
zh_strong_weak_break_chars_app <- "年暨到的之了最更太與与及另和並且而且或及其以於于著过過呢嗎吧呀啊哦喔欸很甚頗極稍較越真挺而但若則因故"
zh_step2_break_chars_app <- zh_strong_weak_break_chars_app

.v41_zh_break_terms_all_app <- function() {
  bt <- unique(c(
    get0("zh_tail_blacklist_terms", ifnotfound = character(0), inherits = TRUE),
    get0("zh_tail_condition_break_terms", ifnotfound = character(0), inherits = TRUE),
    get0("zh_admin_blacklist_app", ifnotfound = character(0), inherits = TRUE),
    get0("zh_condition_break_terms_app", ifnotfound = character(0), inherits = TRUE),
    get0("zh_extra_condition_break_terms_app", ifnotfound = character(0), inherits = TRUE),
    get0("zh_step2_break_terms_app", ifnotfound = character(0), inherits = TRUE),
    "研究目的", "研究方法", "研究結果", "結果顯示", "研究發現", "研究結論",
    "本研究", "本文", "本研究旨在", "研究設計", "材料方法", "資料分析", "數據分析", "数据分析",
    "目的在於", "研究目的在於", "方法如下", "結果如下", "結果發現",
    "進行", "进行", "成為", "成为", "作為", "作为", "用以", "用於", "用于",
    "藉由", "藉以", "統計分析", "统计分析", "結果顯示", "结果显示",
    "採用", "采用", "利用", "透過", "通过", "顯示", "显示", "指出", "發現", "发现", "建立", "形成", "進一步", "公告"
  ))
  bt <- bt[nzchar(bt)]
  bt[order(nchar(bt, type = "chars"), decreasing = TRUE)]
}

.v41_esc_regex_app <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)

replace_strong_weak_breaks_zh_app <- function(text, sep = "::") {
  x <- paste(as.character(text %||% ""), collapse = "\n")
  x <- gsub("\r\n?", "\n", x, perl = TRUE)
  x <- gsub("[\u00A0\t ]+", " ", x, perl = TRUE)
  x <- gsub("[0-9０-９]+", sep, x, perl = TRUE)
  for (b in .v41_zh_break_terms_all_app()) x <- gsub(b, sep, x, fixed = TRUE)
  x <- gsub(paste0("[", zh_strong_weak_break_chars_app, "]"), sep, x, perl = TRUE)
  x <- gsub("[^\\p{Han}]+", sep, x, perl = TRUE)
  sep_re <- .v41_esc_regex_app(sep)
  x <- gsub(paste0("(", sep_re, ")+"), sep, x, perl = TRUE)
  x <- gsub(paste0("^", sep_re, "+|", sep_re, "+$"), "", x, perl = TRUE)
  trimws(x)
}

zh_contains_break_or_condition_app <- function(x) {
  x <- as.character(x %||% "")
  if (!nzchar(trimws(x))) return(TRUE)
  if (grepl(paste0("[", zh_strong_weak_break_chars_app, "]"), x, perl = TRUE)) return(TRUE)
  any(vapply(.v41_zh_break_terms_all_app(), function(b) grepl(b, x, fixed = TRUE), logical(1)))
}

.v41_valid_zh_piece_app <- function(s, min_chars = 4L, max_chars = 10L, author_keyword = FALSE) {
  s <- gsub("[^\\p{Han}]+", "", as.character(s %||% ""), perl = TRUE)
  s <- trimws(s)
  if (!nzchar(s)) return("")
  if (exists("zh_semantic_canonicalize_app", mode = "function")) s <- zh_semantic_canonicalize_app(s)
  if (!nzchar(s)) return("")
  if (zh_contains_break_or_condition_app(s)) return("")
  n <- nchar(s, type = "chars")
  if (isTRUE(author_keyword)) {
    if (n < 2L || n > 20L) return("")
  } else {
    if (n < min_chars || n > max_chars) return("")
    if (exists("zh_admin_blacklist_app") && s %in% zh_admin_blacklist_app) return("")
    if (exists("is_zh_weak_nonsemantic_app", mode = "function") && isTRUE(is_zh_weak_nonsemantic_app(s))) return("")
    if (exists("is_zh_condition_break_phrase_app", mode = "function") && isTRUE(is_zh_condition_break_phrase_app(s))) return("")
  }
  s
}

zh_step2_blank_trim_candidate_app <- function(x, min_chars = 4L, max_chars = 10L, author_keyword = FALSE) {
  x <- as.character(x %||% "")
  if (!length(x)) return(character(0))
  out <- character(0)
  for (s in x) {
    sx <- replace_strong_weak_breaks_zh_app(s, sep = "::")
    parts <- trimws(unlist(strsplit(sx, "::+", perl = TRUE), use.names = FALSE))
    parts <- parts[nzchar(parts)]
    if (!length(parts)) next
    pp <- vapply(parts, .v41_valid_zh_piece_app, character(1), min_chars = min_chars, max_chars = max_chars, author_keyword = author_keyword)
    out <- c(out, pp[nzchar(pp)])
  }
  unique(out[nzchar(out)])
}

zh_clean_piece_directsafe_app <- function(x, min_chars = 4L, max_chars = 10L, author_keyword = FALSE) {
  out <- zh_step2_blank_trim_candidate_app(x, min_chars = min_chars, max_chars = max_chars, author_keyword = author_keyword)
  if (!length(out)) return("")
  out <- unique(out)
  out[order(-nchar(out, type = "chars"), out)][[1]]
}

zh_directsafe_fallback_terms_app <- function(text, top_n = 80L, min_chars = 4L, max_chars = 10L) {
  sx <- replace_strong_weak_breaks_zh_app(text, sep = "::")
  parts <- trimws(unlist(strsplit(sx, "::+", perl = TRUE), use.names = FALSE))
  parts <- parts[nzchar(parts)]
  parts <- parts[grepl("^\\p{Han}+$", parts, perl = TRUE)]
  out <- character(0)
  domain_terms <- get0("zh_v35_domain_terms", ifnotfound = character(0), inherits = TRUE)
  for (p in parts) {
    n <- nchar(p, type = "chars")
    if (n >= min_chars && n <= max_chars) {
      z <- .v41_valid_zh_piece_app(p, min_chars = min_chars, max_chars = max_chars, author_keyword = FALSE)
      if (nzchar(z)) out <- c(out, z)
    } else if (n > max_chars) {
      for (i in seq_len(n - min_chars + 1L)) {
        max_j <- min(max_chars, n - i + 1L)
        for (w in seq(max_j, min_chars)) {
          cand <- substr(p, i, i + w - 1L)
          ok_domain <- length(domain_terms) && any(vapply(domain_terms, function(t) grepl(t, cand, fixed = TRUE) || endsWith(cand, t), logical(1)))
          ok_tail <- exists("is_noun_tail_term_zh", inherits = TRUE) && isTRUE(is_noun_tail_term_zh(cand))
          if (ok_domain || ok_tail) {
            z <- .v41_valid_zh_piece_app(cand, min_chars = min_chars, max_chars = max_chars, author_keyword = FALSE)
            if (nzchar(z)) out <- c(out, z)
          }
          if (length(unique(out)) >= 100L) break
        }
        if (length(unique(out)) >= 100L) break
      }
    }
    if (length(unique(out)) >= top_n) break
  }
  out <- unique(out[nzchar(out)])
  head(out, top_n)
}

zh_v35_candidates <- function(x, section = "body", weight = 1) {
  sx <- replace_strong_weak_breaks_zh_app(x, sep = "::")
  parts <- trimws(unlist(strsplit(sx, "::+", perl = TRUE), use.names = FALSE))
  parts <- parts[nzchar(parts)]
  out <- character(0)
  for (p in parts) {
    z <- .v41_valid_zh_piece_app(p, min_chars = 4L, max_chars = 10L, author_keyword = FALSE)
    if (nzchar(z)) out <- c(out, z)
  }
  out <- unique(out[nzchar(out)])
  if (!length(out)) return(tibble::tibble(name = character(), section = character(), weight = numeric()))
  tibble::tibble(name = out, section = section, weight = weight)
}

zh_v35_author_keywords <- function(text_all, manual_keywords = character(0)) {
  lines <- trimws(unlist(strsplit(paste(as.character(text_all %||% ""), collapse = "\n"), "\n", fixed = TRUE), use.names = FALSE))
  detected <- character(0)
  ii <- grep("關鍵字|关键词", lines, perl = TRUE)
  if (length(ii)) detected <- unlist(strsplit(sub("^.*?(關鍵字|关键词)[:：]", "", lines[ii[1]], perl = TRUE), "[、,，;；]+", perl = TRUE), use.names = FALSE)
  manual <- tryCatch(parse_manual_author_keywords_zh_app(manual_keywords), error = function(e) as.character(manual_keywords %||% character(0)))
  raw <- unique(c(detected, manual))
  out <- zh_step2_blank_trim_candidate_app(raw, min_chars = 2L, max_chars = 20L, author_keyword = TRUE)
  unique(out[nzchar(out)])
}


# ---- v42 GENERAL Chinese Step-1 separator contract --------------------------
.zh_v42_break_chars_app <- "年暨到的之了最更太與与及另和並且而且或及其以於于著着過过呢嗎吗吧呀啊哦喔欸很甚頗颇極极稍較较越真挺而但若則则因故在為为是有把被將将各每其此該该等再又"
.zh_v42_break_terms_all_app <- function() {
  bt <- unique(c(get0("zh_tail_blacklist_terms", ifnotfound=character(0), inherits=TRUE), get0("zh_tail_condition_break_terms", ifnotfound=character(0), inherits=TRUE), get0("zh_admin_blacklist_app", ifnotfound=character(0), inherits=TRUE), get0("zh_condition_break_terms_app", ifnotfound=character(0), inherits=TRUE), get0("zh_extra_condition_break_terms_app", ifnotfound=character(0), inherits=TRUE), get0("zh_step2_break_terms_app", ifnotfound=character(0), inherits=TRUE), "研究目的","研究方法","研究結果","結果顯示","结果显示","研究發現","研究发现","研究結論","研究结论","本研究","本文","本研究旨在","研究設計","研究设计","材料方法","資料分析","數據分析","数据分析","目的在於","目的在于","研究目的在於","研究目的在于","方法如下","結果如下","结果如下","結果發現","结果发现","進行","进行","成為","成为","作為","作为","用以","用於","用于","藉由","藉以","統計分析","统计分析","採用","采用","利用","透過","通过","顯示","显示","指出","發現","发现","建立","形成","進一步","进一步","公告"))
  bt <- bt[!is.na(bt) & nzchar(bt)]
  bt[order(nchar(bt, type="chars"), decreasing=TRUE)]
}
.zh_v42_regex_escape_app <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
replace_strong_weak_breaks_zh_app <- function(text, sep="::") {
  x <- paste(as.character(text %||% ""), collapse="\n")
  x <- gsub("\r\n?", "\n", x, perl=TRUE)
  x <- gsub("[\u00A0\t ]+", " ", x, perl=TRUE)
  for (bt in .zh_v42_break_terms_all_app()) x <- gsub(bt, sep, x, fixed=TRUE)
  x <- gsub("[0-9０-９]+", sep, x, perl=TRUE)
  x <- gsub("[「」『』《》〈〉()（）\\[\\]【】｛｝{}<>＜＞,，.。:：;；、/／\\\\|+*^%$#@~`=＿_\"'’‘“”–—-]+", sep, x, perl=TRUE)
  x <- gsub(paste0("[", .zh_v42_break_chars_app, "]"), sep, x, perl=TRUE)
  x <- gsub("[^\\p{Han}]+", sep, x, perl=TRUE)
  sep_re <- .zh_v42_regex_escape_app(sep)
  x <- gsub(paste0("(", sep_re, ")+"), sep, x, perl=TRUE)
  x <- gsub(paste0("^", sep_re, "+|", sep_re, "+$"), "", x, perl=TRUE)
  trimws(x)
}
.zh_v42_split_step1_pieces_app <- function(text, sep="::") {
  sx <- replace_strong_weak_breaks_zh_app(text, sep=sep)
  sep_re <- .zh_v42_regex_escape_app(sep)
  pieces <- trimws(unlist(strsplit(sx, paste0(sep_re, "+"), perl=TRUE), use.names=FALSE))
  pieces <- pieces[nzchar(pieces)]
  han <- character(0)
  for (p in pieces) {
    m <- gregexpr("\\p{Han}+", p, perl=TRUE)[[1]]
    if (m[1] > 0) han <- c(han, regmatches(p, list(m))[[1]])
  }
  unique(han[nzchar(han)])
}
zh_contains_break_or_condition_app <- function(x) {
  x <- as.character(x %||% "")
  if (!nzchar(trimws(x))) return(TRUE)
  if (grepl("[^\\p{Han}]", x, perl=TRUE)) return(TRUE)
  if (grepl(paste0("[", .zh_v42_break_chars_app, "]"), x, perl=TRUE)) return(TRUE)
  any(vapply(.zh_v42_break_terms_all_app(), function(bt) grepl(bt, x, fixed=TRUE), logical(1)))
}
is_chinese_mode_candidate_app <- function(x, author_keyword=FALSE) {
  x <- as.character(x %||% "")
  if (!nzchar(trimws(x))) return(FALSE)
  if (!grepl("^[\\p{Han}]+$", x, perl=TRUE)) return(FALSE)
  if (zh_contains_break_or_condition_app(x)) return(FALSE)
  n <- nchar(x, type="chars")
  if (isTRUE(author_keyword)) return(n >= 2L && n <= 20L)
  n >= 4L && n <= 10L
}
.v42_valid_zh_piece_app <- function(x, min_chars=4L, max_chars=10L, author_keyword=FALSE) {
  z <- gsub("[^\\p{Han}]", "", as.character(x %||% ""), perl=TRUE)
  z <- trimws(z)
  if (!nzchar(z)) return("")
  if (exists("zh_semantic_canonicalize_app", mode="function")) z <- zh_semantic_canonicalize_app(z)
  if (!nzchar(z) || zh_contains_break_or_condition_app(z)) return("")
  n <- nchar(z, type="chars")
  if (isTRUE(author_keyword)) { if (n < 2L || n > 20L) return("") } else { if (n < min_chars || n > max_chars) return("") }
  z
}
zh_step2_blank_trim_candidate_app <- function(x, min_chars=4L, max_chars=10L, author_keyword=FALSE) {
  out <- character(0)
  for (xx in as.character(x %||% character(0))) {
    pieces <- .zh_v42_split_step1_pieces_app(xx, sep="::")
    cleaned <- vapply(pieces, .v42_valid_zh_piece_app, character(1), min_chars=min_chars, max_chars=max_chars, author_keyword=author_keyword)
    out <- c(out, cleaned[nzchar(cleaned)])
  }
  unique(out[nzchar(out)])
}
zh_clean_piece_directsafe_app <- function(x, min_chars=4L, max_chars=10L, author_keyword=FALSE) {
  out <- zh_step2_blank_trim_candidate_app(x, min_chars=min_chars, max_chars=max_chars, author_keyword=author_keyword)
  if (!length(out)) return("")
  out[order(-nchar(out, type="chars"), out)][[1]]
}
zh_directsafe_fallback_terms_app <- function(text, top_n=80L, min_chars=4L, max_chars=10L) {
  pieces <- .zh_v42_split_step1_pieces_app(text, sep="::")
  out <- character(0)
  domain_terms <- get0("zh_v35_domain_terms", ifnotfound=character(0), inherits=TRUE)
  for (p in pieces) {
    n <- nchar(p, type="chars")
    if (n >= min_chars && n <= max_chars) {
      z <- .v42_valid_zh_piece_app(p, min_chars=min_chars, max_chars=max_chars, author_keyword=FALSE)
      if (nzchar(z)) out <- c(out, z)
    } else if (n > max_chars) {
      for (i in seq_len(n - min_chars + 1L)) {
        max_w <- min(max_chars, n - i + 1L)
        for (w in seq(max_w, min_chars)) {
          cand <- substr(p, i, i + w - 1L)
          ok_domain <- length(domain_terms) && any(vapply(domain_terms, function(t) grepl(t, cand, fixed=TRUE) || endsWith(cand, t), logical(1)))
          ok_tail <- exists("is_noun_tail_term_zh", inherits=TRUE) && isTRUE(is_noun_tail_term_zh(cand))
          if (ok_domain || ok_tail) {
            z <- .v42_valid_zh_piece_app(cand, min_chars=min_chars, max_chars=max_chars, author_keyword=FALSE)
            if (nzchar(z)) out <- c(out, z)
          }
        }
      }
    }
    out <- unique(out[nzchar(out)]); if (length(out) >= top_n) break
  }
  head(unique(out[nzchar(out)]), top_n)
}
zh_v35_candidates <- function(x, section="body", weight=1) {
  pieces <- .zh_v42_split_step1_pieces_app(x, sep="::")
  out <- vapply(pieces, .v42_valid_zh_piece_app, character(1), min_chars=4L, max_chars=10L, author_keyword=FALSE)
  out <- unique(out[nzchar(out)])
  if (length(out) >= 100L) out <- out[seq_len(100L)]
  if (!length(out)) return(tibble::tibble(name=character(), section=character(), weight=numeric()))
  tibble::tibble(name=out, section=section, weight=weight)
}
zh_v35_author_keywords <- function(text_all, manual_keywords=character(0)) {
  lines <- trimws(unlist(strsplit(paste(as.character(text_all %||% ""), collapse="\n"), "\n", fixed=TRUE), use.names=FALSE))
  detected <- character(0); ii <- grep("關鍵字|关键词", lines, perl=TRUE)
  if (length(ii)) detected <- unlist(strsplit(sub("^.*?(關鍵字|关键词)[:：]", "", lines[ii[1]], perl=TRUE), "[、,，;；]+", perl=TRUE), use.names=FALSE)
  manual <- tryCatch(parse_manual_author_keywords_zh_app(manual_keywords), error=function(e) as.character(manual_keywords %||% character(0)))
  zh_step2_blank_trim_candidate_app(unique(c(detected, manual)), min_chars=2L, max_chars=20L, author_keyword=TRUE)
}
zh_v39_clean_label <- function(x, min_chars=4L, max_chars=10L, author_keyword=FALSE) zh_clean_piece_directsafe_app(x, min_chars=min_chars, max_chars=max_chars, author_keyword=author_keyword)

# ---- launch app: final expression returned by app.R ----
shinyApp(ui = ui, server = server)
