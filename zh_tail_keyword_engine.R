# zh_tail_keyword_engine.R
# Chinese Main Document keyword engine (v6)
# Rule summary:
# 1) Author keywords remain mandatory in the final set.
# 2) All non-Chinese characters (spaces, symbols, digits, Latin) are treated as breaks.
# 3) Final emitted terms keep Chinese only, 3-10 chars, with 4/6/8/10 preferred.
# 4) Odd lengths 3/5/7/9 are allowed only when the exact term is comma-delimited.
# 5) Tail-suffix terms must include at least 2 Chinese chars before the suffix.
# 6) Final terms are subset-free; longer and author-keyword terms are preferred.
# 7) Non-author terms must appear at least 2 times in title/abstract/body units.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

zh_tail_nchar <- function(x) nchar(as.character(x %||% ""), type = "chars")

zh_tail_normalize_spaces <- function(x) {
  x <- paste(as.character(x %||% ""), collapse = "\n")
  x <- gsub("\r\n?", "\n", x)
  x <- gsub("[\u00A0\t ]+", " ", x, perl = TRUE)
  x <- gsub("\n{3,}", "\n\n", x, perl = TRUE)
  trimws(x)
}

zh_tail_keep_han <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("[^\\p{Han}]", "", x, perl = TRUE)
  trimws(x)
}

zh_tail_clean_single_term <- function(x, min_chars = 3L, max_chars = 10L) {
  x <- zh_tail_keep_han(x)
  n <- zh_tail_nchar(x)
  if (!nzchar(x) || n < min_chars || n > max_chars) return(NA_character_)
  x
}

normalize_spaces_zh <- function(x) zh_tail_normalize_spaces(x)
normalize_term_zh <- function(x) zh_tail_keep_han(x)
nchar_zh <- function(x) zh_tail_nchar(x)


zh_tail_blacklist_terms <- c(
  "研究目的", "研究方法", "研究結果", "結果顯示", "研究發現", "研究結論",
  "本研究", "本文", "本研究旨在", "研究設計", "材料方法", "資料分析", "数据分析",
  "目的在於", "研究目的在於", "方法如下", "結果如下", "結果發現"
)

zh_tail_condition_break_terms <- c(
  "進行", "进行", "成為", "成为", "作為", "作为", "用以", "用於", "用于",
  "藉由", "藉以", "統計分析", "统计分析", "研究目的", "研究方法",
  "研究結果", "結果顯示", "结果显示", "採用", "采用", "利用", "透過", "通过",
  "顯示", "显示", "指出", "發現", "发现", "建立", "形成", "進一步"
)

# Optional user-editable whitelist for terms that should always be retained
# when they appear in the current document. Leave empty for generic use.
zh_tail_whitelist_terms <- character(0)

zh_tail_is_blacklisted <- function(x) {
  x <- zh_tail_keep_han(x)
  if (!nzchar(x)) return(TRUE)
  x %in% zh_tail_blacklist_terms
}

extract_author_keyword_line_zh <- function(lines) {
  lines <- trimws(as.character(lines %||% character(0)))
  patt <- "^\\s*(關鍵字|关键字|關鍵詞|关键词|keywords?)\\s*[:：]?"
  hit <- grep(patt, lines, perl = TRUE, ignore.case = TRUE)
  if (length(hit)) {
    i <- hit[1]
    out <- sub(patt, "", lines[i], perl = TRUE, ignore.case = TRUE)
    if (!nzchar(out) && i < length(lines)) out <- trimws(lines[i + 1])
    if (nzchar(out)) return(out)
  }

  cand <- head(lines, 25)
  score_line <- function(ln) {
    ln2 <- gsub("[^\\p{Han}；;，,、|/]", " ", ln, perl = TRUE)
    parts <- unlist(strsplit(ln2, "\\s*[；;，,、|/]\\s*", perl = TRUE), use.names = FALSE)
    parts <- zh_tail_keep_han(parts)
    parts <- parts[nzchar(parts)]
    sum(zh_tail_nchar(parts) >= 3L & zh_tail_nchar(parts) <= 12L)
  }
  sc <- vapply(cand, score_line, integer(1))
  pick <- which(sc >= 3L)
  if (length(pick)) return(cand[pick[1]])
  NA_character_
}

zh_tail_contains_subset_relation <- function(x, kept) {
  if (!length(kept)) return(FALSE)
  any(vapply(kept, function(k) identical(x, k) || grepl(x, k, fixed = TRUE) || grepl(k, x, fixed = TRUE), logical(1)))
}

zh_tail_select_subset_free_terms <- function(terms,
                                             scores = NULL,
                                             author_terms = character(0),
                                             top_n = Inf) {
  terms <- as.character(terms %||% character(0))
  terms <- terms[nzchar(terms)]
  if (!length(terms)) return(character(0))

  if (is.null(scores)) scores <- rep(0, length(terms))
  scores <- as.numeric(scores)
  author_terms <- unique(as.character(author_terms %||% character(0)))

  ord <- order(
    !(terms %in% author_terms),
    -zh_tail_nchar(terms),
    -scores,
    terms
  )

  terms <- terms[ord]
  scores <- scores[ord]

  kept <- character(0)
  for (tm in terms) {
    if (!zh_tail_contains_subset_relation(tm, kept)) {
      kept <- c(kept, tm)
      if (length(kept) >= top_n) break
    }
  }
  kept
}

split_author_keywords_zh <- function(keyword_line, min_chars = 3L, max_chars = 10L) {
  x <- trimws(as.character(keyword_line %||% ""))
  if (!nzchar(x)) return(character(0))

  ctx <- replace_strong_weak_breaks_zh(x)
  parts <- unlist(strsplit(ctx, "[,，]+", perl = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  parts <- vapply(parts, zh_tail_clean_single_term, character(1), min_chars = min_chars, max_chars = max_chars)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  parts <- parts[vapply(parts, zh_tail_valid_length, logical(1), context = ctx, min_chars = min_chars, max_chars = max_chars)]
  unique(parts)
}

keyword_subsets_zh <- function(keywords, min_chars = 2L) {
  kw <- unique(as.character(keywords %||% character(0)))
  kw <- kw[nzchar(kw)]
  out <- character(0)
  for (k in kw) {
    n <- zh_tail_nchar(k)
    if (n < 4) next
    chars <- strsplit(k, "", fixed = TRUE)[[1]]
    for (i in seq_len(n)) {
      for (j in i:n) {
        len <- j - i + 1L
        if (len >= min_chars && len < n) {
          out <- c(out, paste(chars[i:j], collapse = ""))
        }
      }
    }
  }
  unique(out[zh_tail_nchar(out) >= min_chars])
}

suffix_terms_zh <- c(
  "分析","模式","模型","方法","機制","系統","結構","理論","架構","效應","現象","結果","問題","策略","流程","設計",
  "病","症","癌","炎","瘤","疾病","病變","病理","細胞","組織","基因","蛋白","酶","受體","感染","反應","作用","表現","表達",
  "演算法","算法","回歸","分類","分群","聚類","預測","指標","變數","參數","分布","機率","相關性","關聯性",
  "信度","效度","測量","評估"
)

zh_tail_valid_length <- function(term, context = NULL, min_chars = 3L, max_chars = 10L) {
  term <- zh_tail_keep_han(term)
  n <- zh_tail_nchar(term)
  if (!nzchar(term) || n < min_chars || n > max_chars) return(FALSE)
  if (n %in% c(4L, 6L, 8L, 10L)) return(TRUE)
  if (!(n %in% c(3L, 5L, 7L, 9L))) return(FALSE)
  if (is.null(context) || !nzchar(context)) return(FALSE)
  grepl(paste0("(^|，)", term, "(，|$)"), context, perl = TRUE)
}

is_noun_tail_term_zh <- function(x, suffix_terms = suffix_terms_zh) {
  x <- zh_tail_keep_han(x)
  if (!nzchar(x)) return(FALSE)
  any(vapply(suffix_terms, function(suf) {
    grepl(paste0(suf, "$"), x, perl = TRUE) && zh_tail_nchar(x) >= (zh_tail_nchar(suf) + 2L)
  }, logical(1)))
}

replace_strong_weak_breaks_zh <- function(text) {
  x <- zh_tail_normalize_spaces(text)
  x <- gsub("[的了最更太與及和並且而且或及其以於著過呢嗎吧呀啊哦喔欸很最更太甚頗極稍較越真挺和與及並且或而但若則因故]", "，", x, perl = TRUE)
  x <- gsub("[^\\p{Han}]+", "，", x, perl = TRUE)
  x <- gsub("，{2,}", "，", x, perl = TRUE)
  x <- gsub("^，+|，+$", "", x, perl = TRUE)
  x
}

split_text_units_zh <- function(text) {
  text <- zh_tail_normalize_spaces(text)
  paras <- unlist(strsplit(text, "\\n{2,}|\\r\\n\\r\\n", perl = TRUE), use.names = FALSE)
  paras <- trimws(paras)
  paras <- paras[nzchar(paras)]
  if (!length(paras)) {
    paras <- unlist(strsplit(text, "(?<=[。！？；!?;])|\\n+", perl = TRUE), use.names = FALSE)
    paras <- trimws(paras)
    paras <- paras[nzchar(paras)]
  }
  paras[zh_tail_nchar(paras) >= 10]
}

final_term_filter_zh <- function(x, min_chars = 3L, max_chars = 10L, context = NULL) {
  x <- as.character(x %||% character(0))
  x <- vapply(x, zh_tail_clean_single_term, character(1), min_chars = min_chars, max_chars = max_chars)
  x <- x[!is.na(x) & nzchar(x)]
  if (!is.null(context) && nzchar(as.character(context))) {
    x <- x[vapply(x, zh_tail_valid_length, logical(1), context = context, min_chars = min_chars, max_chars = max_chars)]
  }
  x <- x[!vapply(x, zh_tail_is_blacklisted, logical(1))]
  unique(x)
}

zh_tail_suffix_candidates_from_part <- function(part,
                                                context,
                                                suffix_terms = suffix_terms_zh,
                                                min_chars = 3L,
                                                max_chars = 10L) {
  part <- zh_tail_keep_han(part)
  n <- zh_tail_nchar(part)
  if (!nzchar(part) || n < min_chars) return(character(0))

  out <- character(0)
  matched <- FALSE
  suffix_terms <- suffix_terms[order(-zh_tail_nchar(suffix_terms), suffix_terms)]

  for (suf in suffix_terms) {
    if (grepl(paste0(suf, "$"), part, perl = TRUE)) {
      matched <- TRUE
      min_needed <- zh_tail_nchar(suf) + 2L
      for (L in c(4L, 6L, 8L, 10L)) {
        if (L <= n && L >= max(min_chars, min_needed) && L <= max_chars) {
          cand <- substring(part, n - L + 1L, n)
          if (zh_tail_valid_length(cand, context = context, min_chars = min_chars, max_chars = max_chars)) {
            out <- c(out, cand)
          }
        }
      }
      if (n >= min_needed && zh_tail_valid_length(part, context = context, min_chars = min_chars, max_chars = max_chars)) {
        out <- c(out, part)
      }
    }
  }

  if (!matched && zh_tail_valid_length(part, context = context, min_chars = min_chars, max_chars = max_chars)) {
    out <- c(out, part)
  }

  unique(out)
}

extract_tail_terms_from_unit_zh <- function(unit_text,
                                            author_keywords = character(0),
                                            keyword_subsets = character(0),
                                            suffix_terms = suffix_terms_zh,
                                            min_chars = 3L,
                                            max_chars = 10L) {
  x <- replace_strong_weak_breaks_zh(unit_text)
  if (!nzchar(x)) return(character(0))

  parts <- unlist(strsplit(x, "[,，]+", perl = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  parts <- unique(zh_tail_keep_han(parts))
  parts <- parts[nzchar(parts)]

  part_hits <- unlist(lapply(parts, zh_tail_suffix_candidates_from_part,
                             context = x,
                             suffix_terms = suffix_terms,
                             min_chars = min_chars,
                             max_chars = max_chars), use.names = FALSE)

  subset_hits <- parts[
    zh_tail_nchar(parts) >= 4 &
      vapply(parts, function(p) {
        any(vapply(keyword_subsets, function(s) nzchar(s) && grepl(s, p, fixed = TRUE), logical(1)))
      }, logical(1)) &
      vapply(parts, zh_tail_valid_length, logical(1), context = x, min_chars = min_chars, max_chars = max_chars)
  ]

  kw_hits <- author_keywords[vapply(author_keywords, function(k) nzchar(k) && grepl(k, x, fixed = TRUE), logical(1))]

  out <- unique(c(kw_hits, part_hits, subset_hits))
  out <- final_term_filter_zh(out, min_chars = min_chars, max_chars = max_chars, context = x)
  out <- out[!vapply(out, zh_tail_is_blacklisted, logical(1)) | out %in% author_keywords]
  out <- zh_tail_select_subset_free_terms(out, scores = zh_tail_nchar(out), author_terms = author_keywords)
  out
}

default_section_weights_zh <- function() {
  list(keyword = 100, title = 30, abstract = 15, body = 5)
}

extract_sections_simple_zh <- function(text) {
  txt <- zh_tail_normalize_spaces(text)
  lines <- unlist(strsplit(txt, "\n", fixed = TRUE), use.names = FALSE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  title <- if (length(lines)) lines[1] else ""
  kw_line <- extract_author_keyword_line_zh(lines)
  kw_idx <- grep("^\\s*(關鍵字|关键字|關鍵詞|关键词)\\s*[:：]?", lines, perl = TRUE)
  abs_idx <- grep("^\\s*(摘要|Abstract)\\s*[:：]?", lines, perl = TRUE)
  ref_idx <- grep("^\\s*(參考文獻|参考文献|References)\\s*[:：]?", lines, perl = TRUE)

  abstract <- ""
  if (length(abs_idx)) {
    start <- abs_idx[1]
    stop <- min(c(if (length(kw_idx)) kw_idx[kw_idx > start] else integer(0),
                  if (length(ref_idx)) ref_idx[ref_idx > start] else integer(0),
                  length(lines) + 1)) - 1
    if (stop >= start) abstract <- paste(lines[start:stop], collapse = " ")
  }

  body_start <- if (length(kw_idx)) kw_idx[1] + 1 else if (length(abs_idx)) abs_idx[1] + 1 else 2
  body_end <- if (length(ref_idx)) ref_idx[1] - 1 else length(lines)
  body <- if (body_end >= body_start) paste(lines[body_start:body_end], collapse = "\n") else txt

  list(title = title, abstract = abstract, body = body, keyword_line = kw_line)
}

zh_tail_occurrence_rows <- function(terms, source, unit_id) {
  terms <- unique(as.character(terms %||% character(0)))
  terms <- terms[nzchar(terms)]
  if (!length(terms)) return(data.frame(term = character(0), source = character(0), unit_id = character(0), stringsAsFactors = FALSE))
  data.frame(term = terms, source = source, unit_id = unit_id, stringsAsFactors = FALSE)
}

build_weighted_nodes_zh <- function(text,
                                    top_n = 50,
                                    min_chars = 3L,
                                    max_chars = 10L,
                                    section_weights = default_section_weights_zh()) {
  sec <- extract_sections_simple_zh(text)
  author_keywords <- split_author_keywords_zh(sec$keyword_line, min_chars = min_chars, max_chars = max_chars)
  whitelist_hits <- zh_tail_whitelist_terms[vapply(zh_tail_whitelist_terms, function(k) nzchar(k) && grepl(k, text, fixed = TRUE), logical(1))]
  author_keywords <- unique(c(author_keywords, whitelist_hits))
  author_keywords <- author_keywords[!vapply(author_keywords, zh_tail_is_blacklisted, logical(1))]
  kw_subsets <- keyword_subsets_zh(author_keywords, min_chars = 2L)

  title_terms <- extract_tail_terms_from_unit_zh(sec$title, author_keywords, kw_subsets, min_chars = min_chars, max_chars = max_chars)
  abs_units <- split_text_units_zh(sec$abstract)
  body_units <- split_text_units_zh(sec$body)

  abs_hits_list <- lapply(abs_units, extract_tail_terms_from_unit_zh,
                          author_keywords = author_keywords,
                          keyword_subsets = kw_subsets,
                          min_chars = min_chars,
                          max_chars = max_chars)
  body_hits_list <- lapply(body_units, extract_tail_terms_from_unit_zh,
                           author_keywords = author_keywords,
                           keyword_subsets = kw_subsets,
                           min_chars = min_chars,
                           max_chars = max_chars)

  rows <- list(
    zh_tail_occurrence_rows(author_keywords, "keyword", paste0("kw_", seq_along(author_keywords))),
    zh_tail_occurrence_rows(title_terms, "title", "title_1")
  )

  if (length(abs_hits_list)) {
    rows <- c(rows, lapply(seq_along(abs_hits_list), function(i) zh_tail_occurrence_rows(abs_hits_list[[i]], "abstract", paste0("abstract_", i))))
  }
  if (length(body_hits_list)) {
    rows <- c(rows, lapply(seq_along(body_hits_list), function(i) zh_tail_occurrence_rows(body_hits_list[[i]], "body", paste0("body_", i))))
  }

  occ <- do.call(rbind, rows)
  if (is.null(occ) || !nrow(occ)) {
    return(list(
      keywords = author_keywords,
      nodes = data.frame(name = character(0), value = numeric(0), source = character(0), occ_n = integer(0), stringsAsFactors = FALSE),
      title_terms = title_terms,
      abstract_terms = unique(unlist(abs_hits_list, use.names = FALSE)),
      body_terms = unique(unlist(body_hits_list, use.names = FALSE)),
      body_units = body_units
    ))
  }

  # Preserve row length when cleaning occurrence terms.
  # final_term_filter_zh() returns a unique filtered vector and can be shorter
  # than nrow(occ), causing: replacement data has X rows, data has Y.
  occ$term <- vapply(
    occ$term,
    zh_tail_clean_single_term,
    character(1),
    min_chars = min_chars,
    max_chars = max_chars
  )
  occ <- occ[!is.na(occ$term) & nzchar(occ$term), , drop = FALSE]
  occ <- occ[!vapply(occ$term, zh_tail_is_blacklisted, logical(1)) | occ$term %in% author_keywords, , drop = FALSE]

  if (!nrow(occ)) {
    return(list(
      keywords = author_keywords,
      nodes = data.frame(name = character(0), value = numeric(0), source = character(0), occ_n = integer(0), stringsAsFactors = FALSE),
      title_terms = title_terms,
      abstract_terms = unique(unlist(abs_hits_list, use.names = FALSE)),
      body_terms = unique(unlist(body_hits_list, use.names = FALSE)),
      body_units = body_units
    ))
  }

  terms <- sort(unique(occ$term))
  nodes <- lapply(terms, function(term) {
    rows_term <- occ[occ$term == term, , drop = FALSE]
    source_counts <- table(rows_term$source)
    value <-
      (as.numeric(source_counts["keyword"] %||% 0) * section_weights$keyword) +
      (as.numeric(source_counts["title"] %||% 0) * section_weights$title) +
      (as.numeric(source_counts["abstract"] %||% 0) * section_weights$abstract) +
      (as.numeric(source_counts["body"] %||% 0) * section_weights$body)

    data.frame(
      name = term,
      value = value,
      source = paste(sort(unique(rows_term$source)), collapse = ";"),
      occ_n = sum(rows_term$source != "keyword"),
      is_author_keyword = term %in% author_keywords,
      stringsAsFactors = FALSE
    )
  })
  nodes <- do.call(rbind, nodes)

  nodes <- nodes[nodes$is_author_keyword | nodes$occ_n >= 2, , drop = FALSE]
  if (!nrow(nodes)) {
    nodes <- nodes[nodes$is_author_keyword, , drop = FALSE]
  }

  if (!nrow(nodes)) {
    return(list(
      keywords = author_keywords,
      nodes = data.frame(name = character(0), value = numeric(0), source = character(0), occ_n = integer(0), stringsAsFactors = FALSE),
      title_terms = title_terms,
      abstract_terms = unique(unlist(abs_hits_list, use.names = FALSE)),
      body_terms = unique(unlist(body_hits_list, use.names = FALSE)),
      body_units = body_units
    ))
  }

  author_nodes <- nodes[nodes$is_author_keyword, , drop = FALSE]
  non_author_nodes <- nodes[!nodes$is_author_keyword, , drop = FALSE]

  author_nodes <- author_nodes[order(
    -zh_tail_nchar(author_nodes$name),
    -author_nodes$value,
    -author_nodes$occ_n,
    author_nodes$name
  ), , drop = FALSE]
  author_nodes <- author_nodes[!duplicated(author_nodes$name), , drop = FALSE]

  if (length(author_keywords)) {
    missing_author <- setdiff(unique(author_keywords), author_nodes$name)
    if (length(missing_author)) {
      author_nodes <- rbind(author_nodes, data.frame(
        name = missing_author,
        value = rep(section_weights$keyword, length(missing_author)),
        source = rep("keyword", length(missing_author)),
        occ_n = rep(0L, length(missing_author)),
        is_author_keyword = TRUE,
        stringsAsFactors = FALSE
      ))
    }
  }

  if (!nrow(author_nodes) && length(author_keywords)) {
    author_nodes <- data.frame(
      name = unique(author_keywords),
      value = rep(section_weights$keyword, length(unique(author_keywords))),
      source = rep("keyword", length(unique(author_keywords))),
      occ_n = rep(0L, length(unique(author_keywords))),
      is_author_keyword = TRUE,
      stringsAsFactors = FALSE
    )
  }

  if (nrow(non_author_nodes)) {
    non_author_nodes <- non_author_nodes[order(
      -zh_tail_nchar(non_author_nodes$name),
      -non_author_nodes$value,
      -non_author_nodes$occ_n,
      non_author_nodes$name
    ), , drop = FALSE]
  }

  kept_non_author_idx <- integer(0)
  protected_terms <- unique(author_nodes$name)

  if (nrow(non_author_nodes)) {
    for (i in seq_len(nrow(non_author_nodes))) {
      tm <- non_author_nodes$name[i]
      if (!zh_tail_contains_subset_relation(tm, protected_terms)) {
        kept_non_author_idx <- c(kept_non_author_idx, i)
        protected_terms <- c(protected_terms, tm)
      }
      if ((nrow(author_nodes) + length(kept_non_author_idx)) >= top_n) break
    }
  }

  if (length(kept_non_author_idx)) {
    non_author_nodes <- non_author_nodes[kept_non_author_idx, , drop = FALSE]
  } else {
    non_author_nodes <- non_author_nodes[0, , drop = FALSE]
  }

  nodes <- rbind(author_nodes, non_author_nodes)
  if (nrow(nodes) > top_n) {
    nodes <- nodes[seq_len(top_n), , drop = FALSE]
  }

  nodes <- nodes[, c("name", "value", "source", "occ_n"), drop = FALSE]
  rownames(nodes) <- NULL

  list(
    keywords = author_keywords,
    nodes = nodes,
    title_terms = unique(title_terms),
    abstract_terms = unique(unlist(abs_hits_list, use.names = FALSE)),
    body_terms = unique(unlist(body_hits_list, use.names = FALSE)),
    body_units = body_units
  )
}

build_weighted_edges_zh <- function(text,
                                    selected_terms,
                                    author_keywords = character(0),
                                    min_chars = 3L,
                                    max_chars = 10L) {
  sec <- extract_sections_simple_zh(text)
  kw_subsets <- keyword_subsets_zh(author_keywords, min_chars = 2L)
  units <- split_text_units_zh(sec$body)
  selected_terms <- unique(final_term_filter_zh(selected_terms, min_chars = min_chars, max_chars = max_chars))

  if (!length(units) || length(selected_terms) < 2) {
    return(data.frame(from = character(0), to = character(0), weight = numeric(0), stringsAsFactors = FALSE))
  }

  pairs_list <- list()
  idx <- 1L
  for (u in units) {
    hits <- unique(extract_tail_terms_from_unit_zh(u, author_keywords, kw_subsets, min_chars = min_chars, max_chars = max_chars))
    hits <- sort(unique(intersect(hits, selected_terms)))
    if (length(hits) >= 2) {
      cmb <- utils::combn(hits, 2, simplify = FALSE)
      for (p in cmb) {
        pairs_list[[idx]] <- data.frame(from = p[1], to = p[2], weight = 1, stringsAsFactors = FALSE)
        idx <- idx + 1L
      }
    }
  }

  if (!length(pairs_list)) {
    return(data.frame(from = character(0), to = character(0), weight = numeric(0), stringsAsFactors = FALSE))
  }

  ed <- do.call(rbind, pairs_list)
  ed <- aggregate(list(weight = ed$weight), by = list(from = ed$from, to = ed$to), FUN = sum)
  ed <- ed[order(-ed$weight, ed$from, ed$to), , drop = FALSE]
  rownames(ed) <- NULL
  ed
}

run_zh_main_document_tail_pipeline <- function(text,
                                               top_n = 50,
                                               min_chars = 3L,
                                               max_chars = 10L) {
  text <- zh_tail_normalize_spaces(text)
  node_out <- build_weighted_nodes_zh(text, top_n = top_n, min_chars = min_chars, max_chars = max_chars)
  edges <- build_weighted_edges_zh(
    text = text,
    selected_terms = node_out$nodes$name,
    author_keywords = node_out$keywords,
    min_chars = min_chars,
    max_chars = max_chars
  )

  list(
    keywords = node_out$keywords,
    nodes = node_out$nodes,
    edges = edges,
    title_terms = node_out$title_terms,
    abstract_terms = node_out$abstract_terms,
    body_terms = node_out$body_terms,
    rule = paste(
      "Chinese emitted terms keep Chinese only; non-author terms need frequency >= 2;",
      "author keywords are pinned into the final set; non-author terms are subset-free; even lengths 4/6/8/10 are preferred;",
      "odd lengths 3/5/7/9 are allowed only when comma-delimited;",
      "tail-suffix terms must include at least 2 preceding Chinese chars."
    )
  )
}
