`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ------------------------------------------------------------
# Comma-first English semantic keyword engine (strict blacklist patch)
# Workflow
# 1) Read title / keyword block / abstract / body
# 2) Convert full text to comma-separated units
# 3) Keep comma-units with 1-4 words (single-word terms limited later)
# 4) Count frequency on full document text (title + keyword block + abstract + body)
# 5) Keep top 50 by frequency
# 6) Remove contained terms (prefer higher-frequency / longer phrases)
# 7) Keep top 20, with at most 5 single-word terms
# 8) Force author-defined keywords back in
# 9) Cap final node set at 25
# ------------------------------------------------------------

en_digit_keep_terms <- c(
  "gpt-4","gpt4","gpt-4o","gpt4o","gpt-5","gpt5",
  "phq-2","phq2","phq-9","phq9","gad-2","gad2","gad-7","gad7",
  "sf-12","sf12","sf-36","sf36","panss-16","panss16",
  "auc-roc","covid-19","covid19","mmse-2","mmse2"
)

en_tail_blacklist_terms <- c(
  "this study","in this study","the present study","results showed",
  "our results","we found","it was found","was performed","were performed",
  "was used","were used","can be used","could be used",
  "is associated with","are associated with","based on","according to",
  "in order to","as shown in","shown in","statistical analysis",
  "further analysis","for example","such as","therefore","however",
  "although","another","widely","also","moreover","furthermore",
  "including","included","using","used","through","via","between",
  "among","within","without","under","over","from","into","onto",
  "upon","than","that","which","whose","while","whereas","because",
  "before","after","many","and","all","may","the","a","an",
  "is","are","was","were","be","been","being","am","to","of","for",
  "on","in","at","by","as","or","if","per",
  "http","https","www","html","xml","json","csv","tsv","xlsx","xls",
  "pdf","docx","doi","org","com","io","gov","edu","raw",
  "github","metadata","author metadata","journal homepage","sciencedirect"
)

en_cut_terms <- unique(c(
  en_tail_blacklist_terms,
  "keywords","keyword","author keywords","abstract","introduction",
  "methods","materials","results","discussion","conclusion",
  "references","bibliography","copyright","corresponding author",
  "available online","received","accepted","published",
  "journal homepage","article history","open access",
  "in","on","at","by","for","to","of","with"
))

en_blacklist_tokens <- c(
  "and","many","all","may","that","which","whose","while","whereas",
  "because","before","after","using","used","within","between","among",
  "http","https","www","html","xml","json","csv","tsv","xlsx","xls",
  "pdf","docx","doi","org","com","io","gov","edu","raw","github",
  "metadata","tion","zation","ization","sion","ment","ity","ware","analy","port","im","sum","marization","the","a","an","is","are","was","were","be","been","being",
  "am","to","of","for","on","in","at","by","as","or","if","via","per",
  "from","into","onto","upon","than","through","with","without"
)

en_short_keep_tokens <- c("ai","nlp","r","us","uk")

normalize_spaces_en <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("[\r\n\t]+", " ", x, perl = TRUE)
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  trimws(x)
}

escape_regex_en <- function(x) {
  gsub("([.^$|()\\[\\]{}*+?\\\\-])", "\\\\\\1", x, perl = TRUE)
}

protect_keep_terms_en <- function(text, keep_terms = en_digit_keep_terms) {
  x <- normalize_spaces_en(text)
  if (!length(keep_terms)) return(list(text = x, map = character(0)))
  keep_terms <- unique(tolower(normalize_spaces_en(keep_terms)))
  keep_terms <- keep_terms[nzchar(keep_terms)]
  keep_terms <- keep_terms[order(nchar(keep_terms), decreasing = TRUE)]
  ph_map <- character(0)
  for (i in seq_along(keep_terms)) {
    kt <- keep_terms[[i]]
    ph <- paste0(" keeptermplaceholder", i, " ")
    pat <- paste0("\\b", escape_regex_en(kt), "\\b")
    x <- gsub(pat, ph, x, perl = TRUE, ignore.case = TRUE)
    ph_map[ph] <- kt
  }
  list(text = x, map = ph_map)
}

restore_keep_terms_en <- function(text, ph_map = character(0)) {
  x <- as.character(text %||% "")
  if (!length(ph_map)) return(normalize_spaces_en(x))
  for (ph in names(ph_map)) x <- gsub(ph, paste0(" ", ph_map[[ph]], " "), x, fixed = TRUE)
  normalize_spaces_en(x)
}

normalize_term_en <- function(x, keep_terms = en_digit_keep_terms) {
  tmp <- protect_keep_terms_en(x, keep_terms = keep_terms)
  z <- tolower(tmp$text)
  z <- gsub("[’']s\\b", "", z, perl = TRUE)
  z <- gsub("[^a-z0-9\\- ]+", " ", z, perl = TRUE)
  z <- gsub("\\s+", " ", z, perl = TRUE)
  z <- trimws(z)
  restore_keep_terms_en(z, tmp$map)
}

is_blank <- function(x) is.na(x) | trimws(x) == ""

line_norm_simple <- function(x) {
  y <- tolower(x)
  y <- gsub("[^a-z0-9 ]+", " ", y, perl = TRUE)
  y <- gsub("\\s+", " ", y, perl = TRUE)
  trimws(y)
}

is_section_heading_en <- function(x) {
  z <- trimws(x)
  y <- line_norm_simple(x)
  if (!nzchar(y)) return(FALSE)
  shortish <- length(strsplit(y, "\\s+", perl = TRUE)[[1]]) <= 12L
  numeric_head <- grepl("^\\d+[.)]?\\s+", z, perl = TRUE)
  named_head <- grepl("^(introduction|background|methods|materials|results|discussion|conclusion|references|bibliography)\\b", y, perl = TRUE)
  shortish && (numeric_head || named_head)
}

extract_sections_simple_en <- function(text) {
  lines <- unlist(strsplit(text %||% "", "\\r?\\n", perl = TRUE), use.names = FALSE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  keyword_idx <- grep("^(keywords?|author keywords?)\\s*[:：]?$", lines, ignore.case = TRUE, perl = TRUE)
  abstract_idx <- grep("^abstract\\s*[:：]?$", lines, ignore.case = TRUE, perl = TRUE)
  refs_idx <- grep("^(references|bibliography)\\s*[:：]?$", lines, ignore.case = TRUE, perl = TRUE)

  title <- if (length(lines)) lines[1] else ""

  keyword_block <- character(0)
  if (length(keyword_idx)) {
    i <- keyword_idx[1]
    line_i <- trimws(lines[i])
    if (grepl("[:：]", line_i)) {
      rhs <- sub("^[^:：]*[:：]\\s*", "", line_i)
      if (!is_blank(rhs)) keyword_block <- c(keyword_block, rhs)
    }
    if (i < length(lines)) {
      j <- i + 1L
      while (j <= length(lines)) {
        lj <- trimws(lines[j])
        if (is_blank(lj)) break
        if (is_section_heading_en(lj)) break
        if (grepl("^(abstract|references|bibliography)\\b", tolower(lj), perl = TRUE)) break
        if (nchar(lj) > 150L) break
        keyword_block <- c(keyword_block, lj)
        j <- j + 1L
      }
    }
  }
  keyword_line <- paste(keyword_block, collapse = "; ")

  abs_start <- if (length(abstract_idx)) abstract_idx[1] else NA_integer_
  refs_start <- if (length(refs_idx)) refs_idx[1] else length(lines) + 1L

  abstract <- ""
  body <- ""
  references <- ""

  if (!is.na(abs_start)) {
    abs_end <- min(refs_start - 1L, length(lines))
    if (abs_end > abs_start) abstract <- paste(lines[(abs_start + 1L):abs_end], collapse = " ")
    body_start <- abs_end + 1L
  } else {
    body_start <- 2L
  }

  if (body_start <= length(lines)) {
    body_end <- min(refs_start - 1L, length(lines))
    if (body_end >= body_start) body <- paste(lines[body_start:body_end], collapse = " ")
  }

  if (length(refs_idx)) {
    ref_start <- refs_idx[1] + 1L
    if (ref_start <= length(lines)) references <- paste(lines[ref_start:length(lines)], collapse = "\n")
  }

  list(title = title, keyword_line = keyword_line, abstract = abstract, body = body, references = references)
}

split_author_keywords_en <- function(keyword_line, max_words = 4L, keep_terms = en_digit_keep_terms) {
  x <- normalize_spaces_en(keyword_line)
  if (!nzchar(x)) return(character(0))
  x <- sub("^(keywords?|author keywords?)\\s*[:：]?\\s*", "", x, ignore.case = TRUE, perl = TRUE)
  parts <- unlist(strsplit(x, "[,;|]+", perl = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  parts <- vapply(parts, function(p) {
    p <- normalize_term_en(p, keep_terms = keep_terms)
    if (!nzchar(p)) return(NA_character_)
    n_words <- length(strsplit(p, "\\s+", perl = TRUE)[[1]])
    if (n_words < 1L || n_words > max_words) return(NA_character_)
    p
  }, character(1))
  parts <- unique(parts[!is.na(parts) & nzchar(parts)])
  parts
}

looks_like_metadata_en <- function(term) {
  x <- normalize_term_en(term)
  if (!nzchar(x)) return(TRUE)
  if (grepl("^(http|https|www|doi|org|com|gov|edu|io|raw|github)$", x, perl = TRUE)) return(TRUE)
  if (grepl("(journal homepage|author metadata|metadata|copyright|sciencedirect|available online)", x, ignore.case = TRUE, perl = TRUE)) return(TRUE)
  toks <- unlist(strsplit(x, "\\s+", perl = TRUE), use.names = FALSE)
  toks <- toks[nzchar(toks)]
  if (!length(toks)) return(TRUE)
  if (all(toks %in% normalize_term_en(en_blacklist_tokens))) return(TRUE)
  if (length(toks) == 1L && (nchar(toks[1]) <= 2L)) return(TRUE)
  if (contains_blacklisted_content_en(x)) return(TRUE)
  FALSE
}

comma_first_document_en <- function(text, keep_terms = en_digit_keep_terms, cut_terms = en_cut_terms) {
  x <- normalize_spaces_en(text)
  tmp <- protect_keep_terms_en(x, keep_terms = keep_terms)
  x <- tolower(tmp$text)

  x <- gsub("\\b[a-z\\-]*\\d+[a-z0-9\\-]*\\b", ",", x, perl = TRUE)
  x <- gsub("\\b\\d+(st|nd|rd|th)\\b", ",", x, perl = TRUE, ignore.case = TRUE)
  x <- gsub("\\b\\d+(\\.\\d+)+\\b", ",", x, perl = TRUE)
  x <- gsub("\\b\\d+\\b", ",", x, perl = TRUE)
  x <- gsub("[_~@#$%^&*=+/\\\\|:;<>!?()\\[\\]{}\"“”‘’`]+", ",", x, perl = TRUE)
  x <- gsub("[,.-]+", ",", x, perl = TRUE)

  if (length(cut_terms)) {
    cut_terms <- unique(normalize_term_en(cut_terms, keep_terms = keep_terms))
    cut_terms <- cut_terms[nzchar(cut_terms)]
    cut_terms <- cut_terms[order(nchar(cut_terms), decreasing = TRUE)]
    for (bt in cut_terms) {
      pat <- paste0("\\b", escape_regex_en(bt), "\\b")
      x <- gsub(pat, ",", x, perl = TRUE, ignore.case = TRUE)
    }
  }

  x <- gsub("\\s*,\\s*", ",", x, perl = TRUE)
  x <- gsub(",+", ",", x, perl = TRUE)
  x <- trimws(x, which = "both")
  restore_keep_terms_en(x, tmp$map)
}

contains_blacklist_token_en <- function(term, blacklist_tokens = en_blacklist_tokens) {
  toks <- unlist(strsplit(normalize_term_en(term), "\\s+", perl = TRUE), use.names = FALSE)
  toks <- toks[nzchar(toks)]
  any(toks %in% normalize_term_en(blacklist_tokens))
}

is_keep_term_exact_en <- function(term, keep_terms = en_digit_keep_terms) {
  x <- normalize_term_en(term, keep_terms = keep_terms)
  kt <- unique(normalize_term_en(keep_terms, keep_terms = keep_terms))
  nzchar(x) && x %in% kt
}

contains_blacklisted_content_en <- function(term,
                                            keep_terms = en_digit_keep_terms,
                                            blacklist_tokens = en_blacklist_tokens,
                                            short_keep_tokens = en_short_keep_tokens) {
  raw <- normalize_spaces_en(term)
  if (!nzchar(raw)) return(TRUE)

  norm <- normalize_term_en(raw, keep_terms = keep_terms)
  if (!nzchar(norm)) return(TRUE)

  if (is_keep_term_exact_en(norm, keep_terms = keep_terms)) return(FALSE)
  if (grepl("[0-9]", norm, perl = TRUE)) return(TRUE)

  toks <- unlist(strsplit(norm, "\\s+", perl = TRUE), use.names = FALSE)
  toks <- toks[nzchar(toks)]
  if (!length(toks)) return(TRUE)

  bad_tokens <- unique(normalize_term_en(blacklist_tokens, keep_terms = keep_terms))
  if (any(toks %in% bad_tokens)) return(TRUE)

  short_keep <- unique(normalize_term_en(short_keep_tokens, keep_terms = keep_terms))
  if (any(nchar(toks) <= 2L & !(toks %in% short_keep))) return(TRUE)

  if (any(!grepl("^[a-z][a-z-]*$", toks, perl = TRUE))) return(TRUE)

  FALSE
}


is_good_single_keyword_en <- function(term) {
  x <- normalize_term_en(term)
  if (!nzchar(x)) return(FALSE)
  if (x %in% c("nlp", "tall", "ai")) return(TRUE)
  if (x %in% c("accessibility", "reproducibility", "reliability", "tokenization", "lemmatization", "visualization", "analysis", "application", "assistant", "processing", "modeling", "modelling")) return(TRUE)
  grepl("(ability|ibility|bility)$", x, ignore.case = TRUE, perl = TRUE)
}

has_bad_keyword_fragment_en <- function(term) {
  x <- normalize_term_en(term)
  if (!nzchar(x)) return(TRUE)
  if (x %in% c("tion", "zation", "ization", "sion", "ment", "ity", "ware", "analy", "port", "im", "sum", "marization")) return(TRUE)
  grepl("\\b(ware|analy|port|im|sum|marization)\\b", x, ignore.case = TRUE, perl = TRUE)
}

starts_bad_keyword_phrase_en <- function(term) {
  x <- normalize_term_en(term)
  bad_start <- c("accompanied", "automatically", "collapsible", "correspondence", "facilitating", "further", "generating", "hierarchical", "inputs", "institutional", "intuitive", "optional", "despite", "including", "maintains", "demonstrating", "analyzing", "combining", "providing", "enabling", "supporting")
  grepl(paste0("^(", paste(bad_start, collapse = "|"), ")\\b"), x, ignore.case = TRUE, perl = TRUE)
}

unit_ok_en <- function(unit, max_words = 4L, keep_terms = en_digit_keep_terms) {
  u <- normalize_term_en(unit, keep_terms = keep_terms)
  if (!nzchar(u)) return(FALSE)
  if (has_bad_keyword_fragment_en(u)) return(FALSE)
  if (starts_bad_keyword_phrase_en(u)) return(FALSE)
  n_words <- length(strsplit(u, "\\s+", perl = TRUE)[[1]])
  if (n_words < 1L || n_words > max_words) return(FALSE)
  if (n_words == 1L && !is_good_single_keyword_en(u)) return(FALSE)
  if (n_words == 4L) {
    has_anchor <- grepl("\\b(nlp|shiny|text|data|visualization|tokenization|lemmatization|reproducibility|reliability|modeling|modelling|analysis|assistant|dependencies|embeddings?)\\b", u, ignore.case = TRUE, perl = TRUE)
    if (!has_anchor) return(FALSE)
  }
  if (contains_blacklisted_content_en(u, keep_terms = keep_terms)) {
    if (!(n_words == 1L && is_good_single_keyword_en(u))) return(FALSE)
  }
  if (looks_like_metadata_en(u)) return(FALSE)
  toks <- unlist(strsplit(u, "\\s+", perl = TRUE), use.names = FALSE)
  toks <- toks[nzchar(toks)]
  if (length(toks) == 1L && nchar(toks[1]) < 3L && !(toks[1] %in% en_short_keep_tokens)) return(FALSE)
  TRUE
}

extract_units_from_text_en <- function(text, max_words = 4L, keep_terms = en_digit_keep_terms, cut_terms = en_cut_terms) {
  x <- comma_first_document_en(text, keep_terms = keep_terms, cut_terms = cut_terms)
  parts <- unlist(strsplit(x, ",+", perl = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  parts <- vapply(parts, normalize_term_en, character(1), keep_terms = keep_terms)
  parts <- parts[nzchar(parts)]
  parts <- parts[vapply(parts, unit_ok_en, logical(1), max_words = max_words, keep_terms = keep_terms)]
  parts
}

count_phrase_in_text_en <- function(phrase, text, keep_terms = en_digit_keep_terms) {
  x <- normalize_term_en(text, keep_terms = keep_terms)
  p <- normalize_term_en(phrase, keep_terms = keep_terms)
  if (!nzchar(x) || !nzchar(p)) return(0L)
  pat <- paste0("\\b", escape_regex_en(p), "\\b")
  m <- gregexpr(pat, x, perl = TRUE, ignore.case = TRUE)[[1]]
  if (length(m) == 1L && m[1] == -1L) return(0L)
  as.integer(length(m))
}

contains_subset_relation_en <- function(x, kept) {
  if (!length(kept)) return(FALSE)
  any(vapply(kept, function(k) {
    identical(x, k) || grepl(paste0("\\b", escape_regex_en(x), "\\b"), k, perl = TRUE) || grepl(paste0("\\b", escape_regex_en(k), "\\b"), x, perl = TRUE)
  }, logical(1)))
}

select_subset_free_terms_en <- function(terms, scores = NULL, top_n = Inf) {
  terms <- normalize_term_en(terms)
  terms <- terms[nzchar(terms)]
  if (!length(terms)) return(character(0))
  if (is.null(scores)) scores <- rep(0, length(terms))
  scores <- as.numeric(scores)
  idx <- match(unique(terms), terms)
  terms <- terms[idx]
  scores <- scores[idx]
  n_words <- vapply(strsplit(terms, "\\s+", perl = TRUE), length, integer(1))
  ord <- order(-scores, -n_words, -nchar(terms), terms)
  terms <- terms[ord]
  kept <- character(0)
  for (tm in terms) {
    if (!contains_subset_relation_en(tm, kept)) {
      kept <- c(kept, tm)
      if (length(kept) >= top_n) break
    }
  }
  kept
}

limit_single_word_terms_en <- function(terms, scores = NULL, max_single = 5L) {
  terms <- normalize_term_en(terms)
  terms <- terms[nzchar(terms)]
  if (!length(terms)) return(character(0))
  if (is.null(scores)) scores <- rep(0, length(terms))
  scores <- as.numeric(scores)
  n_words <- vapply(strsplit(terms, "\\s+", perl = TRUE), length, integer(1))
  idx <- order(-scores, -n_words, -nchar(terms), terms)
  terms <- terms[idx]
  n_words <- n_words[idx]
  kept <- character(0)
  single_n <- 0L
  for (i in seq_along(terms)) {
    tm <- terms[[i]]
    nw <- n_words[[i]]
    if (nw == 1L) {
      if (single_n >= max_single) next
      single_n <- single_n + 1L
    }
    kept <- c(kept, tm)
  }
  unique(kept)
}

sentence_split_en <- function(x) {
  y <- normalize_spaces_en(x)
  if (!nzchar(y)) return(character(0))
  y <- gsub("([.!?;:])", "\\1|SPLIT|", y, perl = TRUE)
  s <- unlist(strsplit(y, "\\|SPLIT\\|", perl = TRUE), use.names = FALSE)
  s <- trimws(s)
  s[nzchar(s)]
}

build_edges_from_terms_en <- function(title, abstract, body, selected_terms, keep_terms = en_digit_keep_terms) {
  selected_terms <- unique(normalize_term_en(selected_terms, keep_terms = keep_terms))
  selected_terms <- selected_terms[nzchar(selected_terms)]
  if (length(selected_terms) < 2L) {
    return(data.frame(from = character(0), to = character(0), weight = numeric(0), stringsAsFactors = FALSE))
  }
  sections <- list(title = sentence_split_en(title), abstract = sentence_split_en(abstract), body = sentence_split_en(body))
  edge_env <- new.env(parent = emptyenv())
  add_edge <- function(a, b) {
    key <- paste(sort(c(a, b)), collapse = " || ")
    if (!exists(key, envir = edge_env, inherits = FALSE)) assign(key, 1L, envir = edge_env)
    else assign(key, get(key, envir = edge_env, inherits = FALSE) + 1L, envir = edge_env)
  }
  for (sec in sections) {
    for (sent in sec) {
      st <- paste(" ", normalize_term_en(sent, keep_terms = keep_terms), " ")
      hits <- selected_terms[vapply(selected_terms, function(term) grepl(paste0("\\b", escape_regex_en(term), "\\b"), st, perl = TRUE), logical(1))]
      hits <- sort(unique(hits))
      if (length(hits) >= 2L) {
        pairs <- utils::combn(hits, 2, simplify = FALSE)
        for (pp in pairs) add_edge(pp[1], pp[2])
      }
    }
  }
  keys <- ls(edge_env)
  if (!length(keys)) return(data.frame(from = character(0), to = character(0), weight = numeric(0), stringsAsFactors = FALSE))
  data.frame(
    from = sub(" \\|\\| .*", "", keys),
    to = sub(".* \\|\\| ", "", keys),
    weight = vapply(keys, function(k) get(k, envir = edge_env, inherits = FALSE), numeric(1)),
    stringsAsFactors = FALSE
  )
}

run_en_main_document_tail_pipeline <- function(text,
                                               top_n = 20L,
                                               pre_top_n = 50L,
                                               min_words = 1L,
                                               max_words = 4L,
                                               keep_terms = en_digit_keep_terms,
                                               cut_terms = en_cut_terms,
                                               final_cap = 25L,
                                               max_single_top = 5L) {
  sec <- extract_sections_simple_en(text)
  full_text <- paste(sec$title %||% "", sec$keyword_line %||% "", sec$abstract %||% "", sec$body %||% "", collapse = " ")
  if (!nzchar(trimws(full_text))) full_text <- text %||% ""

  author_keywords <- split_author_keywords_en(sec$keyword_line, max_words = max_words, keep_terms = keep_terms)
  author_keywords <- author_keywords[nzchar(author_keywords)]
  author_keywords <- author_keywords[!vapply(author_keywords, looks_like_metadata_en, logical(1))]
  author_keywords <- author_keywords[!vapply(author_keywords, contains_blacklisted_content_en, logical(1), keep_terms = keep_terms)]
  author_keywords <- unique(author_keywords)

  units <- extract_units_from_text_en(full_text, max_words = max_words, keep_terms = keep_terms, cut_terms = cut_terms)
  freq_tab <- sort(table(units), decreasing = TRUE)
  cand <- if (length(freq_tab)) data.frame(name = names(freq_tab), value = as.integer(freq_tab), stringsAsFactors = FALSE) else data.frame(name = character(0), value = integer(0), stringsAsFactors = FALSE)
  cand <- cand[!vapply(cand$name, looks_like_metadata_en, logical(1)), , drop = FALSE]
  cand <- cand[!vapply(cand$name, contains_blacklisted_content_en, logical(1), keep_terms = keep_terms), , drop = FALSE]
  cand <- cand[order(-cand$value, -nchar(cand$name), cand$name), , drop = FALSE]

  cand50 <- utils::head(cand, pre_top_n)
  if (nrow(cand50)) {
    kept50 <- select_subset_free_terms_en(cand50$name, scores = cand50$value, top_n = pre_top_n)
    cand50 <- cand50[match(kept50, cand50$name), , drop = FALSE]
    cand50 <- cand50[!is.na(cand50$name), , drop = FALSE]
    cand50 <- cand50[!vapply(cand50$name, contains_blacklisted_content_en, logical(1), keep_terms = keep_terms), , drop = FALSE]
  }

  base20 <- if (nrow(cand50)) limit_single_word_terms_en(cand50$name, scores = cand50$value, max_single = max_single_top) else character(0)
  base20 <- utils::head(base20, top_n)

  forced <- select_subset_free_terms_en(author_keywords, scores = vapply(author_keywords, count_phrase_in_text_en, integer(1), text = full_text, keep_terms = keep_terms), top_n = length(author_keywords))
  remain <- setdiff(base20, forced)
  if (length(forced)) {
    remain <- remain[!vapply(remain, contains_subset_relation_en, logical(1), kept = forced)]
  }
  remain_scores <- vapply(remain, count_phrase_in_text_en, integer(1), text = full_text, keep_terms = keep_terms)
  remain <- select_subset_free_terms_en(remain, scores = remain_scores, top_n = max(0L, final_cap - length(forced)))

  final_terms <- unique(c(forced, remain))
  if (length(final_terms) < min(final_cap, length(unique(c(base20, author_keywords))))) {
    extra_pool <- setdiff(cand50$name, final_terms)
    if (length(forced)) extra_pool <- extra_pool[!vapply(extra_pool, contains_subset_relation_en, logical(1), kept = forced)]
    if (length(extra_pool)) {
      extra_scores <- vapply(extra_pool, count_phrase_in_text_en, integer(1), text = full_text, keep_terms = keep_terms)
      extra_pool <- select_subset_free_terms_en(extra_pool, scores = extra_scores, top_n = final_cap)
      final_terms <- unique(c(final_terms, extra_pool))
    }
  }
  final_terms <- utils::head(final_terms, final_cap)
  final_terms <- final_terms[nzchar(final_terms)]
  final_terms <- final_terms[!vapply(final_terms, looks_like_metadata_en, logical(1))]
  final_terms <- final_terms[!vapply(final_terms, contains_blacklisted_content_en, logical(1), keep_terms = keep_terms)]

  final_values <- vapply(final_terms, count_phrase_in_text_en, integer(1), text = full_text, keep_terms = keep_terms)
  nodes <- data.frame(
    name = final_terms,
    value = as.integer(final_values),
    semantic_total_count = as.integer(final_values),
    author_defined_keyword = final_terms %in% forced,
    semantic_top20_order = seq_along(final_terms),
    stringsAsFactors = FALSE
  )
  nodes$n_words <- vapply(strsplit(nodes$name, "\\s+", perl = TRUE), length, integer(1))
  nodes <- nodes[order(!(nodes$author_defined_keyword %in% TRUE), nodes$semantic_top20_order, -nodes$value, nodes$n_words, nodes$name), , drop = FALSE]
  rownames(nodes) <- NULL

  edges <- build_edges_from_terms_en(sec$title, sec$abstract, sec$body, nodes$name, keep_terms = keep_terms)

  list(
    keywords = forced,
    nodes = nodes,
    edges = edges,
    full_units = units,
    candidates50 = cand50,
    comma_text = comma_first_document_en(full_text, keep_terms = keep_terms, cut_terms = cut_terms),
    log_step1_raw = cand,
    log_step2_purified = cand50,
    log_step3_top50_with_authors = data.frame(name = unique(c(forced, cand50$name %||% character(0))), stringsAsFactors = FALSE),
    log_step4_top20_final = data.frame(name = final_terms, stringsAsFactors = FALSE)
  )
}
