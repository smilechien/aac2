
options(shiny.maxRequestSize = 50 * 1024^2)

required_pkgs <- c(
    "shiny", "pdftools", "officer", "stringr", "dplyr", "purrr",
    "tibble", "readr", "DT", "writexl"
)
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
    stop(
        paste0(
            "Please install required packages first:\n",
            "install.packages(c(",
            paste(sprintf('"%s"', missing_pkgs), collapse = ", "),
            "))"
        ),
        call. = FALSE
    )
}

suppressPackageStartupMessages({
    library(shiny)
    library(pdftools)
    library(officer)
    library(stringr)
    library(dplyr)
    library(purrr)
    library(tibble)
    library(readr)
    library(DT)
})

get_stopwords_en <- function() {
    if (requireNamespace("stopwords", quietly = TRUE)) {
        unique(c(
            stopwords::stopwords("en"),
            "et", "al", "eg", "ie", "fig", "figure", "table"
        ))
    } else {
        unique(c(
            "a","an","and","are","as","at","be","been","being","but","by","for","from",
            "had","has","have","he","her","his","i","if","in","into","is","it","its",
            "itself","me","more","most","my","of","on","or","our","ours","she","so","such",
            "than","that","the","their","them","they","this","those","to","too","under","up",
            "use","used","using","via","was","we","were","what","when","where","which","while",
            "who","with","within","you","your","et","al","eg","ie","fig","figure","table"
        ))
    }
}

stop_words_en <- get_stopwords_en()

weak_tail_terms <- c(
    "paper", "papers", "study", "studies", "article", "articles",
    "result", "results", "section", "sections", "example", "examples",
    "version", "versions", "figure", "figures", "table", "tables",
    "author", "authors", "user", "users", "software", "application"
)

escape_regex <- function(x) {
    gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

clean_page_text <- function(x) {
    x <- gsub("\r", "", x, perl = TRUE)
    x <- gsub("-\\s*\n\\s*", "", x, perl = TRUE)
    x <- gsub("[ \t]+", " ", x, perl = TRUE)
    x <- gsub("\n{2,}", "\n", x, perl = TRUE)
    trimws(x)
}

normalize_phrase <- function(x) {
    x <- tolower(x)
    x <- gsub("[‐-–—/]", " ", x, perl = TRUE)
    x <- gsub("[\"'’`]", "", x, perl = TRUE)
    x <- gsub("[^a-z0-9 ]+", " ", x, perl = TRUE)
    x <- gsub("\\s+", " ", x, perl = TRUE)
    trimws(x)
}

normalize_for_matching <- function(x) {
    x <- tolower(x)
    x <- gsub("[‐-–—/]", " ", x, perl = TRUE)
    x <- gsub("[\"'’`]", "", x, perl = TRUE)
    x <- gsub("[^a-z0-9.!?;: ]+", " ", x, perl = TRUE)
    x <- gsub("\\s+", " ", x, perl = TRUE)
    trimws(x)
}

is_blank <- function(x) {
    is.na(x) | trimws(x) == ""
}

line_norm_simple <- function(x) {
    y <- tolower(x)
    y <- gsub("[^a-z0-9 ]+", " ", y, perl = TRUE)
    y <- gsub("\\s+", " ", y, perl = TRUE)
    trimws(y)
}

is_abstract_line <- function(x) {
    y <- line_norm_simple(x)
    grepl("^abstract$", y) || grepl("^a b s t r a c t$", y)
}

is_keywords_line <- function(x) {
    y <- line_norm_simple(x)
    grepl("^keywords?$", y) ||
        grepl("^keywords?\\s", y) ||
        grepl("^key words?$", y) ||
        grepl("^keywords?:", tolower(trimws(x))) ||
        grepl("^key words?:", tolower(trimws(x)))
}

is_endmatter_line <- function(x) {
    y <- line_norm_simple(x)
    grepl("^references?$", y) ||
        grepl("^acknowledg", y) ||
        grepl("^declaration of competing interest", y) ||
        grepl("^credit authorship contribution statement", y) ||
        grepl("^data availability", y) ||
        grepl("^appendix", y) ||
        grepl("^supplementary data", y)
}

is_section_heading <- function(x) {
    z <- trimws(x)
    y <- line_norm_simple(x)
    if (y == "") return(FALSE)
    if (is_endmatter_line(x)) return(TRUE)
    
    numeric_head <- grepl("^\\d+\\s*[.)]?\\s*", z, perl = TRUE) ||
        grepl("^\\d+\\s*\\.\\s*", z, perl = TRUE) ||
        grepl("^\\d+\\s+[A-Za-z]", z, perl = TRUE)
    
    named_head <- grepl(
        "^(introduction|motivation and significance|background|software description|methods|materials and methods|results|discussion|impact and conclusions|conclusion|illustrative examples|example)\\b",
        y
    )
    
    shortish <- str_count(y, "\\S+") <= 12
    looks_heading <- numeric_head || named_head
    looks_heading && shortish
}

drop_caption_lines <- function(lines) {
    lines[!grepl("^\\s*(fig\\.?|figure|table)\\s*\\d+", tolower(trimws(lines))), , drop = FALSE]
}

read_document_text <- function(path) {
    ext <- tolower(tools::file_ext(path))
    
    if (ext == "pdf") {
        pages <- pdftools::pdf_text(path)
        txt <- paste(vapply(pages, clean_page_text, character(1)), collapse = "\n")
    } else if (ext == "docx") {
        doc <- officer::read_docx(path)
        s <- officer::docx_summary(doc)
        if ("content_type" %in% names(s)) {
            s <- s[s$content_type %in% c("paragraph", "text"), , drop = FALSE]
        }
        txt_vec <- s$text
        txt_vec <- txt_vec[!is_blank(txt_vec)]
        txt <- paste(txt_vec, collapse = "\n")
        txt <- clean_page_text(txt)
    } else {
        stop("Only PDF and DOCX are supported.")
    }
    
    txt
}

extract_title <- function(lines) {
    if (length(lines) == 0) return("")
    
    idx_abs <- which(vapply(lines, is_abstract_line, logical(1)))
    upper_limit <- if (length(idx_abs) > 0) max(1, idx_abs[1] - 1) else min(length(lines), 25)
    cand <- lines[seq_len(upper_limit)]
    cand <- cand[!is_blank(cand)]
    
    if (length(cand) == 0) return("")
    
    bad <- grepl(
        "(science direct|softwarex|journal homepage|doi|received |available online|email address|department of|university|metadata|highlights|keywords?:|abstract)",
        tolower(cand)
    )
    cand <- cand[!bad]
    
    if (length(cand) == 0) return(trimws(lines[1]))
    
    wc <- str_count(cand, "\\S+")
    cand2 <- cand[wc >= 4 & wc <= 35]
    cand2 <- cand2[!grepl("\\.$", trimws(cand2))]
    
    if (length(cand2) == 0) cand2 <- cand
    cand2[which.max(nchar(cand2))]
}

extract_keywords <- function(lines) {
    idx <- which(vapply(lines, is_keywords_line, logical(1)))
    if (length(idx) == 0) return(character(0))
    
    i <- idx[1]
    out <- character(0)
    
    line_i <- trimws(lines[i])
    
    if (grepl(":", line_i)) {
        rhs <- sub("^[^:]*:\\s*", "", line_i)
        if (!is_blank(rhs) && !grepl("^abstract$", line_norm_simple(rhs))) {
            out <- c(out, rhs)
        }
    }
    
    if (i < length(lines)) {
        j <- i + 1
        while (j <= length(lines)) {
            lj <- trimws(lines[j])
            if (is_blank(lj)) break
            if (is_abstract_line(lj) || is_section_heading(lj) || is_endmatter_line(lj)) break
            if (nchar(lj) > 120) break
            out <- c(out, lj)
            j <- j + 1
        }
    }
    
    kw <- paste(out, collapse = "; ")
    kw <- unlist(strsplit(kw, "[,;\\n]+", perl = TRUE))
    kw <- trimws(kw)
    kw <- kw[kw != ""]
    kw <- normalize_phrase(kw)
    kw <- kw[kw != ""]
    kw <- kw[str_count(kw, "\\S+") <= 6]
    unique(kw)
}

extract_abstract <- function(lines) {
    idx <- which(vapply(lines, is_abstract_line, logical(1)))
    if (length(idx) == 0) return("")
    
    i <- idx[1]
    out <- character(0)
    
    first_line <- trimws(lines[i])
    after_head <- sub("^[Aa]\\s*[Bb]\\s*[Ss]\\s*[Tt]\\s*[Rr]\\s*[Aa]\\s*[Cc]\\s*[Tt]\\s*:?\\s*", "", first_line, perl = TRUE)
    if (!is_blank(after_head) && line_norm_simple(after_head) != "abstract") {
        out <- c(out, after_head)
    }
    
    if (i < length(lines)) {
        j <- i + 1
        while (j <= length(lines)) {
            lj <- trimws(lines[j])
            if (is_blank(lj)) {
                if (length(out) > 0) break
                j <- j + 1
                next
            }
            if (is_section_heading(lj) || is_endmatter_line(lj) || is_keywords_line(lj)) break
            out <- c(out, lj)
            j <- j + 1
        }
    }
    
    paste(out, collapse = " ")
}

extract_body <- function(lines) {
    if (length(lines) == 0) return("")
    
    start_idx <- which(vapply(lines, function(x) {
        z <- trimws(x)
        y <- line_norm_simple(x)
        grepl("^\\d+\\s*[.)]?\\s*(introduction|motivation and significance|background|software description|illustrative examples|impact and conclusions)", y) ||
            grepl("^(introduction|motivation and significance|background|software description|illustrative examples|impact and conclusions)$", y) ||
            grepl("^\\d+\\s*\\.$", z)
    }, logical(1)))
    
    if (length(start_idx) == 0) {
        idx_abs <- which(vapply(lines, is_abstract_line, logical(1)))
        if (length(idx_abs) > 0) {
            start <- idx_abs[1] + 1
            while (start <= length(lines) && !is_blank(lines[start]) &&
                   !is_section_heading(lines[start]) && !is_endmatter_line(lines[start])) {
                start <- start + 1
            }
            while (start <= length(lines) && (is_blank(lines[start]) || !is_section_heading(lines[start]))) {
                start <- start + 1
            }
            if (start > length(lines)) start <- idx_abs[1] + 1
        } else {
            start <- 1
        }
    } else {
        start <- start_idx[1]
    }
    
    end_idx <- which(vapply(lines, is_endmatter_line, logical(1)))
    end <- if (length(end_idx) > 0) min(end_idx) - 1 else length(lines)
    
    if (start > end) return("")
    
    body_lines <- lines[start:end]
    body_lines <- body_lines[!grepl(
        "(corresponding author|email address|received |available online|doi|copyright)",
        tolower(body_lines)
    )]
    body_lines <- body_lines[!grepl("^\\s*(fig\\.?|figure|table)\\s*\\d+", tolower(body_lines))]
    body_lines <- body_lines[!is_blank(body_lines)]
    
    paste(body_lines, collapse = " ")
}

sentence_split <- function(x) {
    if (is_blank(x)) return(character(0))
    y <- normalize_for_matching(x)
    y <- gsub("\\n+", " ", y, perl = TRUE)
    y <- gsub("([.!?;:])", "\\1|SPLIT|", y, perl = TRUE)
    s <- unlist(strsplit(y, "\\|SPLIT\\|", perl = TRUE))
    s <- trimws(s)
    s[nchar(s) > 0]
}

valid_token <- function(tok) {
    if (tok == "" || is.na(tok)) return(FALSE)
    if (!grepl("[a-z]", tok)) return(FALSE)
    if (nchar(tok) == 1 && !(tok %in% c("r", "ai"))) return(FALSE)
    TRUE
}

extract_candidate_ngrams <- function(text, min_n = 2, max_n = 4) {
    x <- normalize_phrase(text)
    if (x == "") return(character(0))
    
    toks <- unlist(strsplit(x, "\\s+", perl = TRUE))
    toks <- toks[vapply(toks, valid_token, logical(1))]
    if (length(toks) < min_n) return(character(0))
    
    out <- character(0)
    for (n in seq(min_n, min(max_n, length(toks)))) {
        idxs <- seq_len(length(toks) - n + 1)
        grams <- vapply(idxs, function(i) paste(toks[i:(i + n - 1)], collapse = " "), character(1))
        out <- c(out, grams)
    }
    
    out <- unique(out)
    out <- out[nchar(out) >= 4 & nchar(out) <= 80]
    
    keep <- vapply(out, function(g) {
        parts <- unlist(strsplit(g, "\\s+", perl = TRUE))
        if (length(parts) < 2) return(FALSE)
        if (parts[1] %in% stop_words_en || parts[length(parts)] %in% stop_words_en) return(FALSE)
        if (sum(parts %in% stop_words_en) > floor(length(parts) / 2)) return(FALSE)
        if (parts[length(parts)] %in% weak_tail_terms) return(FALSE)
        if (all(nchar(parts) <= 2 & !(parts %in% c("ai", "r")))) return(FALSE)
        TRUE
    }, logical(1))
    
    out[keep]
}

count_phrase_in_text <- function(phrase, text) {
    x <- normalize_for_matching(text)
    p <- normalize_phrase(phrase)
    if (p == "" || x == "") return(0L)
    patt <- paste0("\\b", escape_regex(p), "\\b")
    as.integer(str_count(x, regex(patt, ignore_case = TRUE)))
}

phrase_presence_in_sentence <- function(phrase, sent) {
    p <- normalize_phrase(phrase)
    if (p == "" || sent == "") return(FALSE)
    patt <- paste0("\\b", escape_regex(p), "\\b")
    str_detect(sent, regex(patt, ignore_case = TRUE))
}

build_candidate_table <- function(title, abstract, body, keywords) {
    title_grams <- extract_candidate_ngrams(title, 1, 4)
    abstract_grams <- extract_candidate_ngrams(abstract, 2, 4)
    body_grams <- extract_candidate_ngrams(body, 2, 4)
    
    all_phrases <- unique(c(title_grams, abstract_grams, body_grams, keywords))
    all_phrases <- all_phrases[all_phrases != ""]
    
    if (length(all_phrases) == 0) {
        return(tibble(
            phrase = character(0),
            title_count = integer(0),
            abstract_count = integer(0),
            body_count = integer(0),
            total_count = integer(0),
            section_presence = integer(0),
            score = numeric(0)
        ))
    }
    
    tibble(phrase = all_phrases) |>
        mutate(
            title_count = vapply(phrase, count_phrase_in_text, integer(1), text = title),
            abstract_count = vapply(phrase, count_phrase_in_text, integer(1), text = abstract),
            body_count = vapply(phrase, count_phrase_in_text, integer(1), text = body),
            total_count = title_count + abstract_count + body_count,
            section_presence = (title_count > 0) + (abstract_count > 0) + (body_count > 0),
            score = title_count * 3 + abstract_count * 2 + body_count * 1 + section_presence * 0.5
        ) |>
        filter(total_count > 0) |>
        arrange(desc(score), desc(total_count), desc(str_count(phrase, "\\S+")), phrase)
}

select_top_phrases <- function(cand_tbl, keywords, top_n = 20) {
    if (nrow(cand_tbl) == 0) {
        return(tibble(name = character(0), value = integer(0)))
    }
    
    keywords <- normalize_phrase(keywords)
    keywords <- keywords[keywords != ""]
    keywords <- unique(keywords)
    
    ranked <- cand_tbl$phrase
    forced <- intersect(keywords, ranked)
    
    missing_kw <- setdiff(keywords, ranked)
    if (length(missing_kw) > 0) {
        extra <- tibble(
            phrase = missing_kw,
            title_count = 0L,
            abstract_count = 0L,
            body_count = 0L,
            total_count = 0L,
            section_presence = 0L,
            score = 0
        )
        cand_tbl <- bind_rows(cand_tbl, extra) |>
            distinct(phrase, .keep_all = TRUE)
    }
    
    forced <- unique(c(forced, missing_kw))
    if (length(forced) > top_n) {
        forced <- forced[seq_len(top_n)]
    }
    
    remainder <- setdiff(cand_tbl$phrase, forced)
    selected <- c(forced, head(remainder, max(0, top_n - length(forced))))
    selected <- unique(selected)
    
    nodes <- cand_tbl |>
        filter(phrase %in% selected) |>
        select(name = phrase, value = total_count) |>
        distinct(name, .keep_all = TRUE)
    
    if (nrow(nodes) < length(selected)) {
        miss <- setdiff(selected, nodes$name)
        nodes <- bind_rows(
            nodes,
            tibble(name = miss, value = 0L)
        )
    }
    
    nodes |>
        arrange(desc(value), name)
}

build_edges <- function(nodes, title, abstract, body) {
    phrases <- nodes$name
    if (length(phrases) < 2) {
        return(tibble(term1 = character(0), term2 = character(0), WCD = integer(0)))
    }
    
    sections <- list(
        title = sentence_split(title),
        abstract = sentence_split(abstract),
        body = sentence_split(body)
    )
    
    edge_map <- new.env(parent = emptyenv())
    
    add_edge <- function(a, b) {
        key <- paste(a, b, sep = " || ")
        if (!exists(key, envir = edge_map, inherits = FALSE)) {
            assign(key, 1L, envir = edge_map)
        } else {
            assign(key, get(key, envir = edge_map, inherits = FALSE) + 1L, envir = edge_map)
        }
    }
    
    for (sec in sections) {
        for (sent in sec) {
            present <- phrases[vapply(phrases, phrase_presence_in_sentence, logical(1), sent = sent)]
            present <- sort(unique(present))
            if (length(present) >= 2) {
                cmb <- combn(present, 2, simplify = FALSE)
                for (pair in cmb) add_edge(pair[1], pair[2])
            }
        }
    }
    
    keys <- ls(edge_map)
    if (length(keys) == 0) {
        return(tibble(term1 = character(0), term2 = character(0), WCD = integer(0)))
    }
    
    tibble(
        key = keys,
        WCD = vapply(keys, function(k) get(k, envir = edge_map, inherits = FALSE), integer(1))
    ) |>
        mutate(
            term1 = sub(" \\|\\| .*", "", key),
            term2 = sub(".* \\|\\| ", "", key)
        ) |>
        select(term1, term2, WCD) |>
        filter(term1 != term2, WCD >= 1L) |>
        arrange(desc(WCD), term1, term2)
}

generate_nodes_edges_from_file <- function(path, top_n = 20) {
    text <- read_document_text(path)
    lines <- unlist(strsplit(text, "\n", fixed = TRUE))
    lines <- trimws(lines)
    lines <- lines[!is.na(lines)]
    
    title <- extract_title(lines)
    keywords <- extract_keywords(lines)
    abstract <- extract_abstract(lines)
    body <- extract_body(lines)
    
    cand_tbl <- build_candidate_table(title, abstract, body, keywords)
    nodes <- select_top_phrases(cand_tbl, keywords, top_n = top_n)
    edges <- build_edges(nodes, title, abstract, body)
    
    list(
        title = title,
        keywords = keywords,
        abstract = abstract,
        body = body,
        nodes = nodes,
        edges = edges
    )
}

save_outputs <- function(res, file_basename = "semantic_phrases") {
    tmpdir <- tempdir()
    nodes_csv <- file.path(tmpdir, paste0(file_basename, "_nodes.csv"))
    edges_csv <- file.path(tmpdir, paste0(file_basename, "_edges.csv"))
    
    readr::write_csv(res$nodes, nodes_csv)
    readr::write_csv(res$edges, edges_csv)
    
    xlsx_path <- file.path(tmpdir, paste0(file_basename, "_nodes_edges.xlsx"))
    writexl::write_xlsx(
        list(nodes = res$nodes, edges = res$edges),
        path = xlsx_path
    )
    
    list(nodes_csv = nodes_csv, edges_csv = edges_csv, xlsx = xlsx_path)
}

ui <- fluidPage(
    titlePanel("Semantic Phrase Nodes & Edges (PDF/DOCX)"),
    sidebarLayout(
        sidebarPanel(
            fileInput(
                "docfile",
                "Upload PDF or DOCX",
                accept = c(".pdf", ".docx")
            ),
            numericInput("top_n", "Top phrases", value = 20, min = 5, max = 100, step = 1),
            actionButton("run", "Generate", class = "btn-primary"),
            tags$hr(),
            helpText("Output:"),
            tags$ul(
                tags$li("nodes$name, nodes$value"),
                tags$li("edges$term1, edges$term2, edges$WCD")
            ),
            helpText("Rules: title + abstract + main body only; references and end matter excluded; author keywords force-included when detected."),
            tags$hr(),
            downloadButton("download_nodes_csv", "Download nodes CSV"),
            br(), br(),
            downloadButton("download_edges_csv", "Download edges CSV"),
            br(), br(),
            downloadButton("download_xlsx", "Download XLSX")
        ),
        mainPanel(
            h4("Document summary"),
            verbatimTextOutput("doc_info"),
            h4("Detected author-defined keywords"),
            verbatimTextOutput("keywords_text"),
            h4("Nodes dataframe"),
            DTOutput("nodes_tbl"),
            h4("Edges dataframe"),
            DTOutput("edges_tbl")
        )
    )
)

server <- function(input, output, session) {
    result_rv <- reactiveVal(NULL)
    files_rv <- reactiveVal(NULL)
    
    observeEvent(input$run, {
        req(input$docfile)
        
        withProgress(message = "Processing document...", value = 0.1, {
            incProgress(0.2, detail = "Reading file")
            ext <- tolower(tools::file_ext(input$docfile$name))
            validate(need(ext %in% c("pdf", "docx"), "Please upload a PDF or DOCX file."))
            
            incProgress(0.4, detail = "Extracting sections and phrases")
            res <- generate_nodes_edges_from_file(input$docfile$datapath, top_n = input$top_n)
            
            incProgress(0.8, detail = "Preparing downloads")
            outfiles <- save_outputs(res, file_basename = "semantic_phrases")
            
            result_rv(res)
            files_rv(outfiles)
            incProgress(1)
        })
    })
    
    output$doc_info <- renderText({
        res <- result_rv()
        if (is.null(res)) return("Upload a PDF or DOCX and click Generate.")
        paste0(
            "Title: ", ifelse(is_blank(res$title), "[not detected]", res$title), "\n\n",
            "Abstract length: ", nchar(res$abstract), " characters\n",
            "Body length: ", nchar(res$body), " characters\n",
            "Nodes: ", nrow(res$nodes), "\n",
            "Edges: ", nrow(res$edges)
        )
    })
    
    output$keywords_text <- renderText({
        res <- result_rv()
        if (is.null(res)) return("")
        if (length(res$keywords) == 0) return("[No author-defined keywords detected]")
        paste(res$keywords, collapse = ", ")
    })
    
    output$nodes_tbl <- renderDT({
        res <- result_rv()
        req(res)
        datatable(
            res$nodes,
            rownames = FALSE,
            options = list(pageLength = 10, scrollX = TRUE)
        )
    })
    
    output$edges_tbl <- renderDT({
        res <- result_rv()
        req(res)
        datatable(
            res$edges,
            rownames = FALSE,
            options = list(pageLength = 10, scrollX = TRUE)
        )
    })
    
    output$download_nodes_csv <- downloadHandler(
        filename = function() {
            "nodes_top20_semantic_phrases.csv"
        },
        content = function(file) {
            fs <- files_rv()
            req(fs)
            file.copy(fs$nodes_csv, file, overwrite = TRUE)
        }
    )
    
    output$download_edges_csv <- downloadHandler(
        filename = function() {
            "edges_top20_semantic_phrase_edges.csv"
        },
        content = function(file) {
            fs <- files_rv()
            req(fs)
            file.copy(fs$edges_csv, file, overwrite = TRUE)
        }
    )
    
    output$download_xlsx <- downloadHandler(
        filename = function() {
            "top20_semantic_phrases_nodes_edges.xlsx"
        },
        content = function(file) {
            fs <- files_rv()
            req(fs)
            file.copy(fs$xlsx, file, overwrite = TRUE)
        },
        contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
}

shinyApp(ui, server)