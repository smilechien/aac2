# App4SemanticPhrases
https://smilechien.shinyapps.io/aac2/?url-link=https%3A%2F%2Fpmc.ncbi.nlm.nih.gov%2Farticles%2FPMC12466245%2F&mode=tfidf_only&autorun=1

**App4SemanticPhrases** is an R/Shiny application for extracting featured semantic phrases from manuscripts and transforming them into co-word network visualizations. The application supports manuscript-oriented phrase extraction, author-keyword protection, top-20 semantic phrase selection, weighted co-occurrence edge construction, FLCA-SIL-MA-style clustering, SSplot visualization, and downloadable reports.

This repository is designed for users who want to run `app.R` locally, deploy it to Shiny Server or shinyapps.io, and reproduce the semantic phrase extraction workflow used in manuscript screening.

---

## 1. Overview

App4SemanticPhrases converts a manuscript-like document into two core analytical dataframes:

### `nodes`

| column | meaning |
|---|---|
| `name` | selected semantic phrase |
| `value` | phrase frequency, salience, or weighted count |

### `edges`

| column | meaning |
|---|---|
| `term1` | first phrase node |
| `term2` | second phrase node |
| `WCD` | weighted co-document/co-occurrence count |

The resulting node-edge structure can be used to generate co-word networks, clustered phrase networks, SSplots, AAC dashboards, Sankey-style summaries, and downloadable reports.

---

## 2. Main Workflow

The application follows a manuscript-to-network workflow:

1. Upload a manuscript or enter a URL.
2. Select the extraction mode.
3. Run semantic phrase extraction.
4. Generate `nodes` and `edges`.
5. Visualize the co-word network and SSplot.
6. Export figures, tables, and reports.

Supported input formats may include PDF, DOCX, TXT, CSV, XLSX/XLS, and web-derived text.

---

## 3. Semantic Phrase Extraction Rule

The extraction rule focuses on meaningful manuscript-derived phrases rather than isolated word-frequency results.

### 3.1 Included sections

Only the following sections should be used for semantic phrase extraction:

1. title
2. abstract
3. main body

### 3.2 Excluded sections

The following sections should not be used for phrase extraction:

- references
- acknowledgements
- supplementary material
- author affiliations
- funding statements
- declaration statements
- figure captions, unless clearly part of main body text
- tables, unless clearly part of main body text
- metadata-only text
- URLs, DOI strings, and repository links

### 3.3 Candidate phrase generation

Candidate semantic phrases are generated from the included sections using the following principles:

- prefer 2- to 4-word semantic phrases
- allow 1-word terms only when they are meaningful acronyms or accepted technical terms
- normalize hyphens and symbols
- remove URLs, DOI fragments, emails, and metadata artifacts
- remove broken word fragments
- avoid generic manuscript terms such as `study`, `result`, `method`, `paper`, `article`, or `analysis` when they appear alone
- retain manuscript-derived surface phrases rather than inventing new concepts

Examples of preferred phrases:

- `semantic phrase extraction`
- `co-word network`
- `data visualization`
- `natural language processing`
- `text mining`
- `author keyword protection`

Examples of weak or usually excluded terms:

- `analysis`
- `result`
- `method`
- `data`
- `paper`
- `article`

---

## 4. Author-defined Keyword Protection

Author-defined keywords are treated as privileged manuscript descriptors.

Rules:

1. Detect author keywords from the manuscript keyword block or user-provided keyword input.
2. Normalize author keywords using the same phrase-cleaning rule as candidate phrases.
3. Force meaningful author-defined keywords into the final top-20 list when possible.
4. Preserve accepted acronyms such as `AI`, `NLP`, `TALL`, `FLCA`, `AAC`, and `SSplot`.
5. If an author keyword is a near-duplicate of another phrase, keep the clearer canonical form.
6. Do not force keywords that are purely generic, administrative, or unrelated to the manuscript content.

---

## 5. Phrase Purification and Top-20 Selection

After candidate phrase generation, the app applies filtering and selection rules.

### 5.1 Frequency and salience

Each phrase is counted across title, abstract, and main body.

Phrases appearing in the title, abstract, or author keyword block may receive higher priority because they often represent the central theme of the manuscript.

### 5.2 Subset-free filtering

When a shorter phrase is fully contained in a more informative longer phrase, keep the longer phrase unless the shorter phrase has independent meaning.

| shorter phrase | longer phrase | preferred |
|---|---|---|
| `network` | `co-word network` | `co-word network` |
| `language processing` | `natural language processing` | `natural language processing` |
| `keyword protection` | `author keyword protection` | `author keyword protection` |

When a shorter phrase is removed, its frequency or salience may be merged into the retained canonical phrase.

### 5.3 Similarity filtering

Near-duplicate phrases should be merged when they share strong semantic or wording overlap.

| variant phrase | canonical phrase |
|---|---|
| `co word network` | `co-word network` |
| `co-occurrence networks` | `co-occurrence network` |
| `semantic phrases` | `semantic phrase extraction`, if the longer phrase is the intended concept |

### 5.4 Final top-20 nodes

The final `nodes` dataframe should contain exactly 20 phrases when possible.

Final selection rule:

1. include meaningful author-defined keywords
2. include high-frequency manuscript phrases
3. prefer multi-word scientific phrases
4. remove redundant or weak phrases
5. fill remaining slots with the next most meaningful manuscript-derived terms

The final `nodes` table should be sorted by `value` in descending order.

---

## 6. Co-occurrence Edge Construction

Edges are built only among the final 20 phrases in `nodes$name`.

### 6.1 Co-occurrence unit

Phrase co-occurrence is counted when two selected phrases appear in the same sentence or short textual unit inside the same section.

The app counts co-occurrences separately within title, abstract, and body. The section-level counts are then summed.

### 6.2 WCD definition

`WCD` means **weighted co-document/co-occurrence count**.

In this app:

```text
WCD = sum of phrase-pair co-occurrence counts across title + abstract + body
```

### 6.3 Edge rules

The `edges` dataframe must follow these rules:

- use only phrases selected in `nodes$name`
- keep only edges with `WCD >= 1`
- remove self-links
- remove duplicated reversed pairs
- sort by `WCD` descending, then alphabetically

Example:

| term1 | term2 | WCD |
|---|---|---:|
| `co-word network` | `semantic phrase extraction` | 4 |
| `author keyword protection` | `top-20 selection` | 2 |

---

## 7. Extraction Modes

### 7.1 TF-IDF / rule-based mode

This mode uses local text processing and does not require an external API key.

Typical steps:

1. parse title, abstract, and body
2. extract candidate phrases
3. compute frequency and TF-IDF-like salience
4. protect author keywords
5. filter redundant phrases
6. generate top-20 nodes
7. build WCD edges

### 7.2 OpenAI API mode

This mode uses the user’s OpenAI API key to assist semantic phrase extraction.

Recommended behavior:

1. send only title, abstract, and body text to the API
2. exclude references, acknowledgements, supplementary materials, and end matter
3. ask the API to return structured JSON or CSV-like tables
4. validate returned phrases in R
5. force author-defined keywords if needed
6. reconstruct or verify `nodes` and `edges` in R for reproducibility

For confidential manuscripts, users should review their institutional data-sharing rules before using API mode.

---

## 8. Recommended OpenAI API Prompt

The app may use a prompt like the following for API-assisted extraction:

```text
Read the uploaded document and extract semantic phrases only from these sections:
1. title
2. abstract
3. main body

Do not use references. Do not use acknowledgements. Do not use supplementary material. Do not use figure captions or tables unless they are clearly part of the main body text.

Return two dataframes only.

Dataframe 1: nodes
Columns must be exactly:
name, value

Dataframe 2: edges
Columns must be exactly:
term1, term2, WCD

Method:
- Extract the top 20 semantic phrases by salience and frequency.
- Force author-defined keywords into the final top 20.
- Normalize near-duplicate phrases into one canonical phrase.
- Prefer multi-word scientific phrases when possible.
- Build pairwise co-occurrence edges only among the final 20 phrases.
- Count co-occurrence separately in title, abstract, and body, then sum into WCD.
- Remove self-links and duplicate reversed pairs.
- Sort nodes by value descending and edges by WCD descending.
```

---

## 9. Repository Structure

Recommended GitHub repository structure:

```text
App4SemanticPhrases/
├── app.R
├── README.md
├── data/
│   └── demo.pdf
├── R/
│   ├── phrase_extraction.R
│   ├── network_construction.R
│   ├── renderSSplot.R
│   ├── kano.R
│   └── sankey.R
├── www/
│   ├── style.css
│   └── images/
├── outputs/
│   └── .gitkeep
└── renv.lock
```

A minimal repository can also contain only:

```text
App4SemanticPhrases/
├── app.R
├── README.md
└── data/
    └── demo.pdf
```

---

## 10. Installation

Install the required R packages:

```r
install.packages(c(
  "shiny",
  "DT",
  "dplyr",
  "stringr",
  "tidyr",
  "tibble",
  "readr",
  "purrr",
  "ggplot2",
  "igraph",
  "visNetwork",
  "pdftools",
  "officer",
  "readxl",
  "writexl",
  "jsonlite",
  "httr2"
))
```

Optional packages may be required depending on the app features:

```r
install.packages(c(
  "shinycssloaders",
  "bslib",
  "plotly",
  "htmlwidgets",
  "zip"
))
```

---

## 11. Running Locally

Clone the repository:

```bash
git clone https://github.com/YOUR-USERNAME/App4SemanticPhrases.git
cd App4SemanticPhrases
```

Run the Shiny app in R:

```r
shiny::runApp()
```

Or run directly from the app folder:

```r
shiny::runApp("path/to/App4SemanticPhrases")
```

---

## 12. OpenAI API Key Setup

If OpenAI API mode is enabled, store the API key outside `app.R`.

Recommended `.Renviron` setting:

```text
OPENAI_API_KEY=your_api_key_here
```

Then restart R and confirm:

```r
Sys.getenv("OPENAI_API_KEY")
```

Do not upload `.Renviron` or API keys to GitHub.

Add this to `.gitignore`:

```text
.Renviron
*.Rhistory
*.RData
outputs/*
!outputs/.gitkeep
```

---

## 13. Deployment to shinyapps.io

Install and configure `rsconnect`:

```r
install.packages("rsconnect")
library(rsconnect)
```

Connect your shinyapps.io account:

```r
rsconnect::setAccountInfo(
  name = "YOUR_ACCOUNT_NAME",
  token = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)
```

Deploy the app:

```r
rsconnect::deployApp(
  appDir = ".",
  appName = "App4SemanticPhrases"
)
```

If the app uses an OpenAI API key, set the environment variable in shinyapps.io:

1. Open the shinyapps.io dashboard.
2. Select the deployed application.
3. Go to **Settings**.
4. Add `OPENAI_API_KEY`.
5. Redeploy or restart the application.

---

## 14. Deployment to Shiny Server

Copy the app folder to the Shiny Server directory:

```bash
sudo cp -r App4SemanticPhrases /srv/shiny-server/
```

Set permissions if needed:

```bash
sudo chown -R shiny:shiny /srv/shiny-server/App4SemanticPhrases
```

Then open:

```text
http://your-server-address:3838/App4SemanticPhrases/
```

---

## 15. Expected Outputs

After analysis, the app should generate:

- `nodes.csv`
- `edges.csv`
- co-word network PNG
- SSplot PNG
- cluster summary table
- optional AAC dashboard
- optional Sankey summary
- downloadable ZIP package
- optional HTML report

---

## 16. Reproducibility Notes

For reproducible results:

- use the same uploaded document
- use the same extraction mode
- keep the same top-N setting
- keep the same stopword and blacklist rules
- keep the same author keyword list
- record the app version and package versions
- store output tables with the generated report

---

## 17. Citation

If you use App4SemanticPhrases in academic work, cite the repository and describe the phrase extraction rule, including:

- title/abstract/body-only extraction
- author-keyword protection
- top-20 phrase selection
- subset-free redundancy filtering
- sentence-level WCD edge construction
- FLCA-SIL-MA-style visualization workflow

Suggested citation format:

```text
App4SemanticPhrases: Mining Featured Phrases from Manuscripts for Co-word Network Visualization.
GitHub repository: https://github.com/YOUR-USERNAME/App4SemanticPhrases
```

---

## 18. License

Recommended license: MIT License.

Update this section if your repository uses a different license.

---

## 19. Contact

For questions, bug reports, or feature requests, open a GitHub Issue or contact the maintainer listed in the repository.
