#!/usr/bin/env Rscript

# Privacy gate for the public Git repository.
#
# The audit checks:
#   1. known restricted paths are not tracked/staged;
#   2. a local private population workbook is available for the release audit;
#   3. empirical editor names do not appear in tracked/staged public text files
#      or public workbooks;
#   4. person-specific URLs containing compacted editor names are rejected.
#
# The script reports filenames and counts only. It never prints editor names.

suppressPackageStartupMessages({
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required. Run Rscript install_packages.R first.", call. = FALSE)
  }
})

run_git <- function(args) {
  out <- suppressWarnings(system2("git", args, stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) stop(paste(out, collapse = "\n"), call. = FALSE)
  out
}

root_line <- run_git(c("rev-parse", "--show-toplevel"))
if (!length(root_line)) stop("Run this script from inside the Git repository.", call. = FALSE)
root <- normalizePath(root_line[[1]], mustWork = TRUE)
setwd(root)

# git ls-files reflects the index, i.e. exactly what can enter the next commit
# after prepare_public_git.sh stages the public working tree.
tracked <- unique(run_git(c("ls-files")))
tracked <- tracked[nzchar(tracked)]

restricted_patterns <- c(
  "^output/",
  "^_targets/",
  "^private/",
  "^private_data/",
  "^data/Dataset_Editorial_Boards_All\\.xlsx$",
  "^data/editorial_board_data\\.xlsx$",
  "^data/confirmed_name_merges\\.csv$",
  "^data/gender_adjudication\\.csv$",
  "^data/gender_classified_full\\.csv$",
  "^data/multi_affiliation_adjudication\\.csv$",
  "^data/interlocking_gender_adjudication_audit_80\\.csv$",
  "^data/person_affiliation_adjudication_.*\\.csv$",
  "^data/.*_person_level_.*\\.csv$"
)
restricted_hits <- tracked[vapply(tracked, function(x) {
  any(vapply(restricted_patterns, grepl, logical(1), x = x, perl = TRUE))
}, logical(1))]

if (length(restricted_hits)) {
  cat("ERROR: restricted paths are tracked/staged:\n", file = stderr())
  cat(paste0("  - ", restricted_hits, collapse = "\n"), "\n", file = stderr())
  quit(status = 2)
}

private_candidates <- c(
  "private/Dataset_Editorial_Boards_All.xlsx",
  "data/Dataset_Editorial_Boards_All.xlsx" # legacy local layout
)
full_path <- private_candidates[file.exists(private_candidates)][1]
if (is.na(full_path) || !length(full_path)) {
  cat(
    "ERROR: private full-population workbook not found. The release privacy audit requires it so public files can be checked against the empirical editor-name list.\n",
    file = stderr()
  )
  quit(status = 4)
}

raw <- readxl::read_xlsx(full_path)
name_cols <- intersect(c("Name", "editor_name"), names(raw))
if (!length(name_cols)) stop("Could not find editor-name columns in the private full dataset.", call. = FALSE)

names_vec <- unique(trimws(unlist(raw[name_cols], use.names = FALSE)))
names_vec <- names_vec[!is.na(names_vec) & nzchar(names_vec)]

normalize_space <- function(x) gsub("[[:space:]]+", " ", trimws(tolower(x)))
compact <- function(x) {
  y <- iconv(tolower(x), from = "", to = "ASCII//TRANSLIT", sub = "")
  gsub("[^[:alnum:]]+", "", y)
}
name_exact <- unique(normalize_space(names_vec))
name_compact <- unique(compact(names_vec))
name_compact <- name_compact[nchar(name_compact) >= 5]

text_ext <- "\\.(R|r|md|txt|csv|yml|yaml|cff|sh|Rproj|gitattributes|gitignore)$"
files_to_scan <- tracked[file.exists(tracked)]
leaks <- character()

scan_text <- function(path) {
  x <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
  if (!length(x)) return(FALSE)
  low <- normalize_space(x)
  exact_hit <- any(vapply(name_exact, function(nm) any(grepl(nm, low, fixed = TRUE)), logical(1)))
  if (exact_hit) return(TRUE)

  # Compact matching is deliberately limited to URLs. It detects profile URLs
  # containing a concatenated editor name without broad prose false positives.
  url_lines <- x[grepl("https?://|www\\.", x, ignore.case = TRUE)]
  if (!length(url_lines)) return(FALSE)
  urls <- unlist(regmatches(
    url_lines,
    gregexpr("https?://[^,[:space:]\"']+|www\\.[^,[:space:]\"']+", url_lines,
             ignore.case = TRUE, perl = TRUE)
  ), use.names = FALSE)
  if (!length(urls)) return(FALSE)
  url_compact <- compact(urls)
  any(vapply(name_compact, function(nm) any(grepl(nm, url_compact, fixed = TRUE)), logical(1)))
}

for (f in files_to_scan) {
  if (grepl(text_ext, f, ignore.case = TRUE)) {
    if (scan_text(f)) leaks <- c(leaks, f)
  } else if (grepl("\\.xlsx$", f, ignore.case = TRUE)) {
    sheets <- readxl::excel_sheets(f)
    hit <- FALSE
    for (sh in sheets) {
      dat <- tryCatch(readxl::read_xlsx(f, sheet = sh), error = function(e) NULL)
      if (is.null(dat)) next
      vals <- normalize_space(as.character(unlist(dat, use.names = FALSE)))
      if (any(vapply(name_exact, function(nm) any(grepl(nm, vals, fixed = TRUE)), logical(1)))) {
        hit <- TRUE
        break
      }
    }
    if (hit) leaks <- c(leaks, f)
  }
}

leaks <- unique(leaks)
if (length(leaks)) {
  cat("ERROR: one or more tracked public files appear to contain empirical editor names.\n", file = stderr())
  cat("Affected files (names suppressed):\n", file = stderr())
  cat(paste0("  - ", leaks, collapse = "\n"), "\n", file = stderr())
  quit(status = 3)
}

cat(sprintf(
  "Privacy audit passed: %d indexed public files checked against %d private editor-name strings; no matches found.\n",
  length(tracked), length(name_exact)
))
