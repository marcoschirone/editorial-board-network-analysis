# R/institution_harmonization.R
# Conservative institution harmonization and person-level affiliation handling.
#
# Design principles:
# 1. Preserve the recorded affiliation string for auditability.
# 2. Collapse only syntactic variants automatically.
# 3. Apply substantive institution equivalences only from an approved alias map.
# 4. Never break person-level affiliation ties by spreadsheet row order.
# 5. Resolve institution and country only to the extent supported by the tied
#    appointment data. If institution is ambiguous but country is common across
#    tied pairs, retain country and set institution to NA (and vice versa).
# 6. Explicit adjudication, when supplied, overrides the automatic assignment
#    jointly and must be documented.

#' Normalize institution strings for exact candidate matching.
#'
#' This key is used only for conservative syntactic equivalence: case,
#' diacritics, punctuation, repeated whitespace, and a leading English article
#' "The" are ignored. It is NOT a fuzzy matcher and does not translate names.
normalize_institution_key <- function(x) {
  x <- as.character(x)
  y <- trimws(x)
  y[is.na(y) | y == ""] <- NA_character_
  y <- iconv(y, from = "", to = "ASCII//TRANSLIT", sub = "")
  y <- tolower(y)
  y <- sub("^the[[:space:]]+", "", y)
  y <- gsub("[^[:alnum:]]+", " ", y)
  y <- gsub("[[:space:]]+", " ", y)
  trimws(y)
}


#' Canonicalize country display labels used by the analysis.
#'
#' This is an exact alias map, not geopolitical inference. It keeps the
#' statistical categories used in the UN M49 lookup stable throughout source
#' overrides and person-level adjudication. In particular, Hong Kong and Macao
#' are represented as separate M49 statistical areas while remaining in
#' Eastern Asia.
canonicalize_country_label <- function(x) {
  x <- trimws(as.character(x))
  dplyr::case_when(
    is.na(x) | x == "" ~ NA_character_,
    x %in% c(
      "Hong Kong", "Hong Kong SAR", "Hong Kong SAR, China",
      "China, Hong Kong Special Administrative Region"
    ) ~ "Hong Kong SAR, China",
    x %in% c(
      "Macau", "Macao", "Macao SAR", "Macao SAR, China",
      "China, Macao Special Administrative Region"
    ) ~ "Macao SAR, China",
    TRUE ~ x
  )
}

#' Strip a leading English article for display normalization only.
strip_leading_the <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  sub("^[Tt][Hh][Ee][[:space:]]+", "", x)
}

#' Remove a redundant terminal bracketed country from an institution string.
#'
#' This is intentionally narrow. It removes only an exact terminal suffix such
#' as " [Finland]" when Country_1 is "Finland". It does not remove arbitrary
#' bracketed text and therefore cannot silently rewrite institution names.
strip_redundant_country_suffix <- function(institution, country) {
  inst <- as.character(institution)
  ctry <- as.character(country)
  out <- inst

  ok <- !is.na(inst) & nzchar(trimws(inst)) & !is.na(ctry) & nzchar(trimws(ctry))
  if (!any(ok)) return(out)

  escape_regex <- function(z) {
    gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", z)
  }

  idx <- which(ok)
  for (i in idx) {
    pat <- paste0("[[:space:]]*\\[", escape_regex(trimws(ctry[[i]])), "\\][[:space:]]*$")
    out[[i]] <- sub(pat, "", inst[[i]], ignore.case = TRUE)
    out[[i]] <- trimws(out[[i]])
  }
  out
}


#' Read explicit source-level institution/country overrides.
#'
#' These corrections are applied before institution normalization and before
#' country-to-M49 joining. They are exact-match, reviewed corrections only; no
#' fuzzy or inferred location rule is applied. Rows not marked approved are
#' exported for review but never change the analytical data.
read_institution_country_overrides <- function(path = NULL) {
  empty <- tibble::tibble(
    institution = character(), country_original = character(),
    corrected_institution = character(), country_corrected = character(),
    status = character(), override_type = character(),
    evidence_source = character(), evidence_note = character(),
    institution_key = character()
  )
  if (is.null(path) || !file.exists(path)) return(empty)

  x <- utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8",
                       check.names = FALSE)
  required <- c("institution", "country_original", "corrected_institution",
                "country_corrected", "status")
  assert_has_columns(x, required, "institution-country override table")
  for (nm in c("override_type", "evidence_source", "evidence_note")) {
    if (!nm %in% names(x)) x[[nm]] <- ""
  }

  x <- x |>
    dplyr::mutate(
      institution = trimws(institution),
      country_original = canonicalize_country_label(country_original),
      corrected_institution = trimws(corrected_institution),
      country_corrected = canonicalize_country_label(country_corrected),
      status = tolower(trimws(status)),
      override_type = tolower(trimws(override_type)),
      evidence_source = trimws(evidence_source),
      evidence_note = trimws(evidence_note),
      institution_key = normalize_institution_key(institution)
    )

  approved <- x |> dplyr::filter(status == "approved")
  bad <- approved |> dplyr::filter(
    is.na(institution_key) | !nzchar(institution_key) |
      is.na(country_original) | !nzchar(country_original) |
      is.na(country_corrected) | !nzchar(country_corrected)
  )
  if (nrow(bad)) {
    stop("Approved institution-country overrides require institution, original country, and corrected country.",
         call. = FALSE)
  }

  dup <- approved |>
    dplyr::count(institution_key, country_original) |>
    dplyr::filter(n > 1)
  if (nrow(dup)) {
    stop("Duplicate approved institution-country overrides for the same institution/country pair.",
         call. = FALSE)
  }
  x
}

#' Apply reviewed source-level institution/country corrections.
apply_institution_country_overrides <- function(positions, path = NULL, output_dir = NULL) {
  assert_has_columns(positions, c("Affiliation_1", "Country_1"), "positions")
  rules <- read_institution_country_overrides(path)
  if (!nrow(rules)) {
    return(positions |> dplyr::mutate(
      Country_raw = Country_1,
      Country_1 = canonicalize_country_label(Country_1)
    ))
  }

  n_pending <- sum(rules$status != "approved", na.rm = TRUE)
  if (n_pending > 0) {
    message("  ", n_pending, " institution-country override row(s) remain pending review and were not applied.")
  }

  approved <- rules |>
    dplyr::filter(status == "approved") |>
    dplyr::select(institution_key, country_original, corrected_institution,
                  country_corrected, override_type, evidence_source, evidence_note)

  out <- positions |>
    dplyr::mutate(
      Affiliation_source_raw = Affiliation_1,
      Country_raw = Country_1,
      Country_1 = canonicalize_country_label(Country_1),
      .source_institution_key = normalize_institution_key(Affiliation_1)
    ) |>
    dplyr::left_join(approved,
      by = c(".source_institution_key" = "institution_key",
             "Country_1" = "country_original")) |>
    dplyr::mutate(
      source_override_applied = !is.na(country_corrected) & nzchar(country_corrected),
      Affiliation_1 = dplyr::if_else(
        source_override_applied & !is.na(corrected_institution) & nzchar(corrected_institution),
        corrected_institution, Affiliation_1
      ),
      Country_1 = dplyr::if_else(source_override_applied, country_corrected, Country_1)
    )

  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    audit <- out |>
      dplyr::filter(source_override_applied) |>
      dplyr::count(Affiliation_source_raw, Country_raw, Affiliation_1, Country_1,
                   override_type, evidence_source, evidence_note,
                   name = "n_appointments")
    utils::write.csv(audit, file.path(output_dir, "institution_country_override_audit.csv"),
                     row.names = FALSE)
    pending <- rules |> dplyr::filter(status != "approved")
    if (nrow(pending)) {
      utils::write.csv(pending, file.path(output_dir, "institution_country_overrides_pending_review.csv"),
                       row.names = FALSE)
    }
  }

  out |> dplyr::select(-.source_institution_key, -corrected_institution,
                       -country_corrected, -override_type, -evidence_source,
                       -evidence_note)
}

#' Read and validate an explicit institution alias map.
#'
#' Required columns: alias, canonical_institution, status. Optional
#' `country_match` limits an alias to one affiliation country, which prevents
#' ambiguous institution labels shared by unrelated organizations in different
#' countries from being collapsed globally. Optional `alias_type` documents why
#' a mapping is valid (e.g., language_variant, department_rollup). Only
#' status == "approved" is applied.
read_institution_aliases <- function(path = NULL) {
  empty <- tibble::tibble(
    alias = character(), canonical_institution = character(), status = character(),
    country_match = character(), alias_type = character(), evidence_source = character(),
    evidence_note = character(), alias_key = character()
  )
  if (is.null(path) || !file.exists(path)) return(empty)

  x <- utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8",
                       check.names = FALSE)
  required <- c("alias", "canonical_institution", "status")
  assert_has_columns(x, required, "institution alias map")

  if (!"country_match" %in% names(x)) x$country_match <- ""
  if (!"alias_type" %in% names(x)) x$alias_type <- "semantic_alias"
  if (!"evidence_source" %in% names(x)) x$evidence_source <- ""
  if (!"evidence_note" %in% names(x)) x$evidence_note <- ""

  x <- x |>
    dplyr::mutate(
      alias = trimws(alias),
      canonical_institution = trimws(canonical_institution),
      status = tolower(trimws(status)),
      country_match = trimws(country_match),
      country_match = dplyr::if_else(
        is.na(country_match) | country_match == "", NA_character_,
        canonicalize_country_label(country_match)
      ),
      alias_type = tolower(trimws(alias_type)),
      evidence_source = trimws(evidence_source),
      evidence_note = trimws(evidence_note),
      alias_key = normalize_institution_key(alias)
    )

  bad <- x |>
    dplyr::filter(
      status == "approved" &
        (is.na(alias_key) | !nzchar(alias_key) |
           is.na(canonical_institution) | !nzchar(canonical_institution))
    )
  if (nrow(bad)) {
    stop("Approved institution alias rows must have non-empty alias and canonical_institution.",
         call. = FALSE)
  }

  approved <- x |> dplyr::filter(status == "approved")

  conflicts <- approved |>
    dplyr::group_by(alias_key, country_match) |>
    dplyr::summarise(n = dplyr::n_distinct(canonical_institution), .groups = "drop") |>
    dplyr::filter(n > 1)
  if (nrow(conflicts)) {
    stop("Institution alias map assigns the same normalized alias/country scope to multiple canonical institutions.",
         call. = FALSE)
  }

  # Duplicate mappings would cause a many-to-many join. Collapse them
  # deterministically and warn so the source CSV can be cleaned.
  duplicate_keys <- approved |>
    dplyr::count(alias_key, country_match, canonical_institution, name = "n") |>
    dplyr::filter(n > 1)
  if (nrow(duplicate_keys)) {
    warning(
      "Duplicate approved institution alias rows detected; identical mappings were collapsed before joining.",
      call. = FALSE
    )
  }

  x
}

#' Harmonize institution names conservatively.
#'
#' Step 0 removes only redundant terminal bracketed country labels.
#' Step 1 collapses syntactic variants sharing the same normalized key.
#' Step 2 applies explicitly approved semantic aliases. No fuzzy matching.
harmonize_institutions <- function(positions, alias_path = NULL, output_dir = NULL) {
  assert_has_columns(positions, c("Affiliation_1", "Country_1"), "positions")

  aliases <- read_institution_aliases(alias_path)
  approved <- aliases |>
    dplyr::filter(status == "approved") |>
    dplyr::distinct(alias_key, country_match, canonical_institution, .keep_all = TRUE)

  approved_country <- approved |>
    dplyr::filter(!is.na(country_match), nzchar(country_match)) |>
    dplyr::select(
      alias_key, country_match,
      canonical_institution_country = canonical_institution,
      alias_type_country = alias_type
    )

  approved_global <- approved |>
    dplyr::filter(is.na(country_match) | !nzchar(country_match)) |>
    dplyr::select(
      alias_key,
      canonical_institution_global = canonical_institution,
      alias_type_global = alias_type
    )

  out <- positions |>
    dplyr::mutate(
      Institution_raw = Affiliation_1,
      Institution_clean = strip_redundant_country_suffix(Affiliation_1, Country_1),
      institution_key = normalize_institution_key(Institution_clean),
      institution_display_base = dplyr::if_else(
        is.na(Institution_clean), NA_character_, strip_leading_the(Institution_clean)
      )
    )

  # Deterministic display label among strings already judged syntactically
  # equivalent by the conservative key. Lexical order is only a display-level
  # tie breaker; it does not establish substantive equivalence.
  labels <- out |>
    dplyr::filter(!is.na(institution_key), !is.na(institution_display_base)) |>
    dplyr::count(institution_key, institution_display_base, name = "n") |>
    dplyr::arrange(institution_key, dplyr::desc(n), institution_display_base) |>
    dplyr::group_by(institution_key) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(institution_key, syntactic_canonical = institution_display_base)

  out <- out |>
    dplyr::left_join(labels, by = "institution_key") |>
    dplyr::left_join(
      approved_country,
      by = c("institution_key" = "alias_key", "Country_1" = "country_match")
    ) |>
    dplyr::left_join(approved_global, by = c("institution_key" = "alias_key")) |>
    dplyr::mutate(
      canonical_institution = dplyr::coalesce(
        canonical_institution_country,
        canonical_institution_global
      ),
      applied_alias_type = dplyr::coalesce(alias_type_country, alias_type_global),
      Institution = dplyr::coalesce(canonical_institution, syntactic_canonical),
      institution_source = dplyr::case_when(
        is.na(Institution_raw) ~ "missing",
        !is.na(canonical_institution_country) ~ paste0("approved_country_alias:", applied_alias_type),
        !is.na(canonical_institution_global) ~ paste0("approved_alias:", applied_alias_type),
        Institution_raw != Institution_clean ~ "country_suffix_removed",
        Institution_clean != Institution ~ "syntactic_normalization",
        TRUE ~ "as_recorded"
      )
    ) |>
    dplyr::select(
      -canonical_institution_country, -canonical_institution_global,
      -alias_type_country, -alias_type_global, -canonical_institution,
      -applied_alias_type
    )

  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

    variant_audit <- out |>
      dplyr::filter(!is.na(Institution_raw)) |>
      dplyr::count(
        institution_key, Institution, Institution_raw, Institution_clean,
        institution_source, name = "n_appointments"
      ) |>
      dplyr::arrange(Institution, dplyr::desc(n_appointments), Institution_raw)
    utils::write.csv(
      variant_audit,
      file.path(output_dir, "institution_normalization_audit.csv"),
      row.names = FALSE
    )

    pending <- aliases |> dplyr::filter(status != "approved")
    if (nrow(pending)) {
      utils::write.csv(
        pending,
        file.path(output_dir, "institution_aliases_pending_review.csv"),
        row.names = FALSE
      )
    }
  }

  out
}

#' Read person-level institution-country adjudications.
#'
#' Institution and country are treated jointly. If a `status` column exists,
#' only rows marked approved are applied. Legacy tables without `status` are
#' treated as approved for backward compatibility.
read_affiliation_adjudications <- function(path = NULL) {
  empty <- tibble::tibble(
    person_id = character(), adjudicated_institution = character(),
    adjudicated_country = character(), evidence_source = character(),
    evidence_note = character(), status = character()
  )
  if (is.null(path) || !file.exists(path)) return(empty)

  x <- utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8",
                       check.names = FALSE)
  required <- c("person_id", "adjudicated_institution", "adjudicated_country", "evidence_source")
  assert_has_columns(x, required, "affiliation adjudication table")

  if (!"evidence_note" %in% names(x)) x$evidence_note <- ""
  if (!"status" %in% names(x)) x$status <- "approved"

  x <- x |>
    dplyr::mutate(
      person_id = trimws(person_id),
      adjudicated_institution = trimws(adjudicated_institution),
      adjudicated_country = canonicalize_country_label(adjudicated_country),
      evidence_source = trimws(evidence_source),
      evidence_note = trimws(evidence_note),
      status = tolower(trimws(status))
    ) |>
    dplyr::filter(nzchar(person_id), status == "approved")

  dup <- x |> dplyr::count(person_id) |> dplyr::filter(n > 1)
  if (nrow(dup)) {
    stop("Affiliation adjudication table must contain at most one approved row per person_id.",
         call. = FALSE)
  }

  incomplete <- x |>
    dplyr::filter(
      is.na(adjudicated_institution) | !nzchar(adjudicated_institution) |
        is.na(adjudicated_country) | !nzchar(adjudicated_country) |
        is.na(evidence_source) | !nzchar(evidence_source)
    )
  if (nrow(incomplete)) {
    stop("Every approved affiliation adjudication must provide institution, country, and evidence_source.",
         call. = FALSE)
  }

  x
}

#' Select person-level affiliation information without arbitrary tie breaking.
#'
#' A unique modal institution-country pair is selected automatically. When top
#' pairs tie, each dimension is retained only if it is identical across every
#' tied top pair. Thus, a same-country institution tie keeps country but sets
#' institution to NA. A tie spanning countries sets country to NA unless all top
#' pairs share one country. Explicit adjudications override both dimensions.
#'
#' `unresolved_action = "retain_missing"` is the recommended primary-analysis
#' policy. It keeps the dataset reproducible and lets the existing missing-data
#' logic exclude unresolved institutions from the institutional model rather
#' than inventing a primary affiliation.
select_person_affiliation_pairs <- function(positions,
                                            adjudication_path = NULL,
                                            output_dir = NULL,
                                            unresolved_action = c("retain_missing", "error", "warn")) {
  unresolved_action <- match.arg(unresolved_action)
  assert_has_columns(
    positions,
    c("person_id", "Journal", "Institution", "Country_1"),
    "harmonized positions"
  )

  adj <- read_affiliation_adjudications(adjudication_path)

  pair_counts <- positions |>
    dplyr::filter(
      !is.na(Institution), nzchar(Institution),
      !is.na(Country_1), nzchar(Country_1)
    ) |>
    dplyr::count(person_id, Institution, Country_1, name = "pair_n")

  # An adjudication may select among observed, harmonized affiliation pairs,
  # but it may not invent a new institution-country combination. This protects
  # the person-level predictor from retrospective pair construction.
  if (nrow(adj)) {
    unsupported_adj <- adj |>
      dplyr::anti_join(
        pair_counts,
        by = c(
          "person_id",
          "adjudicated_institution" = "Institution",
          "adjudicated_country" = "Country_1"
        )
      )
    if (nrow(unsupported_adj)) {
      stop(
        nrow(unsupported_adj),
        " approved affiliation adjudication(s) do not match any observed harmonized institution-country pair. ",
        "Adjudications must select an observed pair; inspect the private adjudication file and alias rules.",
        call. = FALSE
      )
    }
  }

  top <- pair_counts |>
    dplyr::group_by(person_id) |>
    dplyr::mutate(max_pair_n = max(pair_n), is_top = pair_n == max_pair_n) |>
    dplyr::ungroup()

  pair_meta <- pair_counts |>
    dplyr::group_by(person_id) |>
    dplyr::summarise(
      n_observed_pairs = dplyr::n(),
      n_complete_pair_appointments = sum(pair_n),
      .groups = "drop"
    )

  top_summary <- top |>
    dplyr::filter(is_top) |>
    dplyr::group_by(person_id) |>
    dplyr::summarise(
      n_top_pairs = dplyr::n(),
      max_pair_n = max(pair_n),
      n_top_institutions = dplyr::n_distinct(Institution),
      n_top_countries = dplyr::n_distinct(Country_1),
      auto_institution = dplyr::if_else(
        n_top_institutions == 1L, dplyr::first(Institution), NA_character_
      ),
      auto_country = dplyr::if_else(
        n_top_countries == 1L, dplyr::first(Country_1), NA_character_
      ),
      .groups = "drop"
    ) |>
    dplyr::left_join(pair_meta, by = "person_id") |>
    dplyr::mutate(
      auto_status = dplyr::case_when(
        n_top_pairs == 1L & n_observed_pairs == 1L ~ "unique_pair",
        n_top_pairs == 1L & n_observed_pairs > 1L ~ "modal_pair",
        n_top_pairs > 1L & n_top_institutions > 1L & n_top_countries == 1L ~
          "institution_tie_country_resolved",
        n_top_pairs > 1L & n_top_institutions == 1L & n_top_countries > 1L ~
          "country_tie_institution_resolved",
        n_top_pairs > 1L & n_top_institutions > 1L & n_top_countries > 1L ~
          "institution_country_tie",
        TRUE ~ "pair_tie_other"
      )
    )

  people <- positions |>
    dplyr::distinct(person_id) |>
    dplyr::left_join(top_summary, by = "person_id") |>
    dplyr::left_join(
      adj |>
        dplyr::select(
          person_id, adjudicated_institution, adjudicated_country,
          evidence_source, evidence_note
        ),
      by = "person_id"
    ) |>
    dplyr::mutate(
      has_adjudication = !is.na(adjudicated_institution) & nzchar(adjudicated_institution),
      Institution = dplyr::if_else(has_adjudication, adjudicated_institution, auto_institution),
      Country_1 = dplyr::if_else(has_adjudication, adjudicated_country, auto_country),
      affiliation_status = dplyr::case_when(
        has_adjudication ~ "adjudicated",
        is.na(n_top_pairs) ~ "no_complete_pair",
        TRUE ~ auto_status
      ),
      affiliation_source = affiliation_status,
      institution_resolved = !is.na(Institution) & nzchar(Institution),
      country_resolved = !is.na(Country_1) & nzchar(Country_1)
    )

  # If no complete pair exists, retain country only when it has a unique modal
  # value across appointments. Institution remains missing because combining
  # separately selected dimensions could create an institution-country pair
  # never observed in the source data.
  country_counts <- positions |>
    dplyr::filter(!is.na(Country_1), nzchar(Country_1)) |>
    dplyr::count(person_id, Country_1, name = "country_n") |>
    dplyr::group_by(person_id) |>
    dplyr::mutate(max_country_n = max(country_n), is_country_top = country_n == max_country_n) |>
    dplyr::filter(is_country_top) |>
    dplyr::summarise(
      n_top_countries_fallback = dplyr::n(),
      country_fallback = dplyr::if_else(
        n_top_countries_fallback == 1L, dplyr::first(Country_1), NA_character_
      ),
      .groups = "drop"
    )

  people <- people |>
    dplyr::left_join(country_counts, by = "person_id") |>
    dplyr::mutate(
      Country_1 = dplyr::if_else(
        affiliation_status == "no_complete_pair" & !is.na(country_fallback),
        country_fallback,
        Country_1
      ),
      affiliation_status = dplyr::case_when(
        affiliation_status == "no_complete_pair" & !is.na(Country_1) ~
          "country_only_institution_missing",
        TRUE ~ affiliation_status
      ),
      affiliation_source = affiliation_status,
      institution_resolved = !is.na(Institution) & nzchar(Institution),
      country_resolved = !is.na(Country_1) & nzchar(Country_1)
    )

  unresolved <- people |>
    dplyr::filter(
      !has_adjudication,
      affiliation_status %in% c(
        "institution_tie_country_resolved",
        "country_tie_institution_resolved",
        "institution_country_tie",
        "pair_tie_other"
      )
    )

  if (!is.null(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

    selected_for_audit <- people |>
      dplyr::select(
        person_id, affiliation_status, Institution, Country_1,
        institution_resolved, country_resolved,
        n_observed_pairs, n_top_pairs, n_top_institutions, n_top_countries,
        max_pair_n, evidence_source, evidence_note
      )

    conflict_detail <- top |>
      dplyr::left_join(
        positions |>
          dplyr::select(person_id, Journal, Institution, Country_1) |>
          dplyr::distinct(),
        by = c("person_id", "Institution", "Country_1")
      ) |>
      dplyr::left_join(selected_for_audit, by = "person_id") |>
      dplyr::filter(n_observed_pairs > 1 | affiliation_status == "adjudicated") |>
      dplyr::arrange(person_id, dplyr::desc(pair_n), Institution.x, Country_1.x, Journal) |>
      dplyr::rename(
        candidate_institution = Institution.x,
        candidate_country = Country_1.x,
        selected_institution = Institution.y,
        selected_country = Country_1.y
      )

    utils::write.csv(
      conflict_detail,
      file.path(output_dir, "person_affiliation_pair_audit.csv"),
      row.names = FALSE
    )

    status_counts <- people |>
      dplyr::count(affiliation_status, institution_resolved, country_resolved, name = "n_persons") |>
      dplyr::arrange(affiliation_status)
    utils::write.csv(
      status_counts,
      file.path(output_dir, "person_affiliation_status_counts.csv"),
      row.names = FALSE
    )

    if (nrow(unresolved)) {
      journals_by_person <- positions |>
        dplyr::filter(person_id %in% unresolved$person_id) |>
        dplyr::group_by(person_id) |>
        dplyr::summarise(
          journals = paste(sort(unique(Journal)), collapse = " | "),
          .groups = "drop"
        )

      review <- top |>
        dplyr::filter(person_id %in% unresolved$person_id, is_top) |>
        dplyr::group_by(person_id) |>
        dplyr::summarise(
          tied_pairs = paste0(
            Institution, " [", Country_1, "] (n=", pair_n, ")",
            collapse = " | "
          ),
          .groups = "drop"
        ) |>
        dplyr::left_join(journals_by_person, by = "person_id") |>
        dplyr::left_join(
          people |>
            dplyr::select(
              person_id, affiliation_status, Institution, Country_1,
              institution_resolved, country_resolved
            ),
          by = "person_id"
        ) |>
        dplyr::rename(
          retained_institution = Institution,
          retained_country = Country_1
        ) |>
        dplyr::mutate(
          adjudicated_institution = "",
          adjudicated_country = "",
          evidence_source = "",
          evidence_note = "",
          status = "review_optional"
        )

      utils::write.csv(
        review,
        file.path(output_dir, "person_affiliation_review.csv"),
        row.names = FALSE
      )

      if (unresolved_action == "error") {
        required <- review |>
          dplyr::mutate(status = "review_required")
        utils::write.csv(
          required,
          file.path(output_dir, "person_affiliation_adjudication_REQUIRED.csv"),
          row.names = FALSE
        )
      }
    }
  }

  if (nrow(unresolved)) {
    msg <- paste0(
      nrow(unresolved),
      " editor(s) have tied top affiliation pairs without adjudication. ",
      "Ambiguous dimensions were retained as missing; unambiguous dimensions were preserved."
    )
    if (unresolved_action == "error") {
      stop(
        paste0(msg, " Review output/selection/person_affiliation_adjudication_REQUIRED.csv."),
        call. = FALSE
      )
    } else if (unresolved_action == "warn") {
      warning(msg, call. = FALSE)
    } else {
      message("  ", msg)
    }
  }

  people |>
    dplyr::select(
      -has_adjudication, -auto_institution, -auto_country, -auto_status,
      -adjudicated_institution, -adjudicated_country,
      -country_fallback, -n_top_countries_fallback
    )
}
