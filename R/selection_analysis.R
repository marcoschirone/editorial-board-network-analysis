# R/selection_analysis.R
# Selection into interlocking editorship: person-level benchmarking of the
# interlocking subset against the full editorial population.
#
# All functions here operate on the full population file (one row per editorial
# position across all 30 journals). The same reconstructed population is used
# for interlocking status, network membership, and selection analysis.

# Person-name disambiguation (audit_name_formats(), find_name_variants(),
# apply_name_merges()) lives in the companion file R/person_disambiguation.R.
# Sourced here, guarded, so build_person_level()'s merges_path argument and
# run_selection_analysis()'s audits work regardless of caller.
if (!exists("apply_name_merges", mode = "function")) {
  source("R/person_disambiguation.R")
}
if (!exists("harmonize_institutions", mode = "function")) {
  source("R/institution_harmonization.R")
}

# ---------------------------------------------------------------------------
# 1a. Role seniority, duplicate-role collapse, placeholder affiliations
# ---------------------------------------------------------------------------

#' Rank an editorial role title by seniority (1 = most senior).
#'
#' Used only to choose which row survives when one person holds several roles
#' on the same journal. The dataset records such people once per role, so a
#' person-journal pair can occupy two rows; those are two roles, not two
#' appointments, and must be collapsed before any position-based count.
role_seniority <- function(role) {
  r <- tolower(trimws(as.character(role)))
  dplyr::case_when(
    grepl("editor-in-chief|chief editor", r) & !grepl("emeritus|former|past|previous|deputy", r) ~ 1L,
    grepl("deputy editor-in-chief|co-editor-in-chief", r)                                        ~ 2L,
    grepl("executive editor|senior editor|managing editor", r)                                   ~ 3L,
    grepl("^editor$|^co-editor$|^editor,|subject editor|topic editor|section editor", r)         ~ 4L,
    grepl("associate editor", r) & !grepl("junior", r)                                           ~ 5L,
    grepl("junior associate editor|assistant editor", r)                                         ~ 6L,
    grepl("advisory|honorary|emeritus|former|past|previous", r)                                  ~ 8L,
    grepl("early career|junior", r)                                                              ~ 9L,
    TRUE                                                                                          ~ 7L
  )
}

#' Collapse person-journal pairs that occupy more than one row.
#'
#' Keeps the row for the most senior role and records how many roles were
#' merged. Where the merged rows disagree on affiliation, the surviving
#' affiliation is the one attached to the most senior role, and the case is
#' flagged in `affiliation_conflict` for manual review.
collapse_duplicate_roles <- function(positions, output_dir = NULL) {
  message("Collapsing duplicate person-journal rows (multiple roles, one board)...")
  before <- nrow(positions)

  out <- positions |>
    dplyr::mutate(.rank = role_seniority(.data[["Editorial Role"]])) |>
    dplyr::group_by(person_id, Journal) |>
    dplyr::mutate(
      n_roles              = dplyr::n(),
      affiliation_conflict = dplyr::n_distinct(Affiliation_1) > 1,
      roles_merged         = paste(sort(unique(.data[["Editorial Role"]])), collapse = " | ")
    ) |>
    dplyr::arrange(.rank, .by_group = TRUE) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(-.rank)

  conflicts <- out |> dplyr::filter(affiliation_conflict)
  message(sprintf("  %d rows -> %d appointments (%d collapsed); %d with conflicting affiliations.",
                  before, nrow(out), before - nrow(out), nrow(conflicts)))

  if (!is.null(output_dir) && nrow(conflicts)) {
    detail <- positions |>
      dplyr::semi_join(conflicts, by = c("person_id", "Journal")) |>
      dplyr::select(person_id, Journal, dplyr::all_of("Editorial Role"),
                    Affiliation_Original, Country_1) |>
      dplyr::arrange(person_id, Journal)
    utils::write.csv(detail, file.path(output_dir, "role_affiliation_conflicts.csv"),
                     row.names = FALSE)
  }
  out
}

#' Replace placeholder affiliation strings with NA.
#'
#' A literal "-" is not an institution. Left in place it forms a spurious
#' institution whose members all receive an inflated institutional
#' representation score.
recode_placeholder_affiliations <- function(positions,
                                            placeholders = c("-", "", "NA", "n/a", "unknown")) {
  n_before <- sum(positions$Affiliation_1 %in% placeholders, na.rm = TRUE)
  positions <- positions |>
    dplyr::mutate(Affiliation_1 = dplyr::if_else(
      tolower(trimws(dplyr::coalesce(Affiliation_1, ""))) %in% tolower(placeholders),
      NA_character_, Affiliation_1))
  message(sprintf("  %d placeholder affiliations recoded to NA.", n_before))
  positions
}

# ---------------------------------------------------------------------------
# 1. Person-level reconstruction
# ---------------------------------------------------------------------------

#' Collapse the position-level file to one row per unique editor.
#'
#' @param full_path Path to the all-editors workbook (position level).
#' @param lookup_path Path to country -> UN M49 lookup CSV.
#' @param id_col Column identifying a unique person.
#' @param adjudication_path Optional one-row-per-person CSV containing
#'   `person_id`, `adjudicated_institution`, and `adjudicated_country`. The
#'   institution-country pair is applied jointly.
#' @param institution_alias_path Optional reviewed alias map used to collapse
#'   language variants and department-to-parent variants after conservative
#'   syntactic normalization.
#' @param institution_country_override_path Optional reviewed exact-match table
#'   for source-level institution/country corrections applied before M49 mapping.
#' @param unresolved_affiliation_action What to do when two or more complete
#'   institution-country pairs are tied for the modal count and no adjudication
#'   exists. The primary publication pipeline uses "retain_missing": ambiguous
#'   dimensions remain NA while unambiguous dimensions are preserved.
#' @param write_adjudication_to Optional directory; if set, writes a template
#'   listing every multi-country editor with all their countries and journals.
#' @param merges_path Optional CSV of confirmed name-variant merges (the
#'   reviewed output of `find_name_variants()`; see `apply_name_merges()` in
#'   R/person_disambiguation.R). NULL leaves name records as they are.
build_person_level <- function(full_path,
                               lookup_path,
                               id_col = "Name",
                               adjudication_path = NULL,
                               institution_alias_path = NULL,
                               institution_country_override_path = NULL,
                               unresolved_affiliation_action = c("retain_missing", "error", "warn"),
                               write_adjudication_to = NULL,
                               collapse_roles = TRUE,
                               recode_placeholders = TRUE,
                               merges_path = NULL) {
  unresolved_affiliation_action <- match.arg(unresolved_affiliation_action)
  message("Building person-level analytical dataset...")

  positions <- readxl::read_xlsx(full_path)
  lookup <- utils::read.csv(lookup_path, stringsAsFactors = FALSE)

  # Publication-readiness metadata for affiliation corrections. Pending source
  # overrides are intentionally not applied, but their count is carried into
  # the machine-generated manifest so a release cannot silently ignore them.
  override_rules <- read_institution_country_overrides(institution_country_override_path)
  n_pending_source_overrides <- sum(override_rules$status != "approved", na.rm = TRUE)

  # Apply reviewed source-level corrections before M49 validation and joins.
  # This keeps country harmonization auditable and prevents institutional
  # representation from grouping obviously inconsistent location records.
  positions <- apply_institution_country_overrides(
    positions,
    path = institution_country_override_path,
    output_dir = write_adjudication_to
  )

  missing_countries <- setdiff(unique(positions$Country_1), lookup$Country_1)
  if (length(missing_countries)) {
    stop("Countries absent from the M49 lookup: ",
         paste(missing_countries, collapse = ", "), call. = FALSE)
  }

  positions <- positions |>
    dplyr::rename(person_id = dplyr::all_of(id_col)) |>
    dplyr::left_join(lookup, by = "Country_1")

  # Must run before collapse_duplicate_roles(): merging changes which
  # person-journal pairs exist, so role collapsing has to see the merged
  # identities, not the pre-merge name-variant records.
  positions <- apply_name_merges(positions, merges_path, output_dir = write_adjudication_to)
  if (!"person_id_original" %in% names(positions)) {
    positions$person_id_original <- positions$person_id
  }

  if (collapse_roles)      positions <- collapse_duplicate_roles(positions, write_adjudication_to)
  if (recode_placeholders) positions <- recode_placeholder_affiliations(positions)

  # Institution normalization is deliberately conservative. Purely syntactic
  # variants are collapsed automatically; semantic equivalences are applied
  # only from an explicitly reviewed alias table.
  positions <- harmonize_institutions(
    positions,
    alias_path = institution_alias_path,
    output_dir = write_adjudication_to
  )

  # Select institution and country jointly. A unique modal pair is automatic.
  # Tied top pairs never fall back to spreadsheet row order: unambiguous
  # dimensions are retained, ambiguous dimensions remain missing unless an
  # approved adjudication selects one of the observed harmonized pairs.
  pair_assignment <- select_person_affiliation_pairs(
    positions,
    adjudication_path = adjudication_path,
    output_dir = write_adjudication_to,
    unresolved_action = unresolved_affiliation_action
  )

  person <- positions |>
    dplyr::group_by(person_id) |>
    dplyr::summarise(
      n_positions        = dplyr::n(),
      n_journals         = dplyr::n_distinct(Journal),
      multi_country      = dplyr::n_distinct(Country_1, na.rm = TRUE) > 1,
      multi_institution  = dplyr::n_distinct(Institution, na.rm = TRUE) > 1,
      person_id_original = paste(sort(unique(person_id_original)), collapse = " | "),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      pair_assignment |>
        dplyr::select(
          person_id, Institution, Country_1, affiliation_source, affiliation_status,
          institution_resolved, country_resolved, n_observed_pairs, n_top_pairs,
          n_top_institutions, n_top_countries, max_pair_n
        ),
      by = "person_id"
    )

  message("  ", sum(person$affiliation_source == "adjudicated", na.rm = TRUE),
          " person-level affiliation pairs taken from adjudication file.")
  message("  ", sum(!person$institution_resolved, na.rm = TRUE),
          " person(s) have unresolved/missing institution; ",
          sum(!person$country_resolved, na.rm = TRUE),
          " have unresolved/missing country.")

  person <- person |>
    dplyr::left_join(lookup, by = "Country_1") |>
    dplyr::mutate(
      interlocking = n_journals >= 2
    )

  # Leave-one-out institutional representation: the number of other editors
  # with the same canonical institution-country affiliation. The editor is
  # excluded because a self-inclusive count would partly measure the outcome.
  # Counting by institution and country keeps identically named institutions
  # in different countries, and the country branches of one organization,
  # separate.
  inst_n <- person |>
    dplyr::filter(
      !is.na(Institution), nzchar(Institution),
      !is.na(Country_1), nzchar(Country_1)
    ) |>
    dplyr::count(Institution, Country_1, name = "inst_total")

  person <- person |>
    dplyr::left_join(inst_n, by = c("Institution", "Country_1")) |>
    dplyr::mutate(
      # An unknown institution is coded NA rather than 0, because 0 would mean
      # "no other editors at the same institution". Complete-case estimation
      # drops these editors; miss_inst flags them for the missingness-indicator
      # sensitivity model.
      miss_inst      = as.integer(
        is.na(Institution) | !nzchar(dplyr::coalesce(Institution, "")) |
          is.na(Country_1) | !nzchar(dplyr::coalesce(Country_1, ""))
      ),
      inst_loo       = dplyr::if_else(
        miss_inst == 1L, NA_integer_,
        dplyr::coalesce(inst_total, 1L) - 1L
      ),
      log_inst_loo   = log1p(inst_loo),
      log_inst_naive = dplyr::if_else(
        miss_inst == 1L, NA_real_, log1p(as.numeric(inst_total))
      )
    )

  if (!is.null(write_adjudication_to)) {
    person_inst <- person |>
      dplyr::filter(
        !is.na(Institution), nzchar(Institution),
        !is.na(Country_1), nzchar(Country_1)
      ) |>
      dplyr::count(Institution, Country_1, name = "n_editors", sort = TRUE)
    appointment_inst <- positions |>
      dplyr::filter(
        !is.na(Institution), nzchar(Institution),
        !is.na(Country_1), nzchar(Country_1)
      ) |>
      dplyr::count(Institution, Country_1, name = "n_appointments", sort = TRUE)
    utils::write.csv(
      person_inst,
      file.path(write_adjudication_to, "institution_counts_person.csv"),
      row.names = FALSE
    )
    utils::write.csv(
      appointment_inst,
      file.path(write_adjudication_to, "institution_counts_appointments.csv"),
      row.names = FALSE
    )

    person_country <- person |>
      dplyr::filter(!is.na(Country_1), nzchar(Country_1)) |>
      dplyr::count(Country_1, name = "n_editors", sort = TRUE)
    appointment_country <- positions |>
      dplyr::filter(!is.na(Country_1), nzchar(Country_1)) |>
      dplyr::count(Country_1, name = "n_appointments", sort = TRUE)
    utils::write.csv(
      person_country,
      file.path(write_adjudication_to, "country_counts_person.csv"),
      row.names = FALSE
    )
    utils::write.csv(
      appointment_country,
      file.path(write_adjudication_to, "country_counts_appointments.csv"),
      row.names = FALSE
    )
  }

  affiliation_qc <- tibble::tibble(
    metric = c(
      "pending_source_overrides",
      "unresolved_person_institution",
      "unresolved_person_country",
      "adjudicated_person_affiliations"
    ),
    value = c(
      n_pending_source_overrides,
      sum(is.na(person$Institution) | !nzchar(dplyr::coalesce(person$Institution, ""))),
      sum(is.na(person$Country_1) | !nzchar(dplyr::coalesce(person$Country_1, ""))),
      sum(person$affiliation_source == "adjudicated", na.rm = TRUE)
    )
  )

  if (!is.null(write_adjudication_to)) {
    utils::write.csv(
      affiliation_qc,
      file.path(write_adjudication_to, "affiliation_publication_readiness.csv"),
      row.names = FALSE
    )
  }

  if (n_pending_source_overrides > 0) {
    message("  PUBLICATION CHECK: ", n_pending_source_overrides,
            " source-level institution/country override row(s) remain pending review.")
  }

  message(sprintf("  %d positions -> %d unique editors; %d interlocking (%.1f%%)",
                  nrow(positions), nrow(person), sum(person$interlocking),
                  100 * mean(person$interlocking)))
  message(sprintf(
    "  Canonical institution-country units: %d across persons; %d across appointments.",
    dplyr::n_distinct(paste(person$Institution, person$Country_1, sep = " || ")[
      !is.na(person$Institution) & !is.na(person$Country_1)
    ]),
    dplyr::n_distinct(paste(positions$Institution, positions$Country_1, sep = " || ")[
      !is.na(positions$Institution) & !is.na(positions$Country_1)
    ])
  ))
  list(positions = positions, person = person, affiliation_qc = affiliation_qc)
}

# ---------------------------------------------------------------------------
# 2. Definition reconciliation
# ---------------------------------------------------------------------------

#' Count interlocking and super-interlocking editors under every plausible
#' operational definition, so the manuscript can state one and report the rest.
reconcile_interlocking_definitions <- function(person, output_dir = NULL) {
  message("Reconciling interlocking definitions...")
  out <- tibble::tibble(
    definition = c("distinct journals >= 2", "rows (positions) >= 2",
                   "distinct journals >= 3", "rows (positions) >= 3"),
    n_editors  = c(sum(person$n_journals >= 2), sum(person$n_positions >= 2),
                   sum(person$n_journals >= 3), sum(person$n_positions >= 3))
  ) |>
    dplyr::mutate(share_of_population = round(100 * n_editors / nrow(person), 2))

  print(as.data.frame(out))
  message("  NOTE: row-based definitions count duplicate person-journal rows ",
          "(multiple affiliations for the same appointment) as separate ",
          "positions. Check duplicate_person_journal_rows.csv before using them.")
  if (!is.null(output_dir)) {
    utils::write.csv(out, file.path(output_dir, "interlocking_definitions.csv"),
                     row.names = FALSE)
  }
  out
}

#' Audit person-journal pairs that appear on more than one row.
#'
#' These are usually a single appointment recorded under two affiliations, not
#' two appointments. They inflate any position-count definition of interlocking
#' and must be resolved before a row-based definition is used or reported.
audit_duplicate_rows <- function(positions, output_dir = NULL) {
  message("Auditing duplicate person-journal rows...")
  dup <- positions |>
    dplyr::count(person_id, Journal, name = "n_rows") |>
    dplyr::filter(n_rows > 1)
  detail <- positions |>
    dplyr::semi_join(dup, by = c("person_id", "Journal")) |>
    dplyr::select(person_id, Journal, Country_1, Affiliation_1) |>
    dplyr::arrange(person_id, Journal)
  message(sprintf("  %d person-journal pairs on multiple rows (%d extra rows).",
                  nrow(dup), sum(dup$n_rows - 1)))
  if (!is.null(output_dir)) {
    utils::write.csv(detail, file.path(output_dir, "duplicate_person_journal_rows.csv"),
                     row.names = FALSE)
  }
  detail
}

# ---------------------------------------------------------------------------
# 3. Enrichment tests
# ---------------------------------------------------------------------------

#' Observed-versus-expected representation among interlocking editors.
#'
#' Each level is tested against all other editors with Fisher's exact test.
#' Holm correction is applied across levels within a variable. These
#' contrasts are exploratory.
run_enrichment_tests <- function(person, var, output_dir = NULL) {
  message("Enrichment tests for: ", var)
  x <- person[[var]]
  keep <- !is.na(x)
  x <- x[keep]
  y <- person$interlocking[keep]
  N <- length(y); K <- sum(y)

  res <- lapply(sort(unique(as.character(x))), function(lev) {
    in_lev <- as.character(x) == lev
    a <- sum(in_lev & y); b <- sum(in_lev & !y)
    c_ <- sum(!in_lev & y); d <- sum(!in_lev & !y)
    ft <- stats::fisher.test(matrix(c(a, b, c_, d), nrow = 2, byrow = TRUE))
    tibble::tibble(
      variable = var, level = lev,
      n_population = a + b, share_population = 100 * (a + b) / N,
      n_interlocking = a, share_interlocking = 100 * a / K,
      rate_interlocking = 100 * a / (a + b),
      odds_ratio = unname(ft$estimate),
      ci_low = ft$conf.int[1], ci_high = ft$conf.int[2],
      p_value = ft$p.value
    )
  })

  res <- dplyr::bind_rows(res) |>
    dplyr::mutate(p_holm = stats::p.adjust(p_value, method = "holm")) |>
    dplyr::arrange(dplyr::desc(n_population))

  print(as.data.frame(res), digits = 3)
  if (!is.null(output_dir)) {
    utils::write.csv(res, file.path(output_dir, paste0("enrichment_", var, ".csv")),
                     row.names = FALSE)
  }
  res
}

#' Single focal contrast (one level versus all others), without multiplicity
#' correction.
#'
#' Europe was selected after the continent residuals were inspected, so this
#' contrast is post hoc and exploratory. The Holm-adjusted p-value from
#' run_enrichment_tests() is the one reported with it.
run_focal_contrast <- function(person, var, level) {
  in_lev <- as.character(person[[var]]) == level
  y <- person$interlocking
  ft <- stats::fisher.test(table(factor(in_lev, c(TRUE, FALSE)),
                                 factor(y, c(TRUE, FALSE))))
  tibble::tibble(
    variable = var, level = level,
    n_interlocking_in_level = sum(in_lev & y), n_in_level = sum(in_lev),
    odds_ratio = unname(ft$estimate),
    ci_low = ft$conf.int[1], ci_high = ft$conf.int[2],
    p_value = ft$p.value
  )
}

#' Omnibus test of interlocking status across a full geographic contingency
#' table (level x interlocking), as opposed to the per-level 2x2 tests with
#' Holm correction in run_enrichment_tests(). Answers "does interlocking
#' status vary by geography at all", not "which level".
#'
#' Fisher's exact test on the full table uses Monte Carlo simulation
#' (simulate.p.value = TRUE) because exact computation is infeasible once the
#' table has this many cells/small counts. Chi-square is reported alongside
#' for its standardised residuals, which flag which cells drive any
#' association -- the simulated Fisher p-value alone carries no such
#' diagnostic. With many small expected counts (routine for Subregion),
#' chi-square's own asymptotic p-value is unreliable; Fisher's is primary,
#' chi-square/residuals are diagnostic only.
run_omnibus_geography <- function(person, var, output_dir = NULL,
                                  B = 100000, seed = 123) {
  message("Omnibus geographic test: ", var)
  x <- person[[var]]
  keep <- !is.na(x)
  tab <- table(level = factor(x[keep]),
              interlocking = factor(person$interlocking[keep], c(FALSE, TRUE)))

  set.seed(seed)
  ft <- stats::fisher.test(tab, simulate.p.value = TRUE, B = B)
  ct <- suppressWarnings(stats::chisq.test(tab))

  message(sprintf("  Fisher exact (simulated, B=%d): p = %.4f", B, ft$p.value))
  message(sprintf("  Chi-square: X2(%d) = %.3f, p = %.4f%s", ct$parameter, ct$statistic,
                  ct$p.value,
                  if (any(ct$expected < 5)) " -- some expected counts < 5, asymptotic p unreliable, Fisher is primary" else ""))

  residuals <- tibble::tibble(
    level = rownames(tab),
    n_population = as.integer(rowSums(tab)),
    n_interlocking = as.integer(tab[, "TRUE"]),
    expected_interlocking = ct$expected[, "TRUE"],
    std_resid = ct$stdres[, "TRUE"]
  ) |> dplyr::arrange(dplyr::desc(abs(std_resid)))

  print(as.data.frame(residuals), digits = 3)

  summary_tbl <- tibble::tibble(
    variable = var,
    fisher_p_value = ft$p.value, fisher_B = B,
    chisq_statistic = unname(ct$statistic), chisq_df = unname(ct$parameter),
    chisq_p_value = ct$p.value
  )
  if (!is.null(output_dir)) {
    utils::write.csv(summary_tbl, file.path(output_dir, paste0("omnibus_", var, ".csv")),
                     row.names = FALSE)
    utils::write.csv(residuals, file.path(output_dir, paste0("omnibus_", var, "_std_residuals.csv")),
                     row.names = FALSE)
  }
  list(summary = summary_tbl, residuals = residuals, table = tab)
}

# ---------------------------------------------------------------------------
# 4. Multivariable model
# ---------------------------------------------------------------------------

#' Logistic model of interlocking status.
#'
#' Uses Firth penalised likelihood when logistf is installed (recommended for
#' rare events); otherwise falls back to maximum-likelihood glm and says so.
#' Institutional representation must be the leave-one-out version.
fit_selection_model <- function(person, output_dir = NULL,
                                formula = interlocking ~ Europe + log_inst_loo) {
  message("Fitting selection model...")
  dat_full <- person |>
    dplyr::mutate(Europe = as.integer(Continent == "Europe"),
                  interlocking = as.integer(interlocking))

  # Complete-case model: editors with unknown institution have NA on
  # log_inst_loo (see build_person_level()) and are removed here explicitly
  # rather than through each modeling function's na.action default.
  model_vars <- all.vars(formula)
  dat <- dat_full[stats::complete.cases(dat_full[, model_vars]), ]
  n_dropped <- nrow(dat_full) - nrow(dat)
  if (n_dropped > 0) {
    n_inst_missing <- sum(is.na(dat_full$log_inst_loo))
    n_geo_missing <- sum(is.na(dat_full$Europe))
    message(sprintf(
      "  %d editors dropped from the complete-case model (%d unresolved/missing institution; %d unresolved/missing Europe indicator; categories may overlap).",
      n_dropped, n_inst_missing, n_geo_missing
    ))
  }

  use_firth <- requireNamespace("logistf", quietly = TRUE)
  if (use_firth) {
    fit <- logistf::logistf(formula, data = dat)
    out <- tibble::tibble(
      term = names(stats::coef(fit)),
      estimate = unname(stats::coef(fit)),
      odds_ratio = exp(unname(stats::coef(fit))),
      ci_low = exp(fit$ci.lower), ci_high = exp(fit$ci.upper),
      p_value = fit$prob,
      method = "Firth penalised likelihood"
    )
  } else {
    message("  logistf not installed; using maximum-likelihood glm.")
    fit <- stats::glm(formula, data = dat, family = stats::binomial())
    ci <- suppressMessages(stats::confint(fit))
    s <- summary(fit)$coefficients
    out <- tibble::tibble(
      term = rownames(s), estimate = s[, 1],
      odds_ratio = exp(s[, 1]),
      ci_low = exp(ci[, 1]), ci_high = exp(ci[, 2]),
      p_value = s[, 4], method = "ML glm"
    )
  }

  # Endogeneity demonstration: refit with the self-inclusive institution count.
  naive_formula <- stats::update(formula, . ~ . - log_inst_loo + log_inst_naive)
  naive <- stats::glm(naive_formula, data = dat, family = stats::binomial())
  message(sprintf("  institutional OR: leave-one-out = %.3f | self-inclusive = %.3f",
                  out$odds_ratio[out$term == "log_inst_loo"],
                  exp(unname(stats::coef(naive)["log_inst_naive"]))))

  # Model-fit diagnostics (likelihood-ratio test, AIC, pseudo-R2).
  null_fit <- stats::glm(stats::update(formula, . ~ 1), data = dat,
                         family = stats::binomial())
  ml_fit <- stats::glm(formula, data = dat, family = stats::binomial())
  ll_full <- as.numeric(stats::logLik(ml_fit))
  ll_null <- as.numeric(stats::logLik(null_fit))
  lr <- 2 * (ll_full - ll_null)
  df_lr <- length(stats::coef(ml_fit)) - 1

  diagnostics <- tibble::tibble(
    n_observations = nrow(dat),
    n_events = sum(dat$interlocking),
    events_per_parameter = sum(dat$interlocking) / (length(stats::coef(ml_fit)) - 1),
    mcfadden_pseudo_r2 = 1 - ll_full / ll_null,
    lr_chisq = lr, lr_df = df_lr,
    lr_p_value = stats::pchisq(lr, df_lr, lower.tail = FALSE),
    aic_model = stats::AIC(ml_fit), aic_null = stats::AIC(null_fit)
  )
  message(sprintf("  n = %d, events = %d, McFadden pseudo-R2 = %.4f, LR chi2(%d) = %.1f, p = %.2g",
                  diagnostics$n_observations, diagnostics$n_events,
                  diagnostics$mcfadden_pseudo_r2, df_lr, lr, diagnostics$lr_p_value))

  print(as.data.frame(out), digits = 3)
  if (!is.null(output_dir)) {
    utils::write.csv(out, file.path(output_dir, "selection_model.csv"),
                     row.names = FALSE)
    utils::write.csv(diagnostics, file.path(output_dir, "selection_model_diagnostics.csv"),
                     row.names = FALSE)
  }

  missingness <- fit_missingness_indicator_model(dat_full, formula, use_firth, output_dir)

  list(table = out, diagnostics = diagnostics, fit = fit, naive_fit = naive,
       missingness = missingness)
}

#' Sensitivity model that retains editors with unknown institution.
#'
#' The primary model drops them via complete-case estimation. Here, instead,
#' log_inst_loo is filled with 0 for them and miss_inst is added as a
#' covariate, so the model uses every editor while still distinguishing
#' "unknown institution" from "confirmed zero institutional representation".
fit_missingness_indicator_model <- function(dat_full, formula, use_firth, output_dir = NULL) {
  # This sensitivity addresses institutional missingness only. Editors missing
  # another model variable (e.g., unresolved country/Europe) remain excluded;
  # we do not impute geography merely to maximize N.
  other_vars <- setdiff(all.vars(formula), "log_inst_loo")
  keep_other <- stats::complete.cases(dat_full[, other_vars, drop = FALSE])
  dat <- dat_full[keep_other, , drop = FALSE] |>
    dplyr::mutate(log_inst_loo = dplyr::coalesce(log_inst_loo, 0))
  ind_formula <- stats::update(formula, . ~ . + miss_inst)

  if (use_firth) {
    fit <- logistf::logistf(ind_formula, data = dat)
    out <- tibble::tibble(
      term = names(stats::coef(fit)),
      estimate = unname(stats::coef(fit)),
      odds_ratio = exp(unname(stats::coef(fit))),
      ci_low = exp(fit$ci.lower), ci_high = exp(fit$ci.upper),
      p_value = fit$prob,
      method = "Firth penalised likelihood"
    )
  } else {
    fit <- stats::glm(ind_formula, data = dat, family = stats::binomial())
    ci <- suppressMessages(stats::confint(fit))
    s <- summary(fit)$coefficients
    out <- tibble::tibble(
      term = rownames(s), estimate = s[, 1],
      odds_ratio = exp(s[, 1]),
      ci_low = exp(ci[, 1]), ci_high = exp(ci[, 2]),
      p_value = s[, 4], method = "ML glm"
    )
  }

  message(sprintf(
    "  institutional-missingness sensitivity: OR = %.3f, p = %.4g (n = %d of %d; non-institution model variables must be observed)",
    out$odds_ratio[out$term == "log_inst_loo"],
    out$p_value[out$term == "log_inst_loo"], nrow(dat), nrow(dat_full)
  ))
  print(as.data.frame(out), digits = 3)
  if (!is.null(output_dir)) {
    utils::write.csv(out, file.path(output_dir, "selection_model_missingness.csv"),
                     row.names = FALSE)
  }
  out
}

# ---------------------------------------------------------------------------
# 5. Sensitivity to the operational definition
# ---------------------------------------------------------------------------

#' Re-run a planned contrast under alternative definitions of interlocking.
run_definition_sensitivity <- function(person, var, level, output_dir = NULL) {
  message("Definition sensitivity for ", var, " = ", level)
  defs <- list(
    "distinct journals >= 2" = person$n_journals >= 2,
    "positions >= 2"         = person$n_positions >= 2,
    "distinct journals >= 3" = person$n_journals >= 3
  )
  res <- dplyr::bind_rows(lapply(names(defs), function(nm) {
    p <- person; p$interlocking <- defs[[nm]]
    if (sum(p$interlocking) < 5) return(NULL)
    run_focal_contrast(p, var, level) |>
      dplyr::mutate(definition = nm, n_interlocking = sum(p$interlocking))
  }))
  print(as.data.frame(res), digits = 3)
  if (!is.null(output_dir)) {
    utils::write.csv(res, file.path(output_dir, "definition_sensitivity.csv"),
                     row.names = FALSE)
  }
  res
}

# ---------------------------------------------------------------------------
# 6. Minimum detectable effect
# ---------------------------------------------------------------------------

#' Simulate power across a grid of odds ratios, so null results can be reported
#' with an explicit statement of what they do and do not exclude.
estimate_power_by_or <- function(p_baseline, n_interlocking, n_rest,
                         or_grid = c(1.3, 1.5, 1.75, 2, 2.5, 3),
                         n_sim = 2000, alpha = 0.05, seed = 123,
                         label = "", output_dir = NULL) {
  set.seed(seed)
  res <- dplyr::bind_rows(lapply(or_grid, function(OR) {
    odds <- p_baseline / (1 - p_baseline) * OR
    p1 <- odds / (1 + odds)
    hits <- vapply(seq_len(n_sim), function(i) {
      a <- stats::rbinom(1, n_interlocking, p1)
      c_ <- stats::rbinom(1, n_rest, p_baseline)
      stats::fisher.test(matrix(c(a, n_interlocking - a,
                                  c_, n_rest - c_), nrow = 2, byrow = TRUE))$p.value < alpha
    }, logical(1))
    tibble::tibble(contrast = label, baseline = p_baseline,
                   odds_ratio = OR, power = mean(hits))
  }))

  # Interpolate the OR corresponding to 80% power, for a one-line statement of
  # what the null does and does not exclude.
  or80 <- NA_real_
  if (any(res$power >= 0.8) && any(res$power < 0.8)) {
    or80 <- stats::approx(x = res$power, y = res$odds_ratio, xout = 0.8)$y
  }
  attr(res, "or_at_80_power") <- or80
  message(sprintf("  OR detectable at 80%% power: %s",
                  ifelse(is.na(or80), "outside the tested grid", round(or80, 2))))
  print(as.data.frame(res), digits = 3)
  if (!is.null(output_dir)) {
    utils::write.csv(res, file.path(output_dir,
                                    paste0("power_", gsub("[^A-Za-z0-9]+", "_", label), ".csv")),
                     row.names = FALSE)
  }
  res
}

# ---------------------------------------------------------------------------
# 7. Attribute permutation on the observed network
# ---------------------------------------------------------------------------

#' Permute demographic labels across editors, holding the observed network
#' fixed, and test whether prominence (EVC) is concentrated among any group
#' beyond what random attribute allocation would produce.
#'
#' H0: attributes are unrelated to structural position, conditional on the
#' observed network. This tests random reassignment of observed attributes; it
#' does not address unobserved mechanisms such as productivity, seniority, or
#' reputation.
run_attribute_permutation <- function(editor_stats, attr_col, focal_level,
                                      value_col = "EVC", n_perm = 100000,
                                      seed = 123, output_dir = NULL) {
  set.seed(seed)
  d <- editor_stats[!is.na(editor_stats[[attr_col]]) & !is.na(editor_stats[[value_col]]), ]
  x <- d[[value_col]]
  grp <- as.character(d[[attr_col]]) == focal_level
  if (sum(grp) < 2 || sum(!grp) < 2) stop("Focal group too small.", call. = FALSE)

  obs <- mean(x[grp]) - mean(x[!grp])
  null <- replicate(n_perm, {
    s <- sample(grp)
    mean(x[s]) - mean(x[!s])
  })
  p <- (sum(abs(null) >= abs(obs)) + 1) / (n_perm + 1)

  out <- tibble::tibble(
    attribute = attr_col, focal_level = focal_level, statistic = value_col,
    n_focal = sum(grp), n_other = sum(!grp),
    observed_difference = obs,
    null_mean = mean(null), null_sd = stats::sd(null),
    z = (obs - mean(null)) / stats::sd(null),
    p_permutation = p, n_perm = n_perm
  )
  print(as.data.frame(out), digits = 3)
  if (!is.null(output_dir)) {
    utils::write.csv(out, file.path(output_dir,
                                    paste0("permutation_", attr_col, "_", focal_level, ".csv")),
                     row.names = FALSE)
  }
  out
}


# ---------------------------------------------------------------------------
# 7b. Gender selection: instrument-consistent NamSor primary analysis
# ---------------------------------------------------------------------------

run_gender_selection <- function(person, gender_metadata, output_dir) {
  d <- person |>
    dplyr::select(person_id, n_journals) |>
    dplyr::left_join(gender_metadata |>
      dplyr::select(person_id, Gender_namsor, Gender_completed, Gender_source),
      by = "person_id") |>
    dplyr::mutate(group = dplyr::if_else(n_journals >= 2, "Interlocking", "Non-interlocking"))

  if (any(is.na(d$Gender_namsor))) {
    stop("Missing NamSor gender after joining corrected population.", call. = FALSE)
  }

  counts <- d |>
    dplyr::count(group, Gender_namsor, name = "n") |>
    tidyr::complete(group = c("Interlocking", "Non-interlocking"),
                    Gender_namsor = c("Female", "Male", "Low confidence"),
                    fill = list(n = 0L))
  utils::write.csv(counts, file.path(output_dir, "gender_namsor_counts.csv"), row.names = FALSE)

  getn <- function(gr, sex) counts$n[counts$group == gr & counts$Gender_namsor == sex]
  fi <- getn("Interlocking", "Female"); mi <- getn("Interlocking", "Male")
  fn <- getn("Non-interlocking", "Female"); mn <- getn("Non-interlocking", "Male")
  li <- getn("Interlocking", "Low confidence"); ln <- getn("Non-interlocking", "Low confidence")

  tab <- matrix(c(fi, mi, fn, mn), nrow = 2, byrow = TRUE,
                dimnames = list(c("Interlocking", "Non-interlocking"), c("Female", "Male")))
  ft <- stats::fisher.test(tab, alternative = "two.sided")
  primary <- tibble::tibble(
    specification = "Primary: NamSor in both groups; Low confidence excluded symmetrically",
    female_interlocking = fi,
    male_interlocking = mi,
    female_noninterlocking = fn,
    male_noninterlocking = mn,
    n_interlocking_classifiable = fi + mi,
    n_noninterlocking_classifiable = fn + mn,
    female_share_interlocking = fi / (fi + mi),
    female_share_noninterlocking = fn / (fn + mn),
    odds_ratio = unname(ft$estimate),
    ci_low = unname(ft$conf.int[1]),
    ci_high = unname(ft$conf.int[2]),
    p_value = ft$p.value,
    method = "Fisher exact test"
  )
  utils::write.csv(primary, file.path(output_dir, "gender_selection_primary.csv"), row.names = FALSE)

  # Is low-confidence classification associated with interlocking status?
  missing_tab <- matrix(c(li, fi + mi, ln, fn + mn), nrow = 2, byrow = TRUE,
                        dimnames = list(c("Interlocking", "Non-interlocking"),
                                        c("Low confidence", "Classifiable")))
  mt <- stats::fisher.test(missing_tab, alternative = "two.sided")
  missingness <- tibble::tibble(
    low_conf_interlocking = li,
    classifiable_interlocking = fi + mi,
    low_conf_noninterlocking = ln,
    classifiable_noninterlocking = fn + mn,
    low_conf_rate_interlocking = li / (li + fi + mi),
    low_conf_rate_noninterlocking = ln / (ln + fn + mn),
    odds_ratio = unname(mt$estimate),
    ci_low = unname(mt$conf.int[1]),
    ci_high = unname(mt$conf.int[2]),
    p_value = mt$p.value,
    method = "Fisher exact test"
  )
  utils::write.csv(missingness, file.path(output_dir, "gender_low_confidence_missingness.csv"), row.names = FALSE)

  # Deliberately mixed-instrument sensitivity: completed interlocking labels
  # versus NamSor-classifiable non-interlocking labels. This is retained to
  # demonstrate sensitivity to differential measurement, not as primary evidence.
  di <- d |> dplyr::filter(group == "Interlocking", Gender_completed %in% c("Female", "Male"))
  dn <- d |> dplyr::filter(group == "Non-interlocking", Gender_namsor %in% c("Female", "Male"))
  fi2 <- sum(di$Gender_completed == "Female"); mi2 <- sum(di$Gender_completed == "Male")
  fn2 <- sum(dn$Gender_namsor == "Female"); mn2 <- sum(dn$Gender_namsor == "Male")
  mixed_tab <- matrix(c(fi2, mi2, fn2, mn2), nrow = 2, byrow = TRUE,
                      dimnames = list(c("Interlocking", "Non-interlocking"), c("Female", "Male")))
  mixed_ft <- stats::fisher.test(mixed_tab, alternative = "two.sided")
  mixed <- tibble::tibble(
    specification = "Sensitivity only: completed interlocking labels vs NamSor non-interlocking labels (mixed instruments)",
    female_interlocking = fi2,
    male_interlocking = mi2,
    female_noninterlocking = fn2,
    male_noninterlocking = mn2,
    odds_ratio = unname(mixed_ft$estimate),
    ci_low = unname(mixed_ft$conf.int[1]),
    ci_high = unname(mixed_ft$conf.int[2]),
    p_value = mixed_ft$p.value,
    method = "Fisher exact test"
  )
  utils::write.csv(mixed, file.path(output_dir, "gender_selection_mixed_instrument_sensitivity.csv"), row.names = FALSE)

  message(sprintf(
    "Gender primary (NamSor consistent): %dF/%dM interlocking vs %dF/%dM non-interlocking; OR=%.3f, 95%% CI %.3f-%.3f, Fisher p=%.3f",
    fi, mi, fn, mn, primary$odds_ratio, primary$ci_low, primary$ci_high, primary$p_value
  ))
  message(sprintf(
    "NamSor low-confidence: %.1f%% interlocking vs %.1f%% non-interlocking; Fisher p=%.3f",
    100 * missingness$low_conf_rate_interlocking,
    100 * missingness$low_conf_rate_noninterlocking,
    missingness$p_value
  ))
  message(sprintf(
    "Mixed-instrument sensitivity: OR=%.3f, 95%% CI %.3f-%.3f, Fisher p=%.3f (not primary)",
    mixed$odds_ratio, mixed$ci_low, mixed$ci_high, mixed$p_value
  ))

  list(primary = primary, missingness = missingness, mixed_sensitivity = mixed, counts = counts)
}

# ---------------------------------------------------------------------------
# 8. Orchestrator
# ---------------------------------------------------------------------------

run_selection_analysis <- function(full_path = NULL,
                                   lookup_path = NULL,
                                   editor_stats = NULL,
                                   output_dir = "output/selection",
                                   adjudication_path = NULL,
                                   institution_alias_path = NULL,
                                   unresolved_affiliation_action = "retain_missing",
                                   merges_path = NULL,
                                   built = NULL,
                                   gender_metadata = NULL,
                                   fisher_B = 100000,
                                   permutation_n = 100000,
                                   simulation_seed = 123) {
  message("Running selection analysis...")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  if (is.null(built)) {
    if (is.null(full_path) || is.null(lookup_path)) {
      stop("Supply either `built` or both `full_path` and `lookup_path`.", call. = FALSE)
    }
    built <- build_person_level(
      full_path, lookup_path,
      adjudication_path = adjudication_path,
      institution_alias_path = institution_alias_path,
      unresolved_affiliation_action = unresolved_affiliation_action,
      write_adjudication_to = output_dir,
      merges_path = merges_path
    )
  } else {
    message("Using shared post-merge population object from the targets pipeline.")
  }
  person <- built$person
  utils::write.csv(person, file.path(output_dir, "person_level.csv"), row.names = FALSE)

  gender_selection <- NULL
  if (!is.null(gender_metadata)) {
    gender_selection <- run_gender_selection(person, gender_metadata, output_dir)
  }

  # Recorded every run, not just when investigating a specific discrepancy:
  # name-format issues and candidate name variants are silent unless audited.
  name_formats  <- audit_name_formats(built$positions, output_dir)
  name_variants <- find_name_variants(built$positions, output_dir = output_dir)

  # run_omnibus_geography() is the primary geographic test. The per-level
  # contrasts from run_enrichment_tests() are exploratory; the Europe contrast
  # below was selected after the continent residuals were inspected.
  omnibus_continent <- run_omnibus_geography(
    person, "Continent", output_dir, B = fisher_B, seed = simulation_seed)
  omnibus_subregion <- run_omnibus_geography(
    person, "Subregion", output_dir, B = fisher_B, seed = simulation_seed)

  results <- list(
    person            = person,
    name_formats      = name_formats,
    name_variants     = name_variants,
    duplicates        = audit_duplicate_rows(built$positions, output_dir),
    definitions       = reconcile_interlocking_definitions(person, output_dir),
    omnibus_continent = omnibus_continent,
    omnibus_subregion = omnibus_subregion,
    continent    = run_enrichment_tests(person, "Continent", output_dir),
    subregion    = run_enrichment_tests(person, "Subregion", output_dir),
    focal        = run_focal_contrast(person, "Continent", "Europe"),
    model        = fit_selection_model(person, output_dir),
    sensitivity  = run_definition_sensitivity(person, "Continent", "Europe", output_dir)
  )

  if (!is.null(editor_stats)) {
    results$perm_europe <- run_attribute_permutation(
      editor_stats, "Continent_1", "Europe", n_perm = permutation_n,
      seed = simulation_seed, output_dir = output_dir)
    gender_col <- if ("Gender_namsor" %in% names(editor_stats)) "Gender_namsor" else "Gender"
    if (gender_col %in% names(editor_stats)) {
      gender_stats <- editor_stats |>
        dplyr::filter(.data[[gender_col]] %in% c("Female", "Male"))
      results$perm_gender <- run_attribute_permutation(
        gender_stats, gender_col, "Female", n_perm = permutation_n,
        seed = simulation_seed, output_dir = output_dir)
    }
  }

  results$gender_selection <- gender_selection
  message("Selection analysis complete: ", output_dir)
  results
}

# ---------------------------------------------------------------------------
# Pipeline registration (implemented in _targets.R)
# ---------------------------------------------------------------------------
#
#   tar_source(c(..., "R/selection_analysis.R"))
#
#   tar_target(full_population_file, "private/Dataset_Editorial_Boards_All.xlsx",
#              format = "file"),
#   tar_target(m49_lookup_file, "data/country_m49_lookup.csv", format = "file"),
#   tar_target(selection_results, run_selection_analysis(
#     full_path    = full_population_file,
#     lookup_path  = m49_lookup_file,
#     editor_stats = metrics$editor_stats,
#     output_dir   = "output/selection"
#   )),
#
# Gender selection is handled by run_gender_selection(), using the frozen
# population-wide NamSor classifications with the same eligibility rule in
# interlocking and non-interlocking editors.
