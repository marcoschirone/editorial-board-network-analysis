# R/person_disambiguation.R
# Detection of name-variant records that split one person across several
# "editors", and controlled merging of confirmed variants.
#
# Why this exists: editorial boards are published in inconsistent name formats.
# One journal in this sample (Environmental Innovation and Societal Transitions)
# lists 88% of its board with initials only, while every other journal uses full
# given names. Deduplicating on exact name strings therefore split affected
# people into two records, each below the interlocking threshold, removing them
# from the interlocking set entirely.
#
# Nothing here merges automatically. The functions propose candidates; a human
# confirms each one in a review file; only confirmed merges are applied. Merging
# two genuinely different people is worse than leaving one person split, and
# common surnames at large institutions make automated merging unsafe.


#' Write a CSV without mangling non-ASCII names.
#'
#' In a non-UTF-8 locale (LC_CTYPE=C or POSIX, common on servers and in
#' containers), R's write.csv escapes non-ASCII characters into literal text:
#' "Kemp, Rene\u0301" is written as the eleven ASCII characters "<U+00E9>".
#' Neither fileEncoding= nor a UTF-8 connection prevents this; only the locale
#' does. A downstream read then fails to match the real name, and because the
#' damage is silent it can survive into an adjudication table and be reviewed
#' by a human without anyone noticing.
#'
#' This wrapper switches LC_CTYPE for the duration of the write and restores it.
write_csv_utf8 <- function(x, path, ...) {
  old <- Sys.getlocale("LC_CTYPE")
  ok <- FALSE
  for (loc in c("C.UTF-8", "en_US.UTF-8", "C.utf8")) {
    if (suppressWarnings(Sys.setlocale("LC_CTYPE", loc)) != "") { ok <- TRUE; break }
  }
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", old)), add = TRUE)
  if (!ok && any(grepl("[^\\x01-\\x7F]", unlist(lapply(x, as.character))))) {
    warning("No UTF-8 locale available; non-ASCII names in ", basename(path),
            " will be written as literal <U+XXXX> escapes and will not match ",
            "the source data on read.", call. = FALSE)
  }
  utils::write.csv(x, path, row.names = FALSE, ...)
}

# ---------------------------------------------------------------------------
# 1. Name parsing
# ---------------------------------------------------------------------------

#' Surname particles (tussenvoegsels, nobiliary and patronymic prefixes).
#'
#' In inverted "Surname, Given" form these trail the given names
#' ("Bergh, J. van", "Ploeg, R. van"); in direct form they lead the surname
#' ("van den Bergh, Jeroen"). Either way they belong to the family name, not to
#' the given names.
#'
#' Treating "van" as a given-name token makes "Bergh, J. van" look like a
#' two-token given name rather than an initials-only record. On this corpus that
#' affected SCORING only -- surname grouping still generated the pair, and
#' rerunning generation with the corrected parser produced 108 candidates both
#' before and after, with no additional genuine pairs. Under other formatting
#' conventions (particles carried in the surname field, or mixed direct and
#' inverted forms) the same defect can suppress candidate generation itself,
#' which is why the fix belongs in the parser rather than in the scoring rules.
NAME_PARTICLES <- c("van", "von", "der", "den", "de", "del", "della", "di", "da",
                    "das", "dos", "du", "la", "le", "lo", "ten", "ter", "af",
                    "al", "el", "bin", "ibn", "ben", "abu", "vander", "vande")

#' Split "Surname, Given Names" into components, particle-aware.
#'
#' `abbreviated` is TRUE when every given-name token (excluding particles) is a
#' single letter: "Adger, N. W." and "Bergh, J. van" both qualify.
parse_name_parts <- function(name) {
  name <- as.character(name)
  surname_raw <- trimws(sub(",.*$", "", name))
  given_raw   <- trimws(ifelse(grepl(",", name), sub("^[^,]*,", "", name), ""))

  split_tokens <- function(x) {
    t <- strsplit(x, "[[:space:].()-]+")
    lapply(t, function(v) v[nzchar(v)])
  }

  given_tokens <- split_tokens(given_raw)
  surn_tokens  <- split_tokens(surname_raw)

  is_particle <- function(v) tolower(v) %in% NAME_PARTICLES

  # Particles trailing the given names, or leading the surname, move to the
  # family name. The remaining tokens are the true given names.
  parsed <- Map(function(g, s) {
    trail <- character(0)
    while (length(g) && is_particle(g[length(g)])) {
      trail <- c(g[length(g)], trail); g <- g[-length(g)]
    }
    lead <- character(0)
    while (length(s) > 1 && is_particle(s[1])) {
      lead <- c(lead, s[1]); s <- s[-1]
    }
    list(given = g, particles = c(lead, trail), family_core = s)
  }, given_tokens, surn_tokens)

  norm <- function(x) tolower(iconv(x, to = "ASCII//TRANSLIT"))

  tibble::tibble(
    Name        = name,
    surname     = surname_raw,
    family_core = vapply(parsed, function(p) paste(p$family_core, collapse = " "), character(1)),
    particle    = vapply(parsed, function(p) paste(tolower(p$particles), collapse = " "), character(1)),
    given       = vapply(parsed, function(p) paste(p$given, collapse = " "), character(1)),
    initials    = vapply(parsed, function(p)
                    if (!length(p$given)) "" else
                      paste(toupper(substr(p$given, 1, 1)), collapse = ""), character(1)),
    n_tokens    = vapply(parsed, function(p) length(p$given), integer(1)),
    abbreviated = vapply(parsed, function(p)
                    length(p$given) > 0 && all(nchar(p$given) == 1), logical(1))
  ) |>
    dplyr::mutate(surname_key = norm(family_core))
}

#' Report the share of abbreviated-name records per journal.
#'
#' A journal well above the others is a systematic formatting difference, not
#' scattered noise, and should be re-derived from source rather than patched
#' person by person.
audit_name_formats <- function(positions, output_dir = NULL) {
  message("Auditing name formats by journal...")
  parts <- parse_name_parts(positions$person_id)
  out <- positions |>
    dplyr::mutate(abbreviated = parts$abbreviated) |>
    dplyr::group_by(Journal) |>
    dplyr::summarise(n_records = dplyr::n(),
                     n_abbreviated = sum(abbreviated),
                     pct_abbreviated = round(100 * mean(abbreviated), 1),
                     .groups = "drop") |>
    dplyr::arrange(dplyr::desc(pct_abbreviated))

  flagged <- out |> dplyr::filter(pct_abbreviated > 50)
  if (nrow(flagged)) {
    message("  Journals listing most of their board with initials only:")
    for (i in seq_len(nrow(flagged))) {
      message(sprintf("    %s (%.1f%%)", flagged$Journal[i], flagged$pct_abbreviated[i]))
    }
    message("  Consider re-deriving these boards from source with full names.")
  }
  if (!is.null(output_dir)) {
    write_csv_utf8(out, file.path(output_dir, "name_format_by_journal.csv"))
  }
  out
}

# ---------------------------------------------------------------------------
# 2. Candidate detection
# ---------------------------------------------------------------------------

#' Propose pairs of records that may be the same person.
#'
#' Three complementary rules, each with a confidence tier. No single rule finds
#' every case: matching on institution catches people whose initials differ in
#' length ("Howarth, R. B." / "Howarth, Richard"), while matching on the
#' abbreviated-vs-full contrast catches people who have moved institution
#' ("Yarime, M." at Tokyo / "Yarime, Masaru" at HKUST).
#'
#' @return One row per candidate pair, ordered by confidence.
find_name_variants <- function(positions, id_col = "person_id", output_dir = NULL) {
  message("Detecting candidate name variants...")

  person <- positions |>
    dplyr::rename(pid = dplyr::all_of(id_col)) |>
    dplyr::group_by(pid) |>
    dplyr::summarise(
      institutions = list(unique(stats::na.omit(Affiliation_1))),
      countries    = list(unique(Country_1)),
      journals     = list(unique(Journal)),
      n_journals   = dplyr::n_distinct(Journal),
      .groups = "drop"
    )
  parts <- parse_name_parts(person$pid)
  person <- dplyr::bind_cols(person, parts[, c("surname_key", "initials",
                                               "abbreviated", "n_tokens")])

  sort_chars <- function(x) paste(sort(strsplit(x, "")[[1]]), collapse = "")

  initials_compatible <- function(a, b) {
    if (a == "" || b == "") return(FALSE)
    a == b || startsWith(b, a) || startsWith(a, b) ||
      # Surname-first sources sometimes reverse the order of given initials:
      # "Adger, William Neil" (WN) and "Adger, N. W." (NW) are one person.
      sort_chars(a) == sort_chars(b) ||
      (substr(a, 1, 1) == substr(b, 1, 1) &&
         substr(a, nchar(a), nchar(a)) == substr(b, nchar(b), nchar(b)))
  }

  rows <- list()
  for (sk in unique(person$surname_key)) {
    grp <- person[person$surname_key == sk, ]
    if (nrow(grp) < 2) next
    for (i in seq_len(nrow(grp) - 1)) for (j in (i + 1):nrow(grp)) {
      a <- grp[i, ]; b <- grp[j, ]
      if (!initials_compatible(a$initials, b$initials)) next

      same_inst    <- length(intersect(a$institutions[[1]], b$institutions[[1]])) > 0
      same_country <- length(intersect(a$countries[[1]],    b$countries[[1]]))    > 0
      one_abbrev   <- xor(a$abbreviated, b$abbreviated)
      shared_journal <- length(intersect(a$journals[[1]], b$journals[[1]])) > 0

      # A shared journal means both records sit on the same board, which is
      # evidence they are DIFFERENT people, not the same one.
      if (shared_journal) next

      exact_initials <- a$initials == b$initials

      confidence <- dplyr::case_when(
        same_inst && one_abbrev                     ~ "high",
        same_inst                                   ~ "medium-high",
        one_abbrev && exact_initials                ~ "medium",
        one_abbrev && same_country                  ~ "medium",
        one_abbrev                                  ~ "low",
        TRUE                                        ~ "very low"
      )
      rows[[length(rows) + 1]] <- tibble::tibble(
        record_a = a$pid, record_b = b$pid,
        initials_a = a$initials, initials_b = b$initials,
        confidence = confidence,
        same_institution = same_inst, same_country = same_country,
        one_abbreviated = one_abbrev,
        institutions = paste(unique(c(a$institutions[[1]], b$institutions[[1]])),
                             collapse = " | "),
        journals_combined = length(unique(c(a$journals[[1]], b$journals[[1]]))),
        confirmed_same_person = NA, evidence_source = NA_character_
      )
    }
  }

  if (!length(rows)) {
    message("  No candidates found.")
    return(tibble::tibble())
  }

  out <- dplyr::bind_rows(rows) |>
    dplyr::mutate(confidence = factor(confidence,
      levels = c("high", "medium-high", "medium", "low", "very low"))) |>
    dplyr::arrange(confidence, record_a)

  message(sprintf("  %d candidate pairs (%d high, %d medium-high).",
                  nrow(out), sum(out$confidence == "high"),
                  sum(out$confidence == "medium-high")))
  message("  Confirm each in the review file before merging. Same surname at a ",
          "large institution is common; do not merge on the rule alone.")

  if (!is.null(output_dir)) {
    write_csv_utf8(out, file.path(output_dir, "name_variant_candidates.csv"))
  }
  out
}

# ---------------------------------------------------------------------------
# 3. Controlled merging
# ---------------------------------------------------------------------------

#' Apply confirmed merges to the position-level data.
#'
#' @param merges_path The adjudication table: the CSV produced by
#'   `find_name_variants()` with `confirmed_same_person` and `evidence_source`
#'   filled in by hand. This table is the source of truth for identity
#'   decisions. Do NOT pass `applied_name_merges.csv` here -- that file is an
#'   OUTPUT audit (original -> canonical) derived from this input, and does not
#'   carry the adjudication or its evidence.
#'
#' Confirmed record names must match the position data exactly. A confirmed pair
#' naming a record that does not exist is an error, not something to skip: it
#' means an identity decision silently failed to apply and the person counts
#' will be wrong with no warning. Encoding damage to the adjudication file
#' ("Kemp, Ren<U+00E9>" for "Kemp, Ren\u00e9") is the usual cause.
#'
#' Merging is transitive: three records for one person (as with the three Adger
#' strings) resolve to a single canonical identity via connected components.
apply_name_merges <- function(positions, merges_path, id_col = "person_id",
                              output_dir = NULL) {
  if (is.null(merges_path) || !file.exists(merges_path)) {
    message("  No confirmed merge file supplied; leaving name records as they are.")
    return(positions)
  }
  # encoding = "UTF-8" (not fileEncoding) is required: without it R marks the
  # strings "unknown" rather than "UTF-8", and byte-identical names then fail
  # identical()/%in% against readxl output, which marks them "UTF-8". Any name
  # with a non-ASCII character (Rene, Ozkaynak, Baumgartner) silently fails to
  # match. Encoding is normalised on both sides below as a further guard.
  m <- utils::read.csv(merges_path, stringsAsFactors = FALSE, encoding = "UTF-8")

  required <- c("record_a", "record_b", "confirmed_same_person", "evidence_source")
  missing_cols <- setdiff(required, names(m))
  if (length(missing_cols)) {
    stop("`merges_path` is not an adjudication table: missing column(s) ",
         paste(missing_cols, collapse = ", "), ". ",
         "Pass the reviewed name_variant_candidates file, not applied_name_merges.csv.",
         call. = FALSE)
  }

  m <- m |>
    dplyr::filter(!is.na(confirmed_same_person),
                  toupper(as.character(confirmed_same_person)) %in% c("TRUE", "YES", "1"))
  if (!nrow(m)) {
    message("  Merge file contains no confirmed pairs.")
    return(positions)
  }
  if (any(is.na(m$evidence_source) | !nzchar(m$evidence_source))) {
    warning("Some confirmed merges have no evidence_source recorded. ",
            "Provenance for identity decisions should be documented.", call. = FALSE)
  }

  # Fail loudly on records that do not exist in the data. Skipping them would
  # apply fewer merges than adjudicated and report a person count that looks
  # plausible but is wrong.
  norm_enc <- function(x) { x <- as.character(x); Encoding(x) <- "UTF-8"; x }
  m$record_a <- norm_enc(m$record_a)
  m$record_b <- norm_enc(m$record_b)
  positions[[id_col]] <- norm_enc(positions[[id_col]])

  present <- unique(positions[[id_col]])
  absent  <- setdiff(unique(c(m$record_a, m$record_b)), present)
  if (length(absent)) {
    stop("Confirmed merge refers to ", length(absent),
         " record(s) not present in the data:\n  ",
         paste(sprintf("'%s'", absent), collapse = "\n  "),
         "\nCheck for encoding damage in the adjudication file (non-ASCII ",
         "names) or for records dropped upstream. Not skipping these: an ",
         "unapplied merge produces a silently incorrect person count.",
         call. = FALSE)
  }

  # Connected components over confirmed pairs, so chains resolve together.
  ids <- unique(c(m$record_a, m$record_b))
  parent <- stats::setNames(ids, ids)
  find <- function(x) { while (parent[[x]] != x) x <- parent[[x]]; x }
  for (k in seq_len(nrow(m))) {
    ra <- find(m$record_a[k]); rb <- find(m$record_b[k])
    if (ra != rb) parent[[rb]] <- ra
  }

  # Canonical label: the most complete name string in each group, so the
  # merged identity carries the full given name rather than the initials.
  groups <- vapply(ids, find, character(1))
  canon <- tapply(names(groups), groups, function(members) {
    members[which.max(nchar(members))]
  })
  lookup <- stats::setNames(unname(canon[groups]), names(groups))

  before <- dplyr::n_distinct(positions[[id_col]])
  positions[[paste0(id_col, "_original")]] <- positions[[id_col]]
  hit <- positions[[id_col]] %in% names(lookup)
  positions[[id_col]][hit] <- unname(lookup[positions[[id_col]][hit]])
  after <- dplyr::n_distinct(positions[[id_col]])

  # Confirmed pairs are edges; persons removed is the reduction over connected
  # components. Three name strings for one person are two edges but one person
  # removed, so these numbers differ and both belong in the methods.
  message(sprintf("  Applied %d confirmed pair(s) over %d identity component(s): %d records -> %d persons (%d fewer).",
                  nrow(m), length(unique(groups)), before, after, before - after))

  if (!is.null(output_dir)) {
    write_csv_utf8(
      tibble::tibble(original = names(lookup), canonical = unname(lookup)) |>
        dplyr::filter(original != canonical), file.path(output_dir, "applied_name_merges.csv"))
  }
  positions
}

# ---------------------------------------------------------------------------
# Usage
# ---------------------------------------------------------------------------
#
#   positions <- readxl::read_xlsx("data/Dataset_Editorial_Boards_All.xlsx") |>
#     dplyr::rename(person_id = Name)
#
#   audit_name_formats(positions, "output/selection")
#   find_name_variants(positions, output_dir = "output/selection")
#
#   # Review output/selection/name_variant_candidates.csv by hand: set
#   # confirmed_same_person and evidence_source (ORCID iD, institutional page)
#   # for each pair, save as data/confirmed_name_merges.csv, then:
#
#   positions <- apply_name_merges(positions, "data/confirmed_name_merges.csv")
#
# In build_person_level(), call apply_name_merges() BEFORE collapsing duplicate
# roles: merging changes which person-journal pairs exist.
