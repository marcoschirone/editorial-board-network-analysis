# R/data_processing.R
# Data loading, cleaning, and validation

validate_config <- function(cfg) {
  cfgd <- cfg$default %||% cfg
  required <- c("full_population_path", "m49_lookup_path", "confirmed_merges_path", "annotation_path", "gender_adjudication_path")
  miss <- setdiff(required, names(cfgd))
  if (length(miss)) stop("Config is missing fields: ", paste(miss, collapse=", "), call. = FALSE)

  for (path in unlist(cfgd[required], use.names = FALSE)) {
    if (!file.exists(path)) stop("Required data file not found: ", path, call. = FALSE)
  }
  invisible(cfg)
}

load_and_clean_data <- function(cfg) {
  stop("load_and_clean_data() is retired: network membership must be derived from the full population via build_person_level() and build_network_input().", call. = FALSE)
}



# Build the network-analysis input from the same disambiguated full population
# used by the selection analysis. This prevents the legacy 71-editor workbook
# from defining network membership.
build_network_input <- function(built, annotation_path = NULL,
                                merges_path = NULL,
                                gender_adjudication_path = NULL) {
  if (is.null(built$positions) || is.null(built$person)) {
    stop("`built` must be the list returned by build_person_level().", call. = FALSE)
  }

  person <- built$person |>
    dplyr::select(person_id, n_journals, Country_1, Continent, Subregion)

  interlocking_ids <- person |>
    dplyr::filter(n_journals >= 2) |>
    dplyr::pull(person_id)

  # Optional legacy annotations. These enrich the corrected membership set but
  # never determine it. Confirmed merge labels are applied where possible so
  # an old full-name annotation follows its canonical identity.
  annotations <- tibble::tibble(
    person_id = character(), ORCID = character(), Gender = character()
  )

  if (!is.null(annotation_path) && file.exists(annotation_path)) {
    ann_raw <- readxl::read_xlsx(annotation_path)
    assert_has_columns(ann_raw, c("Name", "ORCID", "Gender"), "annotation spreadsheet")

    canonical_map <- character()
    if (!is.null(merges_path) && file.exists(merges_path)) {
      m <- utils::read.csv(merges_path, stringsAsFactors = FALSE, encoding = "UTF-8") |>
        dplyr::filter(!is.na(confirmed_same_person),
                      toupper(as.character(confirmed_same_person)) %in% c("TRUE", "YES", "1"))
      if (nrow(m)) {
        # Prefer the adjudicated canonical_name when supplied. Every confirmed
        # row in the current file has one; the fallback keeps this generic.
        fallback <- ifelse(nchar(m$record_a) >= nchar(m$record_b), m$record_a, m$record_b)
        canon <- if ("canonical_name" %in% names(m)) {
          dplyr::coalesce(dplyr::na_if(trimws(as.character(m$canonical_name)), ""), fallback)
        } else fallback
        canonical_map <- stats::setNames(canon, m$record_a)
        canonical_map[m$record_b] <- canon
      }
    }

    ann <- ann_raw |>
      dplyr::transmute(
        person_id = clean_ids(as.character(Name)),
        ORCID = clean_ids(as.character(ORCID)),
        Gender = clean_ids(as.character(Gender))
      )

    if (length(canonical_map)) {
      hit <- ann$person_id %in% names(canonical_map)
      ann$person_id[hit] <- unname(canonical_map[ann$person_id[hit]])
    }

    first_nonmissing <- function(x) {
      x <- x[!is.na(x) & nzchar(x)]
      if (length(x)) x[[1]] else NA_character_
    }

    annotations <- ann |>
      dplyr::group_by(person_id) |>
      dplyr::summarise(
        ORCID = first_nonmissing(ORCID),
        Gender = first_nonmissing(Gender),
        .groups = "drop"
      )
  }

  # Manual gender adjudications override legacy annotations. The adjudication
  # names are passed through the same confirmed-merge canonical map so a reviewed
  # alias remains attached to the correct post-merge identity.
  gender_adjudications <- tibble::tibble(
    person_id = character(), Gender_adjudicated = character()
  )
  n_gender_adjudications <- 0L

  if (!is.null(gender_adjudication_path) && file.exists(gender_adjudication_path)) {
    ga_raw <- utils::read.csv(gender_adjudication_path, stringsAsFactors = FALSE,
                              encoding = "UTF-8", check.names = FALSE)
    assert_has_columns(ga_raw, c("person_id", "Gender"), "gender adjudication file")

    ga <- ga_raw |>
      dplyr::transmute(
        person_id = clean_ids(as.character(person_id)),
        Gender_adjudicated = clean_ids(as.character(Gender))
      )

    # Resolve adjudication aliases against the *actual* post-merge person IDs
    # produced by build_person_level(), rather than blindly trusting the
    # canonical_name label in confirmed_name_merges.csv. This matters when the
    # merge audit label differs only by initials/diacritics from the retained
    # person_id (e.g. Howarth, Richard B. vs Howarth, Richard; Kemp, Rene vs
    # Kemp, René).
    if (!is.null(merges_path) && file.exists(merges_path)) {
      m_alias <- utils::read.csv(merges_path, stringsAsFactors = FALSE,
                                 encoding = "UTF-8", check.names = FALSE) |>
        dplyr::filter(!is.na(confirmed_same_person),
                      toupper(as.character(confirmed_same_person)) %in% c("TRUE", "YES", "1"))

      if (nrow(m_alias)) {
        current_ids <- unique(person$person_id)
        alias_map <- character()

        for (i in seq_len(nrow(m_alias))) {
          aliases <- clean_ids(c(
            as.character(m_alias$record_a[[i]]),
            as.character(m_alias$record_b[[i]]),
            if ("canonical_name" %in% names(m_alias)) as.character(m_alias$canonical_name[[i]]) else NA_character_
          ))
          aliases <- unique(aliases[!is.na(aliases) & nzchar(aliases)])
          retained <- intersect(aliases, current_ids)

          if (length(retained) == 1L) {
            alias_map[aliases] <- retained[[1]]
          } else if (length(retained) > 1L) {
            # Multiple labels from the component survive as person IDs: do not
            # guess. Leave aliases unchanged so the hard unmatched check below
            # exposes the inconsistency.
            next
          }
        }

        hit <- ga$person_id %in% names(alias_map)
        ga$person_id[hit] <- unname(alias_map[ga$person_id[hit]])
      }
    }

    if (anyDuplicated(ga$person_id)) {
      stop("Gender adjudication file contains duplicate canonical identities.", call. = FALSE)
    }
    bad_gender <- setdiff(unique(stats::na.omit(ga$Gender_adjudicated)), c("Female", "Male"))
    if (length(bad_gender)) {
      stop("Unsupported Gender value(s) in adjudication file: ",
           paste(bad_gender, collapse = ", "), call. = FALSE)
    }

    n_gender_adjudications <- nrow(ga)
    unmatched <- setdiff(ga$person_id, interlocking_ids)
    if (length(unmatched)) {
      stop("Gender adjudication(s) did not match the corrected interlocking population: ",
           paste(unmatched, collapse = "; "), call. = FALSE)
    }

    gender_adjudications <- ga
  }

  out <- built$positions |>
    dplyr::filter(person_id %in% interlocking_ids) |>
    dplyr::select(person_id, Journal) |>
    dplyr::distinct() |>
    dplyr::left_join(person, by = "person_id") |>
    dplyr::left_join(annotations, by = "person_id") |>
    dplyr::left_join(gender_adjudications, by = "person_id") |>
    dplyr::transmute(
      ORCID = ORCID,
      Journal = Journal,
      Country = Country_1,
      Continent = Continent,
      Subregion = Subregion,
      Gender = dplyr::coalesce(Gender_adjudicated, Gender, "Unknown"),
      editor_id = person_id
    )

  matched_adjudications <- if (n_gender_adjudications) {
    dplyr::n_distinct(gender_adjudications$person_id[gender_adjudications$person_id %in% unique(out$editor_id)])
  } else 0L
  message(sprintf("Gender adjudications matched: %d/%d",
                  matched_adjudications, n_gender_adjudications))
  if (matched_adjudications != n_gender_adjudications) {
    stop("Not all gender adjudications matched the network input.", call. = FALSE)
  }

  n_population <- nrow(built$person)
  n_interlocking <- dplyr::n_distinct(out$editor_id)
  n_super <- sum(built$person$n_journals >= 3)
  n_appointments <- nrow(built$positions)
  n_unknown_gender <- out |>
    dplyr::distinct(editor_id, Gender) |>
    dplyr::summarise(n = sum(Gender == "Unknown")) |>
    dplyr::pull(n)

  message(sprintf(
    "Authoritative invariants: %d persons / %d appointments / %d interlocking / %d with >=3 journals",
    n_population, n_appointments, n_interlocking, n_super
  ))
  message(sprintf("Network metadata: %d/%d interlocking editors have Gender=Unknown.",
                  n_unknown_gender, n_interlocking))
  if (n_gender_adjudications > 0L && n_unknown_gender != 0L) {
    stop("Gender adjudication invariant failed: expected 0 Unknown among interlocking editors, found ",
         n_unknown_gender, ".", call. = FALSE)
  }

  # Hard consistency checks. These are relational rather than hard-coded counts,
  # so future confirmed merges can legitimately change the invariant values.
  expected_interlocking <- sum(built$person$n_journals >= 2)
  if (n_interlocking != expected_interlocking) {
    stop("Network membership mismatch: derived ", n_interlocking,
         " editors but person-level data identify ", expected_interlocking,
         " interlocking editors.", call. = FALSE)
  }
  if (anyDuplicated(out[c("editor_id", "Journal")])) {
    stop("Network input contains duplicate person-journal appointments.", call. = FALSE)
  }

  out
}
