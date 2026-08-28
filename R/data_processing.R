# R/data_processing.R
# Data loading, cleaning, and validation

validate_config <- function(cfg) {
  cfgd <- cfg$default %||% cfg
  required <- c("full_population_path", "m49_lookup_path", "confirmed_merges_path", "annotation_path", "gender_namsor_path", "gender_adjudication_path")
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



# Build one gender record per corrected person from the frozen population-wide
# NamSor classifications. The same identity merges used for the analytical
# population are therefore applied to gender before any group comparison.
#
# A corrected identity may contain several pre-merge name strings. If at least
# one alias has a confident NamSor label (Female/Male) and the confident labels
# agree, that label is retained. If all aliases are Low confidence, the merged
# identity remains Low confidence. Conflicting confident labels are a hard error.
build_gender_metadata <- function(built, gender_namsor_path,
                                  annotation_path = NULL,
                                  gender_adjudication_path = NULL) {
  if (is.null(built$person) || is.null(built$positions)) {
    stop("`built` must be the list returned by build_person_level().", call. = FALSE)
  }
  if (is.null(gender_namsor_path) || !file.exists(gender_namsor_path)) {
    stop("Population-wide NamSor file not found: ", gender_namsor_path, call. = FALSE)
  }

  graw <- utils::read.csv(gender_namsor_path, stringsAsFactors = FALSE,
                          encoding = "UTF-8", check.names = FALSE)
  assert_has_columns(graw, c("Name", "gender_final"), "population NamSor file")
  g <- graw |>
    dplyr::transmute(
      original_id = clean_ids(as.character(Name)),
      Gender_namsor_raw = clean_ids(as.character(gender_final)),
      namsor_probability = if ("namsor_probability" %in% names(graw)) {
        suppressWarnings(as.numeric(namsor_probability))
      } else NA_real_
    )

  allowed <- c("Female", "Male", "Low confidence")
  bad <- setdiff(unique(stats::na.omit(g$Gender_namsor_raw)), allowed)
  if (length(bad)) {
    stop("Unsupported gender_final value(s) in population NamSor file: ",
         paste(bad, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(g$original_id)) {
    stop("Population NamSor file must contain one row per pre-merge exact-name identity.",
         call. = FALSE)
  }

  # The person table records every pre-merge alias separated by ' | '. Expand
  # those aliases and map each frozen NamSor result to the corrected identity.
  aliases <- built$person |>
    dplyr::select(person_id, person_id_original) |>
    tidyr::separate_rows(person_id_original, sep = " \\| ") |>
    dplyr::transmute(person_id, original_id = clean_ids(person_id_original))

  missing_gender <- setdiff(unique(aliases$original_id), g$original_id)
  extra_gender <- setdiff(g$original_id, unique(aliases$original_id))
  if (length(missing_gender) || length(extra_gender)) {
    stop(sprintf(
      "NamSor/population identity mismatch after reconciliation: %d source identities missing gender; %d gender identities absent from population.",
      length(missing_gender), length(extra_gender)), call. = FALSE)
  }

  mapped <- aliases |>
    dplyr::left_join(g, by = "original_id")

  resolve_namsor <- function(x) {
    confident <- unique(x[x %in% c("Female", "Male")])
    if (length(confident) > 1L) {
      stop("Confirmed identity merge combines conflicting confident NamSor labels.", call. = FALSE)
    }
    if (length(confident) == 1L) confident[[1]] else "Low confidence"
  }

  namsor_person <- mapped |>
    dplyr::group_by(person_id) |>
    dplyr::summarise(
      Gender_namsor = resolve_namsor(Gender_namsor_raw),
      n_gender_aliases = dplyr::n(),
      n_low_conf_aliases = sum(Gender_namsor_raw == "Low confidence"),
      .groups = "drop"
    )

  # Legacy annotations are retained only for a transparent mixed-instrument
  # sensitivity/descriptive completion. They never enter the primary NamSor
  # inferential analysis.
  legacy <- tibble::tibble(person_id = character(), Gender_legacy = character())
  if (!is.null(annotation_path) && file.exists(annotation_path)) {
    ann <- readxl::read_xlsx(annotation_path)
    assert_has_columns(ann, c("Name", "Gender"), "annotation spreadsheet")
    ann <- ann |>
      dplyr::transmute(
        alias = clean_ids(as.character(Name)),
        Gender_legacy = dplyr::case_when(
          grepl("^[FfWw]", as.character(Gender)) ~ "Female",
          grepl("^[Mm]", as.character(Gender)) ~ "Male",
          TRUE ~ NA_character_
        )
      )
    legacy <- aliases |>
      dplyr::left_join(ann, by = c("original_id" = "alias")) |>
      dplyr::filter(Gender_legacy %in% c("Female", "Male")) |>
      dplyr::group_by(person_id) |>
      dplyr::summarise(
        Gender_legacy = {
          z <- unique(Gender_legacy)
          if (length(z) > 1L) stop("Conflicting legacy gender annotations within a corrected identity.", call. = FALSE)
          z[[1]]
        }, .groups = "drop")
  }

  manual <- tibble::tibble(person_id = character(), Gender_manual = character())
  if (!is.null(gender_adjudication_path) && file.exists(gender_adjudication_path)) {
    ga <- utils::read.csv(gender_adjudication_path, stringsAsFactors = FALSE,
                          encoding = "UTF-8", check.names = FALSE)
    assert_has_columns(ga, c("person_id", "Gender"), "gender adjudication file")
    ga <- ga |>
      dplyr::transmute(alias = clean_ids(as.character(person_id)),
                       Gender_manual = clean_ids(as.character(Gender)))
    bad_manual <- setdiff(unique(stats::na.omit(ga$Gender_manual)), c("Female", "Male"))
    if (length(bad_manual)) stop("Unsupported manual Gender value(s): ", paste(bad_manual, collapse = ", "), call. = FALSE)

    alias_lookup <- dplyr::bind_rows(
      aliases |> dplyr::select(person_id, alias = original_id),
      built$person |> dplyr::transmute(person_id, alias = person_id)
    ) |> dplyr::distinct(alias, person_id)

    manual <- ga |>
      dplyr::left_join(alias_lookup, by = "alias")
    if (any(is.na(manual$person_id))) {
      stop("Manual gender adjudication(s) did not match the corrected population: ",
           paste(manual$alias[is.na(manual$person_id)], collapse = "; "), call. = FALSE)
    }
    manual <- manual |>
      dplyr::select(person_id, Gender_manual) |>
      dplyr::distinct()
    if (anyDuplicated(manual$person_id)) stop("Duplicate manual gender adjudication after identity reconciliation.", call. = FALSE)
  }

  out <- built$person |>
    dplyr::select(person_id, n_journals) |>
    dplyr::left_join(namsor_person, by = "person_id") |>
    dplyr::left_join(legacy, by = "person_id") |>
    dplyr::left_join(manual, by = "person_id") |>
    dplyr::mutate(
      Gender_completed = dplyr::coalesce(Gender_manual, Gender_legacy,
        dplyr::if_else(Gender_namsor %in% c("Female", "Male"), Gender_namsor, NA_character_),
        "Unknown"),
      Gender_source = dplyr::case_when(
        !is.na(Gender_manual) ~ "Manual adjudication",
        !is.na(Gender_legacy) ~ "Legacy annotation",
        Gender_namsor %in% c("Female", "Male") ~ "NamSor",
        TRUE ~ "Unresolved"
      )
    )

  counts <- out |> dplyr::count(Gender_namsor, name = "n")
  message("NamSor gender after identity reconciliation: ",
          paste(paste0(counts$Gender_namsor, "=", counts$n), collapse = "; "))
  out
}

# Build the network-analysis input from the same disambiguated full population
# used by the selection analysis. Network membership is derived from the full
# population, and the primary gender field is the population-wide NamSor label.
build_network_input <- function(built, annotation_path = NULL,
                                gender_metadata = NULL) {
  if (is.null(built$positions) || is.null(built$person)) {
    stop("`built` must be the list returned by build_person_level().", call. = FALSE)
  }
  if (is.null(gender_metadata)) {
    stop("`gender_metadata` is required so gender measurement is instrument-consistent.", call. = FALSE)
  }

  person <- built$person |>
    dplyr::select(person_id, n_journals, Country_1, Continent, Subregion)
  interlocking_ids <- person |>
    dplyr::filter(n_journals >= 2) |>
    dplyr::pull(person_id)

  # ORCID is optional legacy metadata only; it never determines membership or
  # the primary gender classification.
  annotations <- tibble::tibble(person_id = character(), ORCID = character())
  if (!is.null(annotation_path) && file.exists(annotation_path)) {
    ann_raw <- readxl::read_xlsx(annotation_path)
    assert_has_columns(ann_raw, c("Name", "ORCID"), "annotation spreadsheet")
    aliases <- built$person |>
      dplyr::select(person_id, person_id_original) |>
      tidyr::separate_rows(person_id_original, sep = " \\| ") |>
      dplyr::transmute(person_id, alias = clean_ids(person_id_original))
    ann <- ann_raw |>
      dplyr::transmute(alias = clean_ids(as.character(Name)), ORCID = clean_ids(as.character(ORCID)))
    annotations <- aliases |>
      dplyr::left_join(ann, by = "alias") |>
      dplyr::filter(!is.na(ORCID), nzchar(ORCID)) |>
      dplyr::group_by(person_id) |>
      dplyr::summarise(ORCID = dplyr::first(ORCID), .groups = "drop")
  }

  out <- built$positions |>
    dplyr::filter(person_id %in% interlocking_ids) |>
    dplyr::select(person_id, Journal) |>
    dplyr::distinct() |>
    dplyr::left_join(person, by = "person_id") |>
    dplyr::left_join(annotations, by = "person_id") |>
    dplyr::left_join(gender_metadata |>
      dplyr::select(person_id, Gender_namsor, Gender_completed, Gender_source),
      by = "person_id") |>
    dplyr::transmute(
      ORCID = ORCID,
      Journal = Journal,
      Country = Country_1,
      Continent = Continent,
      Subregion = Subregion,
      Gender_namsor = Gender_namsor,
      Gender_completed = Gender_completed,
      Gender_source = Gender_source,
      # Backward-compatible alias used by existing network code. It is now
      # explicitly the NamSor classification, including Low confidence.
      Gender = Gender_namsor,
      editor_id = person_id
    )

  n_population <- nrow(built$person)
  n_interlocking <- dplyr::n_distinct(out$editor_id)
  n_super <- sum(built$person$n_journals >= 3)
  n_appointments <- nrow(built$positions)
  gender_counts <- out |>
    dplyr::distinct(editor_id, Gender_namsor) |>
    dplyr::count(Gender_namsor, name = "n")

  message(sprintf(
    "Authoritative invariants: %d persons / %d appointments / %d interlocking / %d with >=3 journals",
    n_population, n_appointments, n_interlocking, n_super
  ))
  message("Interlocking NamSor gender: ",
          paste(paste0(gender_counts$Gender_namsor, "=", gender_counts$n), collapse = "; "))

  expected_interlocking <- sum(built$person$n_journals >= 2)
  if (n_interlocking != expected_interlocking) {
    stop("Network membership mismatch: expected ", expected_interlocking,
         " interlocking persons from the shared population, got ", n_interlocking, ".",
         call. = FALSE)
  }
  out
}
