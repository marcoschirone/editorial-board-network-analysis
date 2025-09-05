# R/data_processing.R
# Data loading, cleaning, and validation

validate_config <- function(cfg) {
  cfgd <- cfg$default %||% cfg
  required <- c("file_path","col_orcid","col_journal","col_country",
                "col_continent","col_subregion","col_gender")
  miss <- setdiff(required, names(cfgd))
  if (length(miss)) stop("Config is missing fields: ", paste(miss, collapse=", "), call. = FALSE)

  if (!file.exists(cfgd$file_path)) {
    stop("Data file not found at config$file_path: ", cfgd$file_path, call. = FALSE)
  }
  invisible(cfg)
}

load_and_clean_data <- function(cfg) {
  cfgd <- cfg$default %||% cfg
  validate_config(cfg)

  raw <- readxl::read_xlsx(cfgd$file_path)

  map <- list(
    ORCID     = cfgd$col_orcid,
    Journal   = cfgd$col_journal,
    Country   = cfgd$col_country,
    Continent = cfgd$col_continent,
    Subregion = cfgd$col_subregion,
    Gender    = cfgd$col_gender
  )

  assert_has_columns(raw, unlist(map), "input spreadsheet")

  df <- raw |>
    dplyr::select(
      ORCID     = dplyr::all_of(map$ORCID),
      Journal   = dplyr::all_of(map$Journal),
      Country   = dplyr::all_of(map$Country),
      Continent = dplyr::all_of(map$Continent),
      Subregion = dplyr::all_of(map$Subregion),
      Gender    = dplyr::all_of(map$Gender)
    ) |>
    dplyr::mutate(across(everything(), ~clean_ids(as.character(.))))

  df <- df |>
    dplyr::mutate(
      editor_id = dplyr::if_else(
        !is.na(ORCID) & nzchar(ORCID),
        ORCID,
        paste0("ED_", row_number())
      )
    )

  message(sprintf("Loaded data: %d rows, %d unique editors, %d unique journals",
                  nrow(df), n_distinct(df$editor_id), n_distinct(df$Journal)))
  df
}
