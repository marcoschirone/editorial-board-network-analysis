# R/data_export.R
# Export and table creation functions

export_results <- function(final_results, output_dir) {
  message("Exporting results...")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Export CSVs
  if (!is.null(final_results$metrics$editor_stats)) {
    write_csv(final_results$metrics$editor_stats, file.path(output_dir, "editor_metrics.csv"))
  }
  if (!is.null(final_results$journal_metrics$journal_stats)) {
    write_csv(final_results$journal_metrics$journal_stats, file.path(output_dir, "journal_metrics.csv"))
  }
  if (!is.null(final_results$metrics$inequality_measures)) {
    write_csv(final_results$metrics$inequality_measures, file.path(output_dir, "inequality_measures.csv"))
  }
  if (!is.null(final_results$disparity_results$gender)) {
    write_csv(final_results$disparity_results$gender, file.path(output_dir, "gender_disparities.csv"))
  }
  if (!is.null(final_results$disparity_results$geographic)) {
    write_csv(final_results$disparity_results$geographic, file.path(output_dir, "geographic_disparities.csv"))
  }
  if (!is.null(final_results$leiden_sweep)) {
    write_csv(final_results$leiden_sweep$sweep_results, file.path(output_dir, "leiden_sweep_results.csv"))
  }

  # Save complete results
  saveRDS(final_results, file.path(output_dir, "full_analysis_results.rds"))

  invisible(TRUE)
}

create_publication_tables <- function(final_results, output_dir) {
  message("Creating publication tables...")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  wb <- openxlsx::createWorkbook()
  sheets_added <- 0

  add_sheet <- function(name, data) {
    if (!is.null(data)) {
      openxlsx::addWorksheet(wb, name)
      openxlsx::writeData(wb, name, data)
      sheets_added <<- sheets_added + 1
    }
  }

  add_sheet("Editor_Metrics", final_results$metrics$editor_stats)
  add_sheet("Journal_Metrics", final_results$journal_metrics$journal_stats)
  add_sheet("Disparity_Gender", final_results$disparity_results$gender)
  add_sheet("Disparity_Geography", final_results$disparity_results$geographic)
  add_sheet("Board_Analysis", final_results$board_analysis)

  if (sheets_added > 0) {
    openxlsx::saveWorkbook(wb, file.path(output_dir, "publication_summary_tables.xlsx"), overwrite = TRUE)
    message(sprintf("Publication tables saved with %d sheets", sheets_added))
  }

  invisible(TRUE)
}
