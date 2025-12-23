#!/usr/bin/env Rscript
# TLG Converter Script
# This script searches for R files matching a pattern in a specified folder
# and converts them to QMD format using r_to_qmd.R

# Load required libraries
library(stringr)
library(fs)

#' Convert TLG scripts to QMD format
#'
#' @param folder_path Path to the folder containing R scripts to convert
#' @param regex Regular expression pattern to match files (default: "^[tlg].*\\.R$")
#' @param output_dir Output directory for QMD files (default: current directory)
#' @param overwrite Whether to overwrite existing files (default: FALSE)
#' @return Vector of paths to created QMD files
#' @export
convert_tlg_scripts <- function(folder_path, regex = "^[tlg].*\\.R$",
                                output_dir = ".", overwrite = FALSE) {
  # Ensure folder_path exists
  if (!dir.exists(folder_path)) {
    stop(paste("Folder not found:", folder_path))
  }

  # Source the r_to_qmd.R script if it exists
  r_to_qmd_path <- "./convert/r_to_qmd.R"
  if (file.exists(r_to_qmd_path)) {
    source(r_to_qmd_path)
  } else {
    stop("r_to_qmd.R not found in the current directory")
  }

  # Find all R files in the folder that match the regex pattern
  r_files <- list.files(
    path = folder_path,
    pattern = "\\.R$",
    full.names = TRUE,
    recursive = TRUE
  )

  # Filter files based on the regex pattern
  matching_files <- r_files[grepl(regex, basename(r_files))]

  if (length(matching_files) == 0) {
    message(paste("No files matching pattern", regex, "found in", folder_path))
    return(character(0))
  }

  message(paste("Found", length(matching_files), "files matching pattern", regex))

  # Convert each matching file
  qmd_files <- character(0)
  for (r_file in matching_files) {
    message(paste("Converting", r_file))
    tryCatch({
      qmd_file <- r_to_qmd(r_file, output_dir, overwrite)
      qmd_files <- c(qmd_files, qmd_file)
    }, error = function(e) {
      warning(paste("Failed to convert", r_file, ":", e$message))
    })
  }

  message(paste("Successfully converted", length(qmd_files), "files"))
  return(qmd_files)
}

# Command line interface
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0 || args[1] == "--help") {
    cat("Usage: Rscript tlg_converter.R [options] folder_path\n")
    cat("Options:\n")
    cat("  --regex=PATTERN   Regular expression pattern to match files (default: \"^[tlg].*\\.R$\")\n")
    cat("  --output=DIR      Output directory (default: current directory)\n")
    cat("  --overwrite       Overwrite existing files\n")
    cat("  --help            Show this help message\n")
    quit(status = 0)
  }

  # Parse arguments
  folder_path <- NULL
  regex <- "^[tlg].*\\.R$"
  output_dir <- "."
  overwrite <- FALSE

  for (arg in args) {
    if (startsWith(arg, "--regex=")) {
      regex <- substring(arg, 9)
    } else if (startsWith(arg, "--output=")) {
      output_dir <- substring(arg, 10)
    } else if (arg == "--overwrite") {
      overwrite <- TRUE
    } else if (!startsWith(arg, "--")) {
      folder_path <- arg
    }
  }

  if (is.null(folder_path)) {
    cat("Error: No folder path specified\n")
    cat("Use --help for usage information\n")
    quit(status = 1)
  }

  # Run the conversion
  convert_tlg_scripts(folder_path, regex, output_dir, overwrite)
}



path <- "./programs"
#convert_tlg_scripts(path, regex = "lsfae01.R", overwrite = TRUE)
convert_tlg_scripts(path, regex = "^[t].*\\.R$", overwrite = TRUE)
#convert_tlg_scripts(path, regex = "^[l].*\\.R$", overwrite = TRUE)
# If you want to actually run the examples, uncomment the following lines:
# if (dir.exists(path)) {
#   result <- convert_tlg_scripts(path, regex = "^gsfae01.*\\.R$", overwrite = TRUE)
#   cat("Converted files:", paste(result, collapse = ", "), "\n")
# } else {
#   cat("The 'tables' directory doesn't exist. Please adjust the path.\n")
# }

# ---- hardcoded post-processing rules to fix APT wrongs labels ----

move_into <- function(src, dest) {
  if (!dir.exists(src)) return(invisible(NULL))
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(src, full.names = TRUE, recursive = TRUE)
  file.rename(files, file.path(dest, basename(files)))
  unlink(src, recursive = TRUE, force = TRUE)
}

move_into("listings/demographic_and_other_baseline_characteristics", "listings/demographic")
move_into("listings/serious_adverse_events", "listings/adverse_events")
move_into("listings/deaths", "listings/adverse_events")
move_into("listings/adverse_events_of_special_interest", "listings/adverse_events")
move_into("listings/discontinuations_and/or_dose_modifications_due_to_adverse_events", "listings/adverse_events")


move_into("tables/demographic_and_other_baseline_characteristics", "tables/demographic")
move_into("tables/adverse_events_for_japan_submission", "tables/adverse_events")
move_into("tables/serious_adverse_events", "tables/adverse_events")
move_into("tables/deaths", "tables/adverse_events")
move_into("tables/adverse_events_of_special_interest", "tables/adverse_events")
move_into("tables/discontinuations_and/or_dose_modifications_due_to_adverse_events", "tables/adverse_events")

move_into("tables/study_treatment_compliance", "tables/exposure")
