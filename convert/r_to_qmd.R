# R script to convert R files to QMD format
# This script takes an R file as input, extracts the folder path from the header,
# creates the folder structure if it doesn't exist, and outputs a .qmd file
# in the appropriate location.

library(stringr)
library(fs)

#' Convert an R file to QMD format
#'
#' @param r_file_path Path to the R file to convert
#' @param output_dir Base output directory (default: current directory)
#' @param overwrite Whether to overwrite existing files (default: FALSE)
#' @return Path to the created QMD file
#' @export
r_to_qmd <- function(r_file_path, output_dir = ".", overwrite = FALSE) {
  # Verify the R file exists
  if (!file.exists(r_file_path)) {
    stop(paste("R file not found:", r_file_path))
  }

  # Read the R file content
  r_content <- readLines(r_file_path, warn = FALSE)

  # Remove source and original reporting effort comments
  # First, find the lines with the Source pattern (allowing for multiple # symbols)
  source_line_idx <- grep("^\\s*#{1,}\\s*Source:\\s*programs/.*\\.R", r_content)

  # Also look for the pattern in the QMD output format
  qmd_source_line_idx <- grep("^\\s*#\\s*Source:\\s*programs/.*\\.R$", r_content)
  source_line_idx <- c(source_line_idx, qmd_source_line_idx)

  if (length(source_line_idx) > 0) {
    # For each source line, look for the corresponding Original Reporting Effort line
    for (idx in source_line_idx) {
      # Look for Original Reporting Effort within the next few lines
      search_range <- min(idx + 10, length(r_content))
      for (j in idx:search_range) {
        if (j <= length(r_content) && grepl("^\\s*#{1,}\\s*Original\\s*Reporting\\s*Effort:", r_content[j])) {
          # Remove from the source line to the Original Reporting Effort line
          r_content <- r_content[-(idx:j)]
          break
        }
      }
    }
  }

  # Also check for the QMD format pattern directly
  qmd_pattern_idx <- grep("^\\s*#\\s*Source:\\s*programs/.*\\.R$", r_content)
  if (length(qmd_pattern_idx) > 0) {
    for (idx in qmd_pattern_idx) {
      if (idx < length(r_content) && grepl("^\\s*#\\s*Original\\s*Reporting\\s*Effort:", r_content[idx + 1])) {
        # Remove both lines
        r_content <- r_content[-(idx:(idx + 1))]
      }
    }
  }

  # Extract the base name of the file (without extension)
  base_name <- tools::file_path_sans_ext(basename(r_file_path))
  base_name_upper <- toupper(base_name)

  # Determine folder path based on the first letter of the identifier
  first_letter <- tolower(substr(base_name, 1, 1))

  # Replace with input_path = '../..'
  r_content <- gsub("get_titles_from_file(",
                   "get_titles_from_file(input_path = '../../_data/', ",
                   r_content,
                   fixed = TRUE)

 # stumb the results only for listings
 if (first_letter == "l") {
   r_content <- gsub("tt = result",
                    "tt = head(result, 100)",
                    r_content,
                    fixed = TRUE)
 }
  # dpspath
  r_content <- gsub("read_path(dpspath",
                   "file.path('../../_data'",
                   r_content,
                   fixed = TRUE)

  if (first_letter == "t") {
    folder_path <- "tables"
  } else if (first_letter == "l") {
    folder_path <- "listings"
  } else if (first_letter == "g") {
    folder_path <- "graphs"
  } else {
      stop("something wrong with the name")
  }


  # Get template information using the API
  subfolder <- "others"  # Default subfolder if no tag information is available
  tryCatch({
    source("convert/API.R")

    # Get template info using the API
    info <- get_template_info(base_name_upper, include_refmock = FALSE, verbose = FALSE)

    # Extract tag information if available
    if (!is.null(info) && !is.null(info$details[[1]]$tags[[1]]) && length(info$details[[1]]$tags[[1]]) > 0) {
      tag <- info$details[[1]]$tags[[1]]
      # Convert tag to lowercase and replace spaces with underscores for folder name
      subfolder <- tolower(gsub(" ", "_", tag))
    }
  }, error = function(e) {
    message(paste("Failed to get tag information from API:", e$message))
    # Continue with default subfolder
  })

  # Create the full output directory path with subfolder
  full_output_dir <- file.path(output_dir, folder_path, subfolder)

  # Create the directory if it doesn't exist
  if (!dir.exists(full_output_dir)) {
    message(paste("Creating directory:", full_output_dir))
    dir.create(full_output_dir, recursive = TRUE)
  }

  # Set the output file path
  qmd_file_path <- file.path(full_output_dir, paste0(base_name, ".qmd"))

  # Check if the file already exists and handle overwrite
  if (file.exists(qmd_file_path) && !overwrite) {
    warning(paste("File already exists and overwrite=FALSE:", qmd_file_path))
    return(qmd_file_path)
  }

  # Generate QMD content
  qmd_content <- generate_qmd_content(r_content, base_name, base_name_upper, first_letter)

  # Remove source and original reporting effort comments from QMD content
  source_line_idx <- grep("# Source: programs/.*\\.R", qmd_content)
  if (length(source_line_idx) > 0) {
    for (idx in source_line_idx) {
      if (idx < length(qmd_content) && grepl("# Original Reporting Effort:", qmd_content[idx + 1])) {
        # Remove both lines
        qmd_content <- qmd_content[-(idx:(idx + 1))]
      }
    }
  }

  # Write the QMD file
  writeLines(qmd_content, qmd_file_path)

  message(paste("Successfully converted", r_file_path, "to", qmd_file_path))
  return(qmd_file_path)
}



#' Generate QMD content from R content
#'
#' @param r_content Lines of the R file
#' @param base_name Base name of the file
#' @param base_name_upper Uppercase base name
#' @param first_letter First letter of the base name (used to determine file type)
#' @return QMD content as a character vector
generate_qmd_content <- function(r_content, base_name, base_name_upper, first_letter) {
  # Determine the title based on the file prefix
  prefix <- substr(base_name_upper, 1, 2)

  if (prefix == "TS") {
    title_type <- "Table"
  } else if (prefix == "GS") {
    title_type <- "Graphic"
  } else if (prefix == "LS") {
    title_type <- "Listing"
  } else {
    title_type <- "Output"
  }

  # Get subtitle from template info API
  subtitle <- ""
  tryCatch({
    # Source the API functions if not already loaded
    source("convert/API.R")

    # Get template info using the API
    info <- get_template_info(base_name_upper, include_refmock = FALSE, verbose = FALSE)

    # Use the subtitle from the template info if available
    if (!is.null(info) && !is.null(info$details[[1]]$subtitle)) {
      # Clean the subtitle by removing and replacing problematic characters
      subtitle <- info$details[[1]]$subtitle
      subtitle <- gsub("\\[", "", subtitle)  # Remove [
      subtitle <- gsub("\\]", "", subtitle)  # Remove ]
      subtitle <- gsub(":", "", subtitle)    # Remove ;
    }
  }, error = function(e) {
    warning(paste("Failed to get subtitle from API:", e$message))
  })

  # Create YAML header
  yaml_header <- c(
    "---",
    paste0("title: ", base_name_upper),
    paste0("subtitle: ", subtitle),
    "---",
    "",
    "------------------------------------------------------------------------",
    "",
    "{{< include ../../_utils/envir_hook.qmd >}}",
    ""
  )

  # Create setup code chunk
  setup_chunk <- c(
    "```{r setup, echo = FALSE, warning = FALSE, message = FALSE}",
    "options(docx.add_datetime = FALSE, tidytlg.add_datetime = FALSE)",
    "",
    "envsetup_config_name <- \"default\"",
    "",
    "# Path to the combined config file",
    "envsetup_file_path <- file.path(\"../..\", \"envsetup.yml\")",
    "",
    "Sys.setenv(ENVSETUP_ENVIRON = '')",
    "library(envsetup)",
    "loaded_config <- config::get(config = envsetup_config_name, file = envsetup_file_path)",
    "envsetup::rprofile(loaded_config)",
    "",
    "",
    "dpscomp <- compound",
    "dpspdr <- paste(protocol,dbrelease,rpteff,sep=\"__\")",
    "",
    "aptcomp <- compound",
    "aptpdr <- paste(protocol,dbrelease,rpteff,sep=\"__\")",
    "",
    "###### Study specific updates (formerly in envre)",
    "",
    "dpscomp <- \"standards\"",
    "dpspdr <- \"jjcs__NULL__jjcs - core\"",
    "",
    "apt <- FALSE",
    "",
    "```",
    "",
    "## Output",
    "",
    ":::: panel-tabset",
    "## {{< fa regular file-lines sm fw >}} Preview",
    ""
  )

  # Process the R content to create code chunks
  code_chunks <- process_r_content_to_chunks(r_content)

  # Remove source and original reporting effort comments from code chunks
  if (length(code_chunks) > 0) {
    # Find lines with Source: programs/ pattern
    source_lines <- grep("^\\s*#\\s*Source:\\s*programs/.*\\.R$", code_chunks)
    if (length(source_lines) > 0) {
      for (idx in source_lines) {
        if (idx < length(code_chunks) && 
            grepl("^\\s*#\\s*Original\\s*Reporting\\s*Effort:", code_chunks[idx + 1])) {
          # Remove both lines
          code_chunks <- code_chunks[-(idx:(idx + 1))]
        }
      }
    }
  }

  # Create the main code chunk
  # Filter out the source and original reporting effort comments
  filtered_chunks <- code_chunks
  if (length(filtered_chunks) >= 2) {
    # Check if the first two lines match the pattern
    if (grepl("^\\s*#\\s*Source:\\s*programs/.*\\.R$", filtered_chunks[1]) &&
        grepl("^\\s*#\\s*Original\\s*Reporting\\s*Effort:", filtered_chunks[2])) {
      # Remove the first two lines
      filtered_chunks <- filtered_chunks[-(1:2)]
    }
  }

  # Different main_chunk content based on file type
  if (first_letter == "l") {
    # For listings
    main_chunk <- c(
      "```{r variant1, results='hide', warning = FALSE, message = FALSE}",
      filtered_chunks,
      "```",
      "```{r result1, echo=FALSE, message=FALSE, warning=FALSE, test = list(result_v1 = \"result\")}",
      "tt_to_flextable_j(head(result,100), tblid)",
      "```",
      "",
      "[Download RTF file](`r paste0(tolower(tblid), '.rtf')`)",
      "::::",
      ""
    )
  } else {
    # For all other file types
    main_chunk <- c(
      "```{r variant1, results='hide', warning = FALSE, message = FALSE}",
      filtered_chunks,
      "```",
      "```{r result1, echo=FALSE, message=FALSE, warning=FALSE, test = list(result_v1 = \"result\")}",
      "tt_to_flextable_j(result, tblid)",
      "```",
      "",
      "[Download RTF file](`r paste0(tolower(tblid), '.rtf')`)",
      "::::",
      ""
    )
  }

  # Combine all parts
  qmd_content <- c(yaml_header, setup_chunk, main_chunk)

  return(qmd_content)
}

#' Process R content to create code chunks
#'
#' @param r_content Lines of the R file
#' @return Processed R code as a character vector
process_r_content_to_chunks <- function(r_content) {
  # Find all lines with section headers (lines with many # characters)
  header_lines <- grep("^#{10,}$", r_content)

  # Check for the large listing pattern
  large_listing_pattern <- "# If resulting Listing output is too large of a file \\(>20MB\\) then the listing"
  large_listing_idx <- grep(large_listing_pattern, r_content)

  # Skip the first few headers which are usually metadata
  if (length(header_lines) >= 3) {
    # Start after the folder path header (usually the 3rd header)
    start_header_idx <- 3

    # Process the content
    processed_content <- c()

    # Process each section
    for (i in seq(start_header_idx, length(header_lines), by = 2)) {
      if (i + 1 <= length(header_lines)) {
        # Get the section name (line between two header separators)
        section_name_line <- header_lines[i] + 1
        if (section_name_line < header_lines[i + 1]) {
          section_line <- r_content[section_name_line]
          if (grepl("^#+\\s+", section_line)) {
            # Extract the section name
            section_name <- gsub("^#+\\s+", "", section_line)

            # Add the section name as a markdown header
            processed_content <- c(processed_content, "", paste0("# ", section_name))

            # Get the code lines for this section (between the second header separator and the next first header separator)
            if (i + 2 <= length(header_lines)) {
              code_start <- header_lines[i + 1] + 1
              code_end <- header_lines[i + 2] - 1
            } else {
              code_start <- header_lines[i + 1] + 1
              code_end <- length(r_content)
            }

            if (code_end >= code_start) {
              # Extract the code lines
              code_lines <- r_content[code_start:code_end]

              # Check if this section contains the large listing pattern
              if (length(large_listing_idx) > 0 && 
                  any(large_listing_idx >= code_start & large_listing_idx <= code_end) && 
                  tolower(section_name) == "output listing") {
                # Replace with simplified output for this section only
                simplified_output <- c(
                  "",
                  "tt_to_tlgrtf(string_map = string_map, tt = head(result, 100),",
                  "  file = fileid,",
                  "  orientation = \"landscape\"",
                  ")"
                )
                processed_content <- c(processed_content, simplified_output)
              } else {
                # Skip empty lines at the beginning and end
                code_lines <- trimws(paste(code_lines, collapse = "\n"))
                code_lines <- strsplit(code_lines, "\n")[[1]]

                # Comment out lines that start with fileid
                code_lines <- sapply(code_lines, function(line) {
                  if (grepl("^\\s*fileid", line)) {
                    return("fileid <- tblid")
                  } else {
                    return(line)
                  }
                })

                # Add the code lines
                if (length(code_lines) > 0) {
                  processed_content <- c(processed_content, "", code_lines)
                }
              }
            }
          }
        }
      }
    }

    # If we have content, return it
    if (length(processed_content) > 0) {
      return(processed_content)
    }
  }

  # Fallback: extract key sections based on patterns
  # This is used if the header-based extraction didn't work

  # Find library calls
  library_lines <- grep("library\\(", r_content, value = TRUE)

  # Find table layout and build sections
  layout_start <- grep("lyt\\s*<-\\s*basic_table", r_content)
  result_lines <- grep("result\\s*<-", r_content)

  # Extract code chunks if found
  code_chunks <- c()

  # Add a comment to indicate the start of the code
  code_chunks <- c(code_chunks, "# Prep Environment")

  # Add libraries
  if (length(library_lines) > 0) {
    code_chunks <- c(code_chunks, "# Libraries", "", library_lines, "")
  } else {
    code_chunks <- c(code_chunks, "# Libraries", "", "library(dplyr)", "library(tern)", "")
  }

  # Add table layout if found
  if (length(layout_start) > 0) {
    layout_end <- min(c(result_lines[result_lines > layout_start[1]], length(r_content)))
    if (layout_end > layout_start[1]) {
      layout_code <- r_content[layout_start[1]:layout_end]
      code_chunks <- c(code_chunks, "# Table Layout", "", layout_code, "")
    }
  }

  # Add result processing if found
  if (length(result_lines) > 0) {
    result_idx <- result_lines[1]
    result_end <- min(result_idx + 5, length(r_content))
    result_code <- r_content[result_idx:result_end]

    # Comment out lines that start with tt_to_tlgrtf(
    # result_code <- sapply(result_code, function(line) {
    #   if (grepl("^\\s*tt_to_tlgrtf\\(", line)) {
    #     return(paste0("# ", line))
    #   } else {
    #     return(line)
    #   }
    # })

    code_chunks <- c(code_chunks, "# Build Table", "", result_code)
  }

  # If we still don't have code, use default example
  if (length(code_chunks) == 0) {
    # code_chunks <- c(
    #   "# Example code",
    #   "",
    #   "library(dplyr)",
    #   "library(tern)",
    #   "",
    #   "# Define the split function",
    #   "split_fun <- drop_split_levels",
    #   "",
    #   "# Create table layout",
    #   "lyt <- basic_table(show_colcounts = TRUE) %>%",
    #   "  split_cols_by(\"ARM\") %>%",
    #   "  add_overall_col(label = \"All Patients\")",
    #   "",
    #   "# Build the table",
    #   "result <- build_table(lyt, data)"
    # )
  }

  return(code_chunks)
}

#' Batch convert R files to QMD
#'
#' @param input_dir Directory containing R files to convert
#' @param output_dir Base output directory (default: current directory)
#' @param pattern File pattern to match (default: "\\.R$")
#' @param recursive Whether to search recursively (default: TRUE)
#' @param overwrite Whether to overwrite existing files (default: FALSE)
#' @return Vector of paths to created QMD files
#' @export
batch_r_to_qmd <- function(input_dir, output_dir = "..", pattern = "\\.R$",
                           recursive = TRUE, overwrite = FALSE) {
  # Get list of R files
  r_files <- list.files(input_dir, pattern = pattern,
                        full.names = TRUE, recursive = recursive)

  # Convert each file
  qmd_files <- character(0)
  for (r_file in r_files) {
    tryCatch({
      qmd_file <- r_to_qmd(r_file, output_dir, overwrite)
      qmd_files <- c(qmd_files, qmd_file)
    }, error = function(e) {
      warning(paste("Failed to convert", r_file, ":", e$message))
    })
  }

  return(qmd_files)
}

# Command line interface
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0 || args[1] == "--help") {
    cat("Usage: Rscript r_to_qmd.R [options] input_file.R\n")
    cat("Options:\n")
    cat("  --output DIR    Output directory (default: current directory)\n")
    cat("  --batch         Process all R files in the input directory\n")
    cat("  --overwrite     Overwrite existing files\n")
    cat("  --help          Show this help message\n")
    quit(status = 0)
  }

  # Parse arguments
  i <- 1
  output_dir <- ".."
  batch_mode <- FALSE
  overwrite <- FALSE

  while (i <= length(args)) {
    if (args[i] == "--output" && i < length(args)) {
      output_dir <- args[i + 1]
      i <- i + 2
    } else if (args[i] == "--batch") {
      batch_mode <- TRUE
      i <- i + 1
    } else if (args[i] == "--overwrite") {
      overwrite <- TRUE
      i <- i + 1
    } else {
      input_path <- args[i]
      i <- i + 1
    }
  }

  if (batch_mode) {
    if (file.exists(input_path) && file.info(input_path)$isdir) {
      batch_r_to_qmd(input_path, output_dir, overwrite = overwrite)
    } else {
      stop("Input path must be a directory in batch mode")
    }
  } else {
    r_to_qmd(input_path, output_dir, overwrite)
  }
}
