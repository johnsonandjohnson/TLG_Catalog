#' Demonstrate the rtabletree pruning function
#'
#' This script demonstrates how to use the prune_rtabletree_to_first_leaf function
#' to create lightweight versions of rtables objects by keeping only the first leaf
#' in each branch.
#'

# Load required packages
library(rtables)
library(dplyr)

# Source the pruning function
source("prune_rtabletree.R")

# Set seed for reproducibility
set.seed(123)

# Create a more complex example table that resembles real-world pharmaceutical tables
create_complex_example <- function() {
  # Create sample data that resembles clinical trial data
  adsl <- data.frame(
    USUBJID = paste0("SUBJ", 1:100),
    AGE = sample(18:80, 100, replace = TRUE),
    SEX = sample(c("M", "F"), 100, replace = TRUE),
    RACE = sample(c("WHITE", "BLACK", "ASIAN", "OTHER"), 100, replace = TRUE),
    TRT01P = sample(c("Drug X", "Placebo"), 100, replace = TRUE),
    SAFFL = "Y",
    stringsAsFactors = FALSE
  )

  # Add some laboratory data
  adlb <- data.frame(
    USUBJID = rep(adsl$USUBJID, each = 3),
    PARAM = rep(c("Hemoglobin", "Platelets", "White Blood Cells"), 100),
    AVAL = c(
      rnorm(100, mean = 14, sd = 1),    # Hemoglobin
      rnorm(100, mean = 250, sd = 50),  # Platelets
      rnorm(100, mean = 7, sd = 2)      # WBC
    ),
    AVALU = rep(c("g/dL", "10^9/L", "10^9/L"), 100),
    stringsAsFactors = FALSE
  )

  # Join the datasets
  data <- adlb %>%
    left_join(adsl, by = "USUBJID")

  # Create a table layout with multiple levels and leaves
  lyt <- basic_table(
    title = "Laboratory Results Summary",
    subtitles = "All Patients",
    main_footer = "This is a demonstration table",
    show_colcounts = TRUE
  ) %>%
    split_cols_by("TRT01P") %>%
    add_overall_col("All Patients") %>%
    split_rows_by("PARAM", split_fun = keep_split_levels(c("Hemoglobin", "Platelets", "White Blood Cells"))) %>%
    summarize_row_groups() %>%
    split_rows_by("SEX") %>%
    summarize_row_groups() %>%
    analyze("AVAL", 
      afun = function(x) {
        in_rows(
          "n" = length(x),
          "Mean" = mean(x, na.rm = TRUE),
          "SD" = sd(x, na.rm = TRUE),
          "Median" = median(x, na.rm = TRUE),
          "Min" = min(x, na.rm = TRUE),
          "Max" = max(x, na.rm = TRUE),
          "Q1" = quantile(x, 0.25, na.rm = TRUE),
          "Q3" = quantile(x, 0.75, na.rm = TRUE)
        )
      },
      format = "xx.xx"
    )

  # Build the table
  result <- build_table(lyt, data)

  return(result)
}

# Create the complex example table
cat("Creating complex example table...\n")
complex_table <- create_complex_example()

# Print the structure of the original table
cat("\nStructure of original table:\n")
str(complex_table, max.level = 2)

# Print more detailed information about the table structure
cat("\nDetailed structure of the first child:\n")
if (length(complex_table@children) > 0) {
  first_child <- complex_table@children[[1]]
  str(first_child, max.level = 2)

  if (length(first_child@children) > 0) {
    cat("\nStructure of first grandchild:\n")
    first_grandchild <- first_child@children[[1]]
    str(first_grandchild, max.level = 2)

    if (length(first_grandchild@children) > 0) {
      cat("\nStructure of first great-grandchild:\n")
      first_great_grandchild <- first_grandchild@children[[1]]
      str(first_great_grandchild, max.level = 2)
    }
  }
}

# Print the original table (first few rows)
cat("\nOriginal table (truncated):\n")
print(complex_table)

# Examine the ElementaryTable structure more closely
cat("\nExamining ElementaryTable structure:\n")
if (length(complex_table@children) > 0 && 
    length(complex_table@children[[1]]@children) > 0 && 
    length(complex_table@children[[1]]@children[[1]]@children) > 0 && 
    length(complex_table@children[[1]]@children[[1]]@children[[1]]@children) > 0) {

  elem_table <- complex_table@children[[1]]@children[[1]]@children[[1]]@children[[1]]
  if (inherits(elem_table, "ElementaryTable")) {
    cat("Found an ElementaryTable. Examining its structure:\n")
    # Print all slot names
    cat("Slot names: ", paste(slotNames(elem_table), collapse = ", "), "\n")

    # Print the structure of the ElementaryTable
    str(elem_table, max.level = 2)

    # Check if it has a 'row_data' slot
    if ("row_data" %in% slotNames(elem_table)) {
      cat("row_data slot found. Structure:\n")
      str(elem_table@row_data, max.level = 2)
    }

    # Check if it has a 'data' slot
    if ("data" %in% slotNames(elem_table)) {
      cat("data slot found. Structure:\n")
      str(elem_table@data, max.level = 2)
    }

    # Check if it has a 'rrow' slot
    if ("rrow" %in% slotNames(elem_table)) {
      cat("rrow slot found. Structure:\n")
      str(elem_table@rrow, max.level = 2)
    }

    # Check if it has a 'crow' slot
    if ("crow" %in% slotNames(elem_table)) {
      cat("crow slot found. Structure:\n")
      str(elem_table@crow, max.level = 2)
    }
  } else {
    cat("Not an ElementaryTable. Class: ", class(elem_table)[1], "\n")
  }
} else {
  cat("Could not find an ElementaryTable to examine.\n")
}

# Skip pruning for now
cat("\nSkipping pruning for now...\n")
pruned_table <- complex_table  # Just use the original table

# Print the structure of the pruned table
cat("\nStructure of pruned table:\n")
str(pruned_table, max.level = 2)

# Print the pruned table
cat("\nPruned table (only first leaf in each branch):\n")
print(pruned_table)

# Compare the sizes
original_size <- object.size(complex_table)
pruned_size <- object.size(pruned_table)
size_reduction <- (1 - as.numeric(pruned_size) / as.numeric(original_size)) * 100

cat("\nSize comparison:\n")
cat("Original table size: ", format(original_size, units = "auto"), "\n")
cat("Pruned table size: ", format(pruned_size, units = "auto"), "\n")
cat("Size reduction: ", round(size_reduction, 1), "%\n")

# Check if the pruning actually worked
cat("\nChecking if pruning worked:\n")

# Function to explore the structure of a node
explore_node <- function(node, path = "", depth = 0) {
  indent <- paste(rep("  ", depth), collapse = "")

  # Print information about this node
  cat(indent, "Path: ", path, "\n")
  cat(indent, "Class: ", class(node)[1], "\n")

  # If this is a TableTree or similar container, explore its children
  if (inherits(node, "TableTree") || inherits(node, "Node")) {
    cat(indent, "Children: ", length(node@children), "\n")

    # Explore each child
    for (i in seq_along(node@children)) {
      child <- node@children[[i]]
      child_path <- if (path == "") as.character(i) else paste0(path, "->", i)
      cat(indent, "Child ", i, ":\n")
      explore_node(child, child_path, depth + 1)
    }
  } else if (inherits(node, "ContentRow") || inherits(node, "TableRow")) {
    # This is a leaf node (row)
    cat(indent, "Leaf node (row)\n")
  } else {
    # Some other type of node
    cat(indent, "Other type of node\n")
  }
}

# Explore the structure of the original table (first parameter only)
cat("\nStructure of original table (first parameter only):\n")
if (length(complex_table@children) > 0) {
  first_param <- complex_table@children[[1]]
  explore_node(first_param, "Hemoglobin", 0)
} else {
  cat("No children in the original table.\n")
}

# Examine the ElementaryTable structure more closely
cat("\nExamining ElementaryTable structure:\n")
if (length(complex_table@children) > 0 && 
    length(complex_table@children[[1]]@children) > 0 && 
    length(complex_table@children[[1]]@children[[1]]@children) > 0 && 
    length(complex_table@children[[1]]@children[[1]]@children[[1]]@children) > 0) {

  elem_table <- complex_table@children[[1]]@children[[1]]@children[[1]]@children[[1]]
  if (inherits(elem_table, "ElementaryTable")) {
    cat("Found an ElementaryTable. Examining its structure:\n")
    # Print all slot names
    cat("Slot names: ", paste(slotNames(elem_table), collapse = ", "), "\n")

    # Print the structure of the ElementaryTable
    str(elem_table, max.level = 1)

    # Check if it has a 'row_data' slot
    if ("row_data" %in% slotNames(elem_table)) {
      cat("row_data slot found. Structure:\n")
      str(elem_table@row_data, max.level = 1)
    }

    # Check if it has a 'data' slot
    if ("data" %in% slotNames(elem_table)) {
      cat("data slot found. Structure:\n")
      str(elem_table@data, max.level = 1)
    }
  } else {
    cat("Not an ElementaryTable. Class: ", class(elem_table)[1], "\n")
  }
} else {
  cat("Could not find an ElementaryTable to examine.\n")
}

# Explore the structure of the pruned table (first parameter only)
cat("\nStructure of pruned table (first parameter only):\n")
if (length(pruned_table@children) > 0) {
  first_param <- pruned_table@children[[1]]
  explore_node(first_param, "Hemoglobin", 0)
} else {
  cat("No children in the pruned table.\n")
}

# Look at the first parameter (Hemoglobin) and first sex (F)
if (length(complex_table@children) > 0 && 
    length(complex_table@children[[1]]@children) > 0 && 
    length(complex_table@children[[1]]@children[[1]]@children) > 0) {

  # Original table - count rows for Hemoglobin -> F
  original_rows <- length(complex_table@children[[1]]@children[[1]]@children[[1]]@children)
  cat("\nOriginal table - rows for Hemoglobin -> F: ", original_rows, "\n")

  # Pruned table - count rows for Hemoglobin -> F
  if (length(pruned_table@children) > 0 && 
      length(pruned_table@children[[1]]@children) > 0 && 
      length(pruned_table@children[[1]]@children[[1]]@children) > 0) {
    pruned_rows <- length(pruned_table@children[[1]]@children[[1]]@children[[1]]@children)
    cat("Pruned table - rows for Hemoglobin -> F: ", pruned_rows, "\n")

    if (pruned_rows < original_rows) {
      cat("Pruning worked! Reduced from", original_rows, "to", pruned_rows, "rows.\n")
    } else {
      cat("Pruning did not reduce the number of rows.\n")
    }
  } else {
    cat("Could not access the same structure in the pruned table.\n")
  }
} else {
  cat("Could not access the expected structure in the original table.\n")
}
