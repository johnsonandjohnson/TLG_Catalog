#' Prune an rtabletree object to keep only the first leaf in each branch
#'
#' This function takes an S4 "rtabletree" object as input and returns a pruned version
#' with only the first leaf kept in each branch. The pruning is applied recursively
#' to all levels of the table.
#'
#' @param tree An S4 "rtabletree" object from the rtables package
#' @return A pruned version of the input rtabletree with only the first leaf kept in each branch
#' @examples
#' # Create a simple rtables object
#' library(rtables)
#' 
#' # Sample data
#' adsl <- data.frame(
#'   USUBJID = paste0("SUBJ", 1:10),
#'   AGE = sample(30:80, 10),
#'   SEX = sample(c("M", "F"), 10, replace = TRUE),
#'   TRT = sample(c("Drug A", "Placebo"), 10, replace = TRUE)
#' )
#' 
#' # Create a table layout
#' lyt <- basic_table() %>%
#'   split_cols_by("TRT") %>%
#'   analyze("AGE", 
#'     afun = function(x) {
#'       in_rows(
#'         "Mean" = mean(x),
#'         "Median" = median(x),
#'         "SD" = sd(x),
#'         "Min" = min(x),
#'         "Max" = max(x),
#'         "n" = length(x)
#'       )
#'     }
#'   )
#' 
#' # Build the table
#' result <- build_table(lyt, adsl)
#' 
#' # Prune the table to keep only the first leaf in each branch
#' pruned_result <- prune_rtabletree_to_first_leaf(result)
#' 
#' @export
prune_rtabletree_to_first_leaf <- function(tree) {
  # Check if the input is valid
  if (!inherits(tree, "TableTree")) {
    stop("Input must be an rtables TableTree object")
  }

  # Use the S4 implementation which is more reliable
  return(prune_rtabletree_to_first_leaf_s4(tree))
}

#' Alternative implementation using S4 methods if available
#'
#' This function uses rtables S4 methods to prune the tree if they are available.
#' This approach is more idiomatic and less likely to break the rtabletree structure.
#'
#' @param tree An S4 "rtabletree" object from the rtables package
#' @return A pruned version of the input rtabletree with only the first leaf kept in each branch
#' @examples
#' # See examples for prune_rtabletree_to_first_leaf
#' 
#' @export
prune_rtabletree_to_first_leaf_s4 <- function(tree) {
  # Check if the input is valid
  if (!inherits(tree, "TableTree")) {
    stop("Input must be an rtables TableTree object")
  }

  # Helper function to recursively prune the tree using S4 methods
  prune_node_s4 <- function(node) {
    # If this is a leaf node, return it as is
    if (inherits(node, "ContentRow") || inherits(node, "TableRow") || inherits(node, "ElementaryTable")) {
      return(node)
    }

    # If this is a node with children (branch)
    if (inherits(node, "Node") || inherits(node, "TableTree")) {
      # Get the children of this node using @ operator for S4 classes
      children <- node@children

      # If there are no children, return the node as is
      if (length(children) == 0) {
        return(node)
      }

      # Check if this is a special case where we need to prune rows
      if (length(children) == 1 && inherits(children[[1]], "ElementaryTable")) {
        # This is a special case where we have a single ElementaryTable child
        # We need to look at its rows and keep only the first one
        elem_table <- children[[1]]

        # Check if the ElementaryTable has rows that we can prune
        if (length(elem_table@rows) > 1) {
          # Keep only the first row
          elem_table@rows <- elem_table@rows[1]

          # Update the child
          node@children[[1]] <- elem_table
        }

        return(node)
      }

      # Process each child that is a branch (not a leaf)
      branch_children <- list()
      leaf_found <- FALSE
      first_leaf <- NULL

      for (i in seq_along(children)) {
        child <- children[[i]]

        # If the child is a branch, recursively prune it
        if (inherits(child, "Node") || inherits(child, "TableTree")) {
          pruned_child <- prune_node_s4(child)
          branch_children[[length(branch_children) + 1]] <- pruned_child
        } 
        # If the child is a leaf and we haven't found a leaf yet, keep it
        else if (!leaf_found) {
          leaf_found <- TRUE
          first_leaf <- child
        }
        # Otherwise, skip this leaf as we already have one
      }

      # Create a new list of children with branches and the first leaf (if any)
      new_children <- c(branch_children, if (!is.null(first_leaf)) list(first_leaf) else list())

      # Replace the children in the node using proper S4 slot assignment
      node@children <- new_children

      return(node)
    }

    # Default case: return the node unchanged
    return(node)
  }

  # Start the recursive pruning from the root
  pruned_tree <- prune_node_s4(tree)

  return(pruned_tree)
}

#' Create a simple example table to demonstrate the pruning function
#'
#' This function creates a simple rtables object with multiple levels and leaves
#' that can be used to demonstrate the pruning function.
#'
#' @return An rtables object with multiple levels and leaves
#' @examples
#' # Create a simple example table
#' example_table <- create_example_table()
#' 
#' # Prune the table to keep only the first leaf in each branch
#' pruned_table <- prune_rtabletree_to_first_leaf(example_table)
#' 
#' @export
create_example_table <- function() {
  # Load required packages
  library(rtables)

  # Create sample data
  adsl <- data.frame(
    USUBJID = paste0("SUBJ", 1:20),
    AGE = sample(30:80, 20, replace = TRUE),
    SEX = sample(c("M", "F"), 20, replace = TRUE),
    RACE = sample(c("WHITE", "BLACK", "ASIAN"), 20, replace = TRUE),
    TRT = sample(c("Drug A", "Placebo"), 20, replace = TRUE)
  )

  # Create a table layout with multiple levels and leaves
  lyt <- basic_table() %>%
    split_cols_by("TRT") %>%
    split_rows_by("SEX") %>%
    split_rows_by("RACE") %>%
    analyze("AGE", 
      afun = function(x) {
        in_rows(
          "Mean" = mean(x),
          "Median" = median(x),
          "SD" = sd(x),
          "Min" = min(x),
          "Max" = max(x),
          "n" = length(x)
        )
      }
    )

  # Build the table
  result <- build_table(lyt, adsl)

  return(result)
}

#' Demonstrate the pruning function with a simple example
#'
#' This function creates a simple example table, prunes it, and prints both
#' the original and pruned tables for comparison.
#'
#' @return NULL (prints the tables as a side effect)
#' @examples
#' # Demonstrate the pruning function
#' demonstrate_pruning()
#' 
#' @export
demonstrate_pruning <- function() {
  # Create a simple example table
  cat("Creating example table...\n")
  example_table <- create_example_table()

  # Print the original table
  cat("\nOriginal table:\n")
  print(example_table)

  # Prune the table to keep only the first leaf in each branch
  cat("\nPruning table...\n")
  pruned_table <- prune_rtabletree_to_first_leaf(example_table)

  # Print the pruned table
  cat("\nPruned table (only first leaf in each branch):\n")
  print(pruned_table)

  # Return invisibly
  invisible(NULL)
}
