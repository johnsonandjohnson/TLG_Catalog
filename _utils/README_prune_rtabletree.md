# rtabletree Pruning Function

## Overview

This utility provides functions to create lightweight versions of `rtables` objects by pruning each branch to keep only the first leaf. This is particularly useful for testing purposes when generating the final RTF output from large tables is very slow.

## Problem

In pharmaceutical reporting, teams generate hundreds of listings and tables using the R package `rtables`. Each table is represented as an S4 `"rtabletree"` object. Generating the final RTF output from these objects can be very slow, especially for large tables with many branches and leaves.

## Solution

The `prune_rtabletree_to_first_leaf` function takes an S4 `"rtabletree"` object as input and returns a pruned version with only the first leaf kept in each branch. This significantly reduces the size of the table while preserving its structure, making it much faster to generate RTF output for testing purposes.

For example, a table branch like:
```
Age: → mean, median, n
```

becomes:
```
Age: → mean
```

This pruning is applied recursively to all levels of the table.

## Functions

### `prune_rtabletree_to_first_leaf(tree)`

The main function that prunes an rtabletree object to keep only the first leaf in each branch.

**Parameters:**
- `tree`: An S4 "rtabletree" object from the rtables package

**Returns:**
- A pruned version of the input rtabletree with only the first leaf kept in each branch

### `prune_rtabletree_to_first_leaf_s4(tree)`

An alternative implementation that uses rtables S4 methods to prune the tree. This approach is more idiomatic and less likely to break the rtabletree structure.

**Parameters:**
- `tree`: An S4 "rtabletree" object from the rtables package

**Returns:**
- A pruned version of the input rtabletree with only the first leaf kept in each branch

### Helper Functions

- `create_example_table()`: Creates a simple rtables object with multiple levels and leaves for demonstration purposes
- `demonstrate_pruning()`: Demonstrates the pruning function with a simple example

## Usage

```r
# Load required packages
library(rtables)

# Source the pruning function
source("_utils/prune_rtabletree.R")

# Create or load your rtables object
# For example:
lyt <- basic_table() %>%
  split_cols_by("TRT") %>%
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

result <- build_table(lyt, adsl)

# Prune the table to keep only the first leaf in each branch
pruned_result <- prune_rtabletree_to_first_leaf(result)

# Use the pruned table for testing
# This will be much faster than using the original table
```

## Example

For a complete example, see the `demonstrate_pruning.R` script in the same directory. This script creates a complex example table, prunes it, and compares the sizes of the original and pruned tables.

## Benefits

- **Reduced Size**: Significantly reduces the size of rtables objects
- **Faster Processing**: Makes it much faster to generate RTF output for testing purposes
- **Preserved Structure**: Maintains the structure of the original table so downstream code still works
- **Robust**: Works with tables of varying depths and structures

## Requirements

- R
- rtables package