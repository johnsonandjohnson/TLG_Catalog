library(httr)

#' @title Get Autocode API Authorization Token
#' @description Retrieves the authorization token for the Autocode API from a file.
#' @return A character string containing the authorization token.
#' @examples
#' token <- get_autocode_token()
get_autocode_token <- function() {
  if (dir.exists("/adr/brownies")) {
    keys <- readLines("/adr/brownies/utilities/autocode_api_keys/autocode_keys.txt")
  } else {
    keys <- readLines("Z:/brownies/utilities/autocode_api_keys/autocode_keys.txt")
  }
  return(keys[2])
}

#' @title Create a Logger Function
#' @description Creates a function that logs messages if verbose mode is enabled.
#' @param verbose Logical. Whether to print messages.
#' @return A function that takes a message and prints it if verbose is TRUE.
#' @examples
#' logger <- create_logger(TRUE)
#' logger("This message will be printed")
create_logger <- function(verbose = TRUE) {
  function(msg) {
    if (verbose) cat(msg, "\n")
  }
}

#' @title Make Autocode API Request
#' @description Makes a request to the Autocode API.
#' @param endpoint Character. The API endpoint to call.
#' @param query List. The query parameters for the request.
#' @param token Character. The authorization token.
#' @param log_message Function. A function to log messages.
#' @return The parsed response from the API, or NULL if the request failed.
#' @examples
#' token <- get_autocode_token()
#' logger <- create_logger(TRUE)
#' response <- make_api_request("/search/", list(search = "TSFAE01a", varname = "All"), token, logger)
make_api_request <- function(endpoint, query, token, log_message) {
  base_url <- "https://autocode.jnj.com/dps/api/v1"
  full_endpoint <- paste0(base_url, endpoint)

  response <- httr::GET(
    full_endpoint,
    query = query,
    httr::add_headers("Authorization" = token),
    httr::accept_json()
  )

  if (httr::status_code(response) != 200) {
    log_message(paste("Error:", httr::http_status(response)$message))
    log_message(paste("Status code:", httr::status_code(response)))
    return(NULL)
  }

  return(httr::content(response, "parsed"))
}

#' @title Search for Template
#' @description Searches for a template in Autocode.
#' @param search_term Character. The term to search for.
#' @param token Character. The authorization token.
#' @param log_message Function. A function to log messages.
#' @return The parsed search results from the API, or NULL if the request failed.
#' @examples
#' token <- get_autocode_token()
#' logger <- create_logger(TRUE)
#' results <- search_template("TSFAE01a", token, logger)
search_template <- function(search_term, token, log_message) {
  log_message(paste("Searching for template:", search_term))
  return(make_api_request(
    "/search/",
    list(search = search_term, varname = "All"),
    token,
    log_message
  ))
}

#' @title Filter Search Results
#' @description Filters search results based on compound and title.
#' @param search_results List. The search results from the API.
#' @param title Character. The title to filter by.
#' @param compound Character. The compound to filter by.
#' @param log_message Function. A function to log messages.
#' @return The filtered result, or NULL if no match was found.
#' @examples
#' token <- get_autocode_token()
#' logger <- create_logger(TRUE)
#' results <- search_template("TSFAE01a", token, logger)
#' filtered <- filter_results(results, "TSFAE01a", "standards", "jjcs - core", logger)
filter_results <- function(search_results, title, compound="standards", re="jjcs - core", log_message) {
  if (is.null(search_results)) return(NULL)

  filtered_result <- NULL
  filtered_result <- lapply(search_results, function(x) {
    if (tolower(x$title) == tolower(title) &&
        x$compound == compound &&
        x$re == re) {
      return(x)
    }
  })

  filtered_result <- Filter(Negate(is.null), filtered_result)

  if (is.null(filtered_result)) {
    log_message(paste("No results found matching the criteria: compound =", compound, "and title =", title))
    return(NULL)
  }

  log_message("Found matching template.")
  return(filtered_result)
}

#' @title Build Autocode URL
#' @description Builds a URL to access a template in Autocode.
#' @param result List. The template result from the API.
#' @return A character string containing the URL.
#' @examples
#' url <- build_autocode_url(template_result)
build_autocode_url <- function(result) {
  if (is.null(result)) return(NULL)

  return(sprintf("https://autocode.jnj.com/dps/#/c-%s/s-%s/re-%s/tlgg-%s/t-%s",
                result$compound_id, result$study_id, result$re_id,
                result$tlggroup_id, result$id))
}

#' @title Get Template Details
#' @description Gets all details about a template.
#' @param result List. The template result from the API.
#' @param log_message Function. A function to log messages.
#' @param verbose Logical. Whether to print detailed information.
#' @return A list containing all the template details.
#' @examples
#' details <- get_template_details(template_result, logger, TRUE)
get_template_details <- function(result, log_message, verbose = TRUE) {
  if (is.null(result)) return(NULL)

  if (verbose) {
    log_message("\nTemplate details:")
    for (field_name in names(result)) {
      if (!is.null(result[[field_name]])) {
        if (is.list(result[[field_name]])) {
          log_message(paste(field_name, ": [complex object]"))

          # Check if this is the 'others' field and if it contains 'refMock'
          if (field_name == "others" && !is.null(result[[field_name]]$refMock)) {
            log_message(paste("  refMock:", result[[field_name]]$refMock))
          }
        } else {
          log_message(paste(field_name, ":", result[[field_name]]))
        }
      }
    }
  }

  return(result)
}

#' @title Get Autocode URL for TLG Template
#' @description Searches for a TLG template in Autocode, finds its refMock if available,
#'              and returns a direct URL to access the template or its refMock.
#' @param tlg_id Character. The identifier for the TLG template (e.g., "TSFAE01a").
#' @param compound Character. The compound to filter results by (default: "standards").
#' @param verbose Logical. Whether to print detailed information during the process (default: TRUE).
#' @return A character string containing the direct URL to access the template in Autocode.
#'         Returns NULL if no matching template is found.
#' @examples
#' # Get URL for TSFAE01a template
#' url <- get_autocode_url("TSFAE01a")
#'
#' # Get URL without verbose output
#' url <- get_autocode_url("TSFAE01a", verbose = FALSE)
get_autocode_url <- function(tlg_id, compound="standards", re="jjcs - core", verbose = TRUE) {
  # Create logger function
  log_message <- create_logger(verbose)

  # Get authorization token
  token <- get_autocode_token()

  # Search for the TLG template
  search_results <- search_template(tlg_id, token, log_message)

  # Filter results
  filtered_result <- filter_results(search_results, tlg_id, compound, re, log_message)

  if (is.null(filtered_result)) {
    return(NULL)
  }

  # Extract the refMock value if it exists
  refMock <- NULL
  if (!is.null(filtered_result$others) && !is.null(filtered_result$others$refMock)) {
    refMock <- filtered_result$others$refMock
    log_message(paste("Found refMock:", refMock))

    # Search for the refMock
    log_message("Searching for refMock...")
    refMock_results <- search_template(refMock, token, log_message)

    # Filter refMock results
    refMock_filtered_result <- NULL
    if (!is.null(refMock_results)) {
      for (i in seq_along(refMock_results)) {
        result <- refMock_results[[i]]
        if (!is.null(result$title) &&
            is.character(result$title) &&
            is.character(refMock) &&
            result$title == refMock) {
          refMock_filtered_result <- result
          break
        }
      }
    }

    if (!is.null(refMock_filtered_result)) {
      # Build the URL for the refMock
      autocode_url <- build_autocode_url(refMock_filtered_result)

      log_message("Found matching refMock result!")

      if (verbose) {
        log_message("\nDirect URL to access the refMock template in Autocode:")
        log_message(autocode_url)
      }

      return(autocode_url)
    } else {
      log_message(paste("No results found matching the refMock:", refMock))
      # Fall back to original result
    }
  } else {
    log_message("No refMock found in the result. Using original template.")
  }

  # Build the URL for the original result
  autocode_url <- build_autocode_url(filtered_result)

  if (verbose) {
    # Print details about the original result
    get_template_details(filtered_result, log_message, verbose)

    log_message("\nDirect URL to access the original template in Autocode:")
    log_message(autocode_url)
  }

  return(autocode_url)
}

#' @title Get Template Information
#' @description Gets all information about a template based on its identifier.
#' @param tlg_id Character. The identifier for the TLG template (e.g., "TSFAE01a").
#' @param compound Character. The compound to filter results by (default: "standards").
#' @param include_refmock Logical. Whether to include refMock information if available (default: TRUE).
#' @param verbose Logical. Whether to print detailed information during the process (default: TRUE).
#' @return A list containing all the template information, including output title, compound, study, and re Tags.
#'         Returns NULL if no matching template is found.
#' @examples
#' # Get information for TSFAE01a template
#' info <- get_template_info("TSFAE01a")
#'
#' # Get information without verbose output
#' info <- get_template_info("TSFAE01a", verbose = FALSE)
#'
#' # Get information without including refMock
#' info <- get_template_info("TSFAE01a", include_refmock = FALSE)
get_template_info <- function(tlg_id, compound="standards", re="jjcs - core", include_refmock = TRUE, verbose = TRUE) {
  # Create logger function
  log_message <- create_logger(verbose)

  # Get authorization token
  token <- get_autocode_token()

  # Search for the TLG template
  search_results <- search_template(tlg_id, token, log_message)

  # Filter results
  filtered_result <- filter_results(search_results, tlg_id, compound, re, log_message)

  if (is.null(filtered_result)) {
    return(NULL)
  }

  # Create a result list with the template information
  result <- list(
    title = filtered_result$title,
    compound = filtered_result$compound,
    study = filtered_result$study,
    re = filtered_result$re,
    url = build_autocode_url(filtered_result),
    details = filtered_result
  )

  # Extract the refMock value if it exists and include_refmock is TRUE
  if (include_refmock && !is.null(filtered_result$others) && !is.null(filtered_result$others$refMock)) {
    refMock <- filtered_result$others$refMock
    log_message(paste("Found refMock:", refMock))

    # Search for the refMock
    log_message("Searching for refMock...")
    refMock_results <- search_template(refMock, token, log_message)

    # Filter refMock results
    refMock_filtered_result <- NULL
    if (!is.null(refMock_results)) {
      for (i in seq_along(refMock_results)) {
        result_item <- refMock_results[[i]]
        if (!is.null(result_item$title) &&
            is.character(result_item$title) &&
            is.character(refMock) &&
            result_item$title == refMock) {
          refMock_filtered_result <- result_item
          break
        }
      }
    }

    if (!is.null(refMock_filtered_result)) {
      log_message("Found matching refMock result!")

      # Add refMock information to the result
      result$refMock <- list(
        title = refMock_filtered_result$title,
        compound = refMock_filtered_result$compound,
        study = refMock_filtered_result$study,
        re = refMock_filtered_result$re,
        url = build_autocode_url(refMock_filtered_result),
        details = refMock_filtered_result
      )
    } else {
      log_message(paste("No results found matching the refMock:", refMock))
    }
  }

  if (verbose) {
    # Print details about the result
    log_message("\nTemplate information:")
    log_message(paste("Title:", result$title))
    log_message(paste("Compound:", result$compound))
    log_message(paste("Study:", result$study))
    log_message(paste("RE:", result$re))
    log_message(paste("URL:", result$url))

    if (!is.null(result$refMock)) {
      log_message("\nRefMock information:")
      log_message(paste("Title:", result$refMock$title))
      log_message(paste("Compound:", result$refMock$compound))
      log_message(paste("Study:", result$refMock$study))
      log_message(paste("RE:", result$refMock$re))
      log_message(paste("URL:", result$refMock$url))
    }
  }

  return(result)
}



# token <- get_autocode_token()
# log_message <- create_logger(TRUE)
# verbose=FALSE
#
# tlg_id <- "TSFLAB01a"
# compound = "standards"
# re = "jjcs - core"
# # Search for the TLG template
# search_results <- search_template(tlg_id, token, log_message)
#
#   # Filter results
# filtered_result <- filter_results(search_results, tlg_id, "standards", "jjcs - core", log_message)
# info <- get_template_info("tsfae01a", "standards", "jjcs - core", include_refmock = FALSE)
# info$details[[1]]$tags[[1]]