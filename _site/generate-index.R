# Run file contents within TLG-C project, with working directory set
# to tlg-catalog/book, to update the index
# Set locale to Sys.setlocale("LC_COLLATE", "C") or different if issues with sorting of subsections occur.

print_ref_templates <- function(fpath) {
  title <- sub("title: ", "", readLines(fpath)[2], )
  subtitle <- gsub("^'|'$", "", sub("subtitle: ", "", readLines(fpath)[3], ))
  temp_name <- paste(title, subtitle, sep = " -- ")
  cat(
    paste0(strrep("&nbsp;", 8), "[", temp_name, "]", "(", fpath, ")\n\n"),
    file = "tlg-index.qmd",
    append = TRUE
  )
}

section_header <- function(title) {
  cat(
    paste("", "------------------------------------------------------------------------", "",
      paste0("### ", "**", title, "**"), "",
      sep = "\n"
    ),
    file = "tlg-index.qmd", append = TRUE
  )
}

create_subsection <- function(fpath, title) {
  cat(paste("", paste("####", title), "", "", sep = "\n"), file = "tlg-index.qmd", append = TRUE)
  all_files <- list.files(path = fpath, pattern = "*.qmd", full.names = TRUE)
  invisible(sapply(all_files, print_ref_templates))
}

# Create Index Header

cat(
  paste("---", "title: Index", "toc: true", "toc-depth: 4", "---", "", sep = "\n"),
  file = "tlg-index.qmd"
)

# Tables

section_header("Tables")
create_subsection("./tables/adverse_events", "Adverse Events")
create_subsection("./tables/adverse_events", "Adverse Events for Japan Submission")
create_subsection("./tables/clinical_laboratory_evaluation", "Clinical Laboratory Evaluation")
create_subsection("./tables/demographic", "Demographic and Other Baseline Characteristics")
create_subsection("./tables/disposition_of_subjects", "Disposition of Subjects")
create_subsection("./tables/electrocardiograms", "Electrocardiograms")
create_subsection("./tables/exposure", "Exposure")
create_subsection("./tables/prior_and_concomitant_therapies", "Prior and Concomitant Therapies")
create_subsection("./tables/study_treatment_compliance", "Study Treatment Compliance")
create_subsection("./tables/vital_signs_and_physical_findings", "Vital Signs and Physical Findings")

# Listings

section_header("Listings")
create_subsection("./listings/adverse_events", "Adverse Events")
create_subsection("./listings/clinical_laboratory_evaluation", "Clinical Laboratory Evaluation")
create_subsection("./listings/demographic", "Demographic and Other Baseline Characteristics")
create_subsection("./listings/disposition_of_subjects", "Disposition of Subjects")
create_subsection("./listings/electrocardiograms", "Electrocardiograms")
create_subsection("./listings/exposure", "Exposure")
create_subsection("./listings/prior_and_concomitant_therapies", "Prior and Concomitant Therapies")
create_subsection("./listings/vital_signs_and_physical_findings", "Vital Signs and Physical Findings")

# Graphs No graphs for now
# section_header("Graphs")
# create_subsection("./graphs/efficacy", "Efficacy")
# create_subsection("./graphs/pharmacokinetic", "Pharmacokinetic")
# create_subsection("./graphs/other", "Other")
