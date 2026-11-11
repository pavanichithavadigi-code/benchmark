# Task 24: Benchmark alternative implementations
# Synthetic data only

# ---- Setup ----
# Install missing packages
required_packages <- c("bench", "data.table", "dplyr")

installed_packages <- rownames(installed.packages())

missing_packages <- setdiff(
  required_packages,
  installed_packages
)

if (length(missing_packages) > 0) {
  message(
    "Installing missing packages: ",
    paste(missing_packages, collapse = ", ")
  )
  
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org"
  )
}

# Load packages quietly
suppressPackageStartupMessages({
  library(bench)
  library(data.table)
  library(dplyr)
})

# Show versions
cat("\nR version:\n")
print(R.version.string)

cat("\nPackage versions:\n")
print(packageVersion("bench"))
print(packageVersion("data.table"))
print(packageVersion("dplyr"))

# Create folders
dir.create("data", showWarnings = FALSE, recursive = TRUE)
dir.create("results", showWarnings = FALSE, recursive = TRUE)
dir.create("documentation", showWarnings = FALSE, recursive = TRUE)

cat("\nCurrent working directory:\n")
print(getwd())

# ---- Timing basics ----
# Time one pause
cat("\n--- Simple timing demonstration ---\n")

start_time <- Sys.time()

Sys.sleep(1)

end_time <- Sys.time()

elapsed_seconds <- as.numeric(
  difftime(end_time, start_time, units = "secs")
)

cat("Elapsed time in seconds:", elapsed_seconds, "\n")

# Small bench example
small_benchmark <- bench::mark(
  method_one = sum(1:1000),
  method_two = sum(seq_len(1000)),
  iterations = 5,
  check = FALSE
)

print(small_benchmark)

# ---- Synthetic data ----
# Same data every run
set.seed(2401)

number_of_subjects <- 10000
records_per_subject <- 10
number_of_lab_records <- number_of_subjects * records_per_subject

# ADSL: one row per subject
adsl <- data.frame(
  USUBJID = sprintf(
    "SUBJ%05d",
    seq_len(number_of_subjects)
  ),
  AGE = sample(
    18:80,
    number_of_subjects,
    replace = TRUE
  ),
  SEX = sample(
    c("M", "F"),
    number_of_subjects,
    replace = TRUE
  ),
  TRT01A = sample(
    c("Placebo", "Drug A", "Drug B"),
    number_of_subjects,
    replace = TRUE
  ),
  stringsAsFactors = FALSE
)

cat("\n--- ADSL preview ---\n")
print(head(adsl))

cat("\nADSL dimensions (rows, columns):\n")
print(dim(adsl))

cat("\nADSL structure:\n")
str(adsl)

# Check one row per subject
stopifnot(
  nrow(adsl) == number_of_subjects,
  !anyDuplicated(adsl$USUBJID)
)

# ADLB: 10 lab records per subject
adlb <- data.frame(
  USUBJID = rep(
    adsl$USUBJID,
    each = records_per_subject
  ),
  PARAMCD = sample(
    c("ALT", "AST", "HB", "WBC"),
    number_of_lab_records,
    replace = TRUE
  ),
  AVAL = round(
    rnorm(
      number_of_lab_records,
      mean = 50,
      sd = 12
    ),
    2
  ),
  stringsAsFactors = FALSE
)

cat("\n--- ADLB preview ---\n")
print(head(adlb))

cat("\nADLB dimensions (rows, columns):\n")
print(dim(adlb))

cat("\nADLB structure:\n")
str(adlb)

stopifnot(
  nrow(adlb) == number_of_lab_records
)

# Save the input data
write.csv(
  adsl,
  file = "data/synthetic_adsl.csv",
  row.names = FALSE
)

write.csv(
  adlb,
  file = "data/synthetic_adlb.csv",
  row.names = FALSE
)

cat("\nSynthetic datasets saved in the data folder.\n")

# ---- Filtering ----
# Base R
filter_base_r <- function(data) {
  
  result <- data[
    data$PARAMCD == "ALT" & data$AVAL >= 60,
    ,
    drop = FALSE
  ]
  
  return(result)
}

# Copy to data.table, original unchanged
adlb_dt <- data.table::copy(adlb)
data.table::setDT(adlb_dt)

# data.table
filter_data_table <- function(data) {
  
  result <- data[
    PARAMCD == "ALT" & AVAL >= 60
  ]
  
  return(result)
}

# Run both once
filter_base_output <- filter_base_r(adlb)
filter_dt_output <- filter_data_table(adlb_dt)

cat("\n--- Filtering output row counts ---\n")
cat("Base R rows:", nrow(filter_base_output), "\n")
cat("data.table rows:", nrow(filter_dt_output), "\n")

# Sort before comparing
filter_base_check <- filter_base_output[
  order(filter_base_output$USUBJID,
        filter_base_output$PARAMCD,
        filter_base_output$AVAL),
]

filter_dt_check <- as.data.frame(filter_dt_output)

filter_dt_check <- filter_dt_check[
  order(filter_dt_check$USUBJID,
        filter_dt_check$PARAMCD,
        filter_dt_check$AVAL),
]

# Check outputs match
filter_equivalent <- isTRUE(
  all.equal(
    filter_base_check,
    filter_dt_check,
    check.attributes = FALSE
  )
)

cat("Filtering outputs equivalent:", filter_equivalent, "\n")

stopifnot(filter_equivalent)

# ---- Grouping ----
# Joined data used by all methods
joined_base <- merge(
  adlb,
  adsl,
  by = "USUBJID",
  all.x = TRUE,
  sort = FALSE
)

# Copies for data.table and dplyr
joined_dt <- data.table::copy(joined_base)
data.table::setDT(joined_dt)

joined_df <- as.data.frame(joined_base)

# Base R
group_base_r <- function(data) {
  
  mean_result <- aggregate(
    AVAL ~ TRT01A + PARAMCD,
    data = data,
    FUN = mean
  )
  
  names(mean_result)[3] <- "MEAN_AVAL"
  
  sd_result <- aggregate(
    AVAL ~ TRT01A + PARAMCD,
    data = data,
    FUN = sd
  )
  
  names(sd_result)[3] <- "SD_AVAL"
  
  count_result <- aggregate(
    AVAL ~ TRT01A + PARAMCD,
    data = data,
    FUN = length
  )
  
  names(count_result)[3] <- "N_RECORDS"
  
  # Combine the summaries
  result <- merge(
    mean_result,
    sd_result,
    by = c("TRT01A", "PARAMCD"),
    sort = FALSE
  )
  
  result <- merge(
    result,
    count_result,
    by = c("TRT01A", "PARAMCD"),
    sort = FALSE
  )
  
  return(result)
}

# dplyr
group_dplyr <- function(data) {
  
  result <- data %>%
    dplyr::group_by(TRT01A, PARAMCD) %>%
    dplyr::summarise(
      MEAN_AVAL = mean(AVAL),
      SD_AVAL = sd(AVAL),
      N_RECORDS = dplyr::n(),
      .groups = "drop"
    )
  
  return(as.data.frame(result))
}

# data.table
group_data_table <- function(data) {
  
  result <- data[
    ,
    .(
      MEAN_AVAL = mean(AVAL),
      SD_AVAL = sd(AVAL),
      N_RECORDS = .N
    ),
    by = .(TRT01A, PARAMCD)
  ]
  
  return(as.data.frame(result))
}

# Run all three once
group_base_output <- group_base_r(joined_base)
group_dplyr_output <- group_dplyr(joined_df)
group_dt_output <- group_data_table(joined_dt)

# Same order for comparison
sort_group_output <- function(data) {
  
  data <- as.data.frame(data)
  
  data <- data[
    order(data$TRT01A, data$PARAMCD),
    c(
      "TRT01A",
      "PARAMCD",
      "MEAN_AVAL",
      "SD_AVAL",
      "N_RECORDS"
    ),
    drop = FALSE
  ]
  
  rownames(data) <- NULL
  
  return(data)
}

group_base_check <- sort_group_output(group_base_output)
group_dplyr_check <- sort_group_output(group_dplyr_output)
group_dt_check <- sort_group_output(group_dt_output)

# Compare with base R
group_base_vs_dplyr <- isTRUE(
  all.equal(
    group_base_check,
    group_dplyr_check,
    check.attributes = FALSE,
    tolerance = 1e-10
  )
)

group_base_vs_dt <- isTRUE(
  all.equal(
    group_base_check,
    group_dt_check,
    check.attributes = FALSE,
    tolerance = 1e-10
  )
)

cat("\n--- Grouping output validation ---\n")
cat("Base R vs dplyr equivalent:", group_base_vs_dplyr, "\n")
cat("Base R vs data.table equivalent:", group_base_vs_dt, "\n")

stopifnot(
  group_base_vs_dplyr,
  group_base_vs_dt
)

# ---- Joining ----
# Base R merge()
join_base_r <- function(adlb_data, adsl_data) {
  
  result <- merge(
    adlb_data,
    adsl_data,
    by = "USUBJID",
    all.x = TRUE,
    sort = FALSE
  )
  
  return(result)
}

# Keyed ADSL, prepared before timing
adsl_keyed <- data.table::copy(adsl)
data.table::setDT(adsl_keyed)
data.table::setkey(adsl_keyed, USUBJID)

# ADLB as data.table
adlb_for_join <- data.table::copy(adlb)
data.table::setDT(adlb_for_join)

# data.table join
join_data_table <- function(adsl_data, adlb_data) {
  
  result <- adsl_data[
    adlb_data,
    on = "USUBJID"
  ]
  
  return(result)
}

# Run both once
join_base_output <- join_base_r(adlb, adsl)

join_dt_output <- join_data_table(
  adsl_keyed,
  adlb_for_join
)

# Same columns for comparison
join_columns <- c(
  "USUBJID",
  "PARAMCD",
  "AVAL",
  "AGE",
  "SEX",
  "TRT01A"
)

join_base_check <- as.data.frame(join_base_output)
join_dt_check <- as.data.frame(join_dt_output)

join_base_check <- join_base_check[
  ,
  join_columns,
  drop = FALSE
]

join_dt_check <- join_dt_check[
  ,
  join_columns,
  drop = FALSE
]

join_base_check <- join_base_check[
  order(join_base_check$USUBJID,
        join_base_check$PARAMCD,
        join_base_check$AVAL),
]

join_dt_check <- join_dt_check[
  order(join_dt_check$USUBJID,
        join_dt_check$PARAMCD,
        join_dt_check$AVAL),
]

rownames(join_base_check) <- NULL
rownames(join_dt_check) <- NULL

# Check outputs match
join_equivalent <- isTRUE(
  all.equal(
    join_base_check,
    join_dt_check,
    check.attributes = FALSE
  )
)

cat("\n--- Join output validation ---\n")
cat("Base R merge vs data.table join equivalent:",
    join_equivalent, "\n")

cat("Base R joined rows:", nrow(join_base_output), "\n")
cat("data.table joined rows:", nrow(join_dt_output), "\n")

stopifnot(join_equivalent)

# Save example outputs
write.csv(
  group_base_check,
  "results/grouping_summary.csv",
  row.names = FALSE
)

write.csv(
  join_base_check,
  "results/joined_clinical_data.csv",
  row.names = FALSE
)

# ---- Benchmarks ----
# Turn bench output into a table
make_benchmark_report <- function(
    benchmark_object,
    operation_name,
    method_names
) {
  
  if (nrow(benchmark_object) != length(method_names)) {
    stop("The number of method names does not match benchmark rows.")
  }
  
  report <- data.frame(
    Operation = operation_name,
    Method = method_names,
    Median_Time_Seconds = as.numeric(
      benchmark_object$median,
      units = "secs"
    ),
    Memory_Allocated_MB = as.numeric(
      benchmark_object$mem_alloc
    ) / (1024^2),
    Iterations_Per_Second = as.numeric(
      benchmark_object$`itr/sec`
    ),
    stringsAsFactors = FALSE
  )
  
  return(report)
}

cat("\n--- Benchmark: Filtering ---\n")

# Filtering
filter_benchmark <- bench::mark(
  base_R = filter_base_r(adlb),
  data_table = filter_data_table(adlb_dt),
  iterations = 5,
  check = FALSE
)

print(filter_benchmark)

filter_report <- make_benchmark_report(
  benchmark_object = filter_benchmark,
  operation_name = "Filtering",
  method_names = c("Base R", "data.table")
)

write.csv(
  filter_report,
  "results/filtering_results.csv",
  row.names = FALSE
)

cat("\n--- Benchmark: Grouping ---\n")

# Grouping
group_benchmark <- bench::mark(
  base_R = group_base_r(joined_base),
  dplyr = group_dplyr(joined_df),
  data_table = group_data_table(joined_dt),
  iterations = 5,
  check = FALSE
)

print(group_benchmark)

group_report <- make_benchmark_report(
  benchmark_object = group_benchmark,
  operation_name = "Grouping",
  method_names = c("Base R", "dplyr", "data.table")
)

write.csv(
  group_report,
  "results/grouping_results.csv",
  row.names = FALSE
)

cat("\n--- Benchmark: Joining ---\n")

# Joining
join_benchmark <- bench::mark(
  base_R_merge = join_base_r(adlb, adsl),
  data_table_join = join_data_table(
    adsl_keyed,
    adlb_for_join
  ),
  iterations = 5,
  check = FALSE
)

print(join_benchmark)

join_report <- make_benchmark_report(
  benchmark_object = join_benchmark,
  operation_name = "Joining",
  method_names = c("Base R merge", "data.table join")
)

write.csv(
  join_report,
  "results/join_results.csv",
  row.names = FALSE
)

# Combine all results
all_benchmark_results <- rbind(
  filter_report,
  group_report,
  join_report
)

write.csv(
  all_benchmark_results,
  "results/final_benchmark_summary.csv",
  row.names = FALSE
)

cat("\n--- Combined benchmark results ---\n")
print(all_benchmark_results)

# ---- Results ----
# Fastest method per operation
fastest_method <- do.call(
  rbind,
  lapply(
    split(all_benchmark_results, all_benchmark_results$Operation),
    function(operation_data) {
      
      best_row <- operation_data[
        which.min(operation_data$Median_Time_Seconds),
        ,
        drop = FALSE
      ]
      
      return(best_row)
    }
  )
)

rownames(fastest_method) <- NULL

cat("\n--- Fastest method by operation ---\n")
print(
  fastest_method[
    ,
    c(
      "Operation",
      "Method",
      "Median_Time_Seconds",
      "Memory_Allocated_MB"
    )
  ]
)

write.csv(
  fastest_method,
  "results/fastest_method_by_operation.csv",
  row.names = FALSE
)

# Lowest memory per operation
lowest_memory_method <- do.call(
  rbind,
  lapply(
    split(all_benchmark_results, all_benchmark_results$Operation),
    function(operation_data) {
      
      best_row <- operation_data[
        which.min(operation_data$Memory_Allocated_MB),
        ,
        drop = FALSE
      ]
      
      return(best_row)
    }
  )
)

rownames(lowest_memory_method) <- NULL

cat("\n--- Lowest memory allocation by operation ---\n")
print(
  lowest_memory_method[
    ,
    c(
      "Operation",
      "Method",
      "Memory_Allocated_MB",
      "Median_Time_Seconds"
    )
  ]
)

write.csv(
  lowest_memory_method,
  "results/lowest_memory_method_by_operation.csv",
  row.names = FALSE
)

# Validation summary
validation_summary <- data.frame(
  Comparison = c(
    "Filtering: Base R vs data.table",
    "Grouping: Base R vs dplyr",
    "Grouping: Base R vs data.table",
    "Joining: Base R merge vs data.table"
  ),
  Outputs_Equivalent = c(
    filter_equivalent,
    group_base_vs_dplyr,
    group_base_vs_dt,
    join_equivalent
  ),
  stringsAsFactors = FALSE
)

cat("\n--- Output validation summary ---\n")
print(validation_summary)

write.csv(
  validation_summary,
  "results/output_validation_summary.csv",
  row.names = FALSE
)

# Stop if any check failed
stopifnot(
  all(validation_summary$Outputs_Equivalent)
)

# Template for team review
maintainability_review <- data.frame(
  Method = c(
    "Base R",
    "dplyr",
    "data.table"
  ),
  Easy_To_Read_For_This_Team = c(
    "Review with the team",
    "Review with the team",
    "Review with the team"
  ),
  Existing_Team_Standards = c(
    "Check team standards",
    "Check team standards",
    "Check team standards"
  ),
  Reviewer_Comments = c(
    "Add reviewer comments",
    "Add reviewer comments",
    "Add reviewer comments"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  maintainability_review,
  "results/maintainability_review_template.csv",
  row.names = FALSE
)

# Text report
report_lines <- c(
  "TASK 24 - BENCHMARK ALTERNATIVE IMPLEMENTATIONS",
  "================================================",
  "",
  paste("Date:", as.character(Sys.Date())),
  paste("R version:", R.version.string),
  "",
  "DATA USED",
  "---------",
  paste("ADSL subjects:", nrow(adsl)),
  paste("ADLB laboratory records:", nrow(adlb)),
  "",
  "BENCHMARK RESULTS",
  "-----------------",
  capture.output(print(all_benchmark_results)),
  "",
  "FASTEST METHOD BY OPERATION",
  "---------------------------",
  capture.output(print(fastest_method)),
  "",
  "LOWEST MEMORY ALLOCATION BY OPERATION",
  "-------------------------------------",
  capture.output(print(lowest_memory_method)),
  "",
  "OUTPUT VALIDATION",
  "-----------------",
  capture.output(print(validation_summary)),
  "",
  "MAINTAINABILITY",
  "---------------",
  "Complete results/maintainability_review_template.csv after team review.",
  "",
  "IMPORTANT NOTES",
  "---------------",
  "1. These datasets are synthetic training data.",
  "2. Results depend on the computer and software environment.",
  "3. The data.table join benchmark uses an ADSL key prepared before timing.",
  "4. Output equivalence was checked separately from benchmarking.",
  "5. The fastest method is not automatically the best maintainable choice.",
  "6. Review clinical programming standards before using any approach in production."
)

writeLines(
  report_lines,
  "documentation/task24_benchmark_report.txt"
)

cat("\nBenchmark report saved to documentation/task24_benchmark_report.txt\n")

# ---- Final summary ----
cat("\n")
cat("============================================================\n")
cat("TASK 24 EXECUTION COMPLETED\n")
cat("============================================================\n")

cat("\nInput datasets:\n")
cat("- data/synthetic_adsl.csv\n")
cat("- data/synthetic_adlb.csv\n")

cat("\nBenchmark result files:\n")
cat("- results/filtering_results.csv\n")
cat("- results/grouping_results.csv\n")
cat("- results/join_results.csv\n")
cat("- results/final_benchmark_summary.csv\n")
cat("- results/fastest_method_by_operation.csv\n")
cat("- results/lowest_memory_method_by_operation.csv\n")

cat("\nValidation file:\n")
cat("- results/output_validation_summary.csv\n")

cat("\nDocumentation:\n")
cat("- documentation/task24_benchmark_report.txt\n")
cat("- results/maintainability_review_template.csv\n")

cat("\nAll output-equivalence checks passed:",
    all(validation_summary$Outputs_Equivalent), "\n")

cat("\nNEXT STEPS FOR THE PROGRAMMER:\n")
cat("1. Open the CSV files in the results folder.\n")
cat("2. Review the actual timings and memory allocation.\n")
cat("3. Complete the maintainability review with the team lead.\n")
cat("4. Write a short recommendation based on evidence.\n")
cat("5. Do not claim that one method is always faster.\n")
