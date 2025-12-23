#!/usr/bin/env Rscript

# =========================================================
# run_gai.R
# 
# Command-line script to run GAI computation on a file.
# 
# Usage:
# Rscript scripts/run_gai.R <path_to_input_file> [path_to_output_dir]
# =========================================================

# --- Load libraries ---
# This script assumes you have the necessary packages installed.
# You can install them with: install.packages(c("dplyr", "readr", "tidyr", "purrr", "ggplot2", "jsonlite", "digest", "argparse"))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(digest))
suppressPackageStartupMessages(library(argparse))

# --- Source helper scripts ---
# Check if we are running from project root or scripts dir
if (file.exists("R/utils.R")) {
  source("R/config_endpoints.R")
  source("R/utils.R")
  source("R/load_ADMET_data.R")
  source("R/compute_GAI.R")
} else if (file.exists("../R/utils.R")) {
  source("../R/config_endpoints.R")
  source("../R/utils.R")
  source("../R/load_ADMET_data.R")
  source("../R/compute_GAI.R")
} else {
  stop("Could not find R/ helper scripts. Please run from project root.")
}

# --- Argument Parser ---
parser <- ArgumentParser(description = "Compute the Global ADMET Index (GAI) for a given molecule data file.")
parser$add_argument("input_file", help = "Path to the input CSV or TSV file.")
parser$add_argument("-o", "--output", default = "gai_output", help = "Path to the output directory [default: %(default)s]")
parser$add_argument("-s", "--source", default = "Auto Detect",
                    choices = c("Auto Detect", "ADMETlab", "SwissADME", "Deep-PK", "pkCSM", "Manual"),
                    help = "Specify the data source type [default: %(default)s]")

args <- parser$parse_args()

# --- Display Banner ---
cat("  GGGGG     A     IIIII  \n")
cat(" G     G   A A      I    \n")
cat(" G        A   A     I    \n")
cat(" G  GGG  AAAAA     I    \n")
cat(" G     G A   A     I    \n")
cat(" G     G A   A     I    \n")
cat("  GGGGG  A   A   IIIII  \n")
cat("\n    Global ADMET Index Analyzer\n")
cat("    Author: MD. Arshad | (c) 2025 SulkySubject\n")
cat("----------------------------------------------------\n\n")

# --- Create output directory ---
if (!dir.exists(args$output)) {
  dir.create(args$output, recursive = TRUE)
}

# --- Main execution block ---
message("Starting GAI computation for: ", basename(args$input_file))

tryCatch({
  # --- Load and parse data ---
  message("Loading and parsing data...")
  raw_df <- read_any_delim(args$input_file)
  
  # The `load_ADMET_data` function will auto-detect the source type
  parsed_results <- load_ADMET_data(raw_df, source_type = args$source)
  message("Data parsed successfully. Found ", parsed_results$n_molecules, " molecule(s).")
  
  # --- Compute GAI ---
  message("Computing GAI...")
  gai_results <- compute_GAI(parsed_results$data_long)
  message("GAI computation complete.")
  
  # --- Handle results (single vs. batch) ---
  if ("summary" %in% names(gai_results)) {
    # Batch result
    message("\nBatch results summary:")
    print(gai_results$summary)
    write.csv(gai_results$summary, file.path(args$output, "gai_summary.csv"), row.names = FALSE)
    
    # Save plots
    message("\nSaving plots...")
    ggsave(file.path(args$output, "batch_boxplot.png"), gai_results$plots$batch_boxplot, width = 6, height = 8)
    
    # Save detailed results and plots for each molecule
    for (molecule_id in names(gai_results$detailed_results)) {
      res <- gai_results$detailed_results[[molecule_id]]
      # Sanitize molecule_id for use as a directory name
      safe_mol_id <- gsub("[^a-zA-Z0-9_.-]", "_", molecule_id)
      mol_dir <- file.path(args$output, safe_mol_id)
      if (!dir.exists(mol_dir)) dir.create(mol_dir)
      
      write.csv(res$endpoint_table, file.path(mol_dir, "endpoint_table.csv"), row.names = FALSE)
      if (!is.null(res$plots$overall)) ggsave(file.path(mol_dir, "plot_overall_gauge.png"), res$plots$overall, width = 4, height = 4)
      if (!is.null(res$plots$subscores_bar)) ggsave(file.path(mol_dir, "plot_subscores_bar.png"), res$plots$subscores_bar, width = 6, height = 5)
      if (!is.null(res$plots$radar)) ggsave(file.path(mol_dir, "plot_radar.png"), res$plots$radar, width = 6, height = 6)
      if (!is.null(res$plots$toxicity_bar)) ggsave(file.path(mol_dir, "plot_toxicity_bar.png"), res$plots$toxicity_bar, width = 6, height = 5)
      if (!is.null(res$plots$desirability_heatmap)) ggsave(file.path(mol_dir, "plot_desirability_heatmap.png"), res$plots$desirability_heatmap, width = 10, height = 8)
      if (!is.null(res$plots$waterfall)) ggsave(file.path(mol_dir, "plot_waterfall.png"), res$plots$waterfall, width = 8, height = 6)
    }
    
  } else {
    # Single molecule result
    message("\nSingle molecule result:")
    message("GAI Score: ", round(gai_results$GAI, 2))
    print(gai_results$endpoint_table)
    write.csv(gai_results$endpoint_table, file.path(args$output, "endpoint_table.csv"), row.names = FALSE)
    
    # Save plots
    message("\nSaving plots...")
    if (!is.null(gai_results$plots$overall)) ggsave(file.path(args$output, "plot_overall_gauge.png"), gai_results$plots$overall, width = 4, height = 4)
    if (!is.null(gai_results$plots$subscores_bar)) ggsave(file.path(args$output, "plot_subscores_bar.png"), gai_results$plots$subscores_bar, width = 6, height = 5)
    if (!is.null(gai_results$plots$radar)) ggsave(file.path(args$output, "plot_radar.png"), gai_results$plots$radar, width = 6, height = 6)
    if (!is.null(gai_results$plots$toxicity_bar)) ggsave(file.path(args$output, "plot_toxicity_bar.png"), gai_results$plots$toxicity_bar, width = 6, height = 5)
    if (!is.null(gai_results$plots$desirability_heatmap)) ggsave(file.path(args$output, "plot_desirability_heatmap.png"), gai_results$plots$desirability_heatmap, width = 10, height = 8)
    if (!is.null(gai_results$plots$waterfall)) ggsave(file.path(args$output, "plot_waterfall.png"), gai_results$plots$waterfall, width = 8, height = 6)
  }
  
  message("\nSuccess! Results and plots saved to: ", normalizePath(args$output))
  
}, error = function(e) {
  message("\nAn error occurred:")
  print(e)
})
