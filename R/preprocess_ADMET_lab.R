# ==========================================================
# preprocess_ADMET_lab.R
# Converts ADMETlab 3.0 CSV into long-format standardized data
# ==========================================================
preprocess_ADMET_lab <- function(df) {
  message("Preprocessing ADMETlab 3.0 result...")

  # Determine the Molecule ID and the columns to keep fixed.
  # Use 'taskid' if it exists, otherwise fall back to 'smiles'.
  if ("taskid" %in% names(df)) {
    df <- df %>% dplyr::mutate(Molecule_ID = as.character(taskid))
    id_cols_to_exclude <- c("smiles", "taskid", "Molecule_ID")
  } else {
    df <- df %>% dplyr::mutate(Molecule_ID = as.character(smiles))
    id_cols_to_exclude <- c("smiles", "Molecule_ID")
  }
  
  # Ensure columns to exclude actually exist, just in case
  id_cols_to_exclude <- intersect(id_cols_to_exclude, names(df))

  df_long <- df %>%
    tidyr::pivot_longer(
      cols = -dplyr::all_of(id_cols_to_exclude),
      names_to = "Property",
      values_to = "Value"
    ) %>%
    dplyr::mutate(Value = purrr::map_chr(Value, ~paste(.x, collapse = "; "))) %>%
    mutate(
      Property = trimws(Property),
      Source = "ADMETlab"
    ) %>%
    select(Molecule_ID, Property, Value, Source)
  
  message("Parsed ADMETlab data: ", nrow(df_long), " endpoints")
  return(df_long)
}