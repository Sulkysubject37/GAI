# ==========================================================
# preprocess_pkcsm.R
# Converts pkCSM CSV into long-format standardized data
# ==========================================================
preprocess_pkcsm <- function(df) {
  message("Preprocessing pkCSM result...")
  
  # Ensure SMILES column exists
  if (!"smiles" %in% colnames(df)) {
    stop("pkCSM data must contain a 'smiles' column.")
  }
  
  df_long <- df %>%
    tidyr::pivot_longer(
      cols = -smiles,
      names_to = "Property",
      values_to = "Value"
    ) %>% 
    mutate(Value = purrr::map_chr(Value, ~paste(.x, collapse = "; "))) %>%
    mutate(
      Molecule_ID = as.character(smiles),
      Property = trimws(Property),
      Source = "pkCSM"
    ) %>%
    # Clean up the data to remove invalid rows and characters
    dplyr::filter(
      !is.na(Molecule_ID),
      Molecule_ID != "",
      !grepl("SMILES", Molecule_ID, ignore.case = TRUE) # Filter out header rows
    ) %>%
    dplyr::mutate(Molecule_ID = stringr::str_remove_all(Molecule_ID, "\\r$")) %>%
    select(Molecule_ID, Property, Value, Source)
  
  message("Parsed pkCSM data: ", nrow(df_long), " endpoints")
  return(df_long)
}
