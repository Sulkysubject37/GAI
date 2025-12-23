# ==========================================================
# preprocess_swiss_adme.R
# Converts SwissADME CSV into long-format standardized data
# ==========================================================
preprocess_swiss_adme <- function(df) {
  message("Preprocessing SwissADME result...")
  
  # Ensure 'Molecule' column exists and rename for standardization
  if (!"molecule" %in% names(df)) {
    stop("Required column 'molecule' not found in SwissADME data.")
  }
  
  df <- df %>%
    dplyr::rename(Molecule_ID = molecule)
  
  df_long <- df %>%
    tidyr::pivot_longer(
      cols = -c(Molecule_ID, canonical.smiles, formula),
      names_to = "Property",
      values_to = "Value"
    ) %>% 
    dplyr::mutate(Value = purrr::map_chr(Value, ~paste(.x, collapse = "; "))) %>%
    dplyr::mutate(
      Source = "SwissADME"
    ) %>%
    select(Molecule_ID, Property, Value, Source)
  
  message("Parsed SwissADME data: ", nrow(df_long), " endpoints")
  return(df_long)
}