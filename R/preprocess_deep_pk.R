# ==========================================================
# preprocess_deep_pk.R
# Converts Deep-PK result CSV into long-format standardized data
# ==========================================================
preprocess_deep_pk <- function(df) {
  message("Preprocessing Deep-PK result...")
  
  # Deep-PK doesn't have a molecule ID, so we create one from SMILES
  if (!"smiles" %in% names(df)) {
    stop("Required column 'smiles' not found in Deep-PK data.")
  }
  
  df <- df %>%
    # Create a unique Molecule_ID from the SMILES string for tracking
    dplyr::mutate(Molecule_ID = sapply(smiles, digest::digest, algo = "xxhash64"))
  
  df_long <- df %>%
    tidyr::pivot_longer(
      cols = -c(Molecule_ID, smiles),
      names_to = "Property",
      values_to = "Value"
    ) %>% 
    dplyr::mutate(Value = purrr::map_chr(Value, ~paste(.x, collapse = "; "))) %>%
    dplyr::mutate(
      Source = "DeepPK"
    ) %>%
    select(Molecule_ID, Property, Value, Source)
  
  message("Parsed Deep-PK data: ", nrow(df_long), " endpoints")
  return(df_long)
}