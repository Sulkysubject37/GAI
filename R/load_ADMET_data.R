# ==========================================================
# load_ADMET_data.R
# Detects and preprocesses ADMET results from various sources
# Supported: ADMETlab, SwissADME, DeepPK, Manual
# ==========================================================
source("R/preprocess_ADMET_lab.R")
source("R/preprocess_deep_pk.R")
source("R/preprocess_SWISS_ADME.R")
source("R/preprocess_pkcsm.R")

load_ADMET_data <- function(df, source_type = "Auto Detect") {
  # --- Step 1: Validate Input & Clean Names ---
  if (!is.data.frame(df)) stop("Input is not a valid data frame.")
  names(df) <- gsub(" ", ".", tolower(names(df)))
  message("Received data with ", nrow(df), " rows and ", ncol(df), " columns. Names cleaned.")
  
  # --- Step 2: Extract cleaned column names ---
  cols <- colnames(df)
  
  # --- Step 3: Auto-detect source if needed ---
  if (source_type == "Auto Detect") {
    if (any(grepl("fsp3", cols)) && any(grepl("mce-18", cols))) {
      source_type <- "ADMETlab"
    } else if (any(grepl("xlogp3", cols)) && any(grepl("silicos", cols))) {
      source_type <- "SwissADME"
    } else if (any(grepl("caco2.permeability", cols))) {
      source_type <- "pkCSM"
    } else if (any(grepl("absorption", cols)) && any(grepl("toxicity", cols))) {
      source_type <- "DeepPK"
    } else {
      source_type <- "Manual"
    }
  }
  
  message("Detected source type: ", source_type)
  
  # --- Step 4: Route to correct preprocessor ---
  df_long <- switch(
    source_type,
    "ADMETlab" = preprocess_ADMET_lab(df),
    "SwissADME" = preprocess_swiss_adme(df),
    "DeepPK" = preprocess_deep_pk(df),
    "pkCSM" = preprocess_pkcsm(df),
    "Manual" = {
      if (!all(c("Property", "Value") %in% names(df))) {
        stop("Manual mode requires columns: 'Property' and 'Value'")
      }
      if (!"Molecule_ID" %in% names(df)) {
        message("No Molecule_ID found for manual mode. Assuming a single molecule.")
        df$Molecule_ID <- "Molecule_1"
      }
      df %>% mutate(Source = "Manual", Group = "Other")
    },
    stop("Unsupported or unknown data source type: ", source_type)
  )
  
  # --- Step 5: Validate output ---
  required_cols <- c("Property", "Value", "Source", "Molecule_ID")
  missing <- setdiff(required_cols, names(df_long))
  if (length(missing) > 0) {
    stop("Missing required columns in parsed data: ", paste(missing, collapse = ", "))
  }
  
  # --- Step 6: Apply Standardized Mapping (Unflawing) ---
  # We overwrite or create the 'Group' and 'canonical' columns using the central config.
  # This ensures the preview table in Shiny matches the GAI logic exactly.
  if (exists("map_property")) {
    message("Applying standardized property mapping...")
    df_long <- df_long %>%
      dplyr::mutate(meta = purrr::map(Property, map_property)) %>%
      tidyr::unnest(meta) %>%
      dplyr::mutate(Group = group) %>% # Use the standardized 'group' from config
      dplyr::select(Molecule_ID, Property, canonical, Value, Source, Group)
  } else {
    warning("map_property function not found. Using raw groups from preprocessor if available.")
  }

  n_molecules <- length(unique(df_long$Molecule_ID))
  message("Parsed successfully into ", nrow(df_long), " standardized rows for ", n_molecules, " molecule(s).")
  
  return(list(data_long = df_long, n_molecules = n_molecules))
}