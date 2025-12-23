# =========================================================
# compute_GAI.R
# =========================================================
# Global ADMET Index computation (GAI) - robust version
# - Handles both single molecule (from Shiny) and batch (from script)
# - Rowwise text-to-prob conversion fix
# - Explicit subgroup definitions and endpoints
# - Pre-compute parsed-data visualizations
# - Final compute visualizations (bar + radar-style)
# - Detailed returned object for Shiny integration
# =========================================================

# =========================================================
# Main Wrapper: Handles single or batch processing
# =========================================================
compute_GAI <- function(df_long, theme_base = theme_minimal(base_size = 12), ...) {
  
  # If Molecule_ID is not present, we are in single-file mode from Shiny
  if (!"Molecule_ID" %in% colnames(df_long)) {
    if (is.data.frame(df_long) && nrow(df_long) > 0) {
      df_long$Molecule_ID <- "Molecule_1"
    } else {
      stop("Input data is empty or invalid.")
    }
  }
  
  # Group by Molecule_ID and apply the single-molecule GAI computation
  results_list <- df_long %>%
    dplyr::group_by(Molecule_ID) %>%
    dplyr::do(res = compute_GAI_single(., theme_base = theme_base, ...)) %>%
    dplyr::ungroup()
  
  # If it was a single molecule, return the result directly for Shiny
  if (nrow(results_list) == 1) {
    return(results_list$res[[1]])
  }
  
  # Otherwise, return a batch result summary
  summary_df <- results_list %>%
    dplyr::mutate(
      GAI = sapply(res, function(x) x$GAI),
      Flags = sapply(res, function(x) paste(names(x$red_flags), collapse = ", "))
    ) %>%
    dplyr::select(Molecule_ID, GAI, Flags)
  
  # Generate a boxplot for the batch results
  batch_plot <- ggplot(summary_df, aes(y = GAI)) +
    geom_boxplot(fill = "#2b8cbe", alpha = 0.7) +
    labs(title = "Batch GAI Score Distribution", y = "GAI Score", x = "") +
    theme_base
  
  return(list(
    summary = summary_df,
    detailed_results = setNames(results_list$res, results_list$Molecule_ID),
    plots = list(batch_boxplot = batch_plot)
  ))
}

# =========================================================
# Core GAI computation for a single molecule's data
# =========================================================
compute_GAI_single <- function(df_long,
                               endpoint_weights = NULL,
                               group_weights = NULL,
                               method = c("weighted_mean", "pmean"),
                               p = 0.5,
                               fatal_penalty = 0.5,
                               fatal_thresholds = NULL,
                               context = c("default", "CNS"),
                               theme_base = theme_minimal(base_size = 12),
                               verbose = FALSE) {
  

  
  method <- match.arg(method)
  context <- match.arg(context)
  
  # -----------------------
  # 0. Input validation and normalization
  # -----------------------
  if (!is.data.frame(df_long) || nrow(df_long) == 0) {
    stop("df_long must be a non-empty data.frame.")
  }
  required_cols <- c("Property", "Value")
  if (!all(required_cols %in% colnames(df_long))) {
    stop("df_long must contain at least columns: Property, Value")
  }

  df <- df_long %>%
    dplyr::mutate(
      Property = as.character(Property),
      raw_Value = Value,
      Value_str = as.character(Value)
    ) %>%
    dplyr::mutate(Value_num = suppressWarnings(as.numeric(Value_str)))
  
  # -----------------------
  # 1. Endpoint metadata (Assumes groups are already assigned by load_ADMET_data)
  # -----------------------
  # Check if we already have canonical mapping (from load_ADMET_data)
  has_mapping <- "canonical" %in% names(df)
  
    if (has_mapping) {
  
      # Normalize Group -> group (lowercase)
  
      if ("Group" %in% names(df) && !"group" %in% names(df)) {
  
         df <- df %>% dplyr::rename(group = Group) 
  
      }
  
      
  
      # Retrieve metadata for direction/limits based on canonical name
  
      # We need to join with the master config to get L, U, direction, is_prob_bad, weight_in_group
  
      if (exists("get_endpoint_meta")) {
  
          meta_db <- get_endpoint_meta()
  
          
  
          # Avoid duplicate columns in join
  
          cols_to_join <- c("canonical", "direction", "L", "U", "is_prob_bad", "weight_in_group")
  
          cols_to_join <- setdiff(cols_to_join, names(df)) # Only join what's missing
  
          
  
          if (length(cols_to_join) > 0) {
  
             # We must include 'canonical' for the join key
  
             join_db <- meta_db %>% select(canonical, all_of(cols_to_join))
  
             
  
             # Use distinct to avoid row explosion if meta_db has duplicates (unlikely but safe)
  
             join_db <- distinct(join_db, canonical, .keep_all = TRUE)
  
             
  
             df <- df %>% dplyr::left_join(join_db, by = "canonical")
  
          }
  
      } else {
  
          stop("get_endpoint_meta function missing. Cannot compute GAI.")
  
      }
  
  
  
    } else {
  
      # No pre-existing mapping? Try to map it now.
  
      if (exists("map_property")) {
  
          df <- df %>%
  
              dplyr::mutate(meta = purrr::map(Property, map_property)) %>%
  
              tidyr::unnest(meta)
  
      } else {
  
          warning("map_property not found and data lacks group/canonical info. Results may be inaccurate.")
  
      }
  
    }
  
    
  
    # Ensure weight column exists (default to 1 if missing from map)
  
    if(!"weight_in_group" %in% names(df)) df$weight_in_group <- 1
  
    df$weight_in_group[is.na(df$weight_in_group)] <- 1
  
  
  
    # -----------------------
  
    # 2. Text-to-probability (Enhanced)
  
    # -----------------------
  text_to_prob_one <- function(x) {
    if (is.null(x) || is.na(x)) return(NA_real_)
    s <- tolower(trimws(as.character(x)))
    s2 <- gsub("%", "", s)
    num <- suppressWarnings(as.numeric(s2))
    
    # Check if it's a numeric string
    if (!is.na(num)) return(if (grepl("%", s)) num / 100 else num)
    
    # Binary / Qualitative mappings
    if (s %in% c("safe", "non-inhibitor", "no", "absent", "inactive", "low", "negative", "non-toxic")) return(0)
    if (s %in% c("toxic", "inhibitor", "yes", "present", "active", "high", "positive")) return(1)
    if (s %in% c("absorbed", "bioavailable", "penetrable", "permeable")) return(1)
    if (s %in% c("substrate", "medium", "moderate")) return(0.5)
    
    return(NA_real_)
  }
  
  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Value_prob = if (is.na(Value_num)) text_to_prob_one(Value_str) else Value_num) %>%
    dplyr::ungroup()
    
  # -----------------------
  # 3. Compute desirability
  # -----------------------
  compute_desirability_one <- function(val_prob, direction, L, U, is_prob_bad, prop) {
    if (is.na(val_prob)) return(NA_real_)
    
    if (is.numeric(val_prob) && val_prob >= 0 && val_prob <= 1) {
      if (!is.na(is_prob_bad) && is_prob_bad) return(1 - val_prob)
      if (!is.na(direction) && direction == "prob_bad") return(1 - val_prob)
      if (!is.na(prop) && grepl("^BBB$", prop, ignore.case = TRUE)) {
        return(if (context == "CNS") val_prob else 1 - val_prob)
      }
      return(val_prob)
    }
    
    if (!is.na(L) && !is.na(U) && L != U) {
      d <- switch(direction,
                  lower_better = 1 - ((val_prob - L) / (U - L)),
                  higher_better = (val_prob - L) / (U - L),
                  mid_ideal = 1 - (abs(val_prob - ((L + U) / 2)) / ((U - L) / 2)),
                  (val_prob - L) / (U - L))
      return(max(0, min(1, d)))
    }
    
    return(NA_real_)
  }
  
  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(desirability = compute_desirability_one(Value_prob, direction, L, U, is_prob_bad, canonical)) %>%
    dplyr::ungroup()
    
  # -----------------------
  # 4. Weights
  # -----------------------
  # Use the specific weight from metadata
  df$weight <- df$weight_in_group

  
  if (is.null(group_weights)) {
    if (exists("get_group_weights")) {
      group_weights <- get_group_weights()
    } else {
      # Fallback if config is missing (Aligned with Math Model v1.2)
      group_weights <- c(
        Absorption = 0.20,
        Distribution = 0.15,
        Metabolism = 0.20,
        Excretion = 0.10,
        Toxicity = 0.25,
        Physicochemical = 0.10
      )
    }
  }
  
  # Ensure all groups are present in weights vector to avoid NA
  # Normalize group weights to sum to 1 just in case
  group_weights <- group_weights / sum(group_weights)
  
  # -----------------------
  # 5. Subscores & GAI
  # -----------------------
  subscores <- df %>%
    filter(!is.na(desirability)) %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(Subscore = weighted.mean(desirability, weight, na.rm = TRUE), .groups = 'drop')
  
  all_groups <- tibble(group = names(group_weights))
  subscores <- dplyr::left_join(all_groups, subscores, by = "group") %>%
    dplyr::mutate(Subscore = dplyr::coalesce(Subscore, 0)) # Default to 0 if no endpoints
    
  S_raw <- sum(subscores$Subscore * group_weights[subscores$group], na.rm = TRUE)
  GAI_pre <- 100 * S_raw
  
  # -----------------------
  # 6. Penalties (Legacy - Math model v1.2 incorporates penalties into subscores, 
  # but we keep this for extra safety on critical failure)
  # -----------------------
  if (is.null(fatal_thresholds)) {
    fatal_thresholds <- list(hERG = 0.9, DILI = 0.7, Carcinogenicity = 0.8, Ames = 0.5)
  }
  red_flags <- list()
  for (fn in names(fatal_thresholds)) {
    values <- df$Value_prob[df$canonical == fn & !is.na(df$Value_prob)]
    if (length(values) > 0) {
      val <- max(values, na.rm = TRUE)
      if (is.finite(val) && val >= fatal_thresholds[[fn]]) {
        red_flags[[fn]] <- val
      }
    }
  }
  
  GAI_post <- if (length(red_flags) > 0) GAI_pre * (1 - fatal_penalty) else GAI_pre
  
  # -----------------------
  # 7. Plots (Enhanced Visuals, Readability & Layout Fixes)
  # -----------------------
  plots <- list()
  
  # Professional High-Contrast Palette
  gai_palette_v3 <- c(
    Absorption = "#264653", 
    Distribution = "#2a9d8f", 
    Metabolism = "#e9c46a", 
    Excretion = "#f4a261", 
    Toxicity = "#e76f51", 
    Physicochemical = "#1d3557"
  )

  # Enhanced theme with safe margins
  theme_gai_v3 <- function(base_size = 14) {
    theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 11, color = "grey30", margin = margin(b = 12)),
      axis.title = element_text(size = 11, face = "bold", color = "black"),
      axis.text = element_text(size = 10, color = "black", face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(15, 15, 15, 15) # Reduced from 40 to prevent 'margins too large' error
    )
  }

  # Plot 1: Desirability Map
  plot_df_desir <- df %>% 
    filter(!is.na(desirability)) %>%
    arrange(group, canonical)
  
  if(nrow(plot_df_desir) > 0) {
    plots$desirability_heatmap <- ggplot(plot_df_desir, aes(x = canonical, y = group, fill = desirability)) +
      geom_tile(color = "white", linewidth = 1.2) +
      scale_fill_gradientn(
        colors = c("#03071e", "#6a040f", "#d00000", "#f48c06", "#ffba08", "#90be6d", "#43aa8b", "#4d908e"), 
        limits = c(0, 1), 
        name = "Score",
        breaks = c(0, 0.5, 1),
        labels = c("Critical", "Average", "Optimal")
      ) +
      theme_gai_v3() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
        panel.grid.major = element_blank(),
        plot.margin = margin(10, 30, 10, 10)
      ) +
      labs(
        title = "ADMET Desirability Map", 
        subtitle = "Liability (Red) vs. Desirable (Green)",
        x = "", y = ""
      )
  }

  # Plot 2: Subscores Bar Chart
  if(nrow(subscores) > 0) {
    plots$subscores_bar <- ggplot(subscores, aes(x = reorder(group, Subscore), y = Subscore * 100, fill = group)) +
      geom_col(width = 0.75, color = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.0f%%", Subscore * 100)), hjust = -0.4, size = 4.5, fontface = "bold") +
      scale_fill_manual(values = gai_palette_v3) +
      coord_flip() +
      scale_y_continuous(limits = c(0, 130), breaks = seq(0, 100, 25)) +
      labs(
        x = "", y = "Category Score (%)", 
        title = "Subgroup Performance",
        subtitle = "Performance across ADMET domains"
      ) +
      theme_gai_v3() +
      theme(legend.position = "none")
  }

  # Plot 3: Radar Plot
  if(nrow(subscores) > 0) {
    plots$radar <- ggplot(subscores, aes(x = group, y = Subscore * 100, fill = group)) +
      geom_col(width = 1, alpha = 0.9, color = "white", linewidth = 1) +
      scale_fill_manual(values = gai_palette_v3) +
      coord_polar(start = -pi/6) +
      theme_minimal(base_size = 12) +
      theme(
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 11, color = "black"),
        panel.grid.major = element_line(color = "grey85", linewidth = 0.8),
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 11, color = "grey30", hjust = 0.5),
        plot.margin = margin(20, 20, 20, 20) # Balanced margin for polar
      ) +
      labs(title = "ADMET Fingerprint", subtitle = "Domain distribution")
  }

  # Plot 4: Overall GAI Gauge
  gai_df <- data.frame(GAI = GAI_post)
  if(nrow(gai_df) > 0) {
    
    get_color <- function(gai) {
      if (gai < 40) return("#d00000") 
      if (gai < 60) return("#f48c06") 
      if (gai < 80) return("#ffba08") 
      return("#38b000") 
    }
    
    gai_color <- get_color(GAI_post)
    gai_text <- if (GAI_post < 20) "CRITICAL" else if (GAI_post < 40) "POOR" else if (GAI_post < 60) "MODERATE" else if (GAI_post < 80) "GOOD" else "EXCELLENT"
    
    plots$overall <- ggplot(gai_df, aes(x = 1, y = GAI)) +
      geom_rect(aes(xmin = 0.8, xmax = 1.2, ymin = 0, ymax = 100), fill = "#f1f3f5", color = NA) +
      geom_rect(aes(xmin = 0.8, xmax = 1.2, ymin = 0, ymax = GAI), fill = gai_color) +
      coord_polar(theta = "y", start = -pi/2) +
      xlim(0, 1.5) + 
      ylim(0, 100) +
      theme_void() +
      annotate("text", x = 0, y = 0, label = round(GAI_post), size = 20, fontface = "bold", color = "#212529") +
      annotate("text", x = 0, y = 0, label = "GAI SCORE", size = 4.5, color = "grey50", fontface = "bold", vjust = 4.0) +
      annotate("text", x = 0, y = 0, label = gai_text, size = 7, color = gai_color, fontface = "bold", vjust = -4.0) +
      theme(plot.margin = margin(5, 5, 5, 5))
  }

  # Plot 5: Toxicity Profile (Fixed height/contrast/readability)
  tox_df <- df %>% 
    filter(group == "Toxicity", !is.na(desirability)) %>%
    arrange(desirability)
  
  if(nrow(tox_df) > 0) {
    plots$toxicity_bar <- ggplot(tox_df, aes(x = reorder(canonical, desirability), y = desirability)) +
      geom_col(aes(fill = desirability), width = 0.8, color = "black", linewidth = 0.2) +
      # Bold white or black text depending on value for contrast
      geom_text(aes(label = sprintf("%.2f", desirability), color = desirability > 0.5), 
                hjust = 1.5, size = 5, fontface = "bold", show.legend = FALSE) +
      scale_color_manual(values = c("TRUE" = "black", "FALSE" = "white")) +
      scale_fill_gradientn(colors = c("#370617", "#9d0208", "#dc2f02", "#f48c06", "#ffba08", "#d9ed92"), limits = c(0, 1)) +
      coord_flip() +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
      labs(
        x = "", y = "Desirability (1 = Safe / Low Risk)", 
        title = "Toxicological Liability Assessment",
        subtitle = "Higher values indicate safer predicted profile"
      ) +
      theme_gai_v3() +
      theme(legend.position = "none")
  }

  # Plot 6: Waterfall plot (Vibrant contribution palette)
  waterfall_df <- subscores %>%
    mutate(weighted_score = Subscore * group_weights[group] * 100) %>%
    arrange(desc(weighted_score))
  
  # Add Penalty Row if GAI was reduced by red flags
  if (GAI_post < GAI_pre) {
    penalty_val <- GAI_post - GAI_pre
    penalty_row <- data.frame(
      group = "Penalty", 
      Subscore = NA, 
      weighted_score = penalty_val
    )
    waterfall_df <- rbind(waterfall_df, penalty_row)
  }

  waterfall_df <- waterfall_df %>%
    mutate(
      end = cumsum(weighted_score),
      start = lag(end, default = 0),
      group = factor(group, levels = group)
    )
  
  if(nrow(waterfall_df) > 0) {
    # Add Penalty to palette
    waterfall_palette <- c(gai_palette_v3, Penalty = "#343a40")

    plots$waterfall <- ggplot(waterfall_df, aes(x = group, fill = group)) +
      geom_rect(aes(xmin = as.numeric(group) - 0.4, xmax = as.numeric(group) + 0.4, ymin = start, ymax = end), 
                color = "white", linewidth = 0.5) +
      scale_fill_manual(values = waterfall_palette) +
      geom_hline(yintercept = GAI_post, linetype = "dashed", color = "#212529", linewidth = 1) +
      annotate("label", x = 0.5, y = GAI_post, label = paste("ADJUSTED GAI:", round(GAI_post, 1)), 
               hjust = 0, fontface = "bold", fill = "black", color = "white", size = 5) +
      labs(
        x = "", y = "Contribution Points", 
        title = "GAI Value Attribution",
        subtitle = "Showing category gains and Red Flag penalties"
      ) +
      theme_gai_v3() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  }

  # -----------------------
  # 8. Return structured result
  # -----------------------
  return(list(
    GAI = GAI_post,
    GAI_pre_penalty = GAI_pre,
    subscores = subscores,
    endpoint_table = df %>% select(Property, canonical, group, raw_Value, Value_prob, desirability, weight),
    red_flags = red_flags,
    plots = plots,
    explanation = "GAI calculation complete."
  ))
}