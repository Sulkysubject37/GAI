# System Architecture

The GAI project is designed as a modular R-based platform for processing, normalizing, and scoring ADMET data from multiple predictive tools.

## 1. Directory Structure

```text
GAI/
├── app.R                # Shiny Web Application
├── scripts/             # CLI Scripts
│   └── run_gai.R        # Main CLI entry point
├── R/                   # Core Logic (The "Engine")
│   ├── config_endpoints.R # Master mapping and weights
│   ├── load_ADMET_data.R  # Data ingestion and source detection
│   ├── compute_GAI.R      # Mathematical scoring engine
│   ├── utils.R            # Shared helper functions
│   └── preprocess_*.R     # Tool-specific normalizers
├── docs/                # Documentation
└── data/                # Sample datasets (ADMETlab, pkCSM, etc.)
```

## 2. Data Flow Pipeline

The system follows a strict 3-step pipeline to ensure "Unflawed" results:

### Step 1: Ingestion (`load_ADMET_data.R`)
- The system automatically detects the source (ADMETlab, SwissADME, DeepPK, or pkCSM) by analyzing column headers.
- It routes the raw dataframe to the appropriate `preprocess_*.R` script.
- **Cleaning**: All column names are converted to lowercase and spaces/brackets are replaced with dots (e.g., `Caco-2` -> `caco.2`).

### Step 2: Standardization (`config_endpoints.R`)
- Immediately after ingestion, the system applies the **Master Map**.
- Using fuzzy regex matching, raw tool-specific names are mapped to **Canonical GAI Names** (e.g., `gi absorption` and `hia` both become `HIA`).
- This ensures the UI preview and the final calculation use the same definitions.

### Step 3: Computation (`compute_GAI.R`)
- The engine retrieves the `direction`, `L`, `U`, and `weight` for each canonical property from the config.
- It performs text-to-probability conversion (e.g., "Safe" -> `0`, "Active" -> `1`).
- It applies the GAI Mathematical Model to generate subgroup scores and the final index.

## 3. The "Unflawed" Philosophy
- **Single Source of Truth**: All weights and regex patterns reside in `R/config_endpoints.R`. Changing a weight here updates both the CLI and the Shiny App simultaneously.
- **Robustness**: Regex patterns use `.` wildcards to handle diverse formatting across different tool versions.
- **Exclusion**: Properties not defined in the mathematical model are categorized as `Other` and ignored in the final score calculation, preventing noise from affecting the GAI.
