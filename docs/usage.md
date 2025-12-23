# Usage Guide

The GAI platform offers two ways to analyze your ADMET data: a web-based interactive app and a high-throughput CLI script.

## 1. Installation

Ensure you have R (>= 4.0) installed. Clone this repository and install the dependencies:

```R
# From the project root
install.packages("remotes")
remotes::install_deps()
```

## 2. Shiny Web Application

The Shiny app provides a visual interface for exploring molecule ADMET profiles.

### Launching the App
```R
# From R console
shiny::runApp("app.R")
```

### Features
1.  **File Upload**: Upload your CSV results from ADMETlab, SwissADME, pkCSM, or DeepPK.
2.  **Source Detection**: The app automatically detects which tool you used.
3.  **GAI Analysis**: Click "Compute GAI" to generate gauge, radar, and waterfall plots.
4.  **Batch vs Single**: Switch modes to compare multiple molecules or dive deep into one.
5.  **Export**: Download a complete ZIP report including plots and sanitized data tables.

---

## 3. Command-Line Interface (CLI)

The CLI is ideal for automated workflows and large-scale batch processing.

### Execution
Use the `./gai` wrapper script (Unix) or call Rscript directly:

```bash
./gai <input_file> -o <output_dir> -s <source_type>
```

### Arguments
- `<input_file>`: Path to your ADMET results CSV.
- `-o, --output`: Directory to save results (default: `gai_output`).
- `-s, --source`: Manually set tool (`ADMETlab`, `SwissADME`, `DeepPK`, `pkCSM`). Default: `Auto Detect`.

### Example
```bash
./gai data/ADMETlab3_result64.csv -o results/mol_64
```

### Output Files
The CLI generates a structured directory:
- `gai_summary.csv`: A table of all molecules and their final GAI scores.
- `batch_boxplot.png`: Distribution of scores across the batch.
- `[Molecule_ID]/`: A sub-directory for each molecule containing:
    - `endpoint_table.csv`: Raw vs Normalized data for every parameter.
    - `plot_overall_gauge.png`: The final score visual.
    - `plot_radar.png`: Subgroup profile.
    - `plot_waterfall.png`: Contribution of each subgroup to the score.
