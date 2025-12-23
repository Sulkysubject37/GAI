# Global ADMET Index (GAI) Analyzer [v2.0.1]

A unified platform for ADMET profiling, scoring, and visualization. GAI integrates data from multiple predictive tools into a single, interpretable index (0-100) to accelerate drug discovery workflows.

---

## Quick Start

### 1. Installation
Install R (>= 4.0) and the required dependencies:
```R
install.packages("remotes")
remotes::install_deps()
```

### 2. Run the Web App
Launch the interactive Shiny dashboard:
```bash
R -e "shiny::runApp('app.R')"
```

### 3. Run the CLI
Process a file from your terminal:
```bash
./gai data/ADMETlab3_result64.csv -o results/mol_64
```

---

## Features

- **Multi-Tool Support**: Natively parses results from **ADMETlab 3.0**, **SwissADME**, **pkCSM**, and **DeepPK**.
- **Robust Normalization**: Automatically handles unit conversions and "desirability" directions (e.g., Higher is Better for HIA, Lower is Better for Toxicity).
- **Batch Processing**: Score hundreds of molecules simultaneously and generate comparative boxplots.
- **Rich Visuals**: Detailed molecule reports including Gauge plots, Radar charts, and Waterfall contribution charts.

---

## Documentation

Detailed documentation is available in the `docs/` directory:

| Document | Description |
| :--- | :--- |
| [**Mathematical Model**](docs/mathematical_model.md) | GAI aggregation formulas, weights, and normalization logic. |
| [**Architecture**](docs/architecture.md) | File structure, data flow pipeline, and "Unflawed" logic. |
| [**Supported Endpoints**](docs/endpoints.md) | Comprehensive list of canonical parameters and their logic. |
| [**Usage Guide**](docs/usage.md) | In-depth instructions for the Shiny App and CLI options. |

---

## Project Structure

- `app.R`: Shiny web application entry point.
- `scripts/run_gai.R`: Core CLI processing script.
- `R/`: Scoring engine, data loaders, and master configuration.
- `data/`: Example ADMET results for testing.
- `www/`: Application styling and assets.

---

## Citation

If you use this toolkit in your research, please cite:

> Arshad, M. (2025). Global ADMET Index (GAI): v2.0.1 (v2.0.1). Zenodo. https://doi.org/10.5281/zenodo.18030940

---

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---
**Author**: MD. Arshad  
**Institution**: Jamia Millia Islamia
