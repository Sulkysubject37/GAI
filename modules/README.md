# Shiny Modules Directory

This directory is intended for **Shiny Modules**, which allow for modular, reusable, and testable UI and server logic.

## Directory Role
- **Encapsulation**: Each module should reside in its own R script (e.g., `modules/module_gai_plots.R`).
- **Organization**: Complex logic like the "Single Molecule View" or "Batch Comparison" can be moved here to keep `app.R` clean.

## Example Structure
A typical module file contains:
```R
# modules/example_module.R

exampleUI <- function(id) {
  ns <- NS(id)
  tagList(...)
}

exampleServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ...
  })
}
```

## How to use in app.R
1. Source the module: `source("modules/example_module.R")`
2. Call the UI: `exampleUI("my_id")`
3. Call the Server: `exampleServer("my_id", reactive_data)`
