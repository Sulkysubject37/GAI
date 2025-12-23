# ======================================================
# app.R - Global ADMET Index (GAI) Professional Edition
# ======================================================
library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(readr)
library(rmarkdown)
library(knitr)
library(plotly)
library(shinyjs)
library(shinybusy)
library(zip)
library(digest)
library(purrr)
library(tidyr)
library(stringr)
library(digest)

# --- Load helper scripts ---
source("R/config_endpoints.R", local = TRUE)
source("R/utils.R", local = TRUE)
source("R/load_ADMET_data.R", local = TRUE)
source("R/compute_GAI.R", local = TRUE)

# ---- UI Definition ----
ui <- page_navbar(
  title = "Global ADMET Index Analyzer",
  theme = bs_theme(
    bootswatch = "cosmo", 
    primary = "#004c6d",
    base_font = font_google("Inter"),
    "navbar-bg" = "#004c6d"
  ),
  bg = "#004c6d",
  inverse = TRUE,
  fillable = TRUE,
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # --- Sidebar (Common) ---
  sidebar = sidebar(
    title = "Analysis Controls",
    width = 300,
    fileInput("file", "Upload Data", accept = ".csv", width = "100%"),
    selectInput("source_type", "Data Source",
                choices = c("Auto Detect", "ADMETlab", "SwissADME", "Deep-PK", "pkCSM", "Manual")),
    selectInput("context", "Pharmacological Context",
                choices = c("Default" = "default", "CNS / Brain" = "CNS")),
    radioButtons("processing_mode", "Processing Mode",
                 choices = c("Single Molecule", "Batch"), selected = "Batch", inline = TRUE),
    hr(),
    useShinyjs(),
    div(
      class = "d-grid gap-2",
      actionButton("run_gai", "Compute GAI", class = "btn-primary btn-lg"),
      downloadButton("download_report", "Download ZIP Report", class = "btn-outline-primary")
    ),
    hr(),
    selectInput("theme_mode", "UI Theme", 
                choices = c("Light (Cosmo)" = "cosmo", "Professional (Flatly)" = "flatly", 
                            "Modern (Zephyr)" = "zephyr", "Dark (Cyborg)" = "cyborg", 
                            "Vaporwave" = "vapor"), 
                selected = "cosmo")
  ),

  # --- Tab 1: Introduction ---
  nav_panel(
    title = "Home",
    icon = icon("home"),
    layout_column_wrap(
      width = 1,
      card(
        card_header(class = "bg-primary", "Welcome to GAI Analyzer"),
        card_body(
          h2("Standardizing ADMET Assessment"),
          p("The Global ADMET Index (GAI) provides a unified, mathematically rigorous framework for 
             comparing molecule potential across diverse predictive platforms."),
          markdown("
### How to use this tool:
1. **Upload**: Drag and drop your CSV results from SwissADME, ADMETlab, etc.
2. **Standardize**: The system automatically maps tool-specific labels to GAI canonical endpoints.
3. **Analyze**: Visualize liabilities, subgroup strengths, and overall drug-likeness.
4. **Export**: Generate high-quality reports for your research.
          ")
        )
      ),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Mathematical Foundations"),
          p("GAI uses a weighted aggregation model (v1.2) based on 6 major domains:"),
          tags$ul(
            tags$li(strong("Absorption (20%):"), " HIA, Caco-2, logP"),
            tags$li(strong("Distribution (15%):"), " BBB, PPB, VDss"),
            tags$li(strong("Toxicity (25%):"), " Ames, DILI, hERG, etc.")
          )
        ),
        card(
          card_header("Compatible Tools"),
          tags$ul(
            tags$li("ADMETlab 3.0"),
            tags$li("SwissADME"),
            tags$li("pkCSM"),
            tags$li("Deep-PK")
          )
        )
      )
    )
  ),

  # --- Tab 2: Dashboard ---
  nav_panel(
    title = "Dashboard",
    icon = icon("chart-line"),
    uiOutput("dashboard_ui")
  ),

  # --- Tab 3: Data Explorer ---
  nav_panel(
    title = "Data Explorer",
    icon = icon("table"),
    navset_pill(
      nav_panel("Standardized Long Data", DTOutput("parsed_preview")),
      nav_panel("Full Attribute Table", DTOutput("endpoint_table"))
    )
  ),

  # --- Tab 4: Settings/About ---
  nav_panel(
    title = "Info",
    icon = icon("info-circle"),
    card(
      card_header("About GAI v1.2"),
      p("Author: MD. Arshad"),
      p("Standard: Global ADMET Index Math Model v1.2"),
      hr(),
      p("This tool is designed for medicinal chemistry and pharmacokinetic research. 
         Predicted values should be validated with experimental assays.")
    )
  )
)

# ---- Server Logic ----
server <- function(input, output, session) {
  
  # --- Theme Management ---
  observeEvent(input$theme_mode, {
    new_theme <- bs_theme(bootswatch = input$theme_mode, 
                          primary = if(input$theme_mode == "cyborg") "#ff79c6" else "#004c6d",
                          base_font = font_google("Inter"))
    session$setCurrentTheme(new_theme)
  })
  
  # --- Reactive Data ---
  parsed_data <- reactiveVal(NULL)
  gai_results <- reactiveVal(NULL)
  
  # Molecule Selector State
  current_mol_id <- reactiveVal(NULL)
  
  # --- 1. File Ingestion ---
  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      df <- read_any_delim(input$file$datapath)
      res <- load_ADMET_data(df, source_type = input$source_type)
      parsed_data(res$data_long)
      
      if (res$n_molecules > 1) {
        updateRadioButtons(session, "processing_mode", selected = "Batch")
      } else {
        updateRadioButtons(session, "processing_mode", selected = "Single Molecule")
      }
      showNotification(paste("Loaded", res$n_molecules, "molecules."), type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # --- 2. Computation ---
  observeEvent(input$run_gai, {
    req(parsed_data())
    show_modal_spinner(text = "Running Mathematical Engine...")
    
    tryCatch({
      results <- compute_GAI(parsed_data(), context = input$context)
      gai_results(results)
      
      # Set initial selection
      if (input$processing_mode == "Batch") {
        current_mol_id(names(results$detailed_results)[1])
      }
      
      showNotification("GAI Analysis Complete", type = "message")
    }, error = function(e) {
      showNotification(paste("Engine Error:", e$message), type = "error")
    }, finally = {
      remove_modal_spinner()
    })
  })
  
  # --- 3. Dynamic Dashboard UI ---
  output$dashboard_ui <- renderUI({
    req(gai_results())
    
    if (input$processing_mode == "Batch") {
      layout_column_wrap(
        width = 1,
        # Batch Overview Row
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Batch GAI Distribution"),
            plotOutput("batch_boxplot", height = "350px")
          ),
          card(
            card_header("Batch Summary Table"),
            DTOutput("batch_summary_table")
          )
        ),
        # Detailed Drill-down Row
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            "Molecule Detail View",
            selectInput("mol_selector", NULL, choices = names(gai_results()$detailed_results), 
                        selected = current_mol_id(), width = "300px")
          ),
          uiOutput("single_mol_view")
        )
      )
    } else {
      # Single Molecule Direct View
      uiOutput("single_mol_view")
    }
  })
  
  # Update current molecule when selector changes
  observeEvent(input$mol_selector, {
    current_mol_id(input$mol_selector)
  })
  
  # --- 4. Single Molecule Component ---
  output$single_mol_view <- renderUI({
    req(gai_results())
    
    # Extract results for the selected (or only) molecule
    res <- if (input$processing_mode == "Batch") {
      gai_results()$detailed_results[[current_mol_id()]]
    } else {
      gai_results()
    }
    
    req(res)
    
    tagList(
      layout_column_wrap(
        width = 1/3,
        card(
          card_header("Overall Score"),
          plotOutput("mol_gauge", height = "300px"),
          uiOutput("mol_flags")
        ),
        card(
          card_header("ADMET Fingerprint"),
          plotOutput("mol_radar", height = "300px")
        ),
        card(
          card_header("Subgroup Scores"),
          plotOutput("mol_subscores", height = "300px")
        )
      ),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("GAI Contribution Waterfall"),
          plotOutput("mol_waterfall", height = "400px")
        ),
        card(
          card_header("Toxicity Risk Assessment"),
          plotOutput("mol_toxicity", height = "400px")
        )
      ),
      card(
        card_header("Endpoint Desirability Map"),
        plotOutput("mol_heatmap", height = "500px")
      )
    )
  })
  
  # --- 5. Renderers ---
  
  # Helper to get current molecule data
  current_res <- reactive({
    req(gai_results())
    if (input$processing_mode == "Batch") {
      gai_results()$detailed_results[[current_mol_id()]]
    } else {
      gai_results()
    }
  })
  
  output$batch_boxplot <- renderPlot({ req(gai_results()); gai_results()$plots$batch_boxplot })
  output$batch_summary_table <- renderDT({
    req(gai_results()$summary)
    datatable(gai_results()$summary, options = list(pageLength = 5, dom = 'tip'), rownames = FALSE)
  })
  
  output$mol_gauge <- renderPlot({ req(current_res()); current_res()$plots$overall })
  output$mol_radar <- renderPlot({ req(current_res()); current_res()$plots$radar })
  output$mol_subscores <- renderPlot({ req(current_res()); current_res()$plots$subscores_bar })
  output$mol_waterfall <- renderPlot({ req(current_res()); current_res()$plots$waterfall })
  output$mol_toxicity <- renderPlot({ req(current_res()); current_res()$plots$toxicity_bar })
  output$mol_heatmap <- renderPlot({ req(current_res()); current_res()$plots$desirability_heatmap })
  
  output$mol_flags <- renderUI({
    req(current_res())
    flags <- current_res()$red_flags
    if (length(flags) == 0) return(p("No Red Flags Detected", class = "text-success"))
    tagList(
      h6("Red Flags", class = "text-danger"),
      lapply(names(flags), function(f) div(class = "badge bg-danger m-1", f))
    )
  })
  
  output$parsed_preview <- renderDT({
    req(parsed_data())
    datatable(parsed_data(), filter = 'top', options = list(scrollX = TRUE))
  })
  
  output$endpoint_table <- renderDT({
    req(current_res())
    datatable(current_res()$endpoint_table, options = list(scrollX = TRUE))
  })
  
  # --- 6. Export Handler ---
  output$download_report <- downloadHandler(
    filename = function() { paste0("GAI_Analysis_", Sys.Date(), ".zip") },
    content = function(file) {
      req(gai_results())
      temp_dir <- file.path(tempdir(), "gai_export")
      dir.create(temp_dir, showWarnings = FALSE)
      
      # Save Summary
      if (input$processing_mode == "Batch") {
        write.csv(gai_results()$summary, file.path(temp_dir, "gai_summary.csv"), row.names = FALSE)
      }
      
      # Save Detailed results for all molecules
      mols <- if(input$processing_mode == "Batch") names(gai_results()$detailed_results) else "Molecule_1"
      for (m in mols) {
        res <- if(input$processing_mode == "Batch") gai_results()$detailed_results[[m]] else gai_results()
        write.csv(res$endpoint_table, file.path(temp_dir, paste0(m, "_data.csv")), row.names = FALSE)
      }
      
      zip::zip(file, files = dir(temp_dir, full.names = TRUE), mode = "cherry-pick")
    }
  )
}

# ---- Run App ----
shinyApp(ui, server)
