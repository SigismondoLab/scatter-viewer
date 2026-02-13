library(shiny)
library(shinyFiles)
library(plotly)
library(RColorBrewer)
library(viridis)

# Define UI
ui <- fluidPage(
  titlePanel("coordinate Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Load Data Files"),
      shinyFilesButton("coordinate_file", "Load coordinate (.tsv)", 
                       "Select X,Y coordinates file", multiple = FALSE),
      textOutput("coordinate_status"),
      br(),
      shinyFilesButton("meta_file", "Load metadata (.tsv)", 
                       "Select metadata file", multiple = FALSE),
      textOutput("meta_status"),
      hr(),
      
      h4("Color By"),
      uiOutput("color_selector"),
      hr(),
      
      h4("Hover Info"),
      helpText("Select metadata fields to show on hover:"),
      uiOutput("hover_selector")
    ),
    
    mainPanel(
      width = 9,
      plotlyOutput("coordinate_plot", height = "700px")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store data
  coordinate_data <- reactiveVal(NULL)
  meta_data <- reactiveVal(NULL)
  last_directory <- reactiveVal(NULL)
  
  # Set up file browser with dynamic volumes
  volumes <- reactive({
    base_volumes <- c(Home = fs::path_home(), getVolumes()())
    if (!is.null(last_directory())) {
      c("Last Used" = last_directory(), base_volumes)
    } else {
      base_volumes
    }
  })
  
  # File selection for coordinate
  observe({
    shinyFileChoose(input, "coordinate_file", roots = volumes(), session = session,
                    filetypes = c("tsv", "txt"))
  })
  
  observeEvent(input$coordinate_file, {
    if (!is.integer(input$coordinate_file)) {
      file_path <- parseFilePaths(volumes(), input$coordinate_file)
      if (nrow(file_path) > 0) {
        tryCatch({
          data <- read.delim(as.character(file_path$datapath), sep = "\t")
          if (!all(c("X", "Y") %in% colnames(data))) {
            showNotification("Error: coordinate file must have 'X' and 'Y' columns", type = "error")
          } else {
            coordinate_data(data)
            # Update last directory for next file selection
            last_directory(dirname(as.character(file_path$datapath)))
          }
        }, error = function(e) {
          showNotification(paste("Error loading coordinate file:", e$message), type = "error")
        })
      }
    }
  })
  
  output$coordinate_status <- renderText({
    if (is.null(coordinate_data())) {
      "No coordinate file loaded"
    } else {
      paste("coordinate loaded:", nrow(coordinate_data()), "points")
    }
  })
  
  # File selection for metadata
  observe({
    shinyFileChoose(input, "meta_file", roots = volumes(), session = session,
                    filetypes = c("tsv", "txt"))
  })
  
  observeEvent(input$meta_file, {
    if (!is.integer(input$meta_file)) {
      file_path <- parseFilePaths(volumes(), input$meta_file)
      if (nrow(file_path) > 0) {
        tryCatch({
          data <- read.delim(as.character(file_path$datapath), sep = "\t")
          meta_data(data)
          # Update last directory for next file selection
          last_directory(dirname(as.character(file_path$datapath)))
        }, error = function(e) {
          showNotification(paste("Error loading metadata file:", e$message), type = "error")
        })
      }
    }
  })
  
  output$meta_status <- renderText({
    if (is.null(meta_data())) {
      "No metadata file loaded"
    } else {
      paste("metadata loaded:", ncol(meta_data()), "columns,", nrow(meta_data()), "rows")
    }
  })
  
  # Color column selector
  output$color_selector <- renderUI({
    if (is.null(meta_data())) {
      return(helpText("Load metadata file to enable coloring"))
    }
    selectInput("color_column", "Select column:", 
                choices = c("None" = "", colnames(meta_data())),
                selected = "")
  })
  
  # Hover fields selector
  output$hover_selector <- renderUI({
    if (is.null(meta_data())) {
      return(helpText("Load metadata file to select hover fields"))
    }
    checkboxGroupInput("hover_fields", NULL,
                       choices = colnames(meta_data()),
                       selected = colnames(meta_data())[1:min(3, ncol(meta_data()))])
  })
  
  # Generate coordinate plot
  output$coordinate_plot <- renderPlotly({
    req(coordinate_data())
    
    coordinate <- coordinate_data()
    meta <- meta_data()
    
    # Prepare plot data
    plot_data <- data.frame(X = coordinate$X, Y = coordinate$Y)
    
    # Add color column if selected
    if (!is.null(meta) && !is.null(input$color_column) && input$color_column != "") {
      color_col <- meta[[input$color_column]]
      plot_data$color_val <- color_col
      
      # Determine color scale
      if (is.numeric(color_col)) {
        # Check if diverging (has positive and negative)
        if (any(color_col < 0, na.rm = TRUE) && any(color_col > 0, na.rm = TRUE)) {
          # Diverging colormap
          p <- plot_ly(plot_data, x = ~X, y = ~Y, type = "coordinate", mode = "markers",
                       color = ~color_val, colors = colorRamp(c("blue", "white", "red")),
                       marker = list(size = 5))
        } else {
          # Non-diverging colormap (positive only)
          p <- plot_ly(plot_data, x = ~X, y = ~Y, type = "coordinate", mode = "markers",
                       color = ~color_val, colors = viridis(100),
                       marker = list(size = 5))
        }
      } else {
        # Categorical colormap
        n_categories <- length(unique(color_col))
        if (n_categories <= 12) {
          colors <- brewer.pal(min(n_categories, 12), "Set3")
        } else {
          colors <- rainbow(min(n_categories, 30))
        }
        p <- plot_ly(plot_data, x = ~X, y = ~Y, type = "coordinate", mode = "markers",
                     color = ~as.factor(color_val), colors = colors,
                     marker = list(size = 5))
      }
    } else {
      # No coloring
      p <- plot_ly(plot_data, x = ~X, y = ~Y, type = "coordinate", mode = "markers",
                   marker = list(size = 5, color = "steelblue"))
    }
    
    # Add hover text with selected metadata fields
    if (!is.null(meta) && !is.null(input$hover_fields) && length(input$hover_fields) > 0) {
      hover_text <- sapply(1:nrow(meta), function(i) {
        fields <- input$hover_fields
        values <- sapply(fields, function(f) paste0(f, ": ", meta[i, f]))
        paste(values, collapse = "<br>")
      })
      p <- p %>% add_trace(text = hover_text, hoverinfo = "text")
    }
    
    p %>% layout(
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      hovermode = "closest"
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)

