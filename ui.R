# ui.R (Complete Code - Dedicated Filename Output)

library(shiny)
library(shinythemes)

# --- UI Definition ---
ui <- fluidPage(theme = shinytheme("spacelab"),
  tags$head(
    tags$style(HTML("
      /* Basic setup */
      html, body { height: 100%; margin: 0; padding: 0; overflow: hidden; }
      .container-fluid { padding: 5px; height: 100%; display: flex; }

      /* Column Layout */
      .fixed-plot-col { width: 400px; flex-shrink: 0; height: 100%; padding-right: 10px; display: flex; flex-direction: column; }
      .controls-col { flex-grow: 1; height: 100%; overflow-y: auto; overflow-x: auto; padding-left: 10px; }

      /* Plot Area */
      .plot-labels-container { height: 25px; flex-shrink: 0; display: flex; justify-content: space-around; align-items: center; margin-bottom: 5px; }
      #va_plot { flex-grow: 1; border: 1px solid lightgrey; }

      /* Well Panel Styling */
      .well { padding: 10px; margin-bottom: 10px; }
      .well .row { margin-bottom: 8px; }

      /* Row 1 Styling (Heading, Label, Counts) */
      .row1-container { display: flex; align-items: baseline; width: 100%; margin-bottom: 8px; }
      .row1-container h4 { margin: 0 10px 0 0; color: darkblue; flex-shrink: 0; }
      .row1-container label { margin: 0 5px 0 0; font-weight: normal !important; white-space: nowrap; flex-shrink: 0; }
      .row1-container .label-input-div { flex-grow: 0; flex-shrink: 0; width: 200px; margin-right: 15px; }
      .row1-container .label-input-div .form-group { margin-bottom: 0; }
      .row1-container .counts-div { text-align: left; white-space: nowrap; flex-shrink: 0; }
      .row1-container .counts-div span { margin-right: 15px; }

      /* --- MODIFIED File Input Area Styling --- */
      /* Container for button and filename display */
      .file-area { display: flex; align-items: center; margin-bottom: 8px; }
      /* Style the fileInput container itself */
      .file-area .shiny-input-container { margin-bottom: 0 !important; flex-shrink: 0; } /* Remove bottom margin, prevent shrinking */
      /* Hide the default text input box */
      .shiny-input-container:has(input[type='file']) .input-group input[type='text'] {
         display: none;
       }
       /* Hide the default progress bar container entirely */
       .shiny-file-input-progress {
          display: none !important;
       }
       /* Style the Browse button */
       .file-area .btn { margin-right: 5px; } /* Add space after button */

      /* Style for the dedicated filename text output */
      .filename-display {
        font-size: 0.9em;
        color: #555;
        flex-grow: 1; /* Allow filename to take remaining space */
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
        margin-left: 6px;
        line-height: 34px; /* Match button height for alignment */
        height: 34px;
      }
      /* --- END MODIFICATION --- */

       /* Style summary output row */
       .summary-row { display: flex; align-items: flex-start; margin-top: 8px;}
       .summary-row .form-group { flex-basis: 180px; flex-shrink: 0; margin-right: 10px; margin-bottom: 0;}
       .summary-row .shiny-text-output { flex-grow: 1; font-size: 0.9em; white-space: pre; overflow-x: auto; }

    "))
  ),
  # Main Layout Divs
  div(class = "fixed-plot-col",
      div(class = "plot-labels-container",
          span(style="font-weight: bold;", textOutput("plot_label1", inline = TRUE)),
          span(style="font-weight: bold;", textOutput("plot_label2", inline = TRUE))
      ),
      plotOutput("va_plot", width = "100%", height = "100%")
  ),
  div(class = "controls-col",
      # --- Selection Controls Panel ---
      wellPanel(
        h4("Feature Selection Controls"),
        fluidRow(
           column(6, numericInput("pval_thresh", "P.value threshold", value = 1, min = 0, max = 1, step = 0.01)),
           column(6, sliderInput("pval_slider", NULL, min = 0, max = 1, value = 1, step = 0.01))
        ),
        fluidRow(
           column(6, numericInput("top_n", "Top N selection (0 = off)", value = 0, min = 0, step = 1)),
           column(6, sliderInput("topn_slider", NULL, min = 0, max = 1000, value = 0, step = 1)) # Fixed max=1000
        ),
        strong(textOutput("overlap_text")),
      ),

      # --- DE results set 1 Panel ---
      wellPanel(
        # Row 1: Heading, Label, Counts
        div(class="row1-container",
            h4("DE results set 1"),
            tags$label("Label:", `for`="label1"),
            div(class="label-input-div form-group", textInput("label1", label=NULL, value = "DE-results-1")),
            div(class="counts-div",
                htmlOutput("fcount1_static", inline=TRUE),
                htmlOutput("fdr_count1_static", inline=TRUE)
            )
        ),
        # --- MODIFIED: Row 2: File Input Button + Dedicated Filename Display ---
        div(class="file-area",
            fileInput("load1", label=NULL, buttonLabel = "Browse...", placeholder = NULL,
                      accept = c(".tsv", ".tab", ".gz", ".qs", ".rds", ".Rdata")),
            # Dedicated text output for filename (styled by CSS)
            tags$span(class="filename-display",   textOutput("fn_display1")
)
        ),
        # --- END MODIFICATION ---

        # Row 3: Summary row
        div(class="summary-row",
             selectInput("summary_metric1", label="Summary:",
                         choices = c("P.Value", "t", "logFC", "adj.P.Val"), selected = "P.Value"),
             verbatimTextOutput("summary1")
         )
      ), # end wellPanel for set 1

      # --- DE results set 2 Panel ---
      wellPanel(
        # Row 1: Heading, Label, Counts
        div(class="row1-container",
            h4("DE results set 2"),
            tags$label("Label:", `for`="label2"),
            div(class="label-input-div form-group", textInput("label2", label=NULL, value = "DE-results-2")),
            div(class="counts-div",
                htmlOutput("fcount2_static", inline=TRUE),
                htmlOutput("fdr_count2_static", inline=TRUE)
            )
        ),
        # --- MODIFIED: Row 2: File Input Button + Dedicated Filename Display ---
        div(class="file-area",
             fileInput("load2", label=NULL, buttonLabel = "Browse...", placeholder = NULL,
                       accept = c(".tsv", ".tab", ".gz", ".qs, .rds", ".Rdata")),
             tags$span(class="filename-display", textOutput("fn_display2"))
        ),
        # --- END MODIFICATION ---

        # Row 3: Summary row
        div(class="summary-row",
             selectInput("summary_metric2", label="Summary:",
                         choices = c("P.Value", "t", "logFC", "adj.P.Val"), selected = "P.Value"),
             verbatimTextOutput("summary2")
         )
      ) # end wellPanel for set 2
  ) # end controls-col div
)
