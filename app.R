################################################################################
# qPCR Cleaner and Analyzer
# Modern UI Design with enhanced user experience
#
# Features:
# - Upload CSV or Excel files with or without headers
# - Automatic data validation and cleaning
# - Flexible replicate handling (3 or 4 replicates)
# - Outlier detection and removal
# - ΔCt calculation with optimal pairing
# - Expression analysis and relative expression
# - Statistical summaries with mean and SD
# - Interactive data visualization
# - Comprehensive error handling
# - Clear user instructions
################################################################################

library(shiny)
library(gtools)
library(readxl)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(shinyjs)
library(shinyWidgets)

# Modern CSS Theme
modern_css <- "
/* ==================== ROOT VARIABLES ==================== */
:root {
  --primary-gradient: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  --secondary-gradient: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
  --success-gradient: linear-gradient(135deg, #11998e 0%, #38ef7d 100%);
  --info-gradient: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);
  --warning-gradient: linear-gradient(135deg, #f7971e 0%, #ffd200 100%);
  --dark-gradient: linear-gradient(135deg, #232526 0%, #414345 100%);

  --primary-color: #667eea;
  --secondary-color: #764ba2;
  --accent-color: #00f2fe;
  --success-color: #38ef7d;
  --warning-color: #ffd200;
  --danger-color: #f5576c;

  --bg-light: #f8fafc;
  --bg-card: #ffffff;
  --text-primary: #1e293b;
  --text-secondary: #64748b;
  --text-muted: #94a3b8;
  --border-color: #e2e8f0;

  --shadow-sm: 0 1px 2px 0 rgb(0 0 0 / 0.05);
  --shadow-md: 0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1);
  --shadow-lg: 0 10px 15px -3px rgb(0 0 0 / 0.1), 0 4px 6px -4px rgb(0 0 0 / 0.1);
  --shadow-xl: 0 20px 25px -5px rgb(0 0 0 / 0.1), 0 8px 10px -6px rgb(0 0 0 / 0.1);

  --radius-sm: 6px;
  --radius-md: 10px;
  --radius-lg: 16px;
  --radius-xl: 24px;
}

/* ==================== GLOBAL STYLES ==================== */
body {
  background: var(--bg-light);
  font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  color: var(--text-primary);
  line-height: 1.6;
}

.container-fluid {
  padding: 0;
  max-width: 100%;
}

/* ==================== HEADER STYLES ==================== */
.app-header {
  background: var(--primary-gradient);
  padding: 2rem 2.5rem;
  margin-bottom: 2rem;
  position: relative;
  overflow: hidden;
}

.app-header::before {
  content: '';
  position: absolute;
  top: -50%;
  right: -10%;
  width: 400px;
  height: 400px;
  background: rgba(255,255,255,0.1);
  border-radius: 50%;
}

.app-header::after {
  content: '';
  position: absolute;
  bottom: -60%;
  left: 10%;
  width: 300px;
  height: 300px;
  background: rgba(255,255,255,0.05);
  border-radius: 50%;
}

.app-title {
  color: white;
  font-size: 2.25rem;
  font-weight: 700;
  margin: 0 0 0.5rem 0;
  position: relative;
  z-index: 1;
  text-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

.app-subtitle {
  color: rgba(255,255,255,0.9);
  font-size: 1.1rem;
  font-weight: 400;
  margin: 0;
  position: relative;
  z-index: 1;
}

.header-badge {
  display: inline-block;
  background: rgba(255,255,255,0.2);
  color: white;
  padding: 0.35rem 1rem;
  border-radius: 50px;
  font-size: 0.8rem;
  font-weight: 500;
  margin-top: 1rem;
  backdrop-filter: blur(10px);
  position: relative;
  z-index: 1;
}

/* ==================== LAYOUT ==================== */
.main-content {
  padding: 0 2rem 2rem 2rem;
}

.row {
  margin: 0 -1rem;
}

.col-sidebar {
  padding: 0 1rem;
}

.col-main {
  padding: 0 1rem;
}

/* ==================== CARD STYLES ==================== */
.modern-card {
  background: var(--bg-card);
  border-radius: var(--radius-lg);
  box-shadow: var(--shadow-md);
  border: 1px solid var(--border-color);
  margin-bottom: 1.5rem;
  overflow: hidden;
  transition: all 0.3s ease;
}

.modern-card:hover {
  box-shadow: var(--shadow-lg);
  transform: translateY(-2px);
}

.card-header {
  background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%);
  padding: 1.25rem 1.5rem;
  border-bottom: 1px solid var(--border-color);
  display: flex;
  align-items: center;
  gap: 0.75rem;
}

.card-header-icon {
  width: 40px;
  height: 40px;
  border-radius: var(--radius-md);
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 1.25rem;
  color: white;
}

.card-header-icon.purple { background: var(--primary-gradient); }
.card-header-icon.teal { background: var(--info-gradient); }
.card-header-icon.green { background: var(--success-gradient); }
.card-header-icon.orange { background: var(--warning-gradient); }

.card-title {
  font-size: 1.1rem;
  font-weight: 600;
  color: var(--text-primary);
  margin: 0;
}

.card-body {
  padding: 1.5rem;
}

/* ==================== SIDEBAR STYLES ==================== */
.sidebar-card {
  background: var(--bg-card);
  border-radius: var(--radius-lg);
  box-shadow: var(--shadow-md);
  border: 1px solid var(--border-color);
  overflow: hidden;
}

.sidebar-section {
  padding: 1.25rem;
  border-bottom: 1px solid var(--border-color);
}

.sidebar-section:last-child {
  border-bottom: none;
}

.section-label {
  font-size: 0.7rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  color: var(--text-muted);
  margin-bottom: 1rem;
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.section-label::after {
  content: '';
  flex: 1;
  height: 1px;
  background: var(--border-color);
}

/* ==================== FORM CONTROLS ==================== */
.form-group {
  margin-bottom: 1rem;
}

.form-group label {
  font-size: 0.875rem;
  font-weight: 500;
  color: var(--text-secondary);
  margin-bottom: 0.5rem;
  display: block;
}

.form-control, .shiny-input-container input[type='text'],
.shiny-input-container input[type='number'] {
  border: 2px solid var(--border-color);
  border-radius: var(--radius-sm);
  padding: 0.625rem 0.875rem;
  font-size: 0.875rem;
  transition: all 0.2s ease;
  background: var(--bg-card);
}

.form-control:focus, .shiny-input-container input:focus {
  border-color: var(--primary-color);
  box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.15);
  outline: none;
}

/* File Input Styling */
.file-input-wrapper {
  position: relative;
}

.shiny-input-container .input-group-btn .btn,
.btn-file {
  background: var(--primary-gradient) !important;
  border: none !important;
  color: white !important;
  border-radius: var(--radius-sm) !important;
  font-weight: 500 !important;
  padding: 0.5rem 1rem !important;
}

.progress {
  height: 6px;
  border-radius: 3px;
  background: var(--border-color);
  margin-top: 0.5rem;
}

.progress-bar {
  background: var(--primary-gradient);
  border-radius: 3px;
}

/* Radio Buttons */
.radio-modern {
  display: flex;
  gap: 0.5rem;
  flex-wrap: wrap;
}

.shiny-input-radiogroup label.radio-inline,
.radio-inline {
  background: var(--bg-light);
  border: 2px solid var(--border-color);
  border-radius: var(--radius-sm);
  padding: 0.5rem 1rem;
  cursor: pointer;
  transition: all 0.2s ease;
  font-weight: 500;
  font-size: 0.875rem;
}

.shiny-input-radiogroup label.radio-inline:hover {
  border-color: var(--primary-color);
  background: rgba(102, 126, 234, 0.05);
}

.shiny-input-radiogroup input[type='radio']:checked + span,
.radio-inline.active {
  background: rgba(102, 126, 234, 0.1);
  border-color: var(--primary-color);
  color: var(--primary-color);
}

/* Checkbox Styling */
.checkbox-modern {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  padding: 0.75rem;
  background: var(--bg-light);
  border-radius: var(--radius-sm);
  cursor: pointer;
  transition: all 0.2s ease;
}

.checkbox-modern:hover {
  background: rgba(102, 126, 234, 0.05);
}

.shiny-input-container .checkbox label,
.checkbox label {
  font-weight: 500;
  color: var(--text-secondary);
  cursor: pointer;
}

/* ==================== BUTTON STYLES ==================== */
.btn-modern {
  border: none;
  border-radius: var(--radius-md);
  padding: 0.875rem 1.5rem;
  font-weight: 600;
  font-size: 0.95rem;
  cursor: pointer;
  transition: all 0.3s ease;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  gap: 0.5rem;
  width: 100%;
  text-transform: none;
  letter-spacing: 0;
}

.btn-primary-gradient {
  background: var(--primary-gradient);
  color: white;
  box-shadow: 0 4px 15px rgba(102, 126, 234, 0.4);
}

.btn-primary-gradient:hover {
  transform: translateY(-2px);
  box-shadow: 0 6px 20px rgba(102, 126, 234, 0.5);
}

.btn-primary-gradient:active {
  transform: translateY(0);
}

.btn-success-gradient {
  background: var(--success-gradient);
  color: white;
  box-shadow: 0 4px 15px rgba(17, 153, 142, 0.4);
}

.btn-success-gradient:hover {
  transform: translateY(-2px);
  box-shadow: 0 6px 20px rgba(17, 153, 142, 0.5);
}

.btn-outline {
  background: transparent;
  border: 2px solid var(--border-color);
  color: var(--text-secondary);
}

.btn-outline:hover {
  border-color: var(--primary-color);
  color: var(--primary-color);
  background: rgba(102, 126, 234, 0.05);
}

.btn-icon {
  font-size: 1.1rem;
}

/* ==================== TAB STYLES ==================== */
.nav-tabs-modern {
  border: none;
  background: var(--bg-light);
  padding: 0.5rem;
  border-radius: var(--radius-lg);
  display: flex;
  gap: 0.5rem;
  margin-bottom: 1.5rem;
}

.nav-tabs > li > a {
  border: none !important;
  border-radius: var(--radius-md) !important;
  padding: 0.875rem 1.25rem !important;
  font-weight: 500 !important;
  color: var(--text-secondary) !important;
  background: transparent !important;
  transition: all 0.2s ease !important;
  margin: 0 !important;
}

.nav-tabs > li > a:hover {
  background: rgba(102, 126, 234, 0.1) !important;
  color: var(--primary-color) !important;
}

.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover,
.nav-tabs > li.active > a:focus {
  background: var(--primary-gradient) !important;
  color: white !important;
  box-shadow: var(--shadow-md) !important;
}

/* Pills for nested tabs */
.nav-pills > li > a {
  border-radius: var(--radius-sm) !important;
  padding: 0.5rem 1rem !important;
  font-size: 0.875rem !important;
  font-weight: 500 !important;
  color: var(--text-secondary) !important;
  background: var(--bg-light) !important;
  margin-right: 0.5rem !important;
  border: 1px solid var(--border-color) !important;
}

.nav-pills > li.active > a {
  background: var(--primary-color) !important;
  color: white !important;
  border-color: var(--primary-color) !important;
}

/* ==================== ALERT STYLES ==================== */
.alert-modern {
  border: none;
  border-radius: var(--radius-md);
  padding: 1rem 1.25rem;
  display: flex;
  align-items: flex-start;
  gap: 0.75rem;
  font-size: 0.9rem;
}

.alert-info-modern {
  background: linear-gradient(135deg, rgba(79, 172, 254, 0.1) 0%, rgba(0, 242, 254, 0.1) 100%);
  color: #0369a1;
  border-left: 4px solid var(--accent-color);
}

.alert-warning-modern {
  background: linear-gradient(135deg, rgba(247, 151, 30, 0.1) 0%, rgba(255, 210, 0, 0.1) 100%);
  color: #92400e;
  border-left: 4px solid var(--warning-color);
}

.alert-success-modern {
  background: linear-gradient(135deg, rgba(17, 153, 142, 0.1) 0%, rgba(56, 239, 125, 0.1) 100%);
  color: #065f46;
  border-left: 4px solid var(--success-color);
}

.alert-icon {
  font-size: 1.25rem;
  flex-shrink: 0;
}

/* ==================== TABLE STYLES ==================== */
.dataTables_wrapper {
  font-size: 0.875rem;
}

table.dataTable {
  border-collapse: collapse !important;
}

table.dataTable thead th {
  background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%);
  border-bottom: 2px solid var(--primary-color) !important;
  color: var(--text-primary);
  font-weight: 600;
  padding: 1rem !important;
  text-transform: uppercase;
  font-size: 0.75rem;
  letter-spacing: 0.5px;
}

table.dataTable tbody td {
  padding: 0.875rem 1rem !important;
  border-bottom: 1px solid var(--border-color) !important;
  vertical-align: middle;
}

table.dataTable tbody tr:hover {
  background: rgba(102, 126, 234, 0.05) !important;
}

.dataTables_filter input {
  border: 2px solid var(--border-color) !important;
  border-radius: var(--radius-sm) !important;
  padding: 0.5rem 0.75rem !important;
}

.dataTables_filter input:focus {
  border-color: var(--primary-color) !important;
  outline: none !important;
}

/* ==================== STATS CARDS ==================== */
.stat-card {
  background: var(--bg-card);
  border-radius: var(--radius-md);
  padding: 1.25rem;
  border: 1px solid var(--border-color);
  text-align: center;
  transition: all 0.3s ease;
}

.stat-card:hover {
  transform: translateY(-3px);
  box-shadow: var(--shadow-lg);
}

.stat-value {
  font-size: 1.75rem;
  font-weight: 700;
  color: var(--primary-color);
  margin-bottom: 0.25rem;
}

.stat-label {
  font-size: 0.8rem;
  color: var(--text-muted);
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

/* ==================== LOADING SPINNER ==================== */
.loading-spinner {
  display: inline-block;
  width: 20px;
  height: 20px;
  border: 3px solid rgba(102, 126, 234, 0.3);
  border-radius: 50%;
  border-top-color: var(--primary-color);
  animation: spin 1s ease-in-out infinite;
}

@keyframes spin {
  to { transform: rotate(360deg); }
}

.loading-overlay {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: rgba(255,255,255,0.9);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 9999;
  backdrop-filter: blur(5px);
}

.loading-content {
  text-align: center;
}

.loading-spinner-lg {
  width: 50px;
  height: 50px;
  border-width: 4px;
  margin-bottom: 1rem;
}

/* ==================== PLOT CONTAINER ==================== */
.plot-container {
  background: var(--bg-light);
  border-radius: var(--radius-md);
  padding: 1rem;
  border: 1px solid var(--border-color);
}

/* ==================== WELL PANEL OVERRIDE ==================== */
.well {
  background: var(--bg-card);
  border: 1px solid var(--border-color);
  border-radius: var(--radius-lg);
  box-shadow: var(--shadow-sm);
  padding: 1.5rem;
}

/* ==================== FILE INFO DISPLAY ==================== */
.file-info {
  background: linear-gradient(135deg, rgba(102, 126, 234, 0.1) 0%, rgba(118, 75, 162, 0.1) 100%);
  border-radius: var(--radius-md);
  padding: 1rem 1.25rem;
  border-left: 4px solid var(--primary-color);
}

.file-info-item {
  display: flex;
  justify-content: space-between;
  padding: 0.5rem 0;
  border-bottom: 1px dashed rgba(0,0,0,0.1);
}

.file-info-item:last-child {
  border-bottom: none;
}

.file-info-label {
  color: var(--text-secondary);
  font-size: 0.875rem;
}

.file-info-value {
  font-weight: 600;
  color: var(--text-primary);
}

/* ==================== RESPONSIVE ==================== */
@media (max-width: 768px) {
  .app-header {
    padding: 1.5rem;
  }

  .app-title {
    font-size: 1.5rem;
  }

  .main-content {
    padding: 0 1rem 1rem 1rem;
  }

  .nav-tabs > li > a {
    padding: 0.625rem 0.875rem !important;
    font-size: 0.8rem !important;
  }
}

/* ==================== SHINY NOTIFICATION OVERRIDE ==================== */
.shiny-notification {
  border-radius: var(--radius-md);
  border: none;
  box-shadow: var(--shadow-lg);
}

.shiny-notification-message {
  background: var(--info-gradient);
  color: white;
}

.shiny-notification-warning {
  background: var(--warning-gradient);
  color: #1e293b;
}

.shiny-notification-error {
  background: var(--secondary-gradient);
  color: white;
}

/* ==================== INSTRUCTIONS LIST ==================== */
.instruction-list {
  list-style: none;
  padding: 0;
  margin: 0;
}

.instruction-list li {
  padding: 0.75rem 0;
  padding-left: 2rem;
  position: relative;
  border-bottom: 1px solid var(--border-color);
}

.instruction-list li:last-child {
  border-bottom: none;
}

.instruction-list li::before {
  content: '';
  position: absolute;
  left: 0;
  top: 1rem;
  width: 8px;
  height: 8px;
  background: var(--primary-gradient);
  border-radius: 50%;
}

/* ==================== SECTION HEADERS ==================== */
.section-header {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  margin-bottom: 1rem;
  padding-bottom: 0.75rem;
  border-bottom: 2px solid var(--border-color);
}

.section-header-icon {
  width: 36px;
  height: 36px;
  border-radius: var(--radius-sm);
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 1rem;
  color: white;
  background: var(--primary-gradient);
}

.section-header-text {
  font-size: 1.1rem;
  font-weight: 600;
  color: var(--text-primary);
  margin: 0;
}

/* ==================== SUMMARY TABLE ==================== */
.summary-table {
  width: 100%;
  border-collapse: collapse;
}

.summary-table th,
.summary-table td {
  padding: 0.75rem;
  text-align: left;
  border-bottom: 1px solid var(--border-color);
}

.summary-table th {
  background: var(--bg-light);
  font-weight: 600;
  font-size: 0.8rem;
  text-transform: uppercase;
  color: var(--text-secondary);
}

.summary-table tbody tr:hover {
  background: rgba(102, 126, 234, 0.05);
}
"

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML(modern_css)),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap", rel = "stylesheet")
  ),

  # Modern Header
  div(class = "app-header",
    h1(class = "app-title", "qPCR Cleaner & Analyzer"),
    p(class = "app-subtitle", "Professional qPCR data processing with advanced statistical analysis"),
    div(class = "header-badge", "v1.0 • Powered by R/Shiny")
  ),

  # Main Content
  div(class = "main-content",
    fluidRow(
      # Sidebar
      column(3, class = "col-sidebar",
        div(class = "sidebar-card",

          # File Upload Section
          div(class = "sidebar-section",
            div(class = "section-label", "Data Upload"),
            fileInput("file",
                     label = NULL,
                     accept = c(".csv", ".xlsx", ".xls"),
                     placeholder = "Choose CSV or Excel file...",
                     buttonLabel = "Browse"),
            checkboxInput("has_header", "File contains column headers", value = FALSE)
          ),

          # Analysis Settings Section
          div(class = "sidebar-section",
            div(class = "section-label", "Analysis Settings"),

            tags$label("Replicates per gene", style = "font-size: 0.875rem; font-weight: 500; color: #64748b; margin-bottom: 0.5rem; display: block;"),
            radioButtons("reps",
                        label = NULL,
                        choices = c("3 replicates" = 3, "4 replicates" = 4),
                        selected = 3,
                        inline = TRUE),

            div(style = "margin-top: 1rem;",
              checkboxInput("remove_outliers",
                           "Remove outliers (4 replicates only)",
                           value = TRUE)
            ),

            div(style = "margin-top: 0.75rem;",
              numericInput("outlier_threshold",
                          "Outlier threshold (SD):",
                          value = 2.0,
                          min = 1.0,
                          max = 5.0,
                          step = 0.1)
            )
          ),

          # Action Buttons Section
          div(class = "sidebar-section",
            div(class = "section-label", "Actions"),

            actionButton("analyze",
                        HTML("<span class='btn-icon'>&#9654;</span> Run Analysis"),
                        class = "btn-modern btn-primary-gradient",
                        style = "margin-bottom: 0.75rem;"),

            actionButton("reset",
                        HTML("<span class='btn-icon'>&#8635;</span> Reset All"),
                        class = "btn-modern btn-outline"),

            conditionalPanel(
              condition = "output.analysis_complete",
              div(style = "margin-top: 0.75rem;",
                downloadButton("downloadData",
                              HTML("<span class='btn-icon'>&#8681;</span> Download Results"),
                              class = "btn-modern btn-success-gradient")
              )
            )
          )
        )
      ),

      # Main Panel
      column(9, class = "col-main",
        tabsetPanel(
          id = "main_tabs",
          type = "tabs",

          # Instructions Tab
          tabPanel(
            title = "Instructions",
            icon = icon("book-open"),
            div(class = "modern-card", style = "margin-top: 1rem;",
              div(class = "card-header",
                div(class = "card-header-icon purple", HTML("&#128218;")),
                h3(class = "card-title", "Getting Started Guide")
              ),
              div(class = "card-body",

                div(class = "section-header",
                  div(class = "section-header-icon", HTML("&#128196;")),
                  h4(class = "section-header-text", "Data Format Requirements")
                ),
                tags$ul(class = "instruction-list",
                  tags$li("Upload CSV or Excel files containing your qPCR Ct values"),
                  tags$li("Organize data with genes in columns and samples in rows"),
                  tags$li("Place your reference/control gene in the first columns"),
                  tags$li("Missing values can be marked as 'Undetermined', '#VALUE!', or left blank"),
                  tags$li("Select whether your file includes column headers")
                ),

                div(class = "section-header", style = "margin-top: 1.5rem;",
                  div(class = "section-header-icon", style = "background: var(--info-gradient);", HTML("&#9881;")),
                  h4(class = "section-header-text", "Analysis Options")
                ),
                tags$ul(class = "instruction-list",
                  tags$li("Choose the number of technical replicates per gene (3 or 4)"),
                  tags$li("For 4 replicates, enable automatic outlier detection and removal"),
                  tags$li("Adjust the outlier detection sensitivity threshold as needed")
                ),

                div(class = "section-header", style = "margin-top: 1.5rem;",
                  div(class = "section-header-icon", style = "background: var(--success-gradient);", HTML("&#128202;")),
                  h4(class = "section-header-text", "Output Information")
                ),
                tags$ul(class = "instruction-list",
                  tags$li("Cleaned Ct values with outliers removed"),
                  tags$li(HTML("&Delta;Ct values calculated using optimal replicate pairing")),
                  tags$li(HTML("Expression values (2<sup>&Delta;Ct</sup>)")),
                  tags$li("Relative expression compared to control sample"),
                  tags$li(HTML("Statistical summaries (mean &plusmn; SD)"))
                ),

                div(class = "alert-modern alert-info-modern", style = "margin-top: 1.5rem;",
                  div(class = "alert-icon", HTML("&#128161;")),
                  div(
                    tags$strong("Pro Tip: "),
                    "Ensure your reference gene shows stable expression across all samples for accurate normalization."
                  )
                )
              )
            )
          ),

          # Data Preview Tab
          tabPanel(
            title = "Data Preview",
            icon = icon("table"),
            div(class = "modern-card", style = "margin-top: 1rem;",
              div(class = "card-header",
                div(class = "card-header-icon teal", HTML("&#128203;")),
                h3(class = "card-title", "Raw Data Preview")
              ),
              div(class = "card-body",

                conditionalPanel(
                  condition = "!output.file_uploaded",
                  div(class = "alert-modern alert-info-modern",
                    div(class = "alert-icon", HTML("&#128193;")),
                    div(
                      tags$strong("No file uploaded"),
                      tags$br(),
                      "Upload a CSV or Excel file using the sidebar to preview your data."
                    )
                  )
                ),

                conditionalPanel(
                  condition = "output.file_uploaded",

                  uiOutput("file_info_ui"),

                  conditionalPanel(
                    condition = "output.has_outliers",
                    div(class = "alert-modern alert-warning-modern", style = "margin-bottom: 1rem;",
                      div(class = "alert-icon", HTML("&#9888;")),
                      div(
                        tags$strong("Outliers Detected"),
                        tags$br(),
                        "Potential outliers found in your data. Consider enabling outlier removal in settings."
                      )
                    )
                  ),

                  div(class = "section-header",
                    div(class = "section-header-icon", style = "background: var(--info-gradient);", HTML("&#128200;")),
                    h4(class = "section-header-text", "First 10 Rows")
                  ),
                  DTOutput("raw_data_table")
                )
              )
            )
          ),

          # Analysis Results Tab
          tabPanel(
            title = "Analysis Results",
            icon = icon("chart-line"),

            conditionalPanel(
              condition = "!output.analysis_complete",
              div(class = "modern-card", style = "margin-top: 1rem;",
                div(class = "card-body",
                  div(class = "alert-modern alert-info-modern",
                    div(class = "alert-icon", HTML("&#9654;")),
                    div(
                      tags$strong("Ready to Analyze"),
                      tags$br(),
                      "Upload your data and click 'Run Analysis' to process and view results."
                    )
                  )
                )
              )
            ),

            conditionalPanel(
              condition = "output.analysis_complete",

              # Summary Statistics Card
              div(class = "modern-card", style = "margin-top: 1rem;",
                div(class = "card-header",
                  div(class = "card-header-icon green", HTML("&#128202;")),
                  h3(class = "card-title", "Summary Statistics")
                ),
                div(class = "card-body",
                  fluidRow(
                    column(6,
                      div(class = "section-header",
                        h4(class = "section-header-text", style = "font-size: 0.95rem;", "Expression Analysis")
                      ),
                      tableOutput("expr_summary_table")
                    ),
                    column(6,
                      div(class = "section-header",
                        h4(class = "section-header-text", style = "font-size: 0.95rem;", "Relative Expression Analysis")
                      ),
                      tableOutput("rel_summary_table")
                    )
                  )
                )
              ),

              # Detailed Results Card
              div(class = "modern-card",
                div(class = "card-header",
                  div(class = "card-header-icon purple", HTML("&#128269;")),
                  h3(class = "card-title", "Detailed Results")
                ),
                div(class = "card-body",
                  tabsetPanel(
                    type = "pills",
                    tabPanel("Cleaned Ct", DTOutput("ct_table"), style = "padding-top: 1rem;"),
                    tabPanel(HTML("&Delta;Ct Values"), DTOutput("dct_table"), style = "padding-top: 1rem;"),
                    tabPanel("Expression", DTOutput("expr_table"), style = "padding-top: 1rem;"),
                    tabPanel("Relative Expr.", DTOutput("rel_table"), style = "padding-top: 1rem;")
                  )
                )
              ),

              # Visualizations Card
              div(class = "modern-card",
                div(class = "card-header",
                  div(class = "card-header-icon orange", HTML("&#128200;")),
                  h3(class = "card-title", "Data Visualizations")
                ),
                div(class = "card-body",
                  fluidRow(
                    column(6,
                      div(class = "plot-container",
                        plotOutput("ct_boxplot", height = "320px")
                      )
                    ),
                    column(6,
                      div(class = "plot-container",
                        plotOutput("expr_boxplot", height = "320px")
                      )
                    )
                  ),
                  fluidRow(style = "margin-top: 1rem;",
                    column(12,
                      div(class = "plot-container",
                        plotOutput("rel_heatmap", height = "400px")
                      )
                    )
                  )
                )
              )
            )
          ),

          # Quality Control Tab
          tabPanel(
            title = "Quality Control",
            icon = icon("microscope"),

            conditionalPanel(
              condition = "!output.analysis_complete",
              div(class = "modern-card", style = "margin-top: 1rem;",
                div(class = "card-body",
                  div(class = "alert-modern alert-info-modern",
                    div(class = "alert-icon", HTML("&#128300;")),
                    div(
                      tags$strong("QC Metrics Pending"),
                      tags$br(),
                      "Run analysis to generate quality control metrics and visualizations."
                    )
                  )
                )
              )
            ),

            conditionalPanel(
              condition = "output.analysis_complete",

              div(class = "modern-card", style = "margin-top: 1rem;",
                div(class = "card-header",
                  div(class = "card-header-icon teal", HTML("&#128300;")),
                  h3(class = "card-title", "Quality Control Metrics")
                ),
                div(class = "card-body",
                  fluidRow(
                    column(6,
                      div(class = "section-header",
                        h4(class = "section-header-text", style = "font-size: 0.95rem;", "Data Quality Summary")
                      ),
                      tableOutput("qc_summary")
                    ),
                    column(6,
                      div(class = "section-header",
                        h4(class = "section-header-text", style = "font-size: 0.95rem;", "Outlier Detection Results")
                      ),
                      tableOutput("outlier_summary")
                    )
                  ),

                  hr(style = "margin: 1.5rem 0; border-color: var(--border-color);"),

                  div(class = "section-header",
                    div(class = "section-header-icon", style = "background: var(--warning-gradient);", HTML("&#128201;")),
                    h4(class = "section-header-text", "Replicate Variability (CV%)")
                  ),
                  div(class = "plot-container",
                    plotOutput("cv_plot", height = "320px")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Reactive values to store data
  rv <- reactiveValues(
    raw_data = NULL,
    cleaned_data = NULL,
    dct_data = NULL,
    expr_data = NULL,
    rel_data = NULL,
    csv_data = NULL,
    analysis_complete = FALSE,
    file_uploaded = FALSE,
    has_outliers = FALSE,
    qc_metrics = NULL,
    outlier_info = NULL
  )

  # File upload observer
  observeEvent(input$file, {
    req(input$file)

    tryCatch({
      file_path <- input$file$datapath
      file_name <- input$file$name

      # Read file based on extension
      if (grepl("\\.xlsx?$", file_name, ignore.case = TRUE)) {
        raw <- read_excel(file_path,
                          col_names = input$has_header,
                          na = c("Undetermined", "#VALUE!", "NA", ""))
      } else {
        raw <- read.csv(file_path,
                        header = input$has_header,
                        stringsAsFactors = FALSE,
                        na.strings = c("Undetermined", "#VALUE!", "NA", ""))
      }

      # Clean data
      raw <- raw[, colSums(is.na(raw)) < nrow(raw), drop = FALSE]
      raw <- raw[rowSums(is.na(raw)) < ncol(raw), , drop = FALSE]

      if (ncol(raw) == 0 || nrow(raw) == 0) {
        showNotification("Uploaded file has no usable data after cleaning.", type = "error")
        rv$file_uploaded <- FALSE
        return()
      }

      # Convert to numeric
      raw_numeric <- as.data.frame(lapply(raw, function(x) {
        if (is.numeric(x)) x else as.numeric(as.character(x))
      }))

      rv$raw_data <- raw_numeric
      rv$file_uploaded <- TRUE
      rv$analysis_complete <- FALSE

      # Check for outliers
      rv$has_outliers <- check_for_outliers(raw_numeric, input$reps)

      showNotification("File uploaded successfully!", type = "message", duration = 3)

    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      rv$file_uploaded <- FALSE
    })
  })

  # Analysis button observer
  observeEvent(input$analyze, {
    req(rv$raw_data, input$file)

    withProgress(message = "Processing your data...", value = 0, {

      tryCatch({

        incProgress(0.1, detail = "Cleaning and validating...")

        # Step 1: Clean data and remove outliers
        cleaned <- clean_data(rv$raw_data, input$reps, input$remove_outliers, input$outlier_threshold)
        rv$cleaned_data <- cleaned$data
        rv$outlier_info <- cleaned$outlier_info

        incProgress(0.3, detail = "Computing delta Ct values...")

        # Step 2: Compute delta Ct
        dct_result <- compute_dct(rv$cleaned_data, input$reps)
        rv$dct_data <- dct_result$dct_data

        incProgress(0.5, detail = "Calculating expression...")

        # Step 3: Compute expression values
        rv$expr_data <- 2 ^ rv$dct_data

        incProgress(0.7, detail = "Computing relative expression...")

        # Step 4: Compute relative expression
        rv$rel_data <- compute_relative_expression(rv$expr_data, input$reps)

        incProgress(0.9, detail = "Generating reports...")

        # Step 5: Prepare CSV output
        rv$csv_data <- prepare_csv_output(rv$cleaned_data, rv$dct_data, rv$expr_data, rv$rel_data, input$reps)

        # Step 6: Calculate QC metrics
        rv$qc_metrics <- calculate_qc_metrics(rv$cleaned_data, rv$expr_data, rv$rel_data, input$reps)

        rv$analysis_complete <- TRUE

        incProgress(1, detail = "Complete!")

        showNotification("Analysis completed successfully!", type = "message", duration = 4)

        # Switch to results tab
        updateTabsetPanel(session, "main_tabs", selected = "Analysis Results")

      }, error = function(e) {
        showNotification(paste("Analysis error:", e$message), type = "error")
      })
    })
  })

  # Reset button observer
  observeEvent(input$reset, {
    rv$raw_data <- NULL
    rv$cleaned_data <- NULL
    rv$dct_data <- NULL
    rv$expr_data <- NULL
    rv$rel_data <- NULL
    rv$csv_data <- NULL
    rv$analysis_complete <- FALSE
    rv$file_uploaded <- FALSE
    rv$has_outliers <- FALSE
    rv$qc_metrics <- NULL
    rv$outlier_info <- NULL

    reset("file")
    updateTabsetPanel(session, "main_tabs", selected = "Instructions")

    showNotification("Application reset complete", type = "message", duration = 3)
  })

  # Output functions
  output$file_uploaded <- reactive(rv$file_uploaded)
  output$analysis_complete <- reactive(rv$analysis_complete)
  output$has_outliers <- reactive(rv$has_outliers)

  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
  outputOptions(output, "analysis_complete", suspendWhenHidden = FALSE)
  outputOptions(output, "has_outliers", suspendWhenHidden = FALSE)

  output$file_info_ui <- renderUI({
    req(rv$raw_data)
    div(class = "file-info", style = "margin-bottom: 1.5rem;",
      div(class = "file-info-item",
        span(class = "file-info-label", "Rows (Samples)"),
        span(class = "file-info-value", nrow(rv$raw_data))
      ),
      div(class = "file-info-item",
        span(class = "file-info-label", "Columns"),
        span(class = "file-info-value", ncol(rv$raw_data))
      ),
      div(class = "file-info-item",
        span(class = "file-info-label", "Estimated Genes"),
        span(class = "file-info-value", floor(ncol(rv$raw_data) / as.numeric(input$reps)))
      )
    )
  })

  output$raw_data_table <- renderDT({
    req(rv$raw_data)
    datatable(
      head(rv$raw_data, 10),
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        dom = 't',
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })

  output$ct_table <- renderDT({
    req(rv$cleaned_data)
    datatable(
      round(rv$cleaned_data, 3),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
      ),
      caption = "Cleaned Ct Values (outliers removed)",
      class = 'cell-border stripe'
    )
  })

  output$dct_table <- renderDT({
    req(rv$dct_data)
    datatable(
      round(rv$dct_data, 3),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
      ),
      caption = "Delta Ct Values",
      class = 'cell-border stripe'
    )
  })

  output$expr_table <- renderDT({
    req(rv$expr_data)
    datatable(
      round(rv$expr_data, 6),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
      ),
      caption = "Expression Values (2^deltaCt)",
      class = 'cell-border stripe'
    )
  })

  output$rel_table <- renderDT({
    req(rv$rel_data)
    datatable(
      round(rv$rel_data, 6),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = '_all'))
      ),
      caption = "Relative Expression vs. Control",
      class = 'cell-border stripe'
    )
  })

  output$expr_summary_table <- renderTable({
    req(rv$qc_metrics)
    rv$qc_metrics$expr_summary
  }, rownames = TRUE, digits = 6, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$rel_summary_table <- renderTable({
    req(rv$qc_metrics)
    rv$qc_metrics$rel_summary
  }, rownames = TRUE, digits = 6, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$qc_summary <- renderTable({
    req(rv$qc_metrics)
    rv$qc_metrics$qc_summary
  }, rownames = FALSE, digits = 3, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$outlier_summary <- renderTable({
    req(rv$outlier_info)
    if (nrow(rv$outlier_info) == 0) {
      data.frame(Status = "No outliers detected or outlier removal disabled")
    } else {
      rv$outlier_info
    }
  }, rownames = FALSE, digits = 0, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Plots with improved styling
  output$ct_boxplot <- renderPlot({
    req(rv$cleaned_data)
    plot_ct_boxplot(rv$cleaned_data, input$reps)
  }, res = 100)

  output$expr_boxplot <- renderPlot({
    req(rv$expr_data)
    plot_expr_boxplot(rv$expr_data, input$reps)
  }, res = 100)

  output$rel_heatmap <- renderPlot({
    req(rv$rel_data)
    plot_rel_heatmap(rv$rel_data, input$reps)
  }, res = 100)

  output$cv_plot <- renderPlot({
    req(rv$qc_metrics)
    plot_cv_plot(rv$qc_metrics$cv_data, input$reps)
  }, res = 100)

  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("qpcr_analysis_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.table(rv$csv_data,
                  file = file,
                  sep = ",",
                  col.names = FALSE,
                  row.names = FALSE,
                  quote = FALSE,
                  na = "")
    }
  )

  # ==================== HELPER FUNCTIONS ====================

  check_for_outliers <- function(data, reps) {
    if (reps != 4) return(FALSE)

    n_genes <- ncol(data) / reps
    has_outliers <- FALSE

    for (g in 1:n_genes) {
      start_col <- (g - 1) * reps + 1
      end_col <- g * reps

      for (row in 1:nrow(data)) {
        block <- as.numeric(data[row, start_col:end_col])
        if (sum(!is.na(block)) >= 3) {
          mean_val <- mean(block, na.rm = TRUE)
          sd_val <- sd(block, na.rm = TRUE)
          if (any(abs(block - mean_val) > 2 * sd_val, na.rm = TRUE)) {
            has_outliers <- TRUE
            break
          }
        }
      }
      if (has_outliers) break
    }

    return(has_outliers)
  }

  clean_data <- function(raw_data, reps, remove_outliers, threshold) {
    cleaned <- raw_data
    outlier_info <- data.frame(
      Gene = character(),
      Sample = integer(),
      Outliers_Removed = integer(),
      stringsAsFactors = FALSE
    )

    if (remove_outliers && reps == 4) {
      n_genes <- ncol(raw_data) / reps

      for (g in 1:n_genes) {
        start_col <- (g - 1) * reps + 1
        end_col <- g * reps

        for (row in 1:nrow(raw_data)) {
          block <- as.numeric(raw_data[row, start_col:end_col])

          if (sum(!is.na(block)) >= 3) {
            mean_val <- mean(block, na.rm = TRUE)
            sd_val <- sd(block, na.rm = TRUE)

            outliers <- which(abs(block - mean_val) > threshold * sd_val)

            if (length(outliers) > 0 && length(outliers) < length(block)) {
              # Remove the most extreme outlier
              extreme_outlier <- outliers[which.max(abs(block[outliers] - mean_val))]
              cleaned[row, start_col + extreme_outlier - 1] <- NA

              outlier_info <- rbind(outlier_info, data.frame(
                Gene = paste0("Gene", g),
                Sample = row,
                Outliers_Removed = 1,
                stringsAsFactors = FALSE
              ))
            }
          }
        }
      }
    }

    return(list(data = cleaned, outlier_info = outlier_info))
  }

  compute_dct <- function(cleaned_data, reps) {
    n_reps <- reps
    n_genes <- ncol(cleaned_data) / n_reps

    gene_indices <- lapply(0:(n_genes-1), function(i) {
      start <- i * n_reps + 1
      end <- start + n_reps - 1
      start:end
    })

    compute_dCt_optimal <- function(control_vals, target_vals) {
      if (all(is.na(control_vals)) || all(is.na(target_vals))) {
        return(rep(NA, length(target_vals)))
      }

      # Simple pairing if not enough data for permutations
      if (sum(!is.na(target_vals)) < 2) {
        dCt <- control_vals - target_vals
        dCt[is.na(control_vals) | is.na(target_vals)] <- NA
        return(dCt)
      }

      # Try permutations for optimal pairing
      tryCatch({
        perms <- permutations(n = length(target_vals),
                             r = length(target_vals),
                             v = target_vals)

        best_dCt <- NULL
        min_sd <- Inf

        for (i in 1:nrow(perms)) {
          pair <- perms[i, ]
          dCt <- control_vals - pair
          dCt[is.na(control_vals) | is.na(pair)] <- NA

          if (sum(!is.na(dCt)) >= 2) {
            current_sd <- sd(dCt, na.rm = TRUE)
            if (current_sd < min_sd) {
              min_sd <- current_sd
              best_dCt <- dCt
            }
          }
        }

        if (!is.null(best_dCt)) {
          return(best_dCt)
        }
      }, error = function(e) {
        # Fall back to simple pairing
      })

      # Fallback: simple pairing
      dCt <- control_vals - target_vals
      dCt[is.na(control_vals) | is.na(target_vals)] <- NA
      return(dCt)
    }

    dCt_all <- list()

    for (row in 1:nrow(cleaned_data)) {
      row_vals <- as.numeric(cleaned_data[row, ])
      ref_vals <- row_vals[gene_indices[[1]]]
      dCt_row <- c()

      for (g in 2:n_genes) {
        target_vals <- row_vals[gene_indices[[g]]]
        dCt_vals <- compute_dCt_optimal(ref_vals, target_vals)
        dCt_row <- c(dCt_row, dCt_vals)
      }

      dCt_row <- c(rep(NA, n_reps), dCt_row)
      dCt_all[[row]] <- dCt_row
    }

    dCt_df <- as.data.frame(do.call(rbind, dCt_all))

    return(list(dct_data = dCt_df))
  }

  compute_relative_expression <- function(expr_data, reps) {
    control_expr <- as.numeric(expr_data[1, ])

    compute_relative_optimal <- function(control_vals, treat_vals) {
      if (all(is.na(control_vals)) || all(is.na(treat_vals))) {
        return(rep(NA, length(treat_vals)))
      }

      # Simple division if not enough data
      if (sum(!is.na(treat_vals)) < 2) {
        rel_vals <- treat_vals / control_vals
        rel_vals[is.na(treat_vals) | is.na(control_vals)] <- NA
        return(rel_vals)
      }

      # Try permutations for optimal pairing
      tryCatch({
        perms <- permutations(n = length(control_vals),
                             r = length(control_vals),
                             v = control_vals)

        best_rel <- NULL
        min_sd <- Inf

        for (i in 1:nrow(perms)) {
          perm_ctrl <- perms[i, ]
          rel_vals <- treat_vals / perm_ctrl
          rel_vals[is.na(treat_vals) | is.na(perm_ctrl)] <- NA

          if (sum(!is.na(rel_vals)) >= 2) {
            sd_val <- sd(rel_vals, na.rm = TRUE)
            if (sd_val < min_sd) {
              min_sd <- sd_val
              best_rel <- rel_vals
            }
          }
        }

        if (!is.null(best_rel)) {
          return(best_rel)
        }
      }, error = function(e) {
        # Fall back to simple division
      })

      # Fallback: simple division
      rel_vals <- treat_vals / control_vals
      rel_vals[is.na(treat_vals) | is.na(control_vals)] <- NA
      return(rel_vals)
    }

    rel_all <- list()

    for (row in 1:nrow(expr_data)) {
      if (row == 1) {
        rel_all[[row]] <- rep(1, ncol(expr_data))
      } else {
        treat_vals <- as.numeric(expr_data[row, ])
        rel_vals <- compute_relative_optimal(control_expr, treat_vals)
        rel_all[[row]] <- rel_vals
      }
    }

    rel_df <- as.data.frame(do.call(rbind, rel_all))
    return(rel_df)
  }

  calculate_qc_metrics <- function(cleaned_data, expr_data, rel_data, reps) {
    n_genes <- ncol(cleaned_data) / reps

    # Calculate means and SDs
    calc_summary <- function(data, n_reps) {
      means <- list()
      sds <- list()
      cvs <- list()

      for (g in 1:n_genes) {
        start_col <- (g - 1) * n_reps + 1
        end_col <- g * n_reps

        block_means <- apply(data[, start_col:end_col], 1, function(x) {
          if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
        })

        block_sds <- apply(data[, start_col:end_col], 1, function(x) {
          if (all(is.na(x))) NA else sd(x, na.rm = TRUE)
        })

        block_cvs <- block_sds / block_means * 100

        means[[g]] <- block_means
        sds[[g]] <- block_sds
        cvs[[g]] <- block_cvs
      }

      return(list(
        means = do.call(cbind, means),
        sds = do.call(cbind, sds),
        cvs = do.call(cbind, cvs)
      ))
    }

    expr_summary <- calc_summary(expr_data, reps)
    rel_summary <- calc_summary(rel_data, reps)

    # Create summary tables
    expr_summary_table <- data.frame(
      Gene = paste0("Gene", 1:n_genes),
      Mean = colMeans(expr_summary$means, na.rm = TRUE),
      SD = colMeans(expr_summary$sds, na.rm = TRUE),
      CV_percent = colMeans(expr_summary$cvs, na.rm = TRUE)
    )

    rel_summary_table <- data.frame(
      Gene = paste0("Gene", 1:n_genes),
      Mean = colMeans(rel_summary$means, na.rm = TRUE),
      SD = colMeans(rel_summary$sds, na.rm = TRUE),
      CV_percent = colMeans(rel_summary$cvs, na.rm = TRUE)
    )

    # QC summary
    qc_summary <- data.frame(
      Metric = c("Total Samples", "Total Genes", "Missing Values (%)", "Mean CV (%)"),
      Value = c(
        nrow(cleaned_data),
        n_genes,
        round(sum(is.na(cleaned_data)) / length(as.matrix(cleaned_data)) * 100, 1),
        round(mean(colMeans(expr_summary$cvs, na.rm = TRUE), na.rm = TRUE), 1)
      )
    )

    return(list(
      expr_summary = expr_summary_table,
      rel_summary = rel_summary_table,
      qc_summary = qc_summary,
      cv_data = expr_summary$cvs
    ))
  }

  prepare_csv_output <- function(cleaned_data, dct_data, expr_data, rel_data, reps) {
    round_data_frame <- function(df, digits = 6) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(data.frame())
      }
      df[] <- lapply(df, function(x) {
        if (is.numeric(x)) round(x, digits) else x
      })
      return(df)
    }

    clean_csv_block <- function(df) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(matrix("", nrow = 1, ncol = 1))
      }
      mat <- as.matrix(df)
      mat[is.na(mat)] <- "#VALUE!"
      return(apply(mat, c(1,2), as.character))
    }

    # Prepare all data blocks
    ct_block <- clean_csv_block(round_data_frame(cleaned_data))
    dct_block <- clean_csv_block(round_data_frame(dct_data))
    expr_block <- clean_csv_block(round_data_frame(expr_data))
    rel_block <- clean_csv_block(round_data_frame(rel_data))

    # Calculate summaries
    n_genes <- ncol(cleaned_data) / reps

    calc_summary <- function(data, n_reps) {
      means <- list()
      sds <- list()

      for (g in 1:n_genes) {
        start_col <- (g - 1) * n_reps + 1
        end_col <- g * n_reps

        block_means <- apply(data[, start_col:end_col], 1, function(x) {
          if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
        })

        block_sds <- apply(data[, start_col:end_col], 1, function(x) {
          if (all(is.na(x))) NA else sd(x, na.rm = TRUE)
        })

        means[[g]] <- block_means
        sds[[g]] <- block_sds
      }

      return(list(
        means = do.call(cbind, means),
        sds = do.call(cbind, sds)
      ))
    }

    expr_summary <- calc_summary(expr_data, reps)
    rel_summary <- calc_summary(rel_data, reps)

    expr_mean_block <- clean_csv_block(round_data_frame(as.data.frame(expr_summary$means)))
    expr_sd_block <- clean_csv_block(round_data_frame(as.data.frame(expr_summary$sds)))
    rel_mean_block <- clean_csv_block(round_data_frame(as.data.frame(rel_summary$means)))
    rel_sd_block <- clean_csv_block(round_data_frame(as.data.frame(rel_summary$sds)))

    # Combine all blocks
    max_cols <- max(
      ncol(ct_block), ncol(dct_block), ncol(expr_block), ncol(rel_block),
      ncol(expr_mean_block), ncol(expr_sd_block), ncol(rel_mean_block), ncol(rel_sd_block),
      na.rm = TRUE
    )

    pad_block <- function(block, target_cols) {
      if (ncol(block) < target_cols) {
        extra_cols <- target_cols - ncol(block)
        block <- cbind(block, matrix("", nrow = nrow(block), ncol = extra_cols))
      }
      return(block)
    }

    ct_block <- pad_block(ct_block, max_cols)
    dct_block <- pad_block(dct_block, max_cols)
    expr_block <- pad_block(expr_block, max_cols)
    rel_block <- pad_block(rel_block, max_cols)
    expr_mean_block <- pad_block(expr_mean_block, max_cols)
    expr_sd_block <- pad_block(expr_sd_block, max_cols)
    rel_mean_block <- pad_block(rel_mean_block, max_cols)
    rel_sd_block <- pad_block(rel_sd_block, max_cols)

    # Create titles
    title_ct <- matrix(c("### Cleaned Ct Data ###", rep("", max_cols - 1)), nrow = 1)
    title_dct <- matrix(c("### Delta Ct Values ###", rep("", max_cols - 1)), nrow = 1)
    title_expr <- matrix(c("### Expression Values (2^DeltaCt) ###", rep("", max_cols - 1)), nrow = 1)
    title_expr_mean <- matrix(c("### Expression Means ###", rep("", max_cols - 1)), nrow = 1)
    title_expr_sd <- matrix(c("### Expression SDs ###", rep("", max_cols - 1)), nrow = 1)
    title_rel <- matrix(c("### Relative Expression vs. Control ###", rep("", max_cols - 1)), nrow = 1)
    title_rel_mean <- matrix(c("### Relative Expression Means ###", rep("", max_cols - 1)), nrow = 1)
    title_rel_sd <- matrix(c("### Relative Expression SDs ###", rep("", max_cols - 1)), nrow = 1)

    blank_row <- matrix("", nrow = 1, ncol = max_cols)

    final_csv <- rbind(
      title_ct, ct_block, blank_row,
      title_dct, dct_block, blank_row,
      title_expr, expr_block, blank_row,
      title_expr_mean, expr_mean_block, blank_row,
      title_expr_sd, expr_sd_block, blank_row,
      title_rel, rel_block, blank_row,
      title_rel_mean, rel_mean_block, blank_row,
      title_rel_sd, rel_sd_block
    )

    return(final_csv)
  }

  # ==================== PLOTTING FUNCTIONS ====================

  # Modern theme for ggplot
  theme_modern <- function() {
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", color = "#1e293b", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 11, color = "#64748b"),
      axis.title = element_text(size = 11, color = "#475569", face = "medium"),
      axis.text = element_text(size = 10, color = "#64748b"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_line(color = "#e2e8f0", size = 0.5),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )
  }

  # Color palette
  modern_colors <- c("#667eea", "#764ba2", "#f093fb", "#f5576c", "#4facfe", "#00f2fe", "#11998e", "#38ef7d")

  plot_ct_boxplot <- function(cleaned_data, reps) {
    n_genes <- ncol(cleaned_data) / reps

    plot_data <- data.frame()
    for (g in 1:n_genes) {
      start_col <- (g - 1) * reps + 1
      end_col <- g * reps

      for (row in 1:nrow(cleaned_data)) {
        values <- as.numeric(cleaned_data[row, start_col:end_col])
        plot_data <- rbind(plot_data, data.frame(
          Gene = paste0("Gene ", g),
          Sample = paste0("S", row),
          Ct_Value = values
        ))
      }
    }

    plot_data <- plot_data[!is.na(plot_data$Ct_Value), ]

    ggplot(plot_data, aes(x = Gene, y = Ct_Value, fill = Gene)) +
      geom_boxplot(alpha = 0.8, outlier.shape = NA, color = "#475569") +
      geom_jitter(width = 0.2, alpha = 0.6, size = 2, color = "#1e293b") +
      scale_fill_manual(values = modern_colors) +
      labs(
        title = "Ct Values Distribution",
        subtitle = "Distribution of cycle threshold values by gene",
        x = NULL,
        y = "Ct Value"
      ) +
      theme_modern() +
      theme(legend.position = "none")
  }

  plot_expr_boxplot <- function(expr_data, reps) {
    n_genes <- ncol(expr_data) / reps

    plot_data <- data.frame()
    for (g in 1:n_genes) {
      start_col <- (g - 1) * reps + 1
      end_col <- g * reps

      for (row in 1:nrow(expr_data)) {
        values <- as.numeric(expr_data[row, start_col:end_col])
        plot_data <- rbind(plot_data, data.frame(
          Gene = paste0("Gene ", g),
          Sample = paste0("S", row),
          Expression = values
        ))
      }
    }

    plot_data <- plot_data[!is.na(plot_data$Expression), ]

    ggplot(plot_data, aes(x = Gene, y = Expression, fill = Gene)) +
      geom_boxplot(alpha = 0.8, outlier.shape = NA, color = "#475569") +
      geom_jitter(width = 0.2, alpha = 0.6, size = 2, color = "#1e293b") +
      scale_fill_manual(values = modern_colors) +
      labs(
        title = "Expression Values Distribution",
        subtitle = "Distribution of 2^deltaCt values by gene",
        x = NULL,
        y = "Expression (2^deltaCt)"
      ) +
      theme_modern() +
      theme(legend.position = "none")
  }

  plot_rel_heatmap <- function(rel_data, reps) {
    n_genes <- ncol(rel_data) / reps

    # Calculate mean relative expression for each gene
    mean_rel <- data.frame()
    for (g in 1:n_genes) {
      start_col <- (g - 1) * reps + 1
      end_col <- g * reps

      for (row in 1:nrow(rel_data)) {
        values <- as.numeric(rel_data[row, start_col:end_col])
        mean_val <- mean(values, na.rm = TRUE)
        if (!is.na(mean_val)) {
          mean_rel <- rbind(mean_rel, data.frame(
            Gene = paste0("Gene ", g),
            Sample = paste0("Sample ", row),
            Relative_Expression = mean_val
          ))
        }
      }
    }

    ggplot(mean_rel, aes(x = Gene, y = Sample, fill = Relative_Expression)) +
      geom_tile(color = "white", size = 0.5) +
      geom_text(aes(label = round(Relative_Expression, 2)), size = 3.5, color = "#1e293b") +
      scale_fill_gradient2(
        low = "#667eea",
        mid = "white",
        high = "#f5576c",
        midpoint = 1,
        limits = c(0, max(2, max(mean_rel$Relative_Expression, na.rm = TRUE))),
        name = "Relative\nExpression"
      ) +
      labs(
        title = "Relative Expression Heatmap",
        subtitle = "Expression relative to control sample (Sample 1)",
        x = NULL,
        y = NULL
      ) +
      theme_modern() +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid = element_blank()
      )
  }

  plot_cv_plot <- function(cv_data, reps) {
    n_genes <- ncol(cv_data)

    plot_data <- data.frame()
    for (g in 1:n_genes) {
      values <- cv_data[, g]
      plot_data <- rbind(plot_data, data.frame(
        Gene = paste0("Gene ", g),
        CV_percent = values
      ))
    }

    plot_data <- plot_data[!is.na(plot_data$CV_percent), ]

    ggplot(plot_data, aes(x = Gene, y = CV_percent, fill = Gene)) +
      geom_boxplot(alpha = 0.8, outlier.shape = NA, color = "#475569") +
      geom_jitter(width = 0.2, alpha = 0.6, size = 2, color = "#1e293b") +
      geom_hline(yintercept = 20, linetype = "dashed", color = "#f5576c", size = 1) +
      annotate("text", x = 0.7, y = 22, label = "20% threshold", color = "#f5576c", size = 3.5, fontface = "bold") +
      scale_fill_manual(values = modern_colors) +
      labs(
        title = "Coefficient of Variation (CV%)",
        subtitle = "Replicate variability assessment by gene",
        x = NULL,
        y = "CV (%)"
      ) +
      theme_modern() +
      theme(legend.position = "none")
  }
}

shinyApp(ui, server)
