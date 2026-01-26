################################################################################
# qPCR Cleaner and Analyzer
# Modern UI Design with Sample Setup and Batch Editing
#
# Features:
# - Upload CSV or Excel files with or without headers
# - Interactive Sample Setup with batch editing
# - Assign Cell Line, Condition, Treatment to multiple samples at once
# - Define control groups for relative expression
# - Flexible replicate handling (3 or 4 replicates)
# - Outlier detection and removal
# - ΔCt calculation with optimal pairing
# - Expression analysis and relative expression
# - Statistical summaries with mean and SD
# - Interactive data visualization
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

.container-fluid { padding: 0; max-width: 100%; }

/* ==================== HEADER STYLES ==================== */
.app-header {
 background: var(--primary-gradient);
 padding: 1.5rem 2.5rem;
 margin-bottom: 1.5rem;
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

.app-title {
 color: white;
 font-size: 2rem;
 font-weight: 700;
 margin: 0 0 0.25rem 0;
 position: relative;
 z-index: 1;
}

.app-subtitle {
 color: rgba(255,255,255,0.9);
 font-size: 1rem;
 font-weight: 400;
 margin: 0;
 position: relative;
 z-index: 1;
}

/* ==================== LAYOUT ==================== */
.main-content { padding: 0 2rem 2rem 2rem; }

/* ==================== CARD STYLES ==================== */
.modern-card {
 background: var(--bg-card);
 border-radius: var(--radius-lg);
 box-shadow: var(--shadow-md);
 border: 1px solid var(--border-color);
 margin-bottom: 1.5rem;
 overflow: hidden;
}

.card-header {
 background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%);
 padding: 1rem 1.25rem;
 border-bottom: 1px solid var(--border-color);
 display: flex;
 align-items: center;
 gap: 0.75rem;
}

.card-header-icon {
 width: 36px;
 height: 36px;
 border-radius: var(--radius-md);
 display: flex;
 align-items: center;
 justify-content: center;
 font-size: 1.1rem;
 color: white;
}

.card-header-icon.purple { background: var(--primary-gradient); }
.card-header-icon.teal { background: var(--info-gradient); }
.card-header-icon.green { background: var(--success-gradient); }
.card-header-icon.orange { background: var(--warning-gradient); }

.card-title { font-size: 1rem; font-weight: 600; color: var(--text-primary); margin: 0; }
.card-body { padding: 1.25rem; }

/* ==================== SIDEBAR STYLES ==================== */
.sidebar-card {
 background: var(--bg-card);
 border-radius: var(--radius-lg);
 box-shadow: var(--shadow-md);
 border: 1px solid var(--border-color);
 overflow: hidden;
}

.sidebar-section {
 padding: 1rem;
 border-bottom: 1px solid var(--border-color);
}

.sidebar-section:last-child { border-bottom: none; }

.section-label {
 font-size: 0.7rem;
 font-weight: 600;
 text-transform: uppercase;
 letter-spacing: 0.5px;
 color: var(--text-muted);
 margin-bottom: 0.75rem;
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

/* ==================== BATCH EDIT PANEL ==================== */
.batch-edit-panel {
 background: linear-gradient(135deg, rgba(102, 126, 234, 0.05) 0%, rgba(118, 75, 162, 0.05) 100%);
 border: 2px solid var(--primary-color);
 border-radius: var(--radius-lg);
 padding: 1.25rem;
 margin-bottom: 1rem;
}

.batch-edit-title {
 font-size: 0.9rem;
 font-weight: 600;
 color: var(--primary-color);
 margin-bottom: 1rem;
 display: flex;
 align-items: center;
 gap: 0.5rem;
}

.batch-edit-row {
 display: flex;
 gap: 0.75rem;
 margin-bottom: 0.75rem;
 align-items: flex-end;
}

.batch-edit-row:last-child { margin-bottom: 0; }

.batch-input-group {
 flex: 1;
}

.batch-input-group label {
 display: block;
 font-size: 0.75rem;
 font-weight: 500;
 color: var(--text-secondary);
 margin-bottom: 0.25rem;
}

.batch-input-group input,
.batch-input-group select {
 width: 100%;
 padding: 0.5rem 0.75rem;
 border: 2px solid var(--border-color);
 border-radius: var(--radius-sm);
 font-size: 0.875rem;
 transition: all 0.2s ease;
}

.batch-input-group input:focus,
.batch-input-group select:focus {
 border-color: var(--primary-color);
 outline: none;
 box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.15);
}

.btn-apply {
 background: var(--primary-gradient);
 color: white;
 border: none;
 padding: 0.5rem 1rem;
 border-radius: var(--radius-sm);
 font-weight: 600;
 font-size: 0.8rem;
 cursor: pointer;
 white-space: nowrap;
 transition: all 0.2s ease;
}

.btn-apply:hover {
 transform: translateY(-1px);
 box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
}

.btn-apply-success {
 background: var(--success-gradient);
}

.btn-apply-warning {
 background: var(--warning-gradient);
 color: #1e293b;
}

/* ==================== SAMPLE TABLE ==================== */
.sample-table-container {
 border: 1px solid var(--border-color);
 border-radius: var(--radius-md);
 overflow: hidden;
}

table.dataTable tbody tr.selected {
 background: rgba(102, 126, 234, 0.15) !important;
}

table.dataTable tbody tr.selected td {
 background: transparent !important;
}

.selection-info {
 background: var(--info-gradient);
 color: white;
 padding: 0.75rem 1rem;
 border-radius: var(--radius-md);
 margin-bottom: 1rem;
 font-weight: 500;
 display: flex;
 align-items: center;
 gap: 0.5rem;
}

.selection-count {
 background: rgba(255,255,255,0.3);
 padding: 0.25rem 0.75rem;
 border-radius: 50px;
 font-weight: 700;
}

/* ==================== QUICK SELECT BUTTONS ==================== */
.quick-select-panel {
 display: flex;
 flex-wrap: wrap;
 gap: 0.5rem;
 margin-bottom: 1rem;
}

.btn-quick {
 background: var(--bg-light);
 border: 2px solid var(--border-color);
 padding: 0.4rem 0.75rem;
 border-radius: var(--radius-sm);
 font-size: 0.8rem;
 font-weight: 500;
 cursor: pointer;
 transition: all 0.2s ease;
}

.btn-quick:hover {
 border-color: var(--primary-color);
 background: rgba(102, 126, 234, 0.1);
}

.btn-quick.active {
 background: var(--primary-color);
 border-color: var(--primary-color);
 color: white;
}

/* ==================== GROUP CHIPS ==================== */
.group-chip {
 display: inline-block;
 padding: 0.2rem 0.6rem;
 border-radius: 50px;
 font-size: 0.75rem;
 font-weight: 600;
}

.chip-cellline { background: #e0e7ff; color: #3730a3; }
.chip-condition { background: #d1fae5; color: #065f46; }
.chip-treatment { background: #fef3c7; color: #92400e; }
.chip-control { background: #fee2e2; color: #991b1b; }

/* ==================== 96-WELL PLATE STYLES ==================== */
.plate-container {
  background: var(--bg-card);
  border: 2px solid var(--border-color);
  border-radius: var(--radius-lg);
  padding: 1.5rem;
  margin-bottom: 1.5rem;
  overflow-x: auto;
}

.plate-title {
  font-size: 1rem;
  font-weight: 600;
  color: var(--text-primary);
  margin-bottom: 1rem;
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.plate-grid {
  display: grid;
  grid-template-columns: 30px repeat(12, 50px);
  gap: 3px;
  justify-content: center;
  user-select: none;
}

.plate-header {
  font-size: 0.7rem;
  font-weight: 700;
  color: var(--text-secondary);
  text-align: center;
  padding: 0.25rem;
  background: var(--bg-light);
  border-radius: 4px;
}

.plate-row-label {
  font-size: 0.7rem;
  font-weight: 700;
  color: var(--text-secondary);
  display: flex;
  align-items: center;
  justify-content: center;
  background: var(--bg-light);
  border-radius: 4px;
}

.plate-well {
  width: 50px;
  height: 50px;
  border-radius: 50%;
  border: 2px solid #cbd5e1;
  background: linear-gradient(145deg, #f8fafc 0%, #e2e8f0 100%);
  cursor: pointer;
  transition: all 0.12s ease;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  font-size: 0.6rem;
  font-weight: 600;
  color: var(--text-muted);
  position: relative;
  box-shadow: inset 0 2px 4px rgba(0,0,0,0.06);
}

.plate-well .well-id {
  font-size: 0.55rem;
  font-weight: 700;
  color: var(--text-muted);
  opacity: 0.7;
}

.plate-well .well-value {
  font-size: 0.65rem;
  font-weight: 700;
  color: var(--text-primary);
  margin-top: 1px;
}

.plate-well:hover {
  border-color: var(--primary-color);
  transform: scale(1.08);
  z-index: 10;
  box-shadow: 0 4px 12px rgba(102, 126, 234, 0.3);
}

.plate-well.selected {
  border: 4px solid var(--primary-color) !important;
  background: linear-gradient(145deg, #e0e7ff 0%, #c7d2fe 100%) !important;
  box-shadow: 0 0 0 4px rgba(102, 126, 234, 0.4), inset 0 2px 4px rgba(102, 126, 234, 0.2) !important;
  transform: scale(1.05);
}

.plate-well.selected .well-id,
.plate-well.selected .well-value {
  color: var(--primary-color) !important;
  font-weight: 800;
}

.plate-well.has-data {
  background: linear-gradient(145deg, #dbeafe 0%, #bfdbfe 100%);
  border-color: #60a5fa;
}

.plate-well.has-cellline {
  background: linear-gradient(145deg, #e0e7ff 0%, #c7d2fe 100%);
  border-color: #818cf8;
}

.plate-well.has-cellline .well-value { color: #4f46e5; }

.plate-well.has-condition {
  background: linear-gradient(145deg, #d1fae5 0%, #a7f3d0 100%);
  border-color: #34d399;
}

.plate-well.has-condition .well-value { color: #059669; }

.plate-well.has-treatment {
  background: linear-gradient(145deg, #fef3c7 0%, #fde68a 100%);
  border-color: #fbbf24;
}

.plate-well.has-treatment .well-value { color: #d97706; }

.plate-well.is-control {
  background: linear-gradient(145deg, #fee2e2 0%, #fecaca 100%);
  border-color: #f87171;
}

.plate-well.is-control .well-value { color: #dc2626; }

.plate-well.empty {
  background: linear-gradient(145deg, #f1f5f9 0%, #e2e8f0 100%);
  border: 2px dashed #cbd5e1;
  opacity: 0.4;
  cursor: default;
}

.plate-well.empty:hover {
  transform: none;
  box-shadow: none;
}

.plate-legend {
  display: flex;
  flex-wrap: wrap;
  gap: 1.25rem;
  margin-top: 1.25rem;
  padding-top: 1rem;
  border-top: 1px solid var(--border-color);
}

.legend-item {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  font-size: 0.8rem;
  font-weight: 500;
  color: var(--text-secondary);
}

.legend-dot {
  width: 20px;
  height: 20px;
  border-radius: 50%;
  border: 2px solid;
}

.legend-dot.cellline { background: linear-gradient(145deg, #e0e7ff, #c7d2fe); border-color: #818cf8; }
.legend-dot.condition { background: linear-gradient(145deg, #d1fae5, #a7f3d0); border-color: #34d399; }
.legend-dot.treatment { background: linear-gradient(145deg, #fef3c7, #fde68a); border-color: #fbbf24; }
.legend-dot.control { background: linear-gradient(145deg, #fee2e2, #fecaca); border-color: #f87171; }
.legend-dot.selected {
  background: linear-gradient(145deg, #e0e7ff, #c7d2fe);
  border: 3px solid #667eea;
  box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.4);
}

/* Row/Column Select Buttons */
.plate-select-buttons {
  display: flex;
  flex-wrap: wrap;
  gap: 0.4rem;
  margin-bottom: 1rem;
  padding: 0.75rem;
  background: var(--bg-light);
  border-radius: var(--radius-md);
}

.btn-plate-select {
  background: white;
  border: 2px solid var(--border-color);
  padding: 0.4rem 0.75rem;
  border-radius: var(--radius-sm);
  font-size: 0.75rem;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.15s ease;
}

.btn-plate-select:hover {
  border-color: var(--primary-color);
  background: rgba(102, 126, 234, 0.1);
  color: var(--primary-color);
}

.btn-plate-select:active {
  transform: scale(0.95);
}

/* Selection hint */
.plate-hint {
  font-size: 0.75rem;
  color: var(--text-muted);
  margin-top: 0.75rem;
  padding: 0.5rem 0.75rem;
  background: rgba(102, 126, 234, 0.08);
  border-radius: var(--radius-sm);
  border-left: 3px solid var(--primary-color);
}
  background: linear-gradient(135deg, #e0e7ff 0%, #c7d2fe 100%);
}

.plate-well.has-condition {
  background: linear-gradient(135deg, #d1fae5 0%, #a7f3d0 100%);
}

.plate-well.has-treatment {
  background: linear-gradient(135deg, #fef3c7 0%, #fde68a 100%);
}

.plate-well.is-control {
  background: linear-gradient(135deg, #fee2e2 0%, #fecaca 100%);
  border-color: var(--danger-color);
}

.plate-well.empty {
  background: var(--bg-light);
  border-style: dashed;
  opacity: 0.5;
}

.plate-legend {
  display: flex;
  flex-wrap: wrap;
  gap: 1rem;
  margin-top: 1rem;
  padding-top: 1rem;
  border-top: 1px solid var(--border-color);
}

.legend-item {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  font-size: 0.8rem;
  color: var(--text-secondary);
}

.legend-dot {
  width: 16px;
  height: 16px;
  border-radius: 50%;
  border: 2px solid;
}

.legend-dot.cellline { background: #e0e7ff; border-color: #667eea; }
.legend-dot.condition { background: #d1fae5; border-color: #10b981; }
.legend-dot.treatment { background: #fef3c7; border-color: #f59e0b; }
.legend-dot.control { background: #fee2e2; border-color: #ef4444; }
.legend-dot.selected { background: white; border-color: #667eea; box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.3); }

/* Row/Column Select Buttons */
.plate-select-buttons {
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
  margin-bottom: 1rem;
}

.btn-plate-select {
  background: var(--bg-light);
  border: 1px solid var(--border-color);
  padding: 0.35rem 0.6rem;
  border-radius: var(--radius-sm);
  font-size: 0.75rem;
  font-weight: 500;
  cursor: pointer;
  transition: all 0.2s ease;
}

.btn-plate-select:hover {
  border-color: var(--primary-color);
  background: rgba(102, 126, 234, 0.1);
}

/* ==================== BUTTON STYLES ==================== */
.btn-modern {
 border: none;
 border-radius: var(--radius-md);
 padding: 0.75rem 1.25rem;
 font-weight: 600;
 font-size: 0.9rem;
 cursor: pointer;
 transition: all 0.3s ease;
 display: inline-flex;
 align-items: center;
 justify-content: center;
 gap: 0.5rem;
 width: 100%;
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

.btn-success-gradient {
 background: var(--success-gradient);
 color: white;
 box-shadow: 0 4px 15px rgba(17, 153, 142, 0.4);
}

.btn-outline {
 background: transparent;
 border: 2px solid var(--border-color);
 color: var(--text-secondary);
}

.btn-outline:hover {
 border-color: var(--primary-color);
 color: var(--primary-color);
}

/* ==================== TAB STYLES ==================== */
.nav-tabs > li > a {
 border: none !important;
 border-radius: var(--radius-md) !important;
 padding: 0.75rem 1rem !important;
 font-weight: 500 !important;
 color: var(--text-secondary) !important;
 background: transparent !important;
 transition: all 0.2s ease !important;
 margin: 0 2px !important;
}

.nav-tabs > li > a:hover {
 background: rgba(102, 126, 234, 0.1) !important;
 color: var(--primary-color) !important;
}

.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover {
 background: var(--primary-gradient) !important;
 color: white !important;
 box-shadow: var(--shadow-md) !important;
}

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

/* ==================== TABLE STYLES ==================== */
.dataTables_wrapper { font-size: 0.875rem; }

table.dataTable { border-collapse: collapse !important; }

table.dataTable thead th {
 background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%);
 border-bottom: 2px solid var(--primary-color) !important;
 color: var(--text-primary);
 font-weight: 600;
 padding: 0.875rem !important;
 text-transform: uppercase;
 font-size: 0.7rem;
 letter-spacing: 0.5px;
}

table.dataTable tbody td {
 padding: 0.75rem !important;
 border-bottom: 1px solid var(--border-color) !important;
 vertical-align: middle;
}

table.dataTable tbody tr:hover {
 background: rgba(102, 126, 234, 0.05) !important;
}

/* ==================== PLOT CONTAINER ==================== */
.plot-container {
 background: var(--bg-light);
 border-radius: var(--radius-md);
 padding: 1rem;
 border: 1px solid var(--border-color);
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
 width: 32px;
 height: 32px;
 border-radius: var(--radius-sm);
 display: flex;
 align-items: center;
 justify-content: center;
 font-size: 0.9rem;
 color: white;
 background: var(--primary-gradient);
}

.section-header-text {
 font-size: 1rem;
 font-weight: 600;
 color: var(--text-primary);
 margin: 0;
}

/* ==================== WORKFLOW STEPS ==================== */
.workflow-steps {
 display: flex;
 gap: 0.5rem;
 margin-bottom: 1.5rem;
 flex-wrap: wrap;
}

.workflow-step {
 display: flex;
 align-items: center;
 gap: 0.5rem;
 padding: 0.5rem 1rem;
 background: var(--bg-light);
 border-radius: 50px;
 font-size: 0.8rem;
 font-weight: 500;
 color: var(--text-muted);
 border: 2px solid var(--border-color);
}

.workflow-step.active {
 background: var(--primary-color);
 color: white;
 border-color: var(--primary-color);
}

.workflow-step.completed {
 background: #d1fae5;
 color: #065f46;
 border-color: #10b981;
}

.step-number {
 width: 20px;
 height: 20px;
 border-radius: 50%;
 background: currentColor;
 color: white;
 display: flex;
 align-items: center;
 justify-content: center;
 font-size: 0.7rem;
 font-weight: 700;
}

.workflow-step.active .step-number,
.workflow-step.completed .step-number {
 background: rgba(255,255,255,0.3);
}

/* ==================== RESPONSIVE ==================== */
@media (max-width: 768px) {
 .app-header { padding: 1rem; }
 .app-title { font-size: 1.5rem; }
 .main-content { padding: 0 1rem 1rem 1rem; }
 .batch-edit-row { flex-direction: column; }
}

/* ==================== SHINY OVERRIDES ==================== */
.shiny-notification {
 border-radius: var(--radius-md);
 border: none;
 box-shadow: var(--shadow-lg);
}

.form-control {
 border: 2px solid var(--border-color);
 border-radius: var(--radius-sm);
 padding: 0.5rem 0.75rem;
}

.form-control:focus {
 border-color: var(--primary-color);
 box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.15);
}

.checkbox label, .radio label {
 font-weight: 500;
 color: var(--text-secondary);
}
"

ui <- fluidPage(
 useShinyjs(),
 tags$head(
   tags$style(HTML(modern_css)),
   tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap", rel = "stylesheet")
 ),

 # Header
 div(class = "app-header",
   h1(class = "app-title", "qPCR Cleaner & Analyzer"),
   p(class = "app-subtitle", "Professional qPCR data processing with sample management")
 ),

 # Main Content
 div(class = "main-content",
   fluidRow(
     # Sidebar
     column(3,
       div(class = "sidebar-card",
         # File Upload
         div(class = "sidebar-section",
           div(class = "section-label", "1. Upload Data"),
           fileInput("file", label = NULL,
                    accept = c(".csv", ".xlsx", ".xls"),
                    placeholder = "Choose file..."),
           checkboxInput("has_header", "Has column headers", value = FALSE)
         ),

         # Replicate Settings
         div(class = "sidebar-section",
           div(class = "section-label", "2. Settings"),
           radioButtons("reps", "Replicates per gene:",
                       choices = c("3" = 3, "4" = 4),
                       selected = 3, inline = TRUE),
           checkboxInput("remove_outliers", "Remove outliers (4 reps)", value = TRUE),
           numericInput("outlier_threshold", "Outlier threshold (SD):",
                       value = 2.0, min = 1.0, max = 5.0, step = 0.1)
         ),

         # Actions
         div(class = "sidebar-section",
           div(class = "section-label", "3. Analyze"),
           actionButton("analyze", "Run Analysis",
                       class = "btn-modern btn-primary-gradient",
                       style = "margin-bottom: 0.5rem;"),
           actionButton("reset", "Reset All",
                       class = "btn-modern btn-outline"),
           conditionalPanel(
             condition = "output.analysis_complete",
             div(style = "margin-top: 0.5rem;",
               downloadButton("downloadData", "Download Results",
                             class = "btn-modern btn-success-gradient")
             )
           )
         )
       )
     ),

     # Main Panel
     column(9,
       # Workflow indicator
       div(class = "workflow-steps",
         div(class = "workflow-step", id = "step1",
           span(class = "step-number", "1"), "Upload"),
         div(class = "workflow-step", id = "step2",
           span(class = "step-number", "2"), "Sample Setup"),
         div(class = "workflow-step", id = "step3",
           span(class = "step-number", "3"), "Analyze"),
         div(class = "workflow-step", id = "step4",
           span(class = "step-number", "4"), "Results")
       ),

       tabsetPanel(
         id = "main_tabs",
         type = "tabs",

         # ==================== SAMPLE SETUP TAB ====================
         tabPanel(
           title = "Sample Setup",
           icon = icon("users"),
           div(class = "modern-card", style = "margin-top: 1rem;",
             div(class = "card-header",
               div(class = "card-header-icon purple", HTML("&#9998;")),
               h3(class = "card-title", "Sample Setup - Batch Editing")
             ),
             div(class = "card-body",

               conditionalPanel(
                 condition = "!output.file_uploaded",
                 div(class = "alert-modern alert-info-modern",
                   HTML("&#128193;"),
                   div(tags$strong("Upload a file first"),
                       tags$br(),
                       "Upload your qPCR data using the sidebar to set up samples.")
                 )
               ),

               conditionalPanel(
                 condition = "output.file_uploaded",

                 # Instructions
                 div(class = "alert-modern alert-info-modern", style = "margin-bottom: 1rem;",
                   HTML("&#128161;"),
                   div(
                     tags$strong("How to use:"), tags$br(),
                     "1. Click wells on the plate OR select rows in the table below", tags$br(),
                     "2. Enter Cell Line, Condition, or Treatment", tags$br(),
                     "3. Click 'Apply' to assign to selected samples"
                   )
                 ),

                 # 96-Well Plate Visualization
                 div(class = "plate-container",
                   div(class = "plate-title", HTML("&#127981;"), "96-Well Plate View"),

                   # Plate selection buttons
                   div(class = "plate-select-buttons",
                     actionButton("plate_select_all", "All Wells", class = "btn-plate-select"),
                     actionButton("plate_select_none", "Clear", class = "btn-plate-select"),
                     tags$span("|", style = "color: var(--border-color); padding: 0 0.5rem;"),
                     actionButton("plate_row_A", "Row A", class = "btn-plate-select"),
                     actionButton("plate_row_B", "Row B", class = "btn-plate-select"),
                     actionButton("plate_row_C", "Row C", class = "btn-plate-select"),
                     actionButton("plate_row_D", "Row D", class = "btn-plate-select"),
                     actionButton("plate_row_E", "Row E", class = "btn-plate-select"),
                     actionButton("plate_row_F", "Row F", class = "btn-plate-select"),
                     actionButton("plate_row_G", "Row G", class = "btn-plate-select"),
                     actionButton("plate_row_H", "Row H", class = "btn-plate-select"),
                     tags$span("|", style = "color: var(--border-color); padding: 0 0.5rem;"),
                     actionButton("plate_col_1_6", "Col 1-6", class = "btn-plate-select"),
                     actionButton("plate_col_7_12", "Col 7-12", class = "btn-plate-select")
                   ),

                   # The plate grid (generated dynamically)
                   uiOutput("plate_ui"),

                   # Legend
                   div(class = "plate-legend",
                     div(class = "legend-item",
                       div(class = "legend-dot selected"),
                       span("Selected")
                     ),
                     div(class = "legend-item",
                       div(class = "legend-dot cellline"),
                       span("Cell Line")
                     ),
                     div(class = "legend-item",
                       div(class = "legend-dot condition"),
                       span("Condition")
                     ),
                     div(class = "legend-item",
                       div(class = "legend-dot treatment"),
                       span("Treatment")
                     ),
                     div(class = "legend-item",
                       div(class = "legend-dot control"),
                       span("Control")
                     )
                   )
                 ),

                 # Batch Edit Panel
                 div(class = "batch-edit-panel",
                   div(class = "batch-edit-title",
                     HTML("&#9998;"), "Batch Edit Selected Samples"
                   ),

                   # Selection info
                   uiOutput("selection_info"),

                   # Row 1: Cell Line
                   div(class = "batch-edit-row",
                     div(class = "batch-input-group", style = "flex: 2;",
                       tags$label("Cell Line"),
                       textInput("batch_cellline", label = NULL,
                                placeholder = "e.g., HeLa, MCF7, HEK293...")
                     ),
                     actionButton("apply_cellline", "Apply Cell Line",
                                 class = "btn-apply")
                   ),

                   # Row 2: Condition
                   div(class = "batch-edit-row",
                     div(class = "batch-input-group", style = "flex: 2;",
                       tags$label("Condition"),
                       textInput("batch_condition", label = NULL,
                                placeholder = "e.g., Control, Treatment, Knockdown...")
                     ),
                     actionButton("apply_condition", "Apply Condition",
                                 class = "btn-apply btn-apply-success")
                   ),

                   # Row 3: Treatment
                   div(class = "batch-edit-row",
                     div(class = "batch-input-group", style = "flex: 2;",
                       tags$label("Treatment"),
                       textInput("batch_treatment", label = NULL,
                                placeholder = "e.g., Drug A 10uM, siRNA, Vehicle...")
                     ),
                     actionButton("apply_treatment", "Apply Treatment",
                                 class = "btn-apply btn-apply-warning")
                   ),

                   # Row 4: Control Group
                   div(class = "batch-edit-row", style = "margin-top: 0.5rem; padding-top: 0.75rem; border-top: 1px dashed var(--border-color);",
                     div(class = "batch-input-group", style = "flex: 2;",
                       tags$label("Control Group (for relative expression)"),
                       tags$small("Mark selected samples as the control/reference group",
                                 style = "color: var(--text-muted); display: block;")
                     ),
                     actionButton("set_control", "Set as Control",
                                 class = "btn-apply", style = "background: var(--danger-color);")
                   )
                 ),

                 # Quick Select Buttons
                 div(style = "margin-bottom: 1rem;",
                   tags$label("Quick Select:", style = "font-weight: 600; margin-right: 0.5rem; color: var(--text-secondary);"),
                   div(class = "quick-select-panel", style = "display: inline-flex;",
                     actionButton("select_all", "All", class = "btn-quick"),
                     actionButton("select_none", "None", class = "btn-quick"),
                     actionButton("select_odd", "Odd Rows", class = "btn-quick"),
                     actionButton("select_even", "Even Rows", class = "btn-quick"),
                     actionButton("select_first_half", "First Half", class = "btn-quick"),
                     actionButton("select_second_half", "Second Half", class = "btn-quick")
                   )
                 ),

                 # Sample Table
                 div(class = "section-header",
                   div(class = "section-header-icon", HTML("&#128203;")),
                   h4(class = "section-header-text", "Sample Metadata Table")
                 ),
                 div(class = "sample-table-container",
                   DTOutput("sample_table")
                 ),

                 # Clear metadata button
                 div(style = "margin-top: 1rem; text-align: right;",
                   actionButton("clear_metadata", "Clear All Metadata",
                               class = "btn-modern btn-outline",
                               style = "width: auto; padding: 0.5rem 1rem;")
                 )
               )
             )
           )
         ),

         # ==================== DATA PREVIEW TAB ====================
         tabPanel(
           title = "Data Preview",
           icon = icon("table"),
           div(class = "modern-card", style = "margin-top: 1rem;",
             div(class = "card-header",
               div(class = "card-header-icon teal", HTML("&#128203;")),
               h3(class = "card-title", "Raw Ct Data Preview")
             ),
             div(class = "card-body",
               conditionalPanel(
                 condition = "!output.file_uploaded",
                 div(class = "alert-modern alert-info-modern",
                   HTML("&#128193;"),
                   div(tags$strong("No file uploaded"),
                       tags$br(), "Upload a CSV or Excel file to preview.")
                 )
               ),
               conditionalPanel(
                 condition = "output.file_uploaded",
                 uiOutput("file_info_ui"),
                 DTOutput("raw_data_table")
               )
             )
           )
         ),

         # ==================== ANALYSIS RESULTS TAB ====================
         tabPanel(
           title = "Results",
           icon = icon("chart-line"),

           conditionalPanel(
             condition = "!output.analysis_complete",
             div(class = "modern-card", style = "margin-top: 1rem;",
               div(class = "card-body",
                 div(class = "alert-modern alert-info-modern",
                   HTML("&#9654;"),
                   div(tags$strong("Ready to Analyze"),
                       tags$br(), "Set up your samples and click 'Run Analysis'.")
                 )
               )
             )
           ),

           conditionalPanel(
             condition = "output.analysis_complete",

             # Summary
             div(class = "modern-card", style = "margin-top: 1rem;",
               div(class = "card-header",
                 div(class = "card-header-icon green", HTML("&#128202;")),
                 h3(class = "card-title", "Summary Statistics")
               ),
               div(class = "card-body",
                 fluidRow(
                   column(6, tableOutput("expr_summary_table")),
                   column(6, tableOutput("rel_summary_table"))
                 )
               )
             ),

             # Detailed Results
             div(class = "modern-card",
               div(class = "card-header",
                 div(class = "card-header-icon purple", HTML("&#128269;")),
                 h3(class = "card-title", "Detailed Results")
               ),
               div(class = "card-body",
                 tabsetPanel(
                   type = "pills",
                   tabPanel("Cleaned Ct", DTOutput("ct_table"), style = "padding-top: 1rem;"),
                   tabPanel("ΔCt Values", DTOutput("dct_table"), style = "padding-top: 1rem;"),
                   tabPanel("Expression", DTOutput("expr_table"), style = "padding-top: 1rem;"),
                   tabPanel("Relative Expr.", DTOutput("rel_table"), style = "padding-top: 1rem;")
                 )
               )
             ),

             # Visualizations
             div(class = "modern-card",
               div(class = "card-header",
                 div(class = "card-header-icon orange", HTML("&#128200;")),
                 h3(class = "card-title", "Visualizations")
               ),
               div(class = "card-body",
                 fluidRow(
                   column(6, div(class = "plot-container", plotOutput("ct_boxplot", height = "300px"))),
                   column(6, div(class = "plot-container", plotOutput("expr_boxplot", height = "300px")))
                 ),
                 fluidRow(style = "margin-top: 1rem;",
                   column(12, div(class = "plot-container", plotOutput("rel_heatmap", height = "350px")))
                 )
               )
             )
           )
         ),

         # ==================== QC TAB ====================
         tabPanel(
           title = "Quality Control",
           icon = icon("microscope"),

           conditionalPanel(
             condition = "!output.analysis_complete",
             div(class = "modern-card", style = "margin-top: 1rem;",
               div(class = "card-body",
                 div(class = "alert-modern alert-info-modern",
                   HTML("&#128300;"),
                   div(tags$strong("QC Pending"), tags$br(), "Run analysis to view QC metrics.")
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
                   column(6, tableOutput("qc_summary")),
                   column(6, tableOutput("outlier_summary"))
                 ),
                 hr(),
                 div(class = "plot-container",
                   plotOutput("cv_plot", height = "300px")
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

 # ==================== REACTIVE VALUES ====================
 rv <- reactiveValues(
   raw_data = NULL,
   sample_metadata = NULL,
   selected_rows = c(),
   last_clicked_well = NULL,
   cleaned_data = NULL,
   dct_data = NULL,
   expr_data = NULL,
   rel_data = NULL,
   csv_data = NULL,
   analysis_complete = FALSE,
   file_uploaded = FALSE,
   qc_metrics = NULL,
   outlier_info = NULL
 )

 # ==================== FILE UPLOAD ====================
 observeEvent(input$file, {
   req(input$file)

   tryCatch({
     file_path <- input$file$datapath
     file_name <- input$file$name

     if (grepl("\\.xlsx?$", file_name, ignore.case = TRUE)) {
       raw <- read_excel(file_path, col_names = input$has_header,
                        na = c("Undetermined", "#VALUE!", "NA", ""))
     } else {
       raw <- read.csv(file_path, header = input$has_header,
                      stringsAsFactors = FALSE,
                      na.strings = c("Undetermined", "#VALUE!", "NA", ""))
     }

     raw <- raw[, colSums(is.na(raw)) < nrow(raw), drop = FALSE]
     raw <- raw[rowSums(is.na(raw)) < ncol(raw), , drop = FALSE]

     if (ncol(raw) == 0 || nrow(raw) == 0) {
       showNotification("No usable data found.", type = "error")
       return()
     }

     raw_numeric <- as.data.frame(lapply(raw, function(x) {
       if (is.numeric(x)) x else as.numeric(as.character(x))
     }))

     rv$raw_data <- raw_numeric
     rv$file_uploaded <- TRUE
     rv$analysis_complete <- FALSE

     # Initialize sample metadata
     rv$sample_metadata <- data.frame(
       Sample = 1:nrow(raw_numeric),
       Cell_Line = rep("", nrow(raw_numeric)),
       Condition = rep("", nrow(raw_numeric)),
       Treatment = rep("", nrow(raw_numeric)),
       Is_Control = rep(FALSE, nrow(raw_numeric)),
       stringsAsFactors = FALSE
     )

     rv$selected_rows <- c()

     showNotification("File uploaded! Now set up your samples.", type = "message")
     updateTabsetPanel(session, "main_tabs", selected = "Sample Setup")

   }, error = function(e) {
     showNotification(paste("Error:", e$message), type = "error")
   })
 })

 # ==================== SAMPLE TABLE ====================
 output$sample_table <- renderDT({
   req(rv$sample_metadata)

   # Create display table with chips
   display_df <- rv$sample_metadata %>%
     mutate(
       Cell_Line_Display = ifelse(Cell_Line == "", "-",
                                  paste0('<span class="group-chip chip-cellline">', Cell_Line, '</span>')),
       Condition_Display = ifelse(Condition == "", "-",
                                  paste0('<span class="group-chip chip-condition">', Condition, '</span>')),
       Treatment_Display = ifelse(Treatment == "", "-",
                                  paste0('<span class="group-chip chip-treatment">', Treatment, '</span>')),
       Control_Display = ifelse(Is_Control,
                               '<span class="group-chip chip-control">CONTROL</span>', "")
     ) %>%
     select(Sample, Cell_Line_Display, Condition_Display, Treatment_Display, Control_Display)

   colnames(display_df) <- c("Sample", "Cell Line", "Condition", "Treatment", "Control")

   datatable(
     display_df,
     selection = list(mode = 'multiple', selected = rv$selected_rows),
     escape = FALSE,
     options = list(
       pageLength = 15,
       dom = 'tip',
       scrollX = TRUE,
       columnDefs = list(list(className = 'dt-center', targets = '_all'))
     ),
     class = 'cell-border stripe'
   )
 })

 # Track selection
 observeEvent(input$sample_table_rows_selected, {
   rv$selected_rows <- input$sample_table_rows_selected
 }, ignoreNULL = FALSE)

 # Selection info display
 output$selection_info <- renderUI({
   n_selected <- length(rv$selected_rows)
   if (n_selected == 0) {
     div(class = "alert-modern alert-warning-modern", style = "margin-bottom: 1rem; padding: 0.75rem;",
       HTML("&#9888;"),
       div(tags$strong("No samples selected. "), "Click wells on the plate or rows in the table.")
     )
   } else {
     div(class = "selection-info",
       HTML("&#9989;"),
       span(class = "selection-count", n_selected),
       paste("sample(s) selected:", paste(rv$selected_rows, collapse = ", "))
     )
   }
 })

 # ==================== 96-WELL PLATE ====================

 # Render the plate UI
 output$plate_ui <- renderUI({
   req(rv$raw_data)

   n_samples <- nrow(rv$raw_data)
   max_wells <- 96  # 8 rows x 12 columns

   # Row labels
   row_labels <- LETTERS[1:8]
   col_labels <- 1:12

   # Calculate mean Ct for each sample (first gene as reference)
   reps <- as.numeric(input$reps)
   mean_cts <- sapply(1:n_samples, function(i) {
     vals <- as.numeric(rv$raw_data[i, 1:reps])
     if (all(is.na(vals))) NA else round(mean(vals, na.rm = TRUE), 1)
   })

   # Create plate grid
   plate_elements <- list()

   # Add header row with column numbers
   plate_elements[[1]] <- div(class = "plate-header", "")  # Empty corner
   for (col in col_labels) {
     plate_elements[[length(plate_elements) + 1]] <- div(class = "plate-header", col)
   }

   # Add rows with wells
   well_index <- 1
   for (row_idx in 1:8) {
     # Row label
     plate_elements[[length(plate_elements) + 1]] <- div(class = "plate-row-label", row_labels[row_idx])

     # Wells in this row
     for (col in 1:12) {
       if (well_index <= n_samples) {
         # Well position name (A1, A2, etc.)
         well_name <- paste0(row_labels[row_idx], col)

         # Determine well class based on metadata
         well_classes <- "plate-well"

         if (well_index %in% rv$selected_rows) {
           well_classes <- paste(well_classes, "selected")
         }

         if (!is.null(rv$sample_metadata)) {
           meta <- rv$sample_metadata[well_index, ]
           if (meta$Is_Control) {
             well_classes <- paste(well_classes, "is-control")
           } else if (meta$Cell_Line != "") {
             well_classes <- paste(well_classes, "has-cellline")
           } else if (meta$Condition != "") {
             well_classes <- paste(well_classes, "has-condition")
           } else if (meta$Treatment != "") {
             well_classes <- paste(well_classes, "has-treatment")
           } else {
             well_classes <- paste(well_classes, "has-data")
           }
         }

         # Get mean Ct value for display
         ct_val <- mean_cts[well_index]
         ct_display <- if (is.na(ct_val)) "-" else ct_val

         # Create well with click handler (shift+click for range)
         plate_elements[[length(plate_elements) + 1]] <- tags$div(
           class = well_classes,
           onclick = sprintf("Shiny.setInputValue('plate_well_click', {well: %d, shiftKey: event.shiftKey, ctrlKey: event.ctrlKey || event.metaKey, time: Date.now()})", well_index),
           div(class = "well-id", well_name),
           div(class = "well-value", ct_display)
         )
       } else {
         # Empty well (beyond sample count)
         plate_elements[[length(plate_elements) + 1]] <- div(class = "plate-well empty", "")
       }
       well_index <- well_index + 1
     }
   }

   tagList(
     div(class = "plate-grid", plate_elements),
     div(class = "plate-hint",
       HTML("<strong>Tip:</strong> Click to select/deselect. Shift+click for range. Ctrl/Cmd+click to add to selection.")
     )
   )
 })

 # Handle well clicks with shift/ctrl support
 observeEvent(input$plate_well_click, {
   req(input$plate_well_click)
   well <- input$plate_well_click$well
   shift_key <- isTRUE(input$plate_well_click$shiftKey)
   ctrl_key <- isTRUE(input$plate_well_click$ctrlKey)

   n_samples <- nrow(rv$raw_data)

   if (shift_key && !is.null(rv$last_clicked_well)) {
     # Shift+click: select range from last clicked to current
     start_well <- min(rv$last_clicked_well, well)
     end_well <- max(rv$last_clicked_well, well)
     range_wells <- start_well:end_well
     range_wells <- range_wells[range_wells <= n_samples]

     if (ctrl_key) {
       # Shift+Ctrl: add range to existing selection
       rv$selected_rows <- unique(c(rv$selected_rows, range_wells))
     } else {
       # Shift only: replace selection with range
       rv$selected_rows <- range_wells
     }
   } else if (ctrl_key) {
     # Ctrl+click: toggle single well in selection
     if (well %in% rv$selected_rows) {
       rv$selected_rows <- rv$selected_rows[rv$selected_rows != well]
     } else {
       rv$selected_rows <- c(rv$selected_rows, well)
     }
   } else {
     # Normal click: select only this well (replaces selection)
     if (well %in% rv$selected_rows && length(rv$selected_rows) == 1) {
       # Clicking the only selected well deselects it
       rv$selected_rows <- c()
     } else {
       rv$selected_rows <- well
     }
   }

   # Update last clicked
   rv$last_clicked_well <- well

   # Update DT selection to match
   proxy <- dataTableProxy("sample_table")
   selectRows(proxy, rv$selected_rows)
 })

 # Plate row selection buttons
 observeEvent(input$plate_row_A, {
   req(rv$raw_data)
   wells <- 1:min(12, nrow(rv$raw_data))
   rv$selected_rows <- wells
   proxy <- dataTableProxy("sample_table")
   selectRows(proxy, rv$selected_rows)
 })

 observeEvent(input$plate_row_B, {
   req(rv$raw_data)
   wells <- 13:min(24, nrow(rv$raw_data))
   wells <- wells[wells <= nrow(rv$raw_data)]
   if (length(wells) > 0) {
     rv$selected_rows <- wells
     proxy <- dataTableProxy("sample_table")
     selectRows(proxy, rv$selected_rows)
   }
 })

 observeEvent(input$plate_row_C, {
   req(rv$raw_data)
   wells <- 25:min(36, nrow(rv$raw_data))
   wells <- wells[wells <= nrow(rv$raw_data)]
   if (length(wells) > 0) {
     rv$selected_rows <- wells
     proxy <- dataTableProxy("sample_table")
     selectRows(proxy, rv$selected_rows)
   }
 })

 observeEvent(input$plate_row_D, {
   req(rv$raw_data)
   wells <- 37:min(48, nrow(rv$raw_data))
   wells <- wells[wells <= nrow(rv$raw_data)]
   if (length(wells) > 0) {
     rv$selected_rows <- wells
     proxy <- dataTableProxy("sample_table")
     selectRows(proxy, rv$selected_rows)
   }
 })

 observeEvent(input$plate_row_E, {
   req(rv$raw_data)
   wells <- 49:min(60, nrow(rv$raw_data))
   wells <- wells[wells <= nrow(rv$raw_data)]
   if (length(wells) > 0) {
     rv$selected_rows <- wells
     proxy <- dataTableProxy("sample_table")
     selectRows(proxy, rv$selected_rows)
   }
 })

 observeEvent(input$plate_row_F, {
   req(rv$raw_data)
   wells <- 61:min(72, nrow(rv$raw_data))
   wells <- wells[wells <= nrow(rv$raw_data)]
   if (length(wells) > 0) {
     rv$selected_rows <- wells
     proxy <- dataTableProxy("sample_table")
     selectRows(proxy, rv$selected_rows)
   }
 })

 observeEvent(input$plate_row_G, {
   req(rv$raw_data)
   wells <- 73:min(84, nrow(rv$raw_data))
   wells <- wells[wells <= nrow(rv$raw_data)]
   if (length(wells) > 0) {
     rv$selected_rows <- wells
     proxy <- dataTableProxy("sample_table")
     selectRows(proxy, rv$selected_rows)
   }
 })

 observeEvent(input$plate_row_H, {
   req(rv$raw_data)
   wells <- 85:min(96, nrow(rv$raw_data))
   wells <- wells[wells <= nrow(rv$raw_data)]
   if (length(wells) > 0) {
     rv$selected_rows <- wells
     proxy <- dataTableProxy("sample_table")
     selectRows(proxy, rv$selected_rows)
   }
 })

 # Column selection buttons
 observeEvent(input$plate_col_1_6, {
   req(rv$raw_data)
   n <- nrow(rv$raw_data)
   # Wells in columns 1-6 (positions 1-6, 13-18, 25-30, etc.)
   wells <- c()
   for (row in 0:7) {
     start <- row * 12 + 1
     end <- row * 12 + 6
     wells <- c(wells, start:end)
   }
   wells <- wells[wells <= n]
   if (length(wells) > 0) {
     rv$selected_rows <- wells
     proxy <- dataTableProxy("sample_table")
     selectRows(proxy, rv$selected_rows)
   }
 })

 observeEvent(input$plate_col_7_12, {
   req(rv$raw_data)
   n <- nrow(rv$raw_data)
   # Wells in columns 7-12 (positions 7-12, 19-24, 31-36, etc.)
   wells <- c()
   for (row in 0:7) {
     start <- row * 12 + 7
     end <- row * 12 + 12
     wells <- c(wells, start:end)
   }
   wells <- wells[wells <= n]
   if (length(wells) > 0) {
     rv$selected_rows <- wells
     proxy <- dataTableProxy("sample_table")
     selectRows(proxy, rv$selected_rows)
   }
 })

 observeEvent(input$plate_select_all, {
   req(rv$raw_data)
   rv$selected_rows <- 1:nrow(rv$raw_data)
   proxy <- dataTableProxy("sample_table")
   selectRows(proxy, rv$selected_rows)
 })

 observeEvent(input$plate_select_none, {
   rv$selected_rows <- c()
   proxy <- dataTableProxy("sample_table")
   selectRows(proxy, NULL)
 })

 # ==================== BATCH APPLY FUNCTIONS ====================

 observeEvent(input$apply_cellline, {
   req(length(rv$selected_rows) > 0, input$batch_cellline != "")
   rv$sample_metadata$Cell_Line[rv$selected_rows] <- input$batch_cellline
   showNotification(paste("Cell Line set to '", input$batch_cellline, "' for ",
                         length(rv$selected_rows), " samples"), type = "message")
 })

 observeEvent(input$apply_condition, {
   req(length(rv$selected_rows) > 0, input$batch_condition != "")
   rv$sample_metadata$Condition[rv$selected_rows] <- input$batch_condition
   showNotification(paste("Condition set to '", input$batch_condition, "' for ",
                         length(rv$selected_rows), " samples"), type = "message")
 })

 observeEvent(input$apply_treatment, {
   req(length(rv$selected_rows) > 0, input$batch_treatment != "")
   rv$sample_metadata$Treatment[rv$selected_rows] <- input$batch_treatment
   showNotification(paste("Treatment set to '", input$batch_treatment, "' for ",
                         length(rv$selected_rows), " samples"), type = "message")
 })

 observeEvent(input$set_control, {
   req(length(rv$selected_rows) > 0)
   rv$sample_metadata$Is_Control <- FALSE  # Reset all
   rv$sample_metadata$Is_Control[rv$selected_rows] <- TRUE
   showNotification(paste(length(rv$selected_rows),
                         "sample(s) set as control group"), type = "message")
 })

 # ==================== QUICK SELECT FUNCTIONS ====================

 observeEvent(input$select_all, {
   req(rv$sample_metadata)
   proxy <- dataTableProxy("sample_table")
   selectRows(proxy, 1:nrow(rv$sample_metadata))
 })

 observeEvent(input$select_none, {
   proxy <- dataTableProxy("sample_table")
   selectRows(proxy, NULL)
 })

 observeEvent(input$select_odd, {
   req(rv$sample_metadata)
   proxy <- dataTableProxy("sample_table")
   odd_rows <- seq(1, nrow(rv$sample_metadata), by = 2)
   selectRows(proxy, odd_rows)
 })

 observeEvent(input$select_even, {
   req(rv$sample_metadata)
   proxy <- dataTableProxy("sample_table")
   even_rows <- seq(2, nrow(rv$sample_metadata), by = 2)
   selectRows(proxy, even_rows)
 })

 observeEvent(input$select_first_half, {
   req(rv$sample_metadata)
   proxy <- dataTableProxy("sample_table")
   first_half <- 1:ceiling(nrow(rv$sample_metadata) / 2)
   selectRows(proxy, first_half)
 })

 observeEvent(input$select_second_half, {
   req(rv$sample_metadata)
   proxy <- dataTableProxy("sample_table")
   n <- nrow(rv$sample_metadata)
   second_half <- (ceiling(n / 2) + 1):n
   selectRows(proxy, second_half)
 })

 observeEvent(input$clear_metadata, {
   req(rv$sample_metadata)
   rv$sample_metadata$Cell_Line <- ""
   rv$sample_metadata$Condition <- ""
   rv$sample_metadata$Treatment <- ""
   rv$sample_metadata$Is_Control <- FALSE
   showNotification("All metadata cleared", type = "message")
 })

 # ==================== OUTPUT FLAGS ====================
 output$file_uploaded <- reactive(rv$file_uploaded)
 output$analysis_complete <- reactive(rv$analysis_complete)
 outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
 outputOptions(output, "analysis_complete", suspendWhenHidden = FALSE)

 # File info
 output$file_info_ui <- renderUI({
   req(rv$raw_data)
   div(style = "margin-bottom: 1rem; padding: 1rem; background: var(--bg-light); border-radius: var(--radius-md);",
     tags$strong("Rows: "), nrow(rv$raw_data), " | ",
     tags$strong("Columns: "), ncol(rv$raw_data), " | ",
     tags$strong("Genes: "), floor(ncol(rv$raw_data) / as.numeric(input$reps))
   )
 })

 output$raw_data_table <- renderDT({
   req(rv$raw_data)
   datatable(head(rv$raw_data, 10),
            options = list(pageLength = 5, scrollX = TRUE, dom = 't'),
            rownames = FALSE)
 })

 # ==================== ANALYSIS ====================
 observeEvent(input$analyze, {
   req(rv$raw_data)

   withProgress(message = "Analyzing...", value = 0, {

     tryCatch({
       incProgress(0.1, detail = "Cleaning data...")

       cleaned <- clean_data(rv$raw_data, input$reps, input$remove_outliers, input$outlier_threshold)
       rv$cleaned_data <- cleaned$data
       rv$outlier_info <- cleaned$outlier_info

       incProgress(0.3, detail = "Computing ΔCt...")

       dct_result <- compute_dct(rv$cleaned_data, input$reps)
       rv$dct_data <- dct_result$dct_data

       incProgress(0.5, detail = "Computing expression...")

       rv$expr_data <- 2 ^ rv$dct_data

       incProgress(0.7, detail = "Computing relative expression...")

       # Use control group if defined
       control_rows <- which(rv$sample_metadata$Is_Control)
       if (length(control_rows) == 0) control_rows <- 1

       rv$rel_data <- compute_relative_expression(rv$expr_data, input$reps, control_rows)

       incProgress(0.9, detail = "Generating output...")

       rv$csv_data <- prepare_csv_output(rv$cleaned_data, rv$dct_data, rv$expr_data,
                                        rv$rel_data, input$reps, rv$sample_metadata)
       rv$qc_metrics <- calculate_qc_metrics(rv$cleaned_data, rv$expr_data, rv$rel_data, input$reps)

       rv$analysis_complete <- TRUE
       incProgress(1, detail = "Done!")

       showNotification("Analysis complete!", type = "message")
       updateTabsetPanel(session, "main_tabs", selected = "Results")

     }, error = function(e) {
       showNotification(paste("Error:", e$message), type = "error")
     })
   })
 })

 # Reset
 observeEvent(input$reset, {
   rv$raw_data <- NULL
   rv$sample_metadata <- NULL
   rv$selected_rows <- c()
   rv$cleaned_data <- NULL
   rv$dct_data <- NULL
   rv$expr_data <- NULL
   rv$rel_data <- NULL
   rv$csv_data <- NULL
   rv$analysis_complete <- FALSE
   rv$file_uploaded <- FALSE
   rv$qc_metrics <- NULL
   rv$outlier_info <- NULL
   reset("file")
   updateTabsetPanel(session, "main_tabs", selected = "Sample Setup")
   showNotification("Reset complete", type = "message")
 })

 # ==================== RESULT TABLES ====================
 output$ct_table <- renderDT({
   req(rv$cleaned_data)
   df <- round(rv$cleaned_data, 3)
   if (!is.null(rv$sample_metadata)) {
     df <- cbind(Sample = rv$sample_metadata$Sample,
                Cell_Line = rv$sample_metadata$Cell_Line,
                Condition = rv$sample_metadata$Condition, df)
   }
   datatable(df, options = list(pageLength = 10, scrollX = TRUE))
 })

 output$dct_table <- renderDT({
   req(rv$dct_data)
   df <- round(rv$dct_data, 3)
   if (!is.null(rv$sample_metadata)) {
     df <- cbind(Sample = rv$sample_metadata$Sample,
                Cell_Line = rv$sample_metadata$Cell_Line,
                Condition = rv$sample_metadata$Condition, df)
   }
   datatable(df, options = list(pageLength = 10, scrollX = TRUE))
 })

 output$expr_table <- renderDT({
   req(rv$expr_data)
   df <- round(rv$expr_data, 6)
   if (!is.null(rv$sample_metadata)) {
     df <- cbind(Sample = rv$sample_metadata$Sample,
                Cell_Line = rv$sample_metadata$Cell_Line,
                Condition = rv$sample_metadata$Condition, df)
   }
   datatable(df, options = list(pageLength = 10, scrollX = TRUE))
 })

 output$rel_table <- renderDT({
   req(rv$rel_data)
   df <- round(rv$rel_data, 6)
   if (!is.null(rv$sample_metadata)) {
     df <- cbind(Sample = rv$sample_metadata$Sample,
                Cell_Line = rv$sample_metadata$Cell_Line,
                Condition = rv$sample_metadata$Condition, df)
   }
   datatable(df, options = list(pageLength = 10, scrollX = TRUE))
 })

 output$expr_summary_table <- renderTable({
   req(rv$qc_metrics)
   rv$qc_metrics$expr_summary
 }, striped = TRUE, hover = TRUE, bordered = TRUE)

 output$rel_summary_table <- renderTable({
   req(rv$qc_metrics)
   rv$qc_metrics$rel_summary
 }, striped = TRUE, hover = TRUE, bordered = TRUE)

 output$qc_summary <- renderTable({
   req(rv$qc_metrics)
   rv$qc_metrics$qc_summary
 }, striped = TRUE, hover = TRUE, bordered = TRUE)

 output$outlier_summary <- renderTable({
   req(rv$outlier_info)
   if (nrow(rv$outlier_info) == 0) {
     data.frame(Status = "No outliers removed")
   } else {
     rv$outlier_info
   }
 }, striped = TRUE, hover = TRUE, bordered = TRUE)

 # ==================== PLOTS ====================
 output$ct_boxplot <- renderPlot({
   req(rv$cleaned_data)
   plot_ct_boxplot(rv$cleaned_data, input$reps, rv$sample_metadata)
 }, res = 100)

 output$expr_boxplot <- renderPlot({
   req(rv$expr_data)
   plot_expr_boxplot(rv$expr_data, input$reps, rv$sample_metadata)
 }, res = 100)

 output$rel_heatmap <- renderPlot({
   req(rv$rel_data)
   plot_rel_heatmap(rv$rel_data, input$reps, rv$sample_metadata)
 }, res = 100)

 output$cv_plot <- renderPlot({
   req(rv$qc_metrics)
   plot_cv_plot(rv$qc_metrics$cv_data, input$reps)
 }, res = 100)

 # Download
 output$downloadData <- downloadHandler(
   filename = function() { paste0("qpcr_analysis_", Sys.Date(), ".csv") },
   content = function(file) {
     write.table(rv$csv_data, file = file, sep = ",",
                col.names = FALSE, row.names = FALSE, quote = FALSE, na = "")
   }
 )

 # ==================== HELPER FUNCTIONS ====================

 clean_data <- function(raw_data, reps, remove_outliers, threshold) {
   cleaned <- raw_data
   outlier_info <- data.frame(Gene = character(), Sample = integer(),
                             Outliers_Removed = integer(), stringsAsFactors = FALSE)

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
             extreme <- outliers[which.max(abs(block[outliers] - mean_val))]
             cleaned[row, start_col + extreme - 1] <- NA
             outlier_info <- rbind(outlier_info, data.frame(
               Gene = paste0("Gene", g), Sample = row, Outliers_Removed = 1))
           }
         }
       }
     }
   }
   list(data = cleaned, outlier_info = outlier_info)
 }

 compute_dct <- function(cleaned_data, reps) {
   n_reps <- reps
   n_genes <- ncol(cleaned_data) / n_reps

   gene_indices <- lapply(0:(n_genes-1), function(i) {
     start <- i * n_reps + 1
     (start):(start + n_reps - 1)
   })

   compute_dCt_optimal <- function(control_vals, target_vals) {
     if (all(is.na(control_vals)) || all(is.na(target_vals))) {
       return(rep(NA, length(target_vals)))
     }

     if (sum(!is.na(target_vals)) < 2) {
       dCt <- control_vals - target_vals
       dCt[is.na(control_vals) | is.na(target_vals)] <- NA
       return(dCt)
     }

     tryCatch({
       perms <- permutations(n = length(target_vals), r = length(target_vals), v = target_vals)
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
       if (!is.null(best_dCt)) return(best_dCt)
     }, error = function(e) {})

     dCt <- control_vals - target_vals
     dCt[is.na(control_vals) | is.na(target_vals)] <- NA
     dCt
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

   list(dct_data = as.data.frame(do.call(rbind, dCt_all)))
 }

 compute_relative_expression <- function(expr_data, reps, control_rows = 1) {
   # Use mean of control rows as reference
   if (length(control_rows) == 1) {
     control_expr <- as.numeric(expr_data[control_rows, ])
   } else {
     control_expr <- colMeans(expr_data[control_rows, ], na.rm = TRUE)
   }

   compute_relative_optimal <- function(control_vals, treat_vals) {
     if (all(is.na(control_vals)) || all(is.na(treat_vals))) {
       return(rep(NA, length(treat_vals)))
     }

     if (sum(!is.na(treat_vals)) < 2) {
       rel_vals <- treat_vals / control_vals
       rel_vals[is.na(treat_vals) | is.na(control_vals)] <- NA
       return(rel_vals)
     }

     tryCatch({
       perms <- permutations(n = length(control_vals), r = length(control_vals), v = control_vals)
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
       if (!is.null(best_rel)) return(best_rel)
     }, error = function(e) {})

     rel_vals <- treat_vals / control_vals
     rel_vals[is.na(treat_vals) | is.na(control_vals)] <- NA
     rel_vals
   }

   rel_all <- list()

   for (row in 1:nrow(expr_data)) {
     if (row %in% control_rows) {
       rel_all[[row]] <- rep(1, ncol(expr_data))
     } else {
       treat_vals <- as.numeric(expr_data[row, ])
       rel_all[[row]] <- compute_relative_optimal(control_expr, treat_vals)
     }
   }

   as.data.frame(do.call(rbind, rel_all))
 }

 calculate_qc_metrics <- function(cleaned_data, expr_data, rel_data, reps) {
   n_genes <- ncol(cleaned_data) / reps

   calc_summary <- function(data, n_reps) {
     means <- sds <- cvs <- list()
     for (g in 1:n_genes) {
       start_col <- (g - 1) * n_reps + 1
       end_col <- g * n_reps

       block_means <- apply(data[, start_col:end_col], 1, function(x)
         if (all(is.na(x))) NA else mean(x, na.rm = TRUE))
       block_sds <- apply(data[, start_col:end_col], 1, function(x)
         if (all(is.na(x))) NA else sd(x, na.rm = TRUE))

       means[[g]] <- block_means
       sds[[g]] <- block_sds
       cvs[[g]] <- block_sds / block_means * 100
     }
     list(means = do.call(cbind, means), sds = do.call(cbind, sds), cvs = do.call(cbind, cvs))
   }

   expr_summary <- calc_summary(expr_data, reps)
   rel_summary <- calc_summary(rel_data, reps)

   expr_summary_table <- data.frame(
     Gene = paste0("Gene", 1:n_genes),
     Mean = colMeans(expr_summary$means, na.rm = TRUE),
     SD = colMeans(expr_summary$sds, na.rm = TRUE),
     CV = colMeans(expr_summary$cvs, na.rm = TRUE)
   )

   rel_summary_table <- data.frame(
     Gene = paste0("Gene", 1:n_genes),
     Mean = colMeans(rel_summary$means, na.rm = TRUE),
     SD = colMeans(rel_summary$sds, na.rm = TRUE),
     CV = colMeans(rel_summary$cvs, na.rm = TRUE)
   )

   qc_summary <- data.frame(
     Metric = c("Samples", "Genes", "Missing %", "Mean CV%"),
     Value = c(nrow(cleaned_data), n_genes,
              round(sum(is.na(cleaned_data)) / length(as.matrix(cleaned_data)) * 100, 1),
              round(mean(colMeans(expr_summary$cvs, na.rm = TRUE), na.rm = TRUE), 1))
   )

   list(expr_summary = expr_summary_table, rel_summary = rel_summary_table,
       qc_summary = qc_summary, cv_data = expr_summary$cvs)
 }

 prepare_csv_output <- function(cleaned_data, dct_data, expr_data, rel_data, reps, metadata) {
   # Include sample metadata in output
   round_df <- function(df, digits = 6) {
     df[] <- lapply(df, function(x) if (is.numeric(x)) round(x, digits) else x)
     df
   }

   clean_block <- function(df) {
     mat <- as.matrix(df)
     mat[is.na(mat)] <- "#VALUE!"
     apply(mat, c(1, 2), as.character)
   }

   # Add metadata columns
   meta_cols <- data.frame(
     Sample = metadata$Sample,
     Cell_Line = metadata$Cell_Line,
     Condition = metadata$Condition,
     Treatment = metadata$Treatment
   )

   ct_with_meta <- cbind(meta_cols, round_df(cleaned_data, 3))
   dct_with_meta <- cbind(meta_cols, round_df(dct_data, 3))
   expr_with_meta <- cbind(meta_cols, round_df(expr_data, 6))
   rel_with_meta <- cbind(meta_cols, round_df(rel_data, 6))

   ct_block <- clean_block(ct_with_meta)
   dct_block <- clean_block(dct_with_meta)
   expr_block <- clean_block(expr_with_meta)
   rel_block <- clean_block(rel_with_meta)

   max_cols <- max(ncol(ct_block), ncol(dct_block), ncol(expr_block), ncol(rel_block))

   pad <- function(block, target) {
     if (ncol(block) < target) {
       block <- cbind(block, matrix("", nrow = nrow(block), ncol = target - ncol(block)))
     }
     block
   }

   ct_block <- pad(ct_block, max_cols)
   dct_block <- pad(dct_block, max_cols)
   expr_block <- pad(expr_block, max_cols)
   rel_block <- pad(rel_block, max_cols)

   title <- function(txt) matrix(c(txt, rep("", max_cols - 1)), nrow = 1)
   blank <- matrix("", nrow = 1, ncol = max_cols)

   rbind(
     title("### Cleaned Ct Data ###"), ct_block, blank,
     title("### Delta Ct Values ###"), dct_block, blank,
     title("### Expression Values ###"), expr_block, blank,
     title("### Relative Expression ###"), rel_block
   )
 }

 # ==================== PLOTTING ====================
 theme_modern <- function() {
   theme_minimal() +
     theme(
       plot.title = element_text(size = 12, face = "bold", color = "#1e293b"),
       plot.subtitle = element_text(size = 10, color = "#64748b"),
       axis.title = element_text(size = 10, color = "#475569"),
       axis.text = element_text(size = 9, color = "#64748b"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       panel.grid.major = element_line(color = "#e2e8f0", size = 0.5),
       panel.grid.minor = element_blank(),
       plot.background = element_rect(fill = "transparent", color = NA),
       panel.background = element_rect(fill = "transparent", color = NA)
     )
 }

 modern_colors <- c("#667eea", "#764ba2", "#f093fb", "#f5576c", "#4facfe", "#00f2fe", "#11998e", "#38ef7d")

 plot_ct_boxplot <- function(cleaned_data, reps, metadata = NULL) {
   n_genes <- ncol(cleaned_data) / reps

   plot_data <- data.frame()
   for (g in 1:n_genes) {
     start_col <- (g - 1) * reps + 1
     end_col <- g * reps

     for (row in 1:nrow(cleaned_data)) {
       values <- as.numeric(cleaned_data[row, start_col:end_col])

       label <- if (!is.null(metadata) && metadata$Condition[row] != "") {
         metadata$Condition[row]
       } else {
         paste0("S", row)
       }

       plot_data <- rbind(plot_data, data.frame(
         Gene = paste0("Gene ", g), Sample = label, Ct_Value = values))
     }
   }

   plot_data <- plot_data[!is.na(plot_data$Ct_Value), ]

   ggplot(plot_data, aes(x = Gene, y = Ct_Value, fill = Gene)) +
     geom_boxplot(alpha = 0.8, outlier.shape = NA, color = "#475569") +
     geom_jitter(width = 0.2, alpha = 0.6, size = 1.5, color = "#1e293b") +
     scale_fill_manual(values = modern_colors) +
     labs(title = "Ct Values Distribution", x = NULL, y = "Ct Value") +
     theme_modern() + theme(legend.position = "none")
 }

 plot_expr_boxplot <- function(expr_data, reps, metadata = NULL) {
   n_genes <- ncol(expr_data) / reps

   plot_data <- data.frame()
   for (g in 1:n_genes) {
     start_col <- (g - 1) * reps + 1
     end_col <- g * reps

     for (row in 1:nrow(expr_data)) {
       values <- as.numeric(expr_data[row, start_col:end_col])

       label <- if (!is.null(metadata) && metadata$Condition[row] != "") {
         metadata$Condition[row]
       } else {
         paste0("S", row)
       }

       plot_data <- rbind(plot_data, data.frame(
         Gene = paste0("Gene ", g), Sample = label, Expression = values))
     }
   }

   plot_data <- plot_data[!is.na(plot_data$Expression), ]

   ggplot(plot_data, aes(x = Gene, y = Expression, fill = Gene)) +
     geom_boxplot(alpha = 0.8, outlier.shape = NA, color = "#475569") +
     geom_jitter(width = 0.2, alpha = 0.6, size = 1.5, color = "#1e293b") +
     scale_fill_manual(values = modern_colors) +
     labs(title = "Expression Values", x = NULL, y = "Expression (2^ΔCt)") +
     theme_modern() + theme(legend.position = "none")
 }

 plot_rel_heatmap <- function(rel_data, reps, metadata = NULL) {
   n_genes <- ncol(rel_data) / reps

   mean_rel <- data.frame()
   for (g in 1:n_genes) {
     start_col <- (g - 1) * reps + 1
     end_col <- g * reps

     for (row in 1:nrow(rel_data)) {
       values <- as.numeric(rel_data[row, start_col:end_col])
       mean_val <- mean(values, na.rm = TRUE)

       label <- if (!is.null(metadata)) {
         parts <- c()
         if (metadata$Condition[row] != "") parts <- c(parts, metadata$Condition[row])
         if (metadata$Cell_Line[row] != "") parts <- c(parts, metadata$Cell_Line[row])
         if (length(parts) > 0) paste(parts, collapse = " - ") else paste0("Sample ", row)
       } else {
         paste0("Sample ", row)
       }

       if (!is.na(mean_val)) {
         mean_rel <- rbind(mean_rel, data.frame(
           Gene = paste0("Gene ", g), Sample = label, Relative_Expression = mean_val))
       }
     }
   }

   # Preserve order
   mean_rel$Sample <- factor(mean_rel$Sample, levels = unique(mean_rel$Sample))

   ggplot(mean_rel, aes(x = Gene, y = Sample, fill = Relative_Expression)) +
     geom_tile(color = "white", size = 0.5) +
     geom_text(aes(label = round(Relative_Expression, 2)), size = 3, color = "#1e293b") +
     scale_fill_gradient2(low = "#667eea", mid = "white", high = "#f5576c",
                         midpoint = 1, name = "Rel. Expr.") +
     labs(title = "Relative Expression Heatmap", x = NULL, y = NULL) +
     theme_modern() + theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.grid = element_blank())
 }

 plot_cv_plot <- function(cv_data, reps) {
   n_genes <- ncol(cv_data)

   plot_data <- data.frame()
   for (g in 1:n_genes) {
     plot_data <- rbind(plot_data, data.frame(
       Gene = paste0("Gene ", g), CV_percent = cv_data[, g]))
   }

   plot_data <- plot_data[!is.na(plot_data$CV_percent), ]

   ggplot(plot_data, aes(x = Gene, y = CV_percent, fill = Gene)) +
     geom_boxplot(alpha = 0.8, outlier.shape = NA, color = "#475569") +
     geom_jitter(width = 0.2, alpha = 0.6, size = 1.5, color = "#1e293b") +
     geom_hline(yintercept = 20, linetype = "dashed", color = "#f5576c", size = 1) +
     scale_fill_manual(values = modern_colors) +
     labs(title = "Coefficient of Variation", x = NULL, y = "CV (%)") +
     theme_modern() + theme(legend.position = "none")
 }
}

shinyApp(ui, server)
