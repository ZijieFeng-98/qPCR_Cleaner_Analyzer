# qPCR Cleaner and Analyzer (Shiny)

This app:
- Uploads raw Ct data (CSV or .xlsx)
- Optionally removes 1 outlier from blocks of 4 replicates
- Computes ΔCt with optimal replicate pairing (min SD)
- Converts to expression (2^ΔCt)
- Computes relative expression vs. control (optimal pairing)
- Summarizes with mean and SD
- Exports everything as a single CSV with titled sections

## Run locally
```r
install.packages(c("shiny","gtools","readxl"))
shiny::runApp("/Users/zijiefeng/Desktop/Guo's lab/APP")
