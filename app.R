################################################################################
# app.R
#
# Zijie Feng's qPCR Cleaner and Analyzer
#
################################################################################

library(shiny)
library(gtools)
library(readxl)

ui <- fluidPage(
  titlePanel("Zijie’s qPCR Cleaner and Analyzer"),
  
  fileInput("file", "Upload CSV or Excel File (no header):"),
  
  radioButtons("reps", "Number of replicates per gene:",
               choices = c("3" = 3, "4" = 4),
               selected = 3,
               inline = TRUE),
  
  actionButton("analyze", "Run Analysis"),
  
  hr(),
  
  h4("Cleaned Ct Data:"),
  tableOutput("ct_table"),
  
  h4("ΔCt Values:"),
  tableOutput("dct_table"),
  
  h4("Expression Values (2^ΔCt):"),
  tableOutput("expr_table"),
  
  h4("Relative Expression vs. Control:"),
  tableOutput("rel_table"),
  
  hr(),
  
  downloadButton("downloadData", "Download Final CSV File")
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    raw_data = NULL,
    cleaned_data = NULL,
    dct_data = NULL,
    expr_data = NULL,
    rel_data = NULL,
    csv_data = NULL
  )
  
  observeEvent(input$analyze, {
    
    req(input$file)
    
    # --------------------------
    # STEP 1 — Load raw data
    # --------------------------
    file_path <- input$file$datapath
    file_name <- input$file$name
    
    if (grepl("\\.xlsx$", file_name, ignore.case = TRUE)) {
      raw <- read_excel(file_path,
                        col_names = FALSE,
                        na = c("Undetermined", "#VALUE!"))
    } else {
      raw <- read.csv(file_path,
                      header = FALSE,
                      stringsAsFactors = FALSE,
                      na.strings = c("Undetermined", "#VALUE!"))
    }
    
    raw <- raw[, colSums(is.na(raw)) < nrow(raw), drop = FALSE]
    raw <- raw[rowSums(is.na(raw)) < ncol(raw), , drop = FALSE]
    
    if (ncol(raw) == 0 || nrow(raw) == 0) {
      showNotification("Uploaded file has no usable data after cleaning.", type = "error")
      return(NULL)
    }
    
    # --------------------------
    # STEP 1b — Remove outliers
    # --------------------------
    reps <- as.numeric(input$reps)
    cleaned <- raw
    
    if (reps == 4) {
      cleaned <- t(apply(raw, 1, function(row) {
        new_row <- c()
        for (i in seq(1, ncol(raw), by = 4)) {
          block <- as.numeric(row[i:(i+3)])
          if (sum(!is.na(block)) < 3) {
            keep_vals <- rep(NA, 3)
          } else {
            block_mean <- mean(block, na.rm = TRUE)
            diff <- abs(block - block_mean)
            remove_idx <- which.max(diff)
            keep_vals <- block[-remove_idx]
          }
          new_row <- c(new_row, keep_vals)
        }
        return(new_row)
      }))
    }
    
    rv$cleaned_data <- cleaned
    rv$raw_data <- raw
    
    # --------------------------
    # STEP 2 — Compute ΔCt
    # --------------------------
    n_reps <- 3
    n_genes <- ncol(cleaned) / n_reps
    
    gene_indices <- lapply(0:(n_genes-1), function(i) {
      start <- i * n_reps + 1
      end <- start + n_reps - 1
      start:end
    })
    
    compute_dCt <- function(control_vals, target_vals) {
      if (all(is.na(control_vals)) || all(is.na(target_vals))) {
        return(rep(NA, length(target_vals)))
      }
      if (sum(!is.na(target_vals)) < length(target_vals)) {
        dCt <- control_vals - target_vals
        dCt[is.na(control_vals) | is.na(target_vals)] <- NA
        return(dCt)
      }
      if (length(unique(na.omit(target_vals))) < 2) {
        dCt <- control_vals - target_vals
        dCt[is.na(control_vals) | is.na(target_vals)] <- NA
        return(dCt)
      }
      perms <- tryCatch({
        permutations(n = length(target_vals),
                     r = length(target_vals),
                     v = target_vals)
      }, error = function(e) {
        return(NULL)
      })
      
      if (is.null(perms)) {
        dCt <- control_vals - target_vals
        dCt[is.na(control_vals) | is.na(target_vals)] <- NA
        return(dCt)
      }
      
      best_dCt <- NULL
      min_sd <- Inf
      
      for (i in 1:nrow(perms)) {
        pair <- perms[i, ]
        dCt <- control_vals - pair
        dCt[is.na(control_vals) | is.na(pair)] <- NA
        if (sum(!is.na(dCt)) < 2) next
        current_sd <- sd(dCt, na.rm = TRUE)
        if (current_sd < min_sd) {
          min_sd <- current_sd
          best_dCt <- dCt
        }
      }
      if (is.null(best_dCt)) {
        dCt <- control_vals - target_vals
        dCt[is.na(control_vals) | is.na(target_vals)] <- NA
        return(dCt)
      }
      return(best_dCt)
    }
    
    dCt_all <- list()
    
    for (row in 1:nrow(cleaned)) {
      row_vals <- as.numeric(cleaned[row, ])
      ref_vals <- row_vals[gene_indices[[1]]]
      dCt_row <- c()
      for (g in 2:n_genes) {
        target_vals <- row_vals[gene_indices[[g]]]
        dCt_vals <- compute_dCt(ref_vals, target_vals)
        dCt_row <- c(dCt_row, dCt_vals)
      }
      dCt_row <- c(rep(NA, n_reps), dCt_row)
      dCt_all[[row]] <- dCt_row
    }
    
    dCt_df <- as.data.frame(do.call(rbind, dCt_all))
    rv$dct_data <- dCt_df
    
    # --------------------------
    # STEP 3 — Compute Expression
    # --------------------------
    expr_df <- 2 ^ dCt_df
    expr_df[is.na(dCt_df)] <- NA
    rv$expr_data <- expr_df
    
    # --------------------------
    # STEP 4 — Relative Expression
    # --------------------------
    control_expr <- as.numeric(expr_df[1, ])
    
    compute_relative <- function(control_vals, treat_vals) {
      if (all(is.na(control_vals)) || all(is.na(treat_vals))) {
        return(rep(NA, length(treat_vals)))
      }
      ctrl_not_na <- na.omit(control_vals)
      if (length(ctrl_not_na) < 2 || length(unique(ctrl_not_na)) < 2) {
        rel_vals <- treat_vals / control_vals
        rel_vals[is.na(treat_vals) | is.na(control_vals)] <- NA
        return(rel_vals)
      }
      perms <- tryCatch({
        permutations(n = length(control_vals),
                     r = length(control_vals),
                     v = control_vals)
      }, error = function(e) {
        return(NULL)
      })
      if (is.null(perms)) {
        rel_vals <- treat_vals / control_vals
        rel_vals[is.na(treat_vals) | is.na(control_vals)] <- NA
        return(rel_vals)
      }
      best_rel <- NULL
      min_sd <- Inf
      for (i in 1:nrow(perms)) {
        perm_ctrl <- perms[i, ]
        rel_vals <- treat_vals / perm_ctrl
        rel_vals[is.na(treat_vals) | is.na(perm_ctrl)] <- NA
        if (sum(!is.na(rel_vals)) < 2) next
        sd_val <- sd(rel_vals, na.rm = TRUE)
        if (sd_val < min_sd) {
          min_sd <- sd_val
          best_rel <- rel_vals
        }
      }
      if (is.null(best_rel)) {
        rel_vals <- treat_vals / control_vals
        rel_vals[is.na(treat_vals) | is.na(control_vals)] <- NA
        return(rel_vals)
      }
      return(best_rel)
    }
    
    rel_all <- list()
    
    for (row in 1:nrow(expr_df)) {
      if (row == 1) {
        rel_all[[row]] <- rep(1, ncol(expr_df))
      } else {
        treat_vals <- as.numeric(expr_df[row, ])
        rel_vals <- compute_relative(control_expr, treat_vals)
        rel_all[[row]] <- rel_vals
      }
    }
    
    rel_df <- as.data.frame(do.call(rbind, rel_all))
    rv$rel_data <- rel_df
    
    # --------------------------
    # STEP 5 — Prepare CSV
    # --------------------------
    
    round_data_frame <- function(df, digits = 8) {
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
    
    ct_block   <- clean_csv_block(round_data_frame(rv$cleaned_data))
    dct_block  <- clean_csv_block(round_data_frame(rv$dct_data))
    expr_block <- clean_csv_block(round_data_frame(rv$expr_data))
    rel_block  <- clean_csv_block(round_data_frame(rv$rel_data))
    
    compute_mean_sd <- function(df, n_reps) {
      if (is.null(df) || nrow(df) == 0) {
        return(list(mean = data.frame(), sd = data.frame()))
      }
      
      df <- as.data.frame(df)
      n_genes <- ncol(df) / n_reps
      
      means <- list()
      sds <- list()
      
      for (g in 1:n_genes) {
        idxs <- ((g - 1) * n_reps + 1):(g * n_reps)
        block <- df[, idxs, drop = FALSE]
        
        block_mean <- apply(block, 1, function(x) {
          if (all(is.na(x))) NA else mean(x, na.rm = TRUE)
        })
        block_sd <- apply(block, 1, function(x) {
          if (all(is.na(x))) NA else sd(x, na.rm = TRUE)
        })
        
        means[[g]] <- block_mean
        sds[[g]] <- block_sd
      }
      
      mean_df <- as.data.frame(do.call(cbind, means))
      sd_df   <- as.data.frame(do.call(cbind, sds))
      
      names(mean_df) <- paste0("Gene", seq_len(n_genes))
      names(sd_df)   <- paste0("Gene", seq_len(n_genes))
      
      return(list(mean = mean_df, sd = sd_df))
    }
    
    expr_summary <- compute_mean_sd(rv$expr_data, n_reps = 3)
    rel_summary  <- compute_mean_sd(rv$rel_data, n_reps = 3)
    
    expr_mean_block <- clean_csv_block(round_data_frame(expr_summary$mean))
    expr_sd_block   <- clean_csv_block(round_data_frame(expr_summary$sd))
    
    rel_mean_block <- clean_csv_block(round_data_frame(rel_summary$mean))
    rel_sd_block   <- clean_csv_block(round_data_frame(rel_summary$sd))
    
    max_cols <- max(
      ncol(ct_block),
      ncol(dct_block),
      ncol(expr_block),
      ncol(rel_block),
      ncol(expr_mean_block),
      ncol(expr_sd_block),
      ncol(rel_mean_block),
      ncol(rel_sd_block),
      na.rm = TRUE
    )
    
    pad_block <- function(block, target_cols) {
      if (is.null(block) || nrow(block) == 0) {
        return(matrix("", nrow = 1, ncol = target_cols))
      }
      if (ncol(block) < target_cols) {
        extra_cols <- target_cols - ncol(block)
        block <- cbind(block, matrix("", nrow = nrow(block), ncol = extra_cols))
      }
      return(block)
    }
    
    ct_block          <- pad_block(ct_block, max_cols)
    dct_block         <- pad_block(dct_block, max_cols)
    expr_block        <- pad_block(expr_block, max_cols)
    rel_block         <- pad_block(rel_block, max_cols)
    expr_mean_block   <- pad_block(expr_mean_block, max_cols)
    expr_sd_block     <- pad_block(expr_sd_block, max_cols)
    rel_mean_block    <- pad_block(rel_mean_block, max_cols)
    rel_sd_block      <- pad_block(rel_sd_block, max_cols)
    
    title_ct            <- matrix(c("### Cleaned Ct Data ###", rep("", max_cols - 1)), nrow = 1)
    title_dct           <- matrix(c("### ΔCt Values ###", rep("", max_cols - 1)), nrow = 1)
    title_expr          <- matrix(c("### Expression Values (2^ΔCt) ###", rep("", max_cols - 1)), nrow = 1)
    title_expr_mean     <- matrix(c("### Expression Means ###", rep("", max_cols - 1)), nrow = 1)
    title_expr_sd       <- matrix(c("### Expression SDs ###", rep("", max_cols - 1)), nrow = 1)
    title_rel           <- matrix(c("### Relative Expression vs. Control ###", rep("", max_cols - 1)), nrow = 1)
    title_rel_mean      <- matrix(c("### Relative Expression Means ###", rep("", max_cols - 1)), nrow = 1)
    title_rel_sd        <- matrix(c("### Relative Expression SDs ###", rep("", max_cols - 1)), nrow = 1)
    
    blank_row <- matrix("", nrow = 1, ncol = max_cols)
    
    final_csv <- rbind(
      title_ct,
      ct_block,
      blank_row,
      title_dct,
      dct_block,
      blank_row,
      title_expr,
      expr_block,
      blank_row,
      title_expr_mean,
      expr_mean_block,
      blank_row,
      title_expr_sd,
      expr_sd_block,
      blank_row,
      title_rel,
      rel_block,
      blank_row,
      title_rel_mean,
      rel_mean_block,
      blank_row,
      title_rel_sd,
      rel_sd_block
    )
    
    rv$csv_data <- final_csv
    
  }) # closes observeEvent
  
  output$ct_table <- renderTable({
    req(rv$cleaned_data)
    round(head(rv$cleaned_data, 10), 8)
  })
  
  output$dct_table <- renderTable({
    req(rv$dct_data)
    round(head(rv$dct_data, 10), 8)
  })
  
  output$expr_table <- renderTable({
    req(rv$expr_data)
    round(head(rv$expr_data, 10), 8)
  })
  
  output$rel_table <- renderTable({
    req(rv$rel_data)
    round(head(rv$rel_data, 10), 8)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("qpcr_analysis_", Sys.Date(), ".csv")
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
  
}

shinyApp(ui, server)