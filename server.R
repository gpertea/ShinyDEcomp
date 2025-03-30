# server.R (Complete Code - Final Version Based on Discussion)

library(shiny)
library(data.table)
library(qs)
library(tools)
library(graphics)
library(RColorBrewer)

# Helper function to load DE results
load_de_res <- function(filepath) {
  ext <- tolower(tools::file_ext(filepath))
  df <- NULL
  obj_name <- NULL # For .Rdata

  tryCatch({
    if (ext %in% c("tsv", "tab", "gz")) {
      df <- data.table::fread(filepath, data.table = FALSE)
    } else if (ext == "qs") {
      df <- qs::qread(filepath)
    } else if (ext == "rds") {
      df <- readRDS(filepath)
    } else if (ext == "rdata") {
      env <- new.env()
      obj_name <- load(filepath, envir = env)
      for(obj in obj_name) {
          if(is.data.frame(env[[obj]])) {
              df <- env[[obj]]
              break
          }
      }
      if (is.null(df)) stop("No data.frame object found in .Rdata file")
    } else {
      stop("Unsupported file type.")
    }

    required_cols <- c("t", "logFC", "P.Value", "adj.P.Val")
    if (!all(required_cols %in% names(df))) {
      stop(paste("Missing required columns. Need:", paste(required_cols, collapse=", ")))
    }

    if (nrow(df) > 0 && (is.null(rownames(df)) || all(rownames(df) == as.character(1:nrow(df))))) {
        if ("fid" %in% names(df)) {
            if (any(duplicated(df$fid))) stop("Feature IDs ('fid' column) are not unique.")
            rownames(df) <- df$fid
        } else {
            stop("Data frame lacks rownames and a 'fid' column to use as feature IDs.")
        }
    } else if (nrow(df) > 0) {
         if (any(duplicated(rownames(df)))) stop("Feature IDs (rownames) are not unique.")
    }

    for (col in required_cols) {
        if(!is.numeric(df[[col]])) {
            stop(paste("Column", col, "is not numeric."))
        }
    }

    df <- df[order(df$t, decreasing = TRUE), ]
    return(df)

  }, error = function(e) {
    showNotification(paste("Error loading file:", e$message), type = "error", duration = 10)
    return(NULL)
  })
}

# Helper function for line bundling (simple binning)
bundle_lines <- function(y1_vals, y2_vals, n_bins = 100) {
    if (length(y1_vals) == 0 || length(y2_vals) == 0) {
        return(data.frame(y1 = numeric(0), y2 = numeric(0), count = integer(0)))
    }
    breaks <- seq(0, 1, length.out = n_bins + 1)
    breaks[1] <- breaks[1] - .Machine$double.eps
    breaks[n_bins + 1] <- breaks[n_bins + 1] + .Machine$double.eps
    y1_bins <- cut(y1_vals, breaks = breaks, include.lowest = TRUE, labels = FALSE)
    y2_bins <- cut(y2_vals, breaks = breaks, include.lowest = TRUE, labels = FALSE)
    bin_counts <- as.data.frame(table(y1_bin = y1_bins, y2_bin = y2_bins))
    bin_counts <- bin_counts[bin_counts$Freq > 0, ]
    bin_counts$y1_bin <- as.integer(as.character(bin_counts$y1_bin))
    bin_counts$y2_bin <- as.integer(as.character(bin_counts$y2_bin))
    bin_counts <- na.omit(bin_counts)
    if(nrow(bin_counts) == 0) {
       return(data.frame(y1 = numeric(0), y2 = numeric(0), count = integer(0)))
    }
    bin_centers <- (breaks[-1] + breaks[-(n_bins + 1)]) / 2
    bundled <- data.frame(
        y1 = bin_centers[bin_counts$y1_bin],
        y2 = bin_centers[bin_counts$y2_bin],
        count = bin_counts$Freq
    )
    return(bundled)
}

# --- Server Logic ---
server <- function(input, output, session) {

  rv <- reactiveValues(
    de1 = NULL, de2 = NULL,
    fn1 = NULL, fn2 = NULL, # Stores successfully loaded filename
    # --- NEW: Add loading status ---
    load_status1 = "idle", # Possible values: "idle", "loading", "loaded", "error"
    load_status2 = "idle",
    # ------------------------------
    lbl1 = "DE-results-1", lbl2 = "DE-results-2"
  )

  # --- Observers for Loading Data ---
  observeEvent(input$load1, {
    req(input$load1)
    file_info <- input$load1
    datapath <- file_info$datapath
    filename <- file_info$name

    # --- Update status and clear previous state ---
    rv$load_status1 <- "loading"
    rv$de1 <- NULL
    rv$fn1 <- NULL

    progress <- shiny::Progress$new(session, min=0, max=1)
    progress$set(message = "Loading file...", value = 0.2)
    on.exit(progress$close())

    tryCatch({
        loaded_data <- load_de_res(datapath)
        progress$set(detail = "Processing data...", value = 0.8)

        if (!is.null(loaded_data)) {
          rv$de1 <- loaded_data
          rv$fn1 <- filename # Store filename ONLY on successful load
          rv$load_status1 <- "loaded" # Set status to loaded
          # (Top N check logic remains the same)
          current_max_rows <- nrow(rv$de1)
          if (input$top_n > current_max_rows && input$top_n != 0) {
              updateNumericInput(session, "top_n", value = current_max_rows)
              if(input$topn_slider > current_max_rows) {
                  updateSliderInput(session, "topn_slider", value = min(current_max_rows, 1000))
              }
          }
        } else {
          # load_de_res handles error notification
          rv$de1 <- NULL
          rv$fn1 <- NULL
          rv$load_status1 <- "error" # Set status to error
        }
    }, error = function(e) {
        showNotification(paste("Failed to load:", filename, "-", e$message), type = "error", duration = 10)
        rv$de1 <- NULL
        rv$fn1 <- NULL
        rv$load_status1 <- "error" # Set status to error on exception
    })
    progress$set(value = 1)
  })

 observeEvent(input$load2, {
    req(input$load2)
    file_info <- input$load2
    datapath <- file_info$datapath
    filename <- file_info$name

    # --- Update status and clear previous state ---
    rv$load_status2 <- "loading"
    rv$de2 <- NULL
    rv$fn2 <- NULL

    progress <- shiny::Progress$new(session, min=0, max=1)
    progress$set(message = "Loading file...", value = 0.2)
    on.exit(progress$close())

     tryCatch({
        loaded_data <- load_de_res(datapath)
        progress$set(detail = "Processing data...", value = 0.8)

        if (!is.null(loaded_data)) {
          rv$de2 <- loaded_data
          rv$fn2 <- filename # Store filename ONLY on success
          rv$load_status2 <- "loaded" # Set status to loaded
          # (Top N check logic remains the same)
          current_max_rows <- nrow(rv$de2)
           if (input$top_n > current_max_rows && input$top_n != 0) {
              updateNumericInput(session, "top_n", value = current_max_rows)
              if(input$topn_slider > current_max_rows) {
                   updateSliderInput(session, "topn_slider", value = min(current_max_rows, 1000))
              }
          }
        } else {
          rv$de2 <- NULL
          rv$fn2 <- NULL
          rv$load_status2 <- "error"
        }
    }, error = function(e) {
        showNotification(paste("Failed to load:", filename, "-", e$message), type = "error", duration = 10)
        rv$de2 <- NULL
        rv$fn2 <- NULL
        rv$load_status2 <- "error"
    })
     progress$set(value = 1)
  })

  # --- Output: Display Filename or Status ---
   output$fn_display1 <- renderText({
       status <- rv$load_status1
       if (status == "loading") {
           "loading..."
       } else if (status == "loaded") {
           req(rv$fn1) # Should be set if status is loaded
           rv$fn1
       } else if (status == "error") {
           "load failed" # Or keep empty: ""
       } else { # idle
           "" # Empty if idle
       }
   })
   output$fn_display2 <- renderText({
      status <- rv$load_status2
       if (status == "loading") {
           "loading..."
       } else if (status == "loaded") {
           req(rv$fn2)
           rv$fn2
       } else if (status == "error") {
           "load failed" # Or keep empty: ""
       } else { # idle
           ""
       }
   })

  # --- Observers for Labels ---
  observeEvent(input$label1, { rv$lbl1 <- input$label1 })
  observeEvent(input$label2, { rv$lbl2 <- input$label2 })

  # --- Observers for syncing sliders and numeric inputs ---
  observeEvent(input$pval_thresh, {
      updateSliderInput(session, "pval_slider", value = input$pval_thresh)
  })
  observeEvent(input$pval_slider, {
      updateNumericInput(session, "pval_thresh", value = input$pval_slider)
  })

  observeEvent(input$top_n, {
      # Ensure value doesn't exceed actual max features, update slider (max 1000)
      current_max <- max(if(!is.null(rv$de1)) nrow(rv$de1) else 0, if(!is.null(rv$de2)) nrow(rv$de2) else 0, 1)
      valid_top_n <- input$top_n
      if (valid_top_n > current_max && valid_top_n != 0) {
          valid_top_n <- current_max
          updateNumericInput(session, "top_n", value = valid_top_n) # Correct numeric input if > actual max
      }
      # Update slider, clamping value at 1000
      updateSliderInput(session, "topn_slider", value = min(valid_top_n, 1000))
  })
  observeEvent(input$topn_slider, {
      # If slider changes, update numeric input. Slider max is 1000.
      updateNumericInput(session, "top_n", value = input$topn_slider)
  })

  # --- Reactive Expressions for Selection ---
  selected_indices1 <- reactive({
    req(rv$de1)
    n_total <- nrow(rv$de1)
    idx_pval <- which(rv$de1$P.Value <= input$pval_thresh)
    if (input$top_n > 0) {
      idx_topn <- order(rv$de1$P.Value, decreasing = FALSE)[1:min(input$top_n, n_total)]
      intersect(idx_pval, idx_topn)
    } else {
      idx_pval
    }
  })

  selected_indices2 <- reactive({
    req(rv$de2)
    n_total <- nrow(rv$de2)
    idx_pval <- which(rv$de2$P.Value <= input$pval_thresh)
    if (input$top_n > 0) {
      idx_topn <- order(rv$de2$P.Value, decreasing = FALSE)[1:min(input$top_n, n_total)]
      intersect(idx_pval, idx_topn)
    } else {
      idx_pval
    }
  })

  # --- Reactive Expression for Overlapping Features ---
  overlapping_features <- reactive({
    req(rv$de1, rv$de2, selected_indices1(), selected_indices2())
    ids1 <- rownames(rv$de1)[selected_indices1()]
    ids2 <- rownames(rv$de2)[selected_indices2()]
    intersect(ids1, ids2)
  })

  # --- Output: Overlap Text ---
  output$overlap_text <- renderText({
    n_overlap <- length(overlapping_features())
    paste(n_overlap, "overlapping features in selection")
  })

  # --- Output: Plot Labels (for above plot) ---
  output$plot_label1 <- renderText({ rv$lbl1 })
  output$plot_label2 <- renderText({ rv$lbl2 })

  # --- Output: Static Feature Counts (with selective bolding) ---
  output$fcount1_static <- renderUI({
    req(rv$de1)
    HTML(paste0("<strong>", nrow(rv$de1), "</strong> features"))
  })
  output$fcount2_static <- renderUI({
    req(rv$de2)
    HTML(paste0("<strong>", nrow(rv$de2), "</strong> features"))
  })

  # --- Output: Static FDR Counts (with selective bolding) ---
  output$fdr_count1_static <- renderUI({
      req(rv$de1)
      count <- sum(rv$de1$adj.P.Val < 0.05, na.rm = TRUE)
      HTML(paste0("FDR < .05: <strong>", count, "</strong>"))
  })
   output$fdr_count2_static <- renderUI({
      req(rv$de2)
      count <- sum(rv$de2$adj.P.Val < 0.05, na.rm = TRUE)
      HTML(paste0("FDR < .05: <strong>", count, "</strong>"))
  })

  # --- Output: Summaries (React to selection changes) ---
  output$summary1 <- renderPrint({
    # This still depends on selected_indices1()
    req(rv$de1, input$summary_metric1)
    sel_idx <- selected_indices1()
    if (length(sel_idx) == 0) {
        cat("No features selected.")
    } else {
        # Ensure the column exists before trying to summarize
        if(input$summary_metric1 %in% names(rv$de1)) {
             data_to_summarize <- rv$de1[sel_idx, input$summary_metric1]
             summary(data_to_summarize)
        } else {
             cat("Selected metric not found.")
        }
    }
  })

  output$summary2 <- renderPrint({
    # This still depends on selected_indices2()
    req(rv$de2, input$summary_metric2)
    sel_idx <- selected_indices2()
     if (length(sel_idx) == 0) {
        cat("No features selected.")
    } else {
        if(input$summary_metric2 %in% names(rv$de2)) {
            data_to_summarize <- rv$de2[sel_idx, input$summary_metric2]
            summary(data_to_summarize)
        } else {
             cat("Selected metric not found.")
        }
    }
  })

  # --- Color Mapping Function (Vectorized) ---
  va_cols <- colorRampPalette(c("red", "blue", "red"))(101)
  map_t_to_col <- function(t_vals, max_abs_t_global) {
      if (!is.numeric(t_vals)) return(rep("grey50", length(t_vals)))
      if (!is.finite(max_abs_t_global) || max_abs_t_global <= 0) max_abs_t_global <- 1
      na_indices <- !is.finite(t_vals)
      t_vals[na_indices] <- 0
      scaled_t <- t_vals / max_abs_t_global
      scaled_t <- pmax(-1, pmin(1, scaled_t))
      transformed_t <- sign(scaled_t) * sqrt(abs(scaled_t))
      col_idx <- round((transformed_t + 1) / 2 * 100) + 1
      col_idx <- pmax(1, pmin(101, col_idx))
      final_colors <- va_cols[col_idx]
      # final_colors[na_indices] <- "grey80" # Optional: Color NAs differently
      return(final_colors)
  }

  # --- Y-axis Scaling Function (Center Point) ---
  map_idx_to_y <- function(idx, n_current, n_max_global, margin = 0.01) {
      total_height <- 1 - 2 * margin
      if (n_max_global == 0) return(0.5) # Center point if no features
      bar_height_ratio <- ifelse(n_max_global > 0, n_current / n_max_global, 1)
      bar_actual_height <- total_height * bar_height_ratio
      bar_top_offset <- margin + (total_height - bar_actual_height) / 2
      if (n_current == 0) return(bar_top_offset + bar_actual_height / 2)
      y_pos <- bar_top_offset + ((n_current - idx + 0.5) / n_current) * bar_actual_height
      return(y_pos)
  }

  # --- Y-axis Segment Boundary Function ---
  map_idx_to_segment_y <- function(idx, n_current, n_max_global, margin = 0.01) {
      total_height <- 1 - 2 * margin
      if (n_max_global == 0) return(list(top = 1 - margin, bottom = margin))
      bar_height_ratio <- ifelse(n_max_global > 0, n_current / n_max_global, 1)
      bar_actual_height <- total_height * bar_height_ratio
      bar_top_offset <- margin + (total_height - bar_actual_height) / 2
      if (n_current == 0) return(list(top = bar_top_offset + bar_actual_height, bottom = bar_top_offset))
      y_top <- bar_top_offset + ((n_current - idx + 1) / n_current) * bar_actual_height
      y_bottom <- bar_top_offset + ((n_current - idx) / n_current) * bar_actual_height
      y_top <- ifelse(is.finite(y_top), y_top, 1 - margin)
      y_bottom <- ifelse(is.finite(y_bottom), y_bottom, margin)
      return(list(top = y_top, bottom = y_bottom))
  }

  # --- Draw Bars Function ---
  draw_bar <- function(d, n, bar_x_center, bar_w_rel, n_max_global, sel_idx, max_abs_t_for_col) {
      if(n == 0) return(list(top = NA, bottom = NA))

      segment_coords <- map_idx_to_segment_y(1:n, n, n_max_global)
      y_tops <- segment_coords$top
      y_bottoms <- segment_coords$bottom
      segment_colors <- map_t_to_col(d$t[1:n], max_abs_t_global = max_abs_t_for_col)

      # Draw colored segments (no border)
      rect(bar_x_center - bar_w_rel / 2, y_bottoms,
           bar_x_center + bar_w_rel / 2, y_tops,
           col = segment_colors, border = NA)

      # Draw grey rectangle for non-selected features (no border)
      if (length(sel_idx) < n) {
           unsel_idx <- setdiff(1:n, sel_idx)
           if (length(unsel_idx) > 0) {
               unsel_diff <- diff(unsel_idx)
               block_starts_idx <- c(1, which(unsel_diff != 1) + 1)
               block_ends_idx <- c(which(unsel_diff != 1), length(unsel_idx))
               unsel_block_starts <- unsel_idx[block_starts_idx]
               unsel_block_ends <- unsel_idx[block_ends_idx]

               for(j in 1:length(unsel_block_starts)){
                   start_idx <- unsel_block_starts[j]
                   end_idx <- unsel_block_ends[j]
                   y_block_top <- y_tops[start_idx]
                   y_block_bottom <- y_bottoms[end_idx]
                   rect(bar_x_center - bar_w_rel / 2, y_block_bottom,
                        bar_x_center + bar_w_rel / 2, y_block_top,
                        col = "grey80", border = NA)
               }
           }
      }
      # Bar border drawing REMOVED

      # Return y-coordinates of selection boundaries
      if(length(sel_idx) > 0) {
           min_sel_idx <- min(sel_idx)
           max_sel_idx <- max(sel_idx)
           y_bound_top <- y_tops[min_sel_idx]
           y_bound_bottom <- y_bottoms[max_sel_idx]
           return(list(top = y_bound_top, bottom = y_bound_bottom))
      } else {
           return(list(top = NA, bottom = NA))
      }
  } # End draw_bar function definition


  # --- Output: Visualization Area Plot ---
  output$va_plot <- renderPlot({
    req(rv$de1, rv$de2)

    d1 <- rv$de1
    d2 <- rv$de2
    n1 <- nrow(d1)
    n2 <- nrow(d2)
    n_max <- max(n1, n2)
    if (n_max == 0) return()

    sel_idx1 <- selected_indices1()
    sel_idx2 <- selected_indices2()
    ovlp_ids <- overlapping_features()

    # --- Plot Setup ---
    plot_margin <- 0.01
    target_bar_px <- 38
    plot_width_px <- session$clientData$`output_va_plot_width`
    if (is.null(plot_width_px) || !is.finite(plot_width_px) || plot_width_px <= 0) {
        plot_width_px <- 390
    }
    plot_width_px <- max(plot_width_px, 1)
    bar_width_relative <- target_bar_px / plot_width_px
    bar_width_relative <- min(bar_width_relative, 0.4)
    bar_spacing <- 0.2
    total_bar_relative_width <- 2 * bar_width_relative
    if (total_bar_relative_width + bar_spacing >= 1) {
        bar_spacing <- max(0, 1 - total_bar_relative_width)
    }
    remaining_space <- 1 - total_bar_relative_width - bar_spacing
    left_margin <- remaining_space / 2
    bar1_x <- left_margin + bar_width_relative / 2
    bar2_x <- bar1_x + bar_width_relative / 2 + bar_spacing + bar_width_relative / 2

    max_abs_t_global <- max(abs(c(d1$t, d2$t)), na.rm = TRUE)
    if (!is.finite(max_abs_t_global) || max_abs_t_global == 0) max_abs_t_global <- 1

    par(mar = c(0.5, 0.5, 0.5, 0.5))
    plot(NA, xlim = c(0, 1), ylim = c(0, 1), type = 'n', axes = FALSE, xlab = "", ylab = "")

    # Draw the bars (no borders, returns selection bounds if any)
    bounds1 <- draw_bar(d1, n1, bar1_x, bar_width_relative, n_max, sel_idx1, max_abs_t_global)
    bounds2 <- draw_bar(d2, n2, bar2_x, bar_width_relative, n_max, sel_idx2, max_abs_t_global)

    # Dashed line drawing REMOVED

    # --- Draw Connecting Lines for Overlapping Features ---
    if (length(ovlp_ids) > 0) {
        pos1 <- match(ovlp_ids, rownames(d1))
        pos2 <- match(ovlp_ids, rownames(d2))
        valid_match <- !is.na(pos1) & !is.na(pos2)
        pos1 <- pos1[valid_match]
        pos2 <- pos2[valid_match]

        if(length(pos1) > 0) {
            t1_vals <- d1$t[pos1]
            t2_vals <- d2$t[pos2]
            t1_sign <- ifelse(t1_vals > 0, 1, ifelse(t1_vals < 0, -1, 0))
            t2_sign <- ifelse(t2_vals > 0, 1, ifelse(t2_vals < 0, -1, 0))
            same_direction <- t1_sign == t2_sign & t1_sign != 0

            pos1_filt <- pos1[same_direction]
            pos2_filt <- pos2[same_direction]

            if (length(pos1_filt) > 0) {
                y1_coords <- map_idx_to_y(pos1_filt, n1, n_max)
                y2_coords <- map_idx_to_y(pos2_filt, n2, n_max)
                finite_coords <- is.finite(y1_coords) & is.finite(y2_coords)
                y1_coords <- y1_coords[finite_coords]
                y2_coords <- y2_coords[finite_coords]

                if(length(y1_coords) > 0) {
                    n_bins_bundle <- 150
                    bundled_data <- bundle_lines(y1_coords, y2_coords, n_bins = n_bins_bundle)

                    if(nrow(bundled_data) > 0) {
                        max_count <- max(bundled_data$count, 1)
                        line_alpha <- pmin(1, 0.05 + (bundled_data$count / max_count) * 0.7)
                        line_cols <- rgb(0.5, 0.5, 0.5, line_alpha)

                        for (i in 1:nrow(bundled_data)) {
                            y1_spline <- bundled_data$y1[i]
                            y2_spline <- bundled_data$y2[i]
                            if(is.finite(y1_spline) && is.finite(y2_spline)) {
                                spline_points <- xspline(
                                   x = c(bar1_x + bar_width_relative / 2, (bar1_x + bar2_x)/2, bar2_x - bar_width_relative / 2),
                                   y = c(y1_spline, (y1_spline + y2_spline)/2, y2_spline),
                                   shape = -0.5, draw = FALSE
                                )
                                # Check spline points are valid before drawing
                                if(all(is.finite(spline_points$x)) && all(is.finite(spline_points$y))) {
                                     lines(spline_points, col = line_cols[i], lwd = 0.8)
                                }
                            }
                        }
                    }
                }
            }
        }
    }
  },
  height = function() {
      h <- session$clientData$`output_va_plot_height`
      if(is.null(h) || !is.finite(h) || h <=0) h <- 600
      max(h, 100)
  },
  width = function() {
      w <- session$clientData$`output_va_plot_width`
      if(is.null(w) || !is.finite(w) || w <=0) w <- 400
      max(w, 100)
  }
  ) # End renderPlot

} # End server