library(formattable)
library(colorRampPalette)





# 1. Clean and prepare the data
processed_data <- ae_impacts %>%
  filter(period == max(period), ae_type == "Type 1 (Major)") %>%
  select(
    Trust = org, 
    Region = parent_org, 
    `Total admissions` = tot_ae_adm, 
    `Number of DTA > 4 hours` = dta_gt4, 
    `Estimated delay related deaths` = excess_mort,
    Trend = status_arrow
  ) %>%
  # Round values and format with commas right now
  mutate(
    `Total admissions` = round(`Total admissions`, 0),
    `Number of DTA > 4 hours` = round(`Number of DTA > 4 hours`, 0),
    `Delay related deaths` = round(`Estimated delay related deaths`, 0)
  )

# 2. Separate the "Total" row from the rest of the data
total_row <- processed_data %>% filter(Trust == "Total")
main_data  <- processed_data %>% filter(Trust != "Total") %>% arrange(desc(`Estimated delay related deaths`))

# 3. Recombine them with "Total" strictly at the top
final_df <- bind_rows(total_row, main_data)

# 4. Custom formatter that skips the first row ("Total") for conditional styling
# but applies comma formatting to ALL rows.

comma_formatter_except_total <- function(color_code, is_bar = FALSE) {
  formatter("span", style = function(x) {
    # Create a vector of styles, defaulting to empty/normal
    styles <- rep("", length(x))
    
    # Apply the background color/bar ONLY to rows 2 and onwards (skipping Total)
    if(is_bar) {
      # For color bars, we use a subtle background width trick
      styles[-1] <- sprintf("background: %s; width: %d%%; display: inline-block; border-radius: 4px; padding-right: 4px;", color_code, round(x[-1]/max(x[-1], na.rm=TRUE)*100))
    } else {
      # For heatmaps
      styles[-1] <- sprintf("background-color: %s; display: block; border-radius: 4px; padding: 2px;", color_code)
    }
    styles
  }, x ~ comma(x, digits = 0)) # Formats every number with commas and 0 decimals
}

# 5. Render the table
formattable(
  final_df,
  align = c("l", "l", "r", "r", "r", "r"),
  list(
    # Custom styling for the "Total" row to make it stand out at the top
    Trust = formatter("span", style = x ~ style(font.weight = ifelse(x == "Total", "bold", "normal"))),
    
    # Apply our custom comma + conditional formatter
    `Total admissions` = comma_formatter_except_total("#cbd5e1", is_bar = TRUE),
    `Number of DTA > 4 hours` = color_tile("white", "#fca5a5"), # Standard tile still works, but to stop it hitting 'Total', we use:
    
    `Number of DTA > 4 hours` = formatter("span", 
      style = function(x) {
        # Create heatmap colors just for the main data, leaving row 1 white
        cols <- c("white", colorRampPalette(c("white", "#fca5a5"))(x[-1] / max(x[-1])))
        style(background = cols, display = "block", border_radius = "4px")
      },
      x ~ comma(x, digits = 0)
    ),
    
    `Estimated delay related deaths` = formatter(
      "span",
      style = function(x) {
        style(
          # Row 1 (Total) is black/bold, others are conditional red/green
          color = c("black", ifelse(x[-1] > 0, "#991b1b", "darkgreen")),
          font.weight = "bold"
        )
      },
      x ~ comma(x, digits = 0)
    ),
    Trend 
  )
)





# #' Create a custom progress bar that ignores the "Total" row (Row 1)
# #' @param color The hex code color for the progress bar
# custom_bar_formatter <- function(color) {
#   formatter("span", 
#     style = function(x) {
#       # Initialize an empty style vector for all rows
#       styles <- rep("", length(x))
      
#       # Calculate percentages for rows 2 onwards (ignoring the Total row)
#       # This stops the massive Total number from shrinking all other bars to 0%
#       max_val <- max(x[-1], na.rm = TRUE)
#       percentages <- round((x[-1] / max_val) * 100)
      
#       # Apply the bar styling ONLY to rows 2 to N
#       styles[-1] <- sprintf(
#         "background: linear-gradient(90deg, %s %d%%, transparent %d%%); 
#          display: inline-block; 
#          width: 100%%; 
#          border-radius: 4px; 
#          padding-right: 4px;", 
#         color, percentages, percentages
#       )
#       styles
#     },
#     # Apply comma formatting and 0 decimal rounding to ALL rows (including Total)
#     x ~ comma(x, digits = 0)
#   )
# }


#' Create a dual-gradient progress bar with an optional flag for a "Total" row at Row 1
#' @param color The hex code color for the active progress bar
#' @param track_color The hex code color for the unfilled part of the bar
#' @param has_total_row Logical. If TRUE, treats row 1 as a bold, un-barized summary total.
custom_bar_formatter <- function(color, track_color = "#f1f5f9", has_total_row = FALSE) {
  formatter("span", 
    style = function(x) {
      styles <- rep("", length(x))
      
      if (has_total_row) {
        # 1. Handle table WITH a total row (Skip row 1)
        max_val <- max(x[-1], na.rm = TRUE)
        if (is.na(max_val) || max_val == 0) max_val <- 1 # Protect against division by zero
        
        percentages <- round((x[-1] / max_val) * 100)
        idx_to_paint <- seq_along(x)[-1]
        
        # Style for the "Total" row (Row 1) - Bold and no background bar
        styles[1] <- "font-weight: bold; color: #0f172a;"
        
      } else {
        # 2. Handle standard tables (Rankings / Leaderboards - Paint all rows)
        max_val <- max(x, na.rm = TRUE)
        if (is.na(max_val) || max_val == 0) max_val <- 1
        
        percentages <- round((x / max_val) * 100)
        idx_to_paint <- seq_along(x)
      }
      
      # Apply the dual gradient styling to the target rows
      styles[idx_to_paint] <- sprintf(
        "background: linear-gradient(90deg, %s %d%%, %s %d%%); 
         display: inline-block; 
         width: 100%%; 
         text-align: center; 
         color: #1e293b; 
         font-weight: 500;
         border-radius: 4px; 
         padding: 2px 4px;", 
        color, percentages, track_color, percentages
      )
      
      styles
    },
    x ~ comma(x, digits = 0)
  )
}

# -------------------------------------------------------------------------
# Data Preparation
# -------------------------------------------------------------------------

processed_data <- ae_impacts %>%
  filter(period == max(period)) %>%
  select(
    Trust = org, 
    Region = parent_org, 
    `Total admissions` = tot_ae_adm, 
    `Number of DTA > 4 hours` = dta_gt4, 
    `Estimated delay related deaths` = excess_mort,
    `Percent change` = trend_velocity_pct,
    Trend = status_arrow
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Separate and recombine to force "Total" to the top row
total_row <- processed_data %>% filter(Trust == "Total")
main_data  <- processed_data %>% filter(Trust != "Total") %>% arrange(desc(`Estimated delay related deaths`))
final_df   <- bind_rows(total_row, main_data)

# -------------------------------------------------------------------------
# Render Table
# -------------------------------------------------------------------------

formattable(
  final_df,
  align = c("l", "l", "r", "r", "r"),
  list(
    # Bold the Trust column if it is the Total row
    Trust = formatter("span", style = x ~ style(font.weight = ifelse(x == "Total", "bold", "normal"))),
    
    # 🌟 Beautiful, modular progress bars using different colors
    `Total admissions`        = custom_bar_formatter("#cbd5e1"), # Subtle Slate Grey
    `Number of DTA > 4 hours` = custom_bar_formatter("#cbd5e1"), 
    
    # Keep the dynamic text color logic for Deaths
    `Estimated delay related deaths` = formatter(
      "span",
      style = function(x) {
        style(
          color = c("black", ifelse(x[-1] > 0, "#991b1b", "darkgreen")),
          font.weight = "bold"
        )
      },
      x ~ comma(x, digits = 0)
    )
  )
)


top_growers <- final_df %>% 
  filter(Trend == "▲ Growth") %>% 
  select(-Region, -`Total admissions`, -`Number of DTA > 4 hours`, -`Estimated delay related deaths`, -Trend) %>%
  arrange(desc(`Percent change`))

# Render the formattable table safely
formattable(
  top_growers, # 1. First argument must be the data frame explicitly
  align = rep("l", ncol(top_growers)), # Dynamically matches alignment to your column count
  list(
    # Bold entire "Total" row cells for text columns
    Trust = formatter("span", style = x ~ style(font.weight = ifelse(x == "Total", "bold", "normal"))),    
    
    # Clean Progress Bars with internal centering
    `Percent change` = custom_bar_formatter("#cbd5e1")
  )
)






final_df_with_spk <- final_df %>%
  rowwise() %>% 
  mutate(
    # Create the HTML sparkline string from your numeric nested list
    `Death Trend` = 
      spk_chr(
        unlist(data), # Extract the numeric array out of your list-column
        type = "line", 
        lineColor = "#991b1b",    # Dark Red line
        fillColor = "#fee2e2",    # Light Red shaded area underneath
        minSpotColor = "#166534", # Green dot on the lowest point
        maxSpotColor = "#991b1b", # Red dot on the highest point
        spotRadius = 2
      )
    
  ) %>%
  ungroup() %>%
  select(-data, -Trend) # Drop the raw data list and old text trend column

# -------------------------------------------------------------------------
# 2. Render Table and Inject Dependencies
# -------------------------------------------------------------------------

tbl <- formattable(
  final_df_with_spk,
  # Update alignment to accommodate the new 7th column (Death Trend)
  align = c("l", "l", "r", "r", "r", "c"), 
  list(
    # Formatting rules established previously
    Trust = formatter("span", style = x ~ style(font.weight = ifelse(x == "Total", "bold", "normal"))),
    Region = formatter("span", style = function(x) style(font.weight = ifelse(final_df_with_spk$Trust == "Total", "bold", "normal"), color = ifelse(final_df_with_spk$Trust == "Total", "#475569", "#64748b"))),
    
    `Total admissions`        = custom_bar_formatter("#cbd5e1"), 
    `Number of DTA > 4 hours` = custom_bar_formatter("#cbd5e1"), 
    
    `Estimated delay related deaths` = formatter(
      "span",
      style = function(x) style(color = c("#0f172a", ifelse(x[-1] > 0, "#991b1b", "#166534")), font.weight = "bold"),
      x ~ comma(x, digits = 0)
    )
    # Note: We do not need a formatter for `Death Trend` because spk_chr pre-baked the HTML!
  )
)

# ⚠️ CRITICAL STEP: Cast to widget and append JavaScript instructions
tbl_widget <- as.htmlwidget(tbl)
tbl_widget$dependencies <- c(tbl_widget$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))

# Call the widget to see the beautiful table
tbl_widget


library(reactable)
library(shiny)


display_df <- select(processed_data, -`Percent change`)
  
  # Find column maximums for the progress bar scaling (ignoring Total row)
  valid_rows <- display_df %>% filter(Trust != "Total")
  max_admissions <- max(valid_rows$`Total admissions`, na.rm = TRUE)
  max_dta <- max(valid_rows$`Number of DTA > 4 hours`, na.rm = TRUE)
  
  reactable(
    display_df,
    pagination = FALSE,      # Show all rows (like pageLength = -1)
    filterable = TRUE,      # Top filters
    searchable = FALSE,
    highlight = TRUE,
    defaultColDef = colDef(align = "center"), # Center everything by default
    
    # ROW STYLING: Bold the total row perfectly
    rowStyle = function(index) {
      if (display_df$Trust[index] == "Total") {
        list(fontWeight = "bold", background = "#f8fafc")
      }
    },
    
    columns = list(
      # 1. TRUST COLUMN (Left-aligned)
      Trust = colDef(
        name = "Trust",
        align = "left",
        width = 300
      ),
      
      # 2. TOTAL ADMISSIONS (Custom Progress Bar)
      `Total admissions` = colDef(
        name = "Total admissions",
        width = 200,
        cell = function(value, index) {
          # Skip the bar for the Total row
          if (display_df$Trust[index] == "Total") return(comma(value, digits = 0))
          
          # Calculate fluid percentage
          pct <- min(round((abs(value) / max_admissions) * 100), 100)
          
          # Create the exact same visual bar aesthetic
          div(
            style = list(
              background = sprintf("linear-gradient(90deg, #cbd5e1 %d%%, #f1f5f9 %d%%)", pct, pct),
              color = "#1e293b", fontWeight = 500, borderRadius = "4px", padding = "2px 4px", width = "100%"
            ),
            comma(value, digits = 0)
          )
        }
      ),
      
      # 3. NUMBER OF DTA > 4 HOURS (Custom Progress Bar)
      `Number of DTA > 4 hours` = colDef(
        name = "Number of DTA > 4 hours",
        width = 200,
        cell = function(value, index) {
          if (display_df$Trust[index] == "Total") return(comma(value, digits = 0))
          pct <- min(round((abs(value) / max_dta) * 100), 100)
          div(
            style = list(
              background = sprintf("linear-gradient(90deg, #cbd5e1 %d%%, #f1f5f9 %d%%)", pct, pct),
              color = "#1e293b", fontWeight = 500, borderRadius = "4px", padding = "2px 4px", width = "100%"
            ),
            comma(value, digits = 0)
          )
        }
      ),
      
      # 4. ESTIMATED DRD (Conditional Red/Green Text)
      `Estimated DRD` = colDef(
        name = "Estimated DRD",
        width = 130,
        cell = function(value, index) {
          is_total <- display_df$Trust[index] == "Total"
          text_color <- if (is_total) "#0f172a" else if (value > 0) "#991b1b" else "#166534"
          
          span(
            style = list(color = text_color, fontWeight = "bold"),
            comma(value, digits = 0)
          )
        }
      ),
      
      # 5. TREND COLUMN (Conditional Text + Icons)
      Trend = colDef(
        name = "Trend",
        width = 180,
        cell = function(value, index) {
          is_total <- display_df$Trust[index] == "Total"
          text_color <- case_when(
            grepl("Decline", value) ~ "#166534",
            grepl("Growth|Increase", value) ~ "#991b1b",
            TRUE ~ "#475569"
          )
          span(
            style = list(color = text_color, fontWeight = if(is_total) "bold" else "normal"),
            value
          )
        }
      )
    )
  )
