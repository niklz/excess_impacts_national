library(formattable)
library(colorRampPalette)


ae_impacts %>%
  filter(period == max(period), ae_type == "Type 1 (Major)") %>%
    arrange(desc(excess_mort)) %>%
    select(Trust = org, Region = parent_org, `Total admissions` = tot_ae_adm, `Number of DTA > 4 hours` = dta_gt4, `Delay related deaths` = excess_mort) %>%
  formattable()




# Custom color palette for the heatmaps
color_palette <- colorRampPalette(c("#ffffff", "#fecdd3", "#f43f5e")) # White to soft red

ae_impacts %>%
  filter(period == max(period), ae_type == "Type 1 (Major)") %>%
  arrange(desc(excess_mort)) %>%
  select(
    Trust = org, 
    Region = parent_org, 
    `Total admissions` = tot_ae_adm, 
    `Number of DTA > 4 hours` = dta_gt4, 
    `Delay related deaths` = excess_mort
  ) %>%
  formattable(
    align = c("l", "l", "r", "r", "r"), # Left-align text, Right-align numbers
    list(
      # 1. Format numbers with commas for readability
      `Total admissions` = color_bar("#cbd5e1"), # Subtle grey progress bar
      
      # 2. Heatmap for long DTA waits (higher numbers = darker red)
      `Number of DTA > 4 hours` = color_tile("white", "#fca5a5"), 
      
      # 3. Bold and color-code the most critical metric (Excess Deaths)
      `Delay related deaths` = formatter(
        "span",
        style = x ~ style(
          color = ifelse(x > 0, "#991b1b", "darkgreen"),
          font.weight = "bold"
        )
      )
    )
  )



# 1. Clean and prepare the data
processed_data <- ae_impacts %>%
  filter(period == max(period), ae_type == "Type 1 (Major)") %>%
  select(
    Trust = org, 
    Region = parent_org, 
    `Total admissions` = tot_ae_adm, 
    `Number of DTA > 4 hours` = dta_gt4, 
    `Delay related deaths` = excess_mort
  ) %>%
  # Round values and format with commas right now
  mutate(
    `Total admissions` = round(`Total admissions`, 0),
    `Number of DTA > 4 hours` = round(`Number of DTA > 4 hours`, 0),
    `Delay related deaths` = round(`Delay related deaths`, 0)
  )

# 2. Separate the "Total" row from the rest of the data
total_row <- processed_data %>% filter(Trust == "Total")
main_data  <- processed_data %>% filter(Trust != "Total") %>% arrange(desc(`Delay related deaths`))

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
  align = c("l", "l", "r", "r", "r"),
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
    
    `Delay related deaths` = formatter(
      "span",
      style = function(x) {
        style(
          # Row 1 (Total) is black/bold, others are conditional red/green
          color = c("black", ifelse(x[-1] > 0, "#991b1b", "darkgreen")),
          font.weight = "bold"
        )
      },
      x ~ comma(x, digits = 0)
    )
  )
)





#' Create a custom progress bar that ignores the "Total" row (Row 1)
#' @param color The hex code color for the progress bar
custom_bar_formatter <- function(color) {
  formatter("span", 
    style = function(x) {
      # Initialize an empty style vector for all rows
      styles <- rep("", length(x))
      
      # Calculate percentages for rows 2 onwards (ignoring the Total row)
      # This stops the massive Total number from shrinking all other bars to 0%
      max_val <- max(x[-1], na.rm = TRUE)
      percentages <- round((x[-1] / max_val) * 100)
      
      # Apply the bar styling ONLY to rows 2 to N
      styles[-1] <- sprintf(
        "background: linear-gradient(90deg, %s %d%%, transparent %d%%); 
         display: inline-block; 
         width: 100%%; 
         border-radius: 4px; 
         padding-right: 4px;", 
        color, percentages, percentages
      )
      styles
    },
    # Apply comma formatting and 0 decimal rounding to ALL rows (including Total)
    x ~ comma(x, digits = 0)
  )
}

# -------------------------------------------------------------------------
# Data Preparation
# -------------------------------------------------------------------------

processed_data <- ae_impacts %>%
  filter(period == max(period), ae_type == "Type 1 (Major)") %>%
  select(
    Trust = org, 
    Region = parent_org, 
    `Total admissions` = tot_ae_adm, 
    `Number of DTA > 4 hours` = dta_gt4, 
    `Delay related deaths` = excess_mort
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Separate and recombine to force "Total" to the top row
total_row <- processed_data %>% filter(Trust == "Total")
main_data  <- processed_data %>% filter(Trust != "Total") %>% arrange(desc(`Delay related deaths`))
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
    `Delay related deaths` = formatter(
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
