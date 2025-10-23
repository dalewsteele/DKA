library(shiny)
library(bslib)
library(bsicons)
library(dplyr)

# ============================================================================
# CONSTANTS
# ============================================================================
VERSION <- "2.1.3"
WEIGHT_CAP <- 75
BOLUS_10_MAX <- 500
BOLUS_20_MAX <- 1000
MAX_TREKK_RATE <- 250
MIN_WEIGHT <- 5
MAX_WEIGHT <- 150

# ============================================================================
# CALCULATION FUNCTIONS
# ============================================================================
calc_holliday_segar <- function(weight) {
  w <- pmax(weight, 0)
  pmin(w, 10) * 100 + pmin(pmax(w - 10, 0), 10) * 50 + pmax(w - 20, 0) * 20
}

calc_trekk_rate <- function(weight) {
  rate_per_kg <- case_when(
    weight >= 5 & weight < 10 ~ 6.5,
    weight >= 10 & weight < 20 ~ 6,
    weight >= 20 & weight < 40 ~ 5,
    TRUE ~ 4
  )
  rate_mL_hr <- weight * rate_per_kg
  if (weight >= 40) rate_mL_hr <- min(rate_mL_hr, MAX_TREKK_RATE)
  rate_mL_hr
}

# ============================================================================
# MODULE: HOLLIDAY-SEGAR
# ============================================================================
hollidaySegarUI <- function(id) {
  ns <- NS(id)
  conditionalPanel(
    condition = "input.hs == true",
    uiOutput(ns("hsCard"))
  )
}

hollidaySegarServer <- function(id, cappedWeight, maint_hourly, showFormulas) {
  moduleServer(id, function(input, output, session) {
    
    output$hsCard <- renderUI({
      maint <- maint_hourly()
      rate_1_5 <- round(maint * 1.5, 0)
      
      card(
        card_header(
          class = "bg-primary text-white py-1",
          tags$div(style = "font-size: 0.9rem;", "Holliday-Segar ×1.5 Maintenance")
        ),
        card_body(
          class = "py-1",
          tags$div(
            style = "text-align: center;",
            tags$div(class = "text-muted small", style = "font-size: 0.75rem;", "Total Replacement Rate"),
            tags$div(style = "font-size: 1.5rem; font-weight: bold; color: #2C3E50; margin: 2px 0;",
                     paste(rate_1_5, "mL/hr")),
            tags$div(class = "text-muted", style = "font-size: 0.75rem;",
                     sprintf("Maint: %d | ×2.0: %d mL/hr", round(maint, 0), round(maint * 2, 0)))
          )
        ),
        if (isTRUE(showFormulas())) {
          card_body(
            class = "bg-light",
            markdown(
              sprintf("
**Calculation Steps:**

1. **Daily Maintenance (Holliday-Segar)**
   - First 10 kg: %.1f kg × 100 mL/kg = %.0f mL/day
   %s%s
   - **Total**: %.0f mL/day

2. **Hourly Rate**
   - %.0f mL/day ÷ 24 hr = **%.1f mL/hr**

3. **Apply DKA Multiplier**
   - %.1f mL/hr × 1.5 = **%d mL/hr**
              ",
                      min(cappedWeight(), 10),
                      min(cappedWeight(), 10) * 100,
                      if (cappedWeight() > 10) sprintf("- Next 10 kg: %.1f kg × 50 mL/kg = %.0f mL/day\n   ", 
                                                       min(cappedWeight() - 10, 10), 
                                                       min(cappedWeight() - 10, 10) * 50) else "",
                      if (cappedWeight() > 20) sprintf("- Above 20 kg: %.1f kg × 20 mL/kg = %.0f mL/day\n   ", 
                                                       cappedWeight() - 20, 
                                                       (cappedWeight() - 20) * 20) else "",
                      calc_holliday_segar(cappedWeight()),
                      calc_holliday_segar(cappedWeight()),
                      maint,
                      maint,
                      rate_1_5
              ))
          )
        }
      )
    })
  })
}

# ============================================================================
# MODULE: DEFICIT + MAINTENANCE
# ============================================================================
deficitMaintenanceUI <- function(id) {
  ns <- NS(id)
  conditionalPanel(
    condition = "input.md == true",
    uiOutput(ns("deficitCard"))
  )
}

deficitMaintenanceServer <- function(id, cappedWeight, actualWeight, total_deficit, bolus_mL, 
                                     remaining_deficit, deficit_per_hour, maint_hourly, 
                                     final_deficit_maint_rate, deficit_pct, bolusOption,
                                     replaceHours, showFormulas, BOLUS_10_MAX, BOLUS_20_MAX) {
  moduleServer(id, function(input, output, session) {
    
    output$deficitCard <- renderUI({
      deficit <- total_deficit()
      bolus <- bolus_mL()
      remaining <- remaining_deficit()
      deficit_hr <- deficit_per_hour()
      maint <- maint_hourly()
      final_rate <- final_deficit_maint_rate()
      w <- actualWeight()
      double_maint <- maint * 2
      
      bolus_capped <- FALSE
      if (bolusOption() %in% c("10", "20")) {
        bolus_per_kg <- as.numeric(bolusOption())
        max_bolus <- ifelse(bolus_per_kg == 10, BOLUS_10_MAX, BOLUS_20_MAX)
        if (w * bolus_per_kg > max_bolus) {
          bolus_capped <- TRUE
        }
      }
      
      card(
        card_header(
          class = "bg-success text-white py-1",
          tags$div(style = "font-size: 0.9rem;", "Deficit + Maintenance")
        ),
        card_body(
          class = "py-1",
          tags$div(
            style = "text-align: center;",
            tags$div(class = "text-muted small", style = "font-size: 0.75rem;", "Total Replacement Rate"),
            tags$div(style = paste0("font-size: 1.5rem; font-weight: bold; margin: 2px 0; color: ",
                                    if (final_rate > double_maint) "#E74C3C" else "#18BC9C", ";"),
                     paste(round(final_rate, 0), "mL/hr")),
            tags$div(class = "text-muted", style = "font-size: 0.75rem;",
                     sprintf("Deficit: %d | Maint: %d mL/hr", round(deficit_hr, 0), round(maint, 0))),
            if (final_rate > double_maint) {
              tags$div(class = "text-danger", style = "font-size: 0.75rem;",
                       sprintf("⚠️ Exceeds ×2 maint (%d)", round(double_maint, 0)))
            }
          )
        ),
        if (isTRUE(showFormulas())) {
          card_body(
            class = "bg-light",
            markdown(
              sprintf("
**Calculation Steps:**

1. **Total Deficit**: %.1f kg × 1000 × %s%% = **%.0f mL**
2. **Bolus Administered**: **%.0f mL** %s
3. **Remaining Deficit**: %.0f - %.0f = **%.0f mL**
4. **Hourly Deficit Rate**: %.0f ÷ %s hr = **%d mL/hr**
5. **Maintenance Rate**: **%d mL/hr** (Holliday-Segar)
6. **Final Rate**: %d + %d = **%d mL/hr** (%.1f× maintenance)
              ",
                      cappedWeight(), deficit_pct(), deficit,
                      bolus, if(bolus_capped) "*(capped)*" else "",
                      deficit, bolus, remaining,
                      remaining, replaceHours(), round(deficit_hr, 0),
                      round(maint, 0),
                      round(deficit_hr, 0), round(maint, 0), round(final_rate, 0),
                      final_rate / maint
              ))
          )
        }
      )
    })
  })
}

# ============================================================================
# MODULE: TREKK
# ============================================================================
trekkGuidelineUI <- function(id) {
  ns <- NS(id)
  conditionalPanel(
    condition = "input.trekk == true",
    uiOutput(ns("trekkCard"))
  )
}

trekkGuidelineServer <- function(id, actualWeight, trekk_hourly, showFormulas) {
  moduleServer(id, function(input, output, session) {
    
    output$trekkCard <- renderUI({
      wt <- actualWeight()
      rate <- trekk_hourly()
      
      rate_per_kg <- case_when(
        wt >= 5 & wt < 10 ~ 6.5,
        wt >= 10 & wt < 20 ~ 6,
        wt >= 20 & wt < 40 ~ 5,
        TRUE ~ 4
      )
      
      uncapped <- wt * rate_per_kg
      is_capped <- wt >= 40 && uncapped > MAX_TREKK_RATE
      
      card(
        card_header(
          class = "bg-info text-white py-1",
          tags$div(style = "font-size: 0.9rem;",
                   HTML('TREKK Guideline Rate <img src="https://upload.wikimedia.org/wikipedia/commons/c/cf/Flag_of_Canada.svg" height="12px" style="vertical-align:middle; margin-left: 5px;">'))
        ),
        card_body(
          class = "py-1",
          tags$div(
            style = "text-align: center;",
            tags$div(class = "text-muted small", style = "font-size: 0.75rem;", "Total Replacement Rate"),
            tags$div(style = "font-size: 1.5rem; font-weight: bold; color: #3498DB; margin: 2px 0;",
                     paste(round(rate, 0), "mL/hr")),
            tags$div(class = "text-muted", style = "font-size: 0.75rem;",
                     if (is_capped) {
                       sprintf("%.1f mL/kg/hr (capped at %d)", rate_per_kg, MAX_TREKK_RATE)
                     } else {
                       sprintf("%.1f mL/kg/hr", rate_per_kg)
                     })
          )
        ),
        if (isTRUE(showFormulas())) {
          card_body(
            class = "bg-light",
            markdown(
              sprintf("
**Calculation Steps:**

**Weight-Based Rate per kg:**
- 5-10 kg: 6.5 mL/kg/hr
- 10-20 kg: 6.0 mL/kg/hr
- 20-40 kg: 5.0 mL/kg/hr
- ≥40 kg: 4.0 mL/kg/hr (max 250 mL/hr)

**For this patient:**
- Weight: %.1f kg → %.1f mL/kg/hr
- Calculation: %.1f kg × %.1f = %.0f mL/hr%s
              ",
                      wt, rate_per_kg,
                      wt, rate_per_kg, uncapped,
                      if (is_capped) sprintf("\n- **Capped at %d mL/hr**", MAX_TREKK_RATE) else ""
              ))
          )
        }
      )
    })
  })
}

# ============================================================================
# MODULE: TWO-BAG TECHNIQUE
# ============================================================================
twoBagUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_body(
        class = "py-2",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          selectInput(ns("method_select"), 
                      "Select Method:",
                      choices = c("Select method..." = "",
                                  "×1.5 Maintenance" = "hs",
                                  "Deficit + Maintenance" = "dm",
                                  "TREKK Guideline" = "trekk")),
          
          div(
            radioButtons(ns("glucose_units"), 
                         "Glucose Units:",
                         choices = c("mg/dL" = "mgdl", "mmol/L" = "mmol"),
                         selected = "mgdl",
                         inline = TRUE)
          ),
          
          numericInput(ns("blood_glucose"), 
                       "Current Blood Glucose:",
                       value = NULL,
                       min = 0,
                       step = 1),
          
          # DKA checkbox in 4th column - only shows when glucose <= 150
          uiOutput(ns("dkaCheckbox"))
        )
      )
    ),
    
    uiOutput(ns("twoBagResults")),
    
    uiOutput(ns("protocolTable"))
  )
}

twoBagServer <- function(id, maint_hourly, final_deficit_maint_rate, trekk_hourly,
                         hs_selected, md_selected, trekk_selected, showFormulas, actualWeight) {
  moduleServer(id, function(input, output, session) {
    
    # Auto-select method when only one is checked in Calculator tab
    observe({
      # Count how many methods are selected
      methods_selected <- c(hs_selected(), md_selected(), trekk_selected())
      num_selected <- sum(methods_selected, na.rm = TRUE)
      
      # If exactly one method is selected, auto-select it
      if (num_selected == 1) {
        if (isTRUE(hs_selected())) {
          updateSelectInput(session, "method_select", selected = "hs")
        } else if (isTRUE(md_selected())) {
          updateSelectInput(session, "method_select", selected = "dm")
        } else if (isTRUE(trekk_selected())) {
          updateSelectInput(session, "method_select", selected = "trekk")
        }
      }
    })
    
    # Conditionally show DKA checkbox only when glucose <= 150
    output$dkaCheckbox <- renderUI({
      if (is.null(input$blood_glucose) || is.na(input$blood_glucose)) return(NULL)
      
      gluc <- if (input$glucose_units == "mmol") {
        input$blood_glucose * 18
      } else {
        input$blood_glucose
      }
      
      if (gluc <= 150) {
        div(
          style = "padding-top: 25px;",  # Align with other inputs
          checkboxInput(session$ns("still_in_dka"),
                        "Patient still in DKA",
                        value = TRUE)
        )
      }
    })
    
    # Protocol table - only show if formulas checkbox is checked
    output$protocolTable <- renderUI({
      if (!isTRUE(showFormulas())) return(NULL)
      
      card(
        card_header(class = "py-1", "Two-Bag Protocol Reference"),
        card_body(
          class = "py-2 small",
          tags$table(
            class = "table table-sm table-bordered mb-0",
            style = "font-size: 0.85rem;",
            tags$thead(
              tags$tr(
                tags$th("Blood Glucose"),
                tags$th("Bag 1 (No Dextrose, 0.9% NS, 40 mEq/L K+)"),
                tags$th("Bag 2 (10% Dextrose, 0.9% NS, 40 mEq/L K+)")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("≥300 mg/dL (≥16.7 mmol/L)"),
                tags$td("100%"),
                tags$td("0%")
              ),
              tags$tr(
                tags$td("251-299 mg/dL (13.9-16.6 mmol/L)"),
                tags$td("50%"),
                tags$td("50%")
              ),
              tags$tr(
                tags$td("200-250 mg/dL (11.1-13.9 mmol/L)"),
                tags$td("25%"),
                tags$td("75%")
              ),
              tags$tr(
                tags$td("151-199 mg/dL (8.4-11.0 mmol/L)"),
                tags$td("0%"),
                tags$td("100%")
              ),
              tags$tr(
                class = "table-warning",
                tags$td("≤150 mg/dL (≤8.3 mmol/L)"),
                tags$td("0%"),
                tags$td("100% + ↑rate ×1.33 if in DKA")
              )
            )
          )
        )
      )
    })
    
    # Convert mmol/L to mg/dL if needed
    glucose_mgdl <- reactive({
      req(input$blood_glucose)
      if (input$glucose_units == "mmol") {
        input$blood_glucose * 18
      } else {
        input$blood_glucose
      }
    })
    
    # Get the base rate from selected method
    base_rate <- reactive({
      req(input$method_select)
      
      rate <- switch(input$method_select,
                     "hs" = {
                       if (!isTRUE(hs_selected())) return(NULL)
                       maint_hourly() * 1.5
                     },
                     "dm" = {
                       if (!isTRUE(md_selected())) return(NULL)
                       final_deficit_maint_rate()
                     },
                     "trekk" = {
                       if (!isTRUE(trekk_selected())) return(NULL)
                       trekk_hourly()
                     },
                     NULL)
      
      if (is.null(rate)) return(NULL)
      
      # Apply 1.33× multiplier if glucose ≤150 and still in DKA
      gluc <- glucose_mgdl()
      if (!is.null(gluc) && gluc <= 150 && !is.null(input$still_in_dka) && isTRUE(input$still_in_dka)) {
        rate <- rate * 1.33
      }
      
      rate
    })
    
    # Calculate bag percentages based on glucose
    bag_percentages <- reactive({
      req(input$blood_glucose)
      gluc <- glucose_mgdl()
      
      if (gluc >= 300) {
        list(bag1_pct = 100, bag2_pct = 0)
      } else if (gluc >= 251) {
        list(bag1_pct = 50, bag2_pct = 50)
      } else if (gluc >= 200) {
        list(bag1_pct = 25, bag2_pct = 75)
      } else if (gluc >= 151) {
        list(bag1_pct = 0, bag2_pct = 100)
      } else {
        list(bag1_pct = 0, bag2_pct = 100)
      }
    })
    
    # Calculate individual bag rates
    bag_rates <- reactive({
      req(base_rate(), bag_percentages())
      
      rate <- base_rate()
      pcts <- bag_percentages()
      weight <- actualWeight()
      
      bag1_rate <- round(rate * pcts$bag1_pct / 100)
      bag2_rate <- round(rate * pcts$bag2_pct / 100)
      
      # Calculate GIR from Bag 2 (10% dextrose = 100 mg/mL)
      # GIR = (Rate in mL/hr × glucose concentration in mg/mL) / (weight in kg × 60 min/hr)
      gir <- (bag2_rate * 100) / (weight * 60)
      
      # Calculate KIR from total rate (both bags have 40 mEq/L K+ = 0.04 mEq/mL)
      # KIR = (Total rate in mL/hr × K+ concentration in mEq/mL) / weight in kg
      kir <- (rate * 0.04) / weight
      
      list(
        bag1 = bag1_rate,
        bag2 = bag2_rate,
        total = round(rate),
        gir = round(gir, 1),
        kir = round(kir, 2)
      )
    })
    
    output$twoBagResults <- renderUI({
      if (is.null(input$method_select) || input$method_select == "") {
        return(card(
          class = "border-info",
          card_body(
            class = "bg-info bg-opacity-10 text-dark py-2",
            icon("circle-info", class = "text-info me-2"),
            "Please select a method above to calculate two-bag rates"
          )
        ))
      }
      
      if (is.null(input$blood_glucose) || is.na(input$blood_glucose)) {
        return(card(
          class = "border-info",
          card_body(
            class = "bg-info bg-opacity-10 text-dark py-2",
            icon("circle-info", class = "text-info me-2"),
            "Please enter current blood glucose above"
          )
        ))
      }
      
      # Check if selected method is calculated on Calculator tab
      if (input$method_select == "hs" && !isTRUE(hs_selected())) {
        return(card(
          class = "border-warning",
          card_body(
            class = "bg-warning bg-opacity-10 text-dark py-2",
            icon("circle-info", class = "text-warning me-2"),
            "Please check '×1.5 Maint' in the Method checkboxes on the Calculator tab"
          )
        ))
      }
      
      if (input$method_select == "dm" && !isTRUE(md_selected())) {
        return(card(
          class = "border-warning",
          card_body(
            class = "bg-warning bg-opacity-10 text-dark py-2",
            icon("circle-info", class = "text-warning me-2"),
            "Please check 'Deficit+Maint' in the Method checkboxes on the Calculator tab"
          )
        ))
      }
      
      if (input$method_select == "trekk" && !isTRUE(trekk_selected())) {
        return(card(
          class = "border-warning",
          card_body(
            class = "bg-warning bg-opacity-10 text-dark py-2",
            icon("circle-info", class = "text-warning me-2"),
            "Please check 'TREKK' in the Method checkboxes on the Calculator tab"
          )
        ))
      }
      
      rates <- bag_rates()
      if (is.null(rates)) {
        return(card(
          class = "border-danger",
          card_body(
            class = "bg-danger bg-opacity-10 text-dark py-2",
            icon("exclamation-triangle", class = "text-danger me-2"),
            "Unable to calculate rates. Please ensure all required parameters are entered."
          )
        ))
      }
      
      gluc <- glucose_mgdl()
      gluc_display <- if (input$glucose_units == "mmol") {
        sprintf("%.1f mmol/L (%.0f mg/dL)", input$blood_glucose, gluc)
      } else {
        sprintf("%.0f mg/dL (%.1f mmol/L)", gluc, gluc / 18)
      }
      
      method_name <- switch(input$method_select,
                            "hs" = "×1.5 Maintenance",
                            "dm" = "Deficit + Maintenance",
                            "trekk" = "TREKK Guideline")
      
      rate_adjusted <- gluc <= 150 && !is.null(input$still_in_dka) && isTRUE(input$still_in_dka)
      low_glucose_not_dka <- gluc <= 150 && (is.null(input$still_in_dka) || !isTRUE(input$still_in_dka))
      
      tagList(
        card(
          card_header(
            class = "bg-success text-white py-1",
            "Two-Bag Setup"
          ),
          card_body(
            class = "py-2",
            tags$div(
              tags$div(
                style = "display: flex; justify-content: space-between; flex-wrap: wrap; gap: 10px;",
                tags$div(tags$strong("Method: "), method_name),
                tags$div(tags$strong("Blood Glucose: "), gluc_display),
                tags$div(tags$strong("Total Rate: "), sprintf("%d mL/hr", rates$total))
              ),
              if (rate_adjusted) {
                tags$div(
                  class = "text-warning mt-2",
                  icon("triangle-exclamation"), " Rate increased by 1.33× (glucose ≤150 mg/dL, still in DKA)"
                )
              } else if (low_glucose_not_dka) {
                tags$div(
                  class = "text-info mt-2",
                  icon("syringe"), " Decrease insulin infusion to 0.05 U/kg/hr"
                )
              },
              tags$div(
                class = "mt-2",
                tags$strong("Glucose Infusion Rate: "), sprintf("%.1f mg/kg/min", rates$gir)
              ),
              tags$div(
                class = "mt-1",
                tags$strong("Potassium Infusion Rate: "), sprintf("%.2f mEq/kg/hr", rates$kir)
              )
            )
          )
        ),
        
        layout_columns(
          col_widths = c(6, 6),
          value_box(
            title = "Bag 1 Rate",
            value = paste(rates$bag1, "mL/hr"),
            showcase = bs_icon("droplet"),
            theme = "primary",
            height = "100px",
            p(class = "small mb-0", tags$strong("No Dextrose, 0.9% NS, 40 mEq/L K+"))
          ),
          value_box(
            title = "Bag 2 Rate",
            value = paste(rates$bag2, "mL/hr"),
            showcase = bs_icon("droplet-fill"),
            theme = "info",
            height = "100px",
            p(class = "small mb-0", tags$strong("10% Dextrose, 0.9% NS, 40 mEq/L K+"))
          )
        )
      )
    })
  })
}

# ============================================================================
# UI
# ============================================================================
ui <- page_sidebar(
  title = tags$div(
    "DKA: Fluid Calculator",
    tags$small(class = "ms-2", style = "font-size: 0.6em;", 
               sprintf("v%s", VERSION))
  ),
  theme = bs_theme(
    version = 5,
    preset = "flatly",
    primary = "#2C3E50",
    success = "#18BC9C",
    info = "#3498DB",
    warning = "#F39C12",
    danger = "#E74C3C",
    bg = "#FFFFFF",
    fg = "#000000"
  ),
  tags$style(HTML("
    .card { margin-bottom: 0.5rem !important; margin-top: 0 !important; position: relative !important; }
    .form-group { margin-bottom: 0.3rem !important; }
    .radio { margin-bottom: 0.25rem !important; }
    .checkbox { margin-bottom: 0.25rem !important; }
    .card-header { padding: 0.5rem 1rem !important; font-size: 0.95rem !important; }
    .bslib-sidebar-layout > .main { padding-top: 0.5rem !important; }
    .form-label { margin-bottom: 0.25rem !important; }
    
    /* Dark mode transitions */
    body, .card, .card-header, .card-body, .form-control, .form-select, .btn {
      transition: background-color 0.3s ease, color 0.3s ease, border-color 0.3s ease;
    }
    
    /* Dark mode adjustments */
    [data-bs-theme='dark'] .inline-warning {
      background-color: #4a3c1a !important;
      color: #ffc107 !important;
      border-left-color: #ffc107 !important;
    }
    
    [data-bs-theme='dark'] .alert-warning {
      background-color: #4a3c1a !important;
      color: #ffc107 !important;
      border-color: #ffc107 !important;
    }
    
    [data-bs-theme='dark'] .text-muted {
      color: #adb5bd !important;
    }
  ")),
  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      // Check for saved theme preference
      const savedTheme = localStorage.getItem('bslibTheme');
      if (savedTheme === 'dark') {
        Shiny.setInputValue('darkMode', true);
      }
    });
    
    $(document).on('change', '#darkMode', function() {
      const isDark = $(this).prop('checked');
      const theme = isDark ? 'dark' : 'light';
      
      // Update theme
      $('html').attr('data-bs-theme', theme);
      
      // Update icon
      if (isDark) {
        $('#darkModeLabel').html('<i class=\"fa-solid fa-sun\"></i> Dark Mode');
      } else {
        $('#darkModeLabel').html('<i class=\"fa-solid fa-moon\"></i> Dark Mode');
      }
      
      // Save preference
      localStorage.setItem('bslibTheme', theme);
    });
  ")),
  sidebar = sidebar(
    width = 300,
    div(style = "margin-bottom: 0.3rem;",
        numericInput("weight", "Patient Weight (kg):", value = NULL, min = 1, step = 0.1)),
    uiOutput("weightWarning"),
    
    card(
      card_header(class = "py-1", "Deficit Calculation Parameters"),
      card_body(
        class = "py-1",
        div(style = "margin-bottom: 0.3rem;",
            selectInput("deficitCategory", "Estimated Fluid Deficit:",
                        choices = c("Select one..." = "", 
                                    "Mild (5%)" = 5, 
                                    "Moderate (7%)" = 7, 
                                    "Severe (10%)" = 10, 
                                    "Custom" = "custom"), 
                        selected = "")),
        conditionalPanel(
          condition = "input.deficitCategory == 'custom'",
          div(style = "margin-bottom: 0.3rem;",
              numericInput("customDeficit", "Custom Deficit (%)", 
                           value = 7, min = 0, max = 15, step = 0.1))
        ),
        uiOutput("deficitWarning"),
        
        div(style = "margin-bottom: 0.3rem;",
            selectInput("bolusOption", "Fluid Bolus Option:",
                        choices = c("Select one..." = "", 
                                    "10 mL/kg (max 500 mL)" = "10",
                                    "20 mL/kg (max 1000 mL)" = "20", 
                                    "Custom bolus (mL)" = "custom"), 
                        selected = "")),
        conditionalPanel(
          condition = "input.bolusOption == 'custom'",
          div(style = "margin-bottom: 0.3rem;",
              numericInput("customBolus", "Custom Bolus (mL)", 
                           value = 0, min = 0, step = 10))
        ),
        uiOutput("bolusWarning"),
        
        div(style = "margin-bottom: 0;",
            selectInput("replaceHours", "Replace Remaining Deficit Over:",
                        choices = c("24 hours" = 24, 
                                    "36 hours" = 36, 
                                    "48 hours" = 48, 
                                    "72 hours" = 72), 
                        selected = 48))
      )
    ),
    
    card(
      card_header(class = "py-1", "Display Options"),
      card_body(
        class = "py-1",
        checkboxInput("showFormulas", "Show Calculation Formulas", value = FALSE),
        div(style = "margin-top: 0.5rem;",
            checkboxInput("darkMode", 
                          HTML('<span id="darkModeLabel"><i class="fa-solid fa-moon"></i> Dark Mode</span>'), 
                          value = FALSE))
      )
    ),
    
    actionButton("resetButton", "Reset to Defaults", 
                 class = "btn-secondary w-100 mt-1",
                 icon = icon("rotate-left"))
  ),
  
  # Main content
  navset_card_underline(
    title = "",
    nav_panel(
      title = "Calculator",
      
      # Disclaimer
      div(
        id = "disclaimerCard",
        class = "alert alert-warning py-1 mb-1",
        style = "display: flex; justify-content: space-between; align-items: center;",
        tags$small(
          icon("triangle-exclamation", class = "me-2"),
          tags$strong("Disclaimer:"),
          " For educational purposes only. Verify all calculations before clinical use."
        ),
        actionButton("hideDisclaimer", "×", 
                     class = "btn-close btn-sm",
                     style = "padding: 0; font-size: 0.8rem;",
                     onclick = "$('#disclaimerCard').slideUp()")
      ),
      
      # Getting started message
      uiOutput("gettingStartedNote"),
      
      # Weight notice
      uiOutput("weightNotice"),
      
      # Compact calculation method selection
      conditionalPanel(
        condition = "input.weight",
        div(
          class = "card",
          style = "border-left: 4px solid #2C3E50; margin-bottom: 0;",
          div(
            class = "card-body py-1 px-3",
            div(
              style = "display: flex; align-items: center; flex-wrap: wrap; gap: 15px;",
              tags$div(style = "margin-right: 10px; padding-top: 5px;", tags$strong("Method:")),
              div(style = "display: inline-block;",
                  checkboxInput("hs", "×1.5 Maint", value = TRUE, width = "auto")),
              div(style = "display: inline-block;",
                  checkboxInput("md", "Deficit+Maint", value = FALSE, width = "auto")),
              div(style = "display: inline-block;",
                  checkboxInput("trekk", 
                                HTML('TREKK <img src="https://upload.wikimedia.org/wikipedia/commons/c/cf/Flag_of_Canada.svg" height="14px" style="vertical-align:middle;">'), 
                                value = FALSE, width = "auto"))
            ),
            conditionalPanel(
              condition = "input.md == true && (input.deficitCategory == '' || input.bolusOption == '')",
              div(class = "alert alert-warning py-1 px-2 mb-0 mt-1 small",
                  icon("circle-info"), " ",
                  "Select deficit % and bolus option in sidebar")
            )
          )
        )
      ),
      
      # Horizontal separator line
      conditionalPanel(
        condition = "input.weight",
        div(style = "height: 1px; background-color: #999; margin: 0.25rem 0 0.25rem 0;")
      ),
      
      # Results
      hollidaySegarUI("hs_module"),
      deficitMaintenanceUI("dm_module"),
      trekkGuidelineUI("trekk_module")
    ),
    
    nav_panel(
      title = "Two-Bag Calculator",
      icon = icon("flask"),
      twoBagUI("twobag_module")
    ),
    
    nav_panel(
      title = "Formula Reference",
      icon = icon("book"),
      card(
        card_header(
          class = "bg-secondary text-white",
          icon("calculator"), " Formula Reference"
        ),
        card_body(
          tags$h5("Holliday-Segar Maintenance Calculation"),
          tags$ul(
            tags$li("First 10 kg: 100 mL/kg/day"),
            tags$li("Next 10 kg (10-20 kg): 50 mL/kg/day"),
            tags$li("Above 20 kg: 20 mL/kg/day")
          ),
          tags$p(class = "text-muted", "Example: For a 25 kg patient:"),
          tags$p(class = "ms-3 text-muted", 
                 "(10 × 100) + (10 × 50) + (5 × 20) = 1,000 + 500 + 100 = 1,600 mL/day = 66.7 mL/hr"),
          
          tags$hr(),
          
          tags$h5("Fluid Deficit Calculation"),
          tags$p(tags$strong("Total Deficit (mL) = Weight (kg) × 1000 × Deficit (%)")),
          tags$p(class = "text-muted", "Common deficit levels:"),
          tags$ul(
            tags$li("Mild dehydration: 5%"),
            tags$li("Moderate dehydration: 7%"),
            tags$li("Severe dehydration: 10%")
          ),
          
          tags$hr(),
          
          tags$h5("TREKK Guideline Rates"),
          tags$p("Weight-based hourly rates:"),
          tags$ul(
            tags$li("5-10 kg: 6.5 mL/kg/hr"),
            tags$li("10-20 kg: 6 mL/kg/hr"),
            tags$li("20-40 kg: 5 mL/kg/hr"),
            tags$li("≥40 kg: 4 mL/kg/hr (maximum 250 mL/hr)")
          ),
          tags$p(class = "text-muted", 
                 HTML('<img src="https://upload.wikimedia.org/wikipedia/commons/c/cf/Flag_of_Canada.svg" height="12px" style="vertical-align:middle;"> Based on the TREKK (Translating Emergency Knowledge for Kids) guideline from Canada')),
          
          tags$hr(),
          
          tags$h5("DKA Fluid Management Principles"),
          tags$ul(
            tags$li(tags$strong("Initial Bolus:"), " Typically 10-20 mL/kg of 0.9% NaCl over 1 hour (max 500-1000 mL)"),
            tags$li(tags$strong("Deficit Replacement:"), " Remaining deficit replaced over 24-72 hours (commonly 48 hours)"),
            tags$li(tags$strong("Maintenance:"), " Continue maintenance fluids alongside deficit replacement"),
            tags$li(tags$strong("Two-Bag System:"), " Allows rapid glucose adjustment without changing total fluid rate")
          )
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {
  
  # Dark mode initialization
  observeEvent(input$darkMode, {
    if (isTRUE(input$darkMode)) {
      session$sendCustomMessage("setTheme", "dark")
    } else {
      session$sendCustomMessage("setTheme", "light")
    }
  }, ignoreInit = FALSE)
  
  # Weight validation
  output$weightWarning <- renderUI({
    if (is.null(input$weight) || is.na(input$weight)) return(NULL)
    
    if (input$weight < MIN_WEIGHT | input$weight > MAX_WEIGHT) {
      tags$div(
        class = "inline-warning",
        style = "background-color: #fff3cd; color: #856404; border-left: 3px solid #ffc107; padding: 8px; border-radius: 3px; font-size: 14px;",
        sprintf("⚠️ Weight (%.1f kg) is outside typical range (%d-%d kg)", input$weight, MIN_WEIGHT, MAX_WEIGHT)
      )
    }
  })
  
  # Deficit validation
  output$deficitWarning <- renderUI({
    if (input$deficitCategory == "" || is.null(input$deficitCategory)) return(NULL)
    
    deficit_val <- if (input$deficitCategory == "custom") input$customDeficit else as.numeric(input$deficitCategory)
    
    if (!is.na(deficit_val) && deficit_val > 12) {
      tags$div(
        class = "inline-warning",
        style = "background-color: #fff3cd; color: #856404; border-left: 3px solid #ffc107; padding: 8px; border-radius: 3px; font-size: 14px;",
        sprintf("⚠️ Deficit (%.1f%%) is very high. Verify accuracy.", deficit_val)
      )
    }
  })
  
  # Bolus validation
  output$bolusWarning <- renderUI({
    if (input$bolusOption == "" || is.null(input$bolusOption) || is.null(input$weight) || is.na(input$weight)) return(NULL)
    
    w <- input$weight
    
    bolus <- if (input$bolusOption == "custom") {
      if (is.null(input$customBolus) || is.na(input$customBolus)) return(NULL)
      input$customBolus
    } else {
      bolus_per_kg <- as.numeric(input$bolusOption)
      max_bolus <- ifelse(bolus_per_kg == 10, BOLUS_10_MAX, BOLUS_20_MAX)
      min(w * bolus_per_kg, max_bolus)
    }
    
    if (w > 0 && bolus > (w * 25)) {
      tags$div(
        class = "inline-warning",
        style = "background-color: #f8d7da; color: #721c24; border-left: 3px solid #dc3545; padding: 8px; border-radius: 3px; font-size: 14px;",
        sprintf("🛑 Bolus (%.0f mL) is very large for %.1f kg weight. Verify accuracy.", bolus, w)
      )
    } else if (w > 0 && bolus > (w * 20)) {
      tags$div(
        class = "inline-warning",
        style = "background-color: #fff3cd; color: #856404; border-left: 3px solid #ffc107; padding: 8px; border-radius: 3px; font-size: 14px;",
        sprintf("⚠️ Bolus (%.0f mL) exceeds 20 mL/kg. Verify this is intentional.", bolus)
      )
    }
  })
  
  # Reset button
  observeEvent(input$resetButton, {
    updateNumericInput(session, "weight", value = NA)
    updateSelectInput(session, "deficitCategory", selected = "")
    updateSelectInput(session, "bolusOption", selected = "")
    updateSelectInput(session, "replaceHours", selected = 48)
    updateCheckboxInput(session, "hs", value = TRUE)
    updateCheckboxInput(session, "md", value = FALSE)
    updateCheckboxInput(session, "trekk", value = FALSE)
    updateCheckboxInput(session, "showFormulas", value = FALSE)
    
    # Reset Two-Bag Calculator inputs
    updateSelectInput(session, "twobag_module-method_select", selected = "")
    updateRadioButtons(session, "twobag_module-glucose_units", selected = "mgdl")
    updateNumericInput(session, "twobag_module-blood_glucose", value = NA)
    
    showNotification("Reset to default values", 
                     type = "message", 
                     duration = 2)
  })
  
  # Reactives
  actualWeight <- reactive({ 
    req(input$weight)
    input$weight 
  })
  
  cappedWeight <- reactive({ 
    min(actualWeight(), WEIGHT_CAP) 
  })
  
  maint_hourly <- reactive({ 
    calc_holliday_segar(cappedWeight()) / 24 
  })
  
  deficit_pct <- reactive({
    if (!isTRUE(input$md)) return(7)
    req(input$deficitCategory)
    if (input$deficitCategory == "custom") {
      req(input$customDeficit)
      input$customDeficit
    } else {
      as.numeric(input$deficitCategory)
    }
  })
  
  total_deficit <- reactive({ 
    cappedWeight() * 1000 * deficit_pct() / 100 
  })
  
  bolus_mL <- reactive({
    if (!isTRUE(input$md)) return(0)
    req(input$bolusOption)
    w <- actualWeight()
    if (input$bolusOption == "custom") {
      req(input$customBolus)
      return(input$customBolus)
    }
    bolus_per_kg <- as.numeric(input$bolusOption)
    max_bolus <- ifelse(bolus_per_kg == 10, BOLUS_10_MAX, BOLUS_20_MAX)
    min(w * bolus_per_kg, max_bolus)
  })
  
  remaining_deficit <- reactive({ 
    max(total_deficit() - bolus_mL(), 0) 
  })
  
  deficit_per_hour <- reactive({ 
    remaining_deficit() / as.numeric(input$replaceHours) 
  })
  
  final_deficit_maint_rate <- reactive({ 
    maint_hourly() + deficit_per_hour() 
  })
  
  trekk_hourly <- reactive({ 
    calc_trekk_rate(actualWeight()) 
  })
  
  # Modules
  hollidaySegarServer("hs_module", cappedWeight, maint_hourly, 
                      reactive(input$showFormulas))
  
  deficitMaintenanceServer("dm_module", cappedWeight, actualWeight, 
                           total_deficit, bolus_mL,
                           remaining_deficit, deficit_per_hour, maint_hourly, 
                           final_deficit_maint_rate,
                           deficit_pct, reactive(input$bolusOption),
                           reactive(input$replaceHours), 
                           reactive(input$showFormulas), 
                           BOLUS_10_MAX, BOLUS_20_MAX)
  
  trekkGuidelineServer("trekk_module", actualWeight, trekk_hourly, 
                       reactive(input$showFormulas))
  
  # Two-bag module
  twoBagServer("twobag_module", 
               maint_hourly, 
               final_deficit_maint_rate, 
               trekk_hourly,
               reactive(input$hs),
               reactive(input$md),
               reactive(input$trekk),
               reactive(input$showFormulas),
               actualWeight)
  
  # Weight notice
  output$weightNotice <- renderUI({
    req(input$weight)
    w <- actualWeight()
    if (w > WEIGHT_CAP) {
      card(
        class = "border-danger mb-1",
        card_body(
          class = "bg-danger bg-opacity-10 py-1 text-dark",
          icon("triangle-exclamation", class = "text-danger"), " ",
          tags$strong(sprintf("Calculations capped at %d kg (entered %.1f kg)", 
                              WEIGHT_CAP, w))
        )
      )
    }
  })
  
  # Getting started note
  output$gettingStartedNote <- renderUI({
    if (is.null(input$weight) || is.na(input$weight)) {
      card(
        class = "border-info mb-1",
        card_body(
          class = "bg-info bg-opacity-10 text-dark py-1",
          icon("circle-info", class = "text-info me-2"),
          tags$strong("Getting Started:"),
          "Enter patient weight in the sidebar to begin."
        )
      )
    }
  })
}

shinyApp(ui = ui, server = server)