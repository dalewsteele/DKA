library(shiny)
library(DT)
library(tibble)
library(dplyr)
library(openxlsx)

# ============================================================================
# CONSTANTS
# ============================================================================
VERSION <- "1.32.0"
WEIGHT_CAP <- 75
BOLUS_10_MAX <- 500
BOLUS_20_MAX <- 1000
MAX_TREKK_RATE <- 250
MIN_WEIGHT <- 5
MAX_WEIGHT <- 150
MAX_SAFE_GLUCOSE <- 12.5
MAX_K_CONC <- 40

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

calc_GIR <- function(rate_mL_hr, glucose_pct, weight) {
  glucose_mg_per_mL <- as.numeric(glucose_pct) * 10
  gir_mg_per_kg_min <- (rate_mL_hr * glucose_mg_per_mL) / (weight * 60)
  round(gir_mg_per_kg_min, 1)
}

calc_K_mEq_kg_hr <- function(rate_mL_hr, k_mEq_L, weight) {
  k_rate <- rate_mL_hr * k_mEq_L / 1000 / weight
  round(k_rate, 2)
}

# ============================================================================
# UI HELPER: Build Infusion Formulas
# ============================================================================
build_infusion_formulas_ui <- function(infusion_data, weight, glucose_conc, k_conc) {
  glucose_mg_mL <- as.numeric(glucose_conc) * 10
  
  lapply(seq_len(nrow(infusion_data)), function(i) {
    rate <- infusion_data$rate_mL_hr[i]
    method <- infusion_data$method[i]
    gir <- infusion_data$gir[i]
    k_rate <- infusion_data$k_rate[i]
    
    glucose_per_min <- rate * glucose_mg_mL / 60
    k_per_hr <- rate * as.numeric(k_conc) / 1000
    
    tags$div(
      class = "formula-box",
      tags$div(class = "formula-title", sprintf("%s Method (Rate: %d mL/hr):", method, rate)),
      
      tags$div(class = "formula-step", tags$strong("Glucose Infusion Rate (GIR):")),
      tags$div(class = "formula-step", "Formula: GIR = (Rate × Glucose concentration) ÷ (Weight × 60)"),
      tags$div(class = "formula-step", 
               sprintf("Step 1: Rate × Glucose = %d mL/hr × %s mg/mL = %s mg/hr",
                       rate, glucose_mg_mL, rate * glucose_mg_mL)),
      tags$div(class = "formula-step", 
               sprintf("Step 2: Convert to per minute = %s mg/hr ÷ 60 = %.1f mg/min",
                       rate * glucose_mg_mL, glucose_per_min)),
      tags$div(class = "formula-step", 
               sprintf("Step 3: Per kg per min = %.1f mg/min ÷ %.1f kg = %.1f mg/kg/min",
                       glucose_per_min, weight, gir)),
      tags$div(class = "calculation-details", sprintf("GIR = %.1f mg/kg/min", gir)),
      
      tags$br(),
      
      tags$div(class = "formula-step", tags$strong("Potassium Rate:")),
      tags$div(class = "formula-step", "Formula: K rate = (Rate × K concentration) ÷ (Weight × 1000)"),
      tags$div(class = "formula-step", 
               sprintf("Step 1: Convert concentration to mEq/mL = %d mEq/L ÷ 1000 = %.3f mEq/mL",
                       as.numeric(k_conc), as.numeric(k_conc) / 1000)),
      tags$div(class = "formula-step", 
               sprintf("Step 2: Calculate mEq/hr = %d mL/hr × %.3f mEq/mL = %.2f mEq/hr",
                       rate, as.numeric(k_conc) / 1000, k_per_hr)),
      tags$div(class = "formula-step", 
               sprintf("Step 3: Per kg = %.2f mEq/hr ÷ %.1f kg = %.2f mEq/kg/hr",
                       k_per_hr, weight, k_rate)),
      tags$div(class = "calculation-details",
               sprintf("K rate = %.2f mEq/kg/hr", k_rate),
               if (k_rate > 0.5) {
                 tags$div(style = "color: #dc3545; font-weight: bold; margin-top: 5px;",
                          "⚠️ Exceeds 0.5 mEq/kg/hr - verify concentration")
               })
    )
  }) %>% tags$div()
}

# ============================================================================
# MODULE: HOLLIDAY-SEGAR
# ============================================================================
hollidaySegarUI <- function(id) {
  ns <- NS(id)
  conditionalPanel(
    condition = "input.hs == true",
    uiOutput(ns("hsBox")),
    uiOutput(ns("hsMaintenanceRates")),
    uiOutput(ns("hsFormula"))
  )
}

hollidaySegarServer <- function(id, cappedWeight, maint_hourly, showFormulas) {
  moduleServer(id, function(input, output, session) {
    
    output$hsBox <- renderUI({
      tags$div(
        style = "background-color:#007bff;color:white;padding:10px;border-radius:5px;font-weight:bold;font-size:18px;margin:10px 0;",
        sprintf("💧 ×1.5 Maintenance (Holliday-Segar): %d mL/hr", round(maint_hourly() * 1.5, 0))
      )
    })
    
    output$hsMaintenanceRates <- renderUI({
      maint <- maint_hourly()
      tags$div(
        style = "margin-top: 10px; font-size: 14px;",
        sprintf("Maintenance: %d mL/hr  |  ×1.5 Maintenance: %d mL/hr  |  ×2.0 Maintenance: %d mL/hr",
                round(maint, 0), round(maint * 1.5, 0), round(maint * 2, 0))
      )
    })
    
    output$hsFormula <- renderUI({
      if (!isTRUE(showFormulas())) return(NULL)
      
      wt <- cappedWeight()
      maint <- maint_hourly()
      
      first_10 <- min(wt, 10) * 100
      next_10 <- min(max(wt - 10, 0), 10) * 50
      above_20 <- max(wt - 20, 0) * 20
      total_daily <- first_10 + next_10 + above_20
      
      tags$div(
        class = "formula-box",
        tags$div(class = "formula-title", "Detailed Calculation:"),
        tags$div(class = "formula-step", tags$strong("Step 1: Calculate Daily Maintenance")),
        tags$div(class = "formula-step", 
                 sprintf("• First 10 kg: %.1f kg × 100 mL/kg = %.0f mL/day", min(wt, 10), first_10)),
        if (wt > 10) tags$div(class = "formula-step", 
                              sprintf("• Next 10 kg: %.1f kg × 50 mL/kg = %.0f mL/day", min(wt - 10, 10), next_10)),
        if (wt > 20) tags$div(class = "formula-step", 
                              sprintf("• Above 20 kg: %.1f kg × 20 mL/kg = %.0f mL/day", wt - 20, above_20)),
        tags$div(class = "formula-step", sprintf("• Total Daily Maintenance = %.0f mL/day", total_daily)),
        tags$div(class = "formula-step", tags$strong("Step 2: Convert to Hourly Rate")),
        tags$div(class = "formula-step", sprintf("• Hourly Maintenance = %.0f mL/day ÷ 24 hr = %.1f mL/hr", total_daily, maint)),
        tags$div(class = "formula-step", tags$strong("Step 3: Apply 1.5× Multiplier for DKA")),
        tags$div(class = "formula-step", sprintf("• ×1.5 Maintenance = %.1f mL/hr × 1.5 = %.1f mL/hr", maint, maint * 1.5))
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
    h4("Deficit + Maintenance"),
    uiOutput(ns("deficitBox")),
    uiOutput(ns("deficitRate")),
    uiOutput(ns("deficitFormula"))
  )
}

deficitMaintenanceServer <- function(id, cappedWeight, actualWeight, total_deficit, bolus_mL, 
                                     remaining_deficit, deficit_per_hour, maint_hourly, 
                                     final_deficit_maint_rate, deficit_pct, bolusOption,
                                     customBolus, replaceHours, showFormulas, BOLUS_10_MAX, BOLUS_20_MAX) {
  moduleServer(id, function(input, output, session) {
    
    output$deficitBox <- renderUI({
      deficit <- total_deficit()
      bolus <- bolus_mL()
      remaining <- remaining_deficit()
      deficit_hr <- deficit_per_hour()
      maint <- maint_hourly()
      w <- actualWeight()
      
      bolus_text <- sprintf("Bolus administered: %.1f mL", bolus)
      if (bolusOption() %in% c("10", "20")) {
        bolus_per_kg <- as.numeric(bolusOption())
        max_bolus <- ifelse(bolus_per_kg == 10, BOLUS_10_MAX, BOLUS_20_MAX)
        if (w * bolus_per_kg > max_bolus) {
          bolus_text <- sprintf('Bolus administered: %.1f mL <span style="color:#0066cc;font-weight:bold;"> ℹ️ (Capped at %d mL)</span>', 
                                bolus, max_bolus)
        }
      }
      
      color <- switch(as.character(deficit_pct()),
                      "5" = "#d4edda", "7" = "#fff3cd", "10" = "#f8d7da", "#cce5ff")
      
      tags$div(
        style = sprintf("background-color:%s; padding:10px; border-radius:5px; font-weight:bold;", color),
        tags$ul(
          tags$li(HTML(sprintf("Estimated deficit: %s%% (%.1f mL)", deficit_pct(), deficit))),
          tags$li(HTML(bolus_text)),
          tags$li(HTML(sprintf("Remaining deficit: %.1f mL", remaining))),
          tags$li(HTML(sprintf("Hourly deficit replacement: %d mL/hr", round(deficit_hr, 0)))),
          tags$li(HTML(sprintf("Hourly maintenance replacement: %d mL/hr", round(maint, 0))))
        )
      )
    })
    
    output$deficitRate <- renderUI({
      final_rate <- final_deficit_maint_rate()
      double_maint <- maint_hourly() * 2
      
      tags$div(
        style = "background-color:#007bff;color:white;padding:10px;border-radius:5px;font-weight:bold;font-size:18px;margin:10px 0;",
        sprintf("💧 Final Replacement Rate: %d mL/hr", round(final_rate, 0)),
        if (final_rate > double_maint) {
          tags$div(style = "color:#ffcccc;font-size:14px;margin-top:5px;",
                   "⚠️ Rate exceeds ×2 maintenance")
        }
      )
    })
    
    output$deficitFormula <- renderUI({
      if (!isTRUE(showFormulas())) return(NULL)
      
      wt <- cappedWeight()
      deficit <- total_deficit()
      bolus <- bolus_mL()
      remaining <- remaining_deficit()
      deficit_hr <- deficit_per_hour()
      maint <- maint_hourly()
      hours <- as.numeric(replaceHours())
      final_rate <- final_deficit_maint_rate()
      
      tags$div(
        class = "formula-box",
        tags$div(class = "formula-title", "Detailed Calculation Steps:"),
        tags$div(class = "formula-step", tags$strong("Step 1: Calculate Total Deficit")),
        tags$div(class = "formula-step", sprintf("• Total Deficit = %.1f kg × 1000 × %s%% = %.1f mL", wt, deficit_pct(), deficit)),
        tags$div(class = "formula-step", tags$strong("Step 2: Account for Bolus Given")),
        tags$div(class = "formula-step", sprintf("• Bolus Administered = %.1f mL", bolus)),
        tags$div(class = "formula-step", tags$strong("Step 3: Calculate Remaining Deficit")),
        tags$div(class = "formula-step", sprintf("• Remaining Deficit = %.1f - %.1f = %.1f mL", deficit, bolus, remaining)),
        tags$div(class = "formula-step", tags$strong("Step 4: Calculate Hourly Deficit Replacement")),
        tags$div(class = "formula-step", sprintf("• Deficit per Hour = %.1f mL ÷ %d hr = %.1f mL/hr", remaining, hours, deficit_hr)),
        tags$div(class = "formula-step", tags$strong("Step 5: Add Hourly Maintenance")),
        tags$div(class = "formula-step", sprintf("• Maintenance (from Holliday-Segar) = %.1f mL/hr", maint)),
        tags$div(class = "formula-step", tags$strong("Step 6: Calculate Final Rate")),
        tags$div(class = "formula-step", sprintf("• Total Rate = %.1f + %.1f = %.1f mL/hr", deficit_hr, maint, final_rate)),
        tags$div(class = "calculation-details",
                 tags$div(tags$strong("Comparison to Maintenance Multiples:")),
                 tags$div(sprintf("• ×1.5 Maintenance: %.1f mL/hr", maint * 1.5)),
                 tags$div(sprintf("• ×2.0 Maintenance: %.1f mL/hr", maint * 2.0)),
                 tags$div(sprintf("• This rate is %.1f×  maintenance", final_rate / maint)))
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
    h4("TREKK Guideline Rate"),
    uiOutput(ns("trekkBox")),
    uiOutput(ns("trekkFormula"))
  )
}

trekkGuidelineServer <- function(id, actualWeight, trekk_hourly, showFormulas) {
  moduleServer(id, function(input, output, session) {
    
    output$trekkBox <- renderUI({
      tags$div(
        style = "background-color:#007bff;color:white;padding:10px;border-radius:5px;font-weight:bold;font-size:18px;margin:10px 0;",
        sprintf("💧 TREKK Guideline Rate: %d mL/hr", round(trekk_hourly(), 0))
      )
    })
    
    output$trekkFormula <- renderUI({
      if (!isTRUE(showFormulas())) return(NULL)
      
      wt <- actualWeight()
      rate <- trekk_hourly()
      
      rate_per_kg <- case_when(
        wt >= 5 & wt < 10 ~ 6.5,
        wt >= 10 & wt < 20 ~ 6,
        wt >= 20 & wt < 40 ~ 5,
        TRUE ~ 4
      )
      
      uncapped <- wt * rate_per_kg
      
      tags$div(
        class = "formula-box",
        tags$div(class = "formula-title", "Detailed Calculation:"),
        tags$div(class = "formula-step", tags$strong("Step 1: Determine Rate per kg based on Weight")),
        tags$div(class = "formula-step", sprintf("• Weight: %.1f kg", wt)),
        tags$div(class = "formula-step", 
                 sprintf("• Category: %.1f mL/kg/hr (weight %.1f kg)", rate_per_kg, wt)),
        tags$div(class = "formula-step", tags$strong("Step 2: Calculate Rate")),
        tags$div(class = "formula-step", sprintf("• Rate = %.1f kg × %.1f mL/kg/hr = %.1f mL/hr", wt, rate_per_kg, uncapped)),
        if (wt >= 40 & uncapped > MAX_TREKK_RATE) {
          tags$div(
            tags$div(class = "formula-step", tags$strong("Step 3: Apply Maximum Cap")),
            tags$div(class = "formula-step", sprintf("• Final Rate = %d mL/hr (capped)", MAX_TREKK_RATE))
          )
        }
      )
    })
  })
}

# ============================================================================
# MODULE: INFUSION RATES
# ============================================================================
infusionRatesUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("infusionTableHeader")),
    DTOutput(ns("infusionTable")),
    uiOutput(ns("infusionFormulas"))
  )
}

infusionRatesServer <- function(id, selected_rates, actualWeight, glucose, k_conc, 
                                calc_GIR, calc_K_mEq_kg_hr, showFormulas) {
  moduleServer(id, function(input, output, session) {
    
    infusion_data <- reactive({
      req(actualWeight())
      rates_data <- selected_rates()
      wt <- actualWeight()
      rates_rounded <- round(rates_data$rates, 0)
      
      tibble(
        method = rates_data$names,
        rate_mL_hr = rates_rounded,
        gir = sapply(rates_rounded, \(r) calc_GIR(r, as.numeric(glucose()), wt)),
        k_rate = sapply(rates_rounded, \(r) calc_K_mEq_kg_hr(r, as.numeric(k_conc()), wt))
      )
    })
    
    output$infusionTableHeader <- renderUI({
      if (!is.null(actualWeight()) && !is.na(actualWeight())) {
        h4("Calculated Infusion Rates (GIR mg/kg/min & K mEq/kg/hr)")
      }
    })
    
    output$infusionTable <- renderDT({
      data <- infusion_data()
      df <- tibble(
        Method = data$method,
        `Rate (mL/hr)` = data$rate_mL_hr,
        `GIR (mg/kg/min)` = data$gir,
        `K (mEq/kg/hr)` = data$k_rate
      )
      
      datatable(df, rownames = FALSE, 
                options = list(dom = 't', paging = FALSE, scrollX = TRUE)) %>%
        formatRound('K (mEq/kg/hr)', digits = 2) %>%
        formatStyle('K (mEq/kg/hr)', target = 'row',
                    backgroundColor = styleInterval(0.5, c(NA, '#f8d7da')))
    })
    
    output$infusionFormulas <- renderUI({
      if (!isTRUE(showFormulas())) return(NULL)
      build_infusion_formulas_ui(infusion_data(), actualWeight(), glucose(), k_conc())
    })
    
    infusion_data
  })
}

# ============================================================================
# UI
# ============================================================================
ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$script(HTML("
      $(document).on('click', '#hideDisclaimer', function() {
        $('#disclaimerSection').slideUp(300);
      });
      Shiny.addCustomMessageHandler('showDisclaimer', function(message) {
        $('#disclaimerSection').slideDown(300);
      });
    ")),
    tags$style(HTML("
      @media (max-width: 768px) {
        .container-fluid { padding-left: 10px !important; padding-right: 10px !important; }
        .col-sm-4, .col-sm-8 { width: 100% !important; padding: 5px !important; }
        .well { padding: 10px !important; }
        h4 { font-size: 18px !important; margin-top: 20px !important; }
        .btn { width: 100% !important; margin-bottom: 10px !important; }
        table { font-size: 12px !important; }
        input[type='number'], select { font-size: 16px !important; }
      }
      .formula-box { background-color: #f8f9fa; border-left: 4px solid #007bff; padding: 12px; margin: 10px 0; border-radius: 4px; font-family: 'Courier New', monospace; font-size: 13px; }
      .formula-title { font-weight: bold; color: #007bff; margin-bottom: 8px; }
      .formula-step { margin: 5px 0; padding-left: 10px; }
      .calculation-details { background-color: #e9ecef; padding: 10px; border-radius: 4px; margin-top: 10px; }
      .calculation-selection { background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px; padding: 15px; margin: 15px 0; }
      .inline-warning { margin: 8px 0 0 0; }
    "))
  ),
  
  titlePanel(sprintf("DKA: Fluid Calculator (v%s)", VERSION)),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("weight", "Weight (kg):", value = NULL, min = 1, step = 0.1),
      uiOutput("weightWarning"),
      
      radioButtons("deficitCategory", "Estimated Fluid Deficit:",
                   choices = c("Select one..." = "", "Mild (5%)" = 5, "Moderate (7%)" = 7, 
                               "Severe (10%)" = 10, "Custom" = "custom"), selected = ""),
      conditionalPanel(condition = "input.deficitCategory == 'custom'",
                       numericInput("customDeficit", "Custom Deficit (%)", value = 7, min = 0, max = 15, step = 0.1)
      ),
      uiOutput("deficitWarning"),
      
      radioButtons("bolusOption", "Fluid Bolus Option:",
                   choices = c("Select one..." = "", "10 mL/kg (max 500 mL)" = "10",
                               "20 mL/kg (max 1000 mL)" = "20", "Custom bolus (mL)" = "custom"), selected = ""),
      conditionalPanel(condition = "input.bolusOption == 'custom'",
                       numericInput("customBolus", "Custom Bolus (mL)", value = 0, min = 0, step = 10)
      ),
      uiOutput("bolusWarning"),
      
      radioButtons("replaceHours", "Replace Remaining Deficit Over:",
                   choices = c("24" = 24, "36" = 36, "48" = 48, "72" = 72), selected = 48),
      selectInput("glucose", "Glucose %", 
                  choices = c("Select one..." = "", 0, 2.5, 5, 7.5, 10, 12.5, "Other" = "other"), 
                  selected = 0),
      conditionalPanel(condition = "input.glucose == 'other'",
                       numericInput("customGlucose", "Custom Glucose (%)", value = 12.5, min = 0, max = 50, step = 2.5)
      ),
      uiOutput("glucoseWarning"),
      numericInput("k_conc", "Potassium (mEq/L)", value = 40, min = 0, max = 80, step = 1),
      uiOutput("kWarning"),
      tags$hr(),
      checkboxInput("showFormulas", "Show Calculation Formulas", value = FALSE),
      tags$hr(),
      actionButton("resetButton", "Reset to Defaults", class = "btn-secondary", style = "width: 100%;")
    ),
    
    mainPanel(
      tags$div(
        id = "disclaimerSection",
        tags$div(
          style = "background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 12px; margin-bottom: 15px; border-radius: 4px;",
          tags$div(
            style = "display: flex; justify-content: space-between; align-items: start;",
            tags$div(
              tags$strong("⚠️ Medical Disclaimer:"),
              "For demonstration only. Verify all calculations independently before clinical use."
            ),
            actionButton("hideDisclaimer", "✕", style = "background: none; border: none; font-size: 20px; padding: 0;")
          )
        )
      ),
      
      uiOutput("weightNotice"),
      uiOutput("gettingStartedNote"),
      
      conditionalPanel(
        condition = "input.weight",
        tags$div(
          class = "calculation-selection",
          tags$div(style = "font-weight: bold; margin-bottom: 10px; font-size: 16px;", "Select Calculation(s):"),
          checkboxInput("hs", "Holliday–Segar ×1.5 (always calculated)", value = TRUE),
          checkboxInput("md", "Deficit + Maintenance", value = FALSE),
          conditionalPanel(
            condition = "input.md == true && (input.deficitCategory == '' || input.bolusOption == '')",
            tags$div(style = "background-color: #fff3cd; border-left: 3px solid #ffc107; padding: 8px; margin: 5px 0 10px 20px; border-radius: 3px;",
                     HTML("⚠️ <strong>Reminder:</strong> Select deficit % and bolus option"))
          ),
          checkboxInput("trekk", HTML('TREKK <img src="https://upload.wikimedia.org/wikipedia/commons/c/cf/Flag_of_Canada.svg" height="16px" style="vertical-align:middle;">'), value = FALSE)
        )
      ),
      
      conditionalPanel(
        condition = "input.showFormulas == true",
        h4("📐 Calculation Formulas"),
        uiOutput("formulasDisplay")
      ),
      
      hollidaySegarUI("hs_module"),
      deficitMaintenanceUI("dm_module"),
      trekkGuidelineUI("trekk_module"),
      
      br(),
      infusionRatesUI("infusion_module"),
      br(),
      uiOutput("downloadButton")
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {
  
  # Weight validation display (now in sidebar)
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
  
  # Deficit validation display (now in sidebar)
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
  
  # Bolus validation display (now in sidebar)
  output$bolusWarning <- renderUI({
    if (input$bolusOption == "" || is.null(input$bolusOption) || is.null(input$weight) || is.na(input$weight)) return(NULL)
    
    w <- input$weight
    
    # Calculate bolus independently
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
  
  # Glucose validation display (in sidebar)
  output$glucoseWarning <- renderUI({
    if (is.null(input$glucose) || input$glucose == "") return(NULL)
    
    glucose_val <- if (input$glucose == "other") {
      if (is.null(input$customGlucose) || is.na(input$customGlucose)) return(NULL)
      input$customGlucose
    } else {
      as.numeric(input$glucose)
    }
    
    if (glucose_val > MAX_SAFE_GLUCOSE) {
      tags$div(
        class = "inline-warning",
        style = "background-color: #f8d7da; color: #721c24; border-left: 3px solid #dc3545; padding: 8px; border-radius: 3px; font-size: 14px;",
        sprintf("🛑 Glucose concentration (%.1f%%) exceeds safe maximum of %.1f%% for peripheral IV. Central access required.", glucose_val, MAX_SAFE_GLUCOSE)
      )
    }
  })
  
  # Potassium validation display (in sidebar)
  output$kWarning <- renderUI({
    if (is.null(input$k_conc) || is.na(input$k_conc)) return(NULL)
    
    if (input$k_conc > MAX_K_CONC) {
      tags$div(
        class = "inline-warning",
        style = "background-color: #f8d7da; color: #721c24; border-left: 3px solid #dc3545; padding: 8px; border-radius: 3px; font-size: 14px;",
        sprintf("🛑 Potassium concentration (%d mEq/L) exceeds safe maximum of %d mEq/L for peripheral IV. Central access required.", input$k_conc, MAX_K_CONC)
      )
    }
  })
  
  # Reset button
  observeEvent(input$resetButton, {
    updateNumericInput(session, "weight", value = NA)
    updateRadioButtons(session, "deficitCategory", selected = "")
    updateRadioButtons(session, "bolusOption", selected = "")
    updateRadioButtons(session, "replaceHours", selected = 48)
    updateSelectInput(session, "glucose", selected = 0)
    updateNumericInput(session, "customGlucose", value = 12.5)
    updateNumericInput(session, "k_conc", value = 40)
    updateCheckboxInput(session, "hs", value = TRUE)
    updateCheckboxInput(session, "md", value = FALSE)
    updateCheckboxInput(session, "trekk", value = FALSE)
    updateCheckboxInput(session, "showFormulas", value = FALSE)
    session$sendCustomMessage(type = "showDisclaimer", message = list())
    showNotification("Reset to default values", type = "message", duration = 2)
  })
  
  # Reactives
  actualWeight <- reactive({ req(input$weight); input$weight })
  cappedWeight <- reactive({ min(actualWeight(), WEIGHT_CAP) })
  maint_hourly <- reactive({ calc_holliday_segar(cappedWeight()) / 24 })
  
  deficit_pct <- reactive({
    if (!isTRUE(input$md)) return(7)
    req(input$deficitCategory)
    if (input$deficitCategory == "custom") {
      req(input$customDeficit); input$customDeficit
    } else {
      as.numeric(input$deficitCategory)
    }
  })
  
  total_deficit <- reactive({ cappedWeight() * 1000 * deficit_pct() / 100 })
  
  bolus_mL <- reactive({
    if (!isTRUE(input$md)) return(0)
    req(input$bolusOption)
    w <- actualWeight()
    if (input$bolusOption == "custom") {
      req(input$customBolus); return(input$customBolus)
    }
    bolus_per_kg <- as.numeric(input$bolusOption)
    max_bolus <- ifelse(bolus_per_kg == 10, BOLUS_10_MAX, BOLUS_20_MAX)
    min(w * bolus_per_kg, max_bolus)
  })
  
  remaining_deficit <- reactive({ max(total_deficit() - bolus_mL(), 0) })
  deficit_per_hour <- reactive({ remaining_deficit() / as.numeric(input$replaceHours) })
  final_deficit_maint_rate <- reactive({ maint_hourly() + deficit_per_hour() })
  trekk_hourly <- reactive({ calc_trekk_rate(actualWeight()) })
  
  selected_rates <- reactive({
    rates <- c(maint_hourly() * 1.5)
    names <- c("×1.5 Maint")
    if (isTRUE(input$md)) {
      rates <- c(rates, final_deficit_maint_rate())
      names <- c(names, "Deficit+Maint")
    }
    if (isTRUE(input$trekk)) {
      rates <- c(rates, trekk_hourly())
      names <- c(names, "TREKK")
    }
    list(rates = rates, names = names)
  })
  
  # Handle empty glucose selection by defaulting to 0 for calculations
  glucose_value <- reactive({
    if (is.null(input$glucose) || input$glucose == "") {
      return("0")
    }
    if (input$glucose == "other") {
      req(input$customGlucose)
      return(as.character(input$customGlucose))
    }
    input$glucose
  })
  
  # Modules
  hollidaySegarServer("hs_module", cappedWeight, maint_hourly, reactive(input$showFormulas))
  deficitMaintenanceServer("dm_module", cappedWeight, actualWeight, total_deficit, bolus_mL,
                           remaining_deficit, deficit_per_hour, maint_hourly, final_deficit_maint_rate,
                           deficit_pct, reactive(input$bolusOption), reactive(input$customBolus),
                           reactive(input$replaceHours), reactive(input$showFormulas), BOLUS_10_MAX, BOLUS_20_MAX)
  trekkGuidelineServer("trekk_module", actualWeight, trekk_hourly, reactive(input$showFormulas))
  infusion_data <- infusionRatesServer("infusion_module", selected_rates, actualWeight,
                                       glucose_value, reactive(input$k_conc),
                                       calc_GIR, calc_K_mEq_kg_hr, reactive(input$showFormulas))
  
  # General formulas
  output$formulasDisplay <- renderUI({
    tags$div(
      tags$div(class = "formula-box",
               tags$div(class = "formula-title", "Holliday–Segar Formula:"),
               "• First 10 kg: 100 mL/kg/day | Next 10 kg: 50 mL/kg/day | Above 20 kg: 20 mL/kg/day"
      ),
      if (isTRUE(input$md)) tags$div(class = "formula-box",
                                     tags$div(class = "formula-title", "Deficit Calculation:"),
                                     "• Deficit (mL) = Weight × 1000 × Deficit % | Final Rate = Deficit Rate + Maintenance"
      ),
      if (isTRUE(input$trekk)) tags$div(class = "formula-box",
                                        tags$div(class = "formula-title", "TREKK Rate:"),
                                        "5-10 kg: 6.5 mL/kg/hr | 10-20 kg: 6 mL/kg/hr | 20-40 kg: 5 mL/kg/hr | ≥40 kg: 4 mL/kg/hr (max 250)"
      ),
      tags$div(class = "formula-box",
               tags$div(class = "formula-title", "GIR & K:"),
               "GIR = (Rate × Glucose mg/mL) ÷ (Weight × 60)",
               tags$br(),
               "K rate = (Rate × K mEq/L) ÷ (Weight × 1000)"
      )
    )
  })
  
  output$weightNotice <- renderUI({
    req(input$weight)
    w <- actualWeight()
    if (w > WEIGHT_CAP) {
      HTML(sprintf('<span style="color:red;font-weight:bold;">⚠️ Calculations capped at %d kg (entered %.1f kg).</span>', WEIGHT_CAP, w))
    } else {
      HTML(sprintf("Weight: %.1f kg", w))
    }
  })
  
  output$gettingStartedNote <- renderUI({
    if (is.null(input$weight) || is.na(input$weight)) {
      tags$div(
        style = "background-color: #e7f3ff; border-left: 4px solid #2196F3; padding: 15px; margin: 10px 0; border-radius: 4px;",
        tags$strong("Getting Started:"),
        "Enter patient weight to begin calculations."
      )
    }
  })
  
  output$downloadButton <- renderUI({
    req(input$weight)
    downloadButton("downloadSummary", "Download Summary (Excel)", style = "width: 100%; max-width: 300px;")
  })
  
  # Download
  output$downloadSummary <- downloadHandler(
    filename = function() sprintf("DKA_Summary_%s.xlsx", format(Sys.time(), "%Y%m%d_%H%M")),
    content = function(file) {
      wt <- isolate(actualWeight())
      maint <- isolate(maint_hourly())
      infusion_df <- isolate(infusion_data())
      
      if (nrow(infusion_df) == 0) {
        showNotification("No data to download", type = "warning")
        return(NULL)
      }
      
      wb <- createWorkbook()
      addWorksheet(wb, "Summary")
      
      header_style <- createStyle(fontSize = 14, textDecoration = "bold", fgFill = "#4472C4", fontColour = "white")
      sub_style <- createStyle(fontSize = 11, textDecoration = "bold", fgFill = "#D9E1F2")
      
      writeData(wb, "Summary", "DKA Calculator Summary", startRow = 1, startCol = 1)
      addStyle(wb, "Summary", header_style, rows = 1, cols = 1)
      writeData(wb, "Summary", sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), startRow = 2, startCol = 1)
      writeData(wb, "Summary", sprintf("Weight: %.1f kg | Glucose: %s%% | K: %.0f mEq/L",
                                       wt, isolate(glucose_value()), isolate(input$k_conc)), startRow = 4, startCol = 1)
      
      current_row <- 6
      writeData(wb, "Summary", "Infusion Rates", startRow = current_row, startCol = 1)
      addStyle(wb, "Summary", sub_style, rows = current_row, cols = 1)
      
      output_df <- data.frame(
        Method = infusion_df$method,
        `Rate (mL/hr)` = infusion_df$rate_mL_hr,
        `GIR (mg/kg/min)` = infusion_df$gir,
        `K (mEq/kg/hr)` = infusion_df$k_rate,
        check.names = FALSE
      )
      writeData(wb, "Summary", output_df, startRow = current_row + 1, startCol = 1)
      
      setColWidths(wb, "Summary", cols = 1:ncol(output_df), widths = "auto")
      
      tryCatch({
        saveWorkbook(wb, file, overwrite = TRUE)
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    }
  )
}

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))