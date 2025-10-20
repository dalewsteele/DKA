library(shiny)
library(shinydashboard)

# Two-bag calculation function with interpolation
calculate_two_bag_rates <- function(glucose_mg_dl, replacement_rate_ml_hr, potassium_meq_l, weight_kg = NULL) {
  
  # Determine target dextrose concentration with LINEAR INTERPOLATION
  if (glucose_mg_dl >= 300) {
    # High glucose: no dextrose
    target_dextrose_pct <- 0
    
  } else if (glucose_mg_dl > 250) {
    # Interpolate between 0% at 300 and 5% at 250
    target_dextrose_pct <- (300 - glucose_mg_dl) / 10
    
  } else if (glucose_mg_dl >= 200) {
    # Interpolate between 5% at 250 and 7.5% at 200
    target_dextrose_pct <- 5 + (250 - glucose_mg_dl) / 20
    
  } else if (glucose_mg_dl >= 150) {
    # Interpolate between 7.5% at 200 and 10% at 150
    target_dextrose_pct <- 7.5 + (200 - glucose_mg_dl) / 20
    
  } else {
    # Below 150: maximum dextrose
    target_dextrose_pct <- 10
  }
  
  # Round to 1 decimal place for display
  target_dextrose_pct <- round(target_dextrose_pct, 1)
  
  # If glucose < 150, increase rate by 1.33x
  if (glucose_mg_dl < 150) {
    actual_rate <- round(replacement_rate_ml_hr * 1.33)
    rate_increased <- TRUE
    glucose_warning <- paste0("CLINICAL ALERT: Glucose is ", glucose_mg_dl, 
                              " mg/dL (below 150). Infusion rate increased by 1.33x from ",
                              replacement_rate_ml_hr, " to ", actual_rate, " mL/hr.")
  } else {
    actual_rate <- replacement_rate_ml_hr
    rate_increased <- FALSE
    glucose_warning <- NULL
  }
  
  # Calculate bag rates
  # Bag 1: No dextrose (0% dextrose)
  # Bag 2: D10 (10% dextrose)
  # Mix to achieve target concentration
  
  if (target_dextrose_pct == 0) {
    # No dextrose needed - all from Bag 1
    bag1_rate <- round(actual_rate)
    bag2_rate <- 0
  } else {
    # Calculate percentage from D10 bag needed to achieve target
    # target% = (bag2_rate / total_rate) × 10%
    # So: bag2_rate = (target% / 10%) × total_rate
    bag2_rate_exact <- actual_rate * (target_dextrose_pct / 10)
    bag1_rate_exact <- actual_rate - bag2_rate_exact
    
    # Round both to nearest whole number
    bag2_rate <- round(bag2_rate_exact)
    bag1_rate <- round(bag1_rate_exact)
  }
  
  # Calculate actual total rate after rounding
  actual_total_rate <- bag1_rate + bag2_rate
  
  # Calculate actual dextrose percentage delivered after rounding
  if (actual_total_rate > 0) {
    actual_dextrose_pct <- round((bag2_rate / actual_total_rate) * 10, 1)
  } else {
    actual_dextrose_pct <- 0
  }
  
  # Calculate dextrose delivery (g/hr)
  # D10 = 0.1 g/mL
  dextrose_g_hr <- bag2_rate * 0.1
  
  # Calculate potassium delivery based on actual total rate
  potassium_meq_hr <- round(actual_total_rate * (potassium_meq_l / 1000), 1)
  
  # Calculate potassium infusion rate (mEq/kg/hr) if weight provided
  if (!is.null(weight_kg) && weight_kg > 0) {
    potassium_meq_kg_hr <- round(potassium_meq_hr / weight_kg, 2)
  } else {
    potassium_meq_kg_hr <- NULL
  }
  
  # Calculate GIR (mg/kg/min) if weight provided
  if (!is.null(weight_kg) && weight_kg > 0) {
    gir <- (dextrose_g_hr * 1000) / (weight_kg * 60)
    gir <- round(gir, 2)
  } else {
    gir <- NULL
  }
  
  return(list(
    bag1_rate = bag1_rate,
    bag2_rate = bag2_rate,
    actual_total_rate = actual_total_rate,
    target_dextrose_pct = target_dextrose_pct,
    actual_dextrose_pct = actual_dextrose_pct,
    potassium_meq_l = potassium_meq_l,
    potassium_meq_hr = potassium_meq_hr,
    potassium_meq_kg_hr = potassium_meq_kg_hr,
    actual_rate = actual_rate,
    replacement_rate = replacement_rate_ml_hr,
    rate_increased = rate_increased,
    dextrose_g_hr = round(dextrose_g_hr, 2),
    gir = gir,
    glucose_warning = glucose_warning
  ))
}

# UI
ui <- dashboardPage(
  
  dashboardHeader(title = "Pediatric DKA Two-Bag Calculator"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calculator", tabName = "calculator", icon = icon("calculator")),
      menuItem("Reference", tabName = "reference", icon = icon("book"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Calculator tab
      tabItem(tabName = "calculator",
              fluidRow(
                box(
                  title = "Patient Parameters",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  
                  numericInput("weight",
                               "Patient Weight (kg):",
                               value = 20,
                               min = 5,
                               max = 100,
                               step = 0.5),
                  
                  numericInput("glucose",
                               "Blood Glucose (mg/dL):",
                               value = 280,
                               min = 50,
                               max = 600,
                               step = 10),
                  
                  numericInput("replacement_rate",
                               "Replacement IV Fluid Rate (mL/hr):",
                               value = 100,
                               min = 10,
                               max = 500,
                               step = 5),
                  
                  numericInput("potassium",
                               "Potassium Concentration (mEq/L):",
                               value = 40,
                               min = 0,
                               max = 60,
                               step = 5),
                  
                  hr(),
                  
                  p(icon("info-circle"), " Rates update automatically with smooth interpolation",
                    style = "color: #777; font-style: italic;")
                ),
                
                box(
                  title = "Two-Bag Infusion Rates",
                  status = "success",
                  solidHeader = TRUE,
                  width = 8,
                  
                  uiOutput("warning_box"),
                  
                  tableOutput("bag_rates_table"),
                  
                  hr(),
                  
                  h4(textOutput("gir_display"), 
                     style = "color: #00a65a; font-weight: bold;"),
                  
                  h4(textOutput("potassium_display"), 
                     style = "color: #f39c12; font-weight: bold;"),
                  
                  hr(),
                  
                  h4("Bag Setup:"),
                  verbatimTextOutput("bag_setup")
                )
              ),
              
              fluidRow(
                box(
                  title = "Glucose Management Guide",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  
                  tableOutput("glucose_guide")
                )
              )
      ),
      
      # Reference tab
      tabItem(tabName = "reference",
              fluidRow(
                box(
                  title = "Two-Bag DKA Fluid Management",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  
                  h4("Two-Bag Setup:"),
                  tags$ul(
                    tags$li(strong("Bag 1:"), " 0.9% NaCl (or 0.45% NaCl) with (KCl + KPhos) - NO dextrose"),
                    tags$li(strong("Bag 2:"), " Same solution WITH D10 (10% dextrose) and (KCl + KPhos)")
                  ),
                  
                  h4("Protocol with Smooth Interpolation:"),
                  tags$ol(
                    tags$li("Start with calculated replacement fluid rate"),
                    tags$li("Dextrose concentration adjusts smoothly based on glucose:"),
                    tags$ul(
                      tags$li("Glucose ≥ 300 mg/dL: 0% dextrose (100% Bag 1)"),
                      tags$li("Glucose 250-300 mg/dL: Gradually increase from 0% to 5% dextrose"),
                      tags$li("Glucose 200-250 mg/dL: Gradually increase from 5% to 7.5% dextrose"),
                      tags$li("Glucose 150-200 mg/dL: Gradually increase from 7.5% to 10% dextrose"),
                      tags$li("Glucose < 150 mg/dL: 10% dextrose (100% Bag 2) + increase rate by 1.33x")
                    ),
                    tags$li("Bag ratios adjust automatically for smooth dextrose delivery")
                  ),
                  
                  h4("Interpolation Benefits:"),
                  tags$ul(
                    tags$li("Smoother glucose control - avoids large step changes in dextrose"),
                    tags$li("More physiologic response to glucose changes"),
                    tags$li("Proportional adjustment - dextrose increases gradually as glucose falls"),
                    tags$li("Reduces risk of glucose swings from sudden dextrose changes")
                  ),
                  
                  h4("Rounding Convention:"),
                  p("All bag rates are rounded to the nearest whole mL/hr for practical pump settings."),
                  
                  h4("Advantages of Two-Bag System:"),
                  tags$ul(
                    tags$li("No need to change IV bags when adjusting dextrose"),
                    tags$li("Smooth titration of dextrose delivery"),
                    tags$li("Maintains constant total fluid rate (unless glucose < 150)"),
                    tags$li("Allows rapid response to glucose changes")
                  ),
                  
                  h4("Potassium Management:"),
                  tags$ul(
                    tags$li("Both bags should contain the same potassium concentration"),
                    tags$li("Typical concentration: 40 mEq/L (20 mEq KCl + 20 mEq KPhos)"),
                    tags$li("Maximum recommended rate: 0.5 mEq/kg/hr (peripheral line)"),
                    tags$li("Do NOT start potassium if patient is anuric or hyperkalemic"),
                    tags$li("Check serum potassium frequently (every 2-4 hours initially)")
                  ),
                  
                  h4("Goals:"),
                  tags$ul(
                    tags$li("Maintain glucose 150-250 mg/dL during DKA treatment"),
                    tags$li("Maintain serum potassium 3.5-5.0 mEq/L"),
                    tags$li("Prevent hypoglycemia and cerebral edema"),
                    tags$li("Allow continued insulin therapy")
                  ),
                  
                  h4("Glucose Infusion Rate (GIR):"),
                  p("GIR = (Dextrose g/hr × 1000) ÷ (Weight kg × 60)"),
                  p("Target: 4-6 mg/kg/min during DKA treatment"),
                  
                  h4("Important Notes:"),
                  tags$ul(
                    tags$li("Check glucose hourly or more frequently"),
                    tags$li("Adjust bag rates as glucose changes - system provides smooth transitions"),
                    tags$li("Monitor serum potassium closely"),
                    tags$li("Do NOT stop insulin when adding dextrose"),
                    tags$li("Follow your institutional DKA protocol")
                  ),
                  
                  hr(),
                  
                  p(em("This calculator is for educational purposes. Always follow your institutional protocols and consult with attending physicians."))
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive calculation with validation
  rates <- reactive({
    # Validate inputs
    validate(
      need(input$weight, "Please enter patient weight"),
      need(input$glucose, "Please enter a blood glucose value"),
      need(input$replacement_rate, "Please enter a replacement IV rate"),
      need(input$potassium, "Please enter potassium concentration"),
      need(!is.na(input$weight), "Weight must be a valid number"),
      need(!is.na(input$glucose), "Blood glucose must be a valid number"),
      need(!is.na(input$replacement_rate), "Replacement IV rate must be a valid number"),
      need(!is.na(input$potassium), "Potassium concentration must be a valid number"),
      need(input$weight > 0, "Weight must be greater than 0"),
      need(input$glucose > 0, "Blood glucose must be greater than 0"),
      need(input$replacement_rate > 0, "Replacement IV rate must be greater than 0"),
      need(input$potassium >= 0, "Potassium concentration must be 0 or greater")
    )
    
    calculate_two_bag_rates(input$glucose, input$replacement_rate, input$potassium, input$weight)
  })
  
  # Warning box
  output$warning_box <- renderUI({
    rates_data <- rates()
    
    if (!is.null(rates_data$glucose_warning)) {
      div(
        class = "alert alert-danger",
        style = "margin-bottom: 15px;",
        icon("exclamation-triangle"),
        " ", rates_data$glucose_warning
      )
    }
  })
  
  # Bag rates table
  output$bag_rates_table <- renderTable({
    rates_data <- rates()
    
    data.frame(
      Bag = c("Bag 1 (No Dextrose)", "Bag 2 (D10)", "TOTAL"),
      `Rate (mL/hr)` = c(
        rates_data$bag1_rate,
        rates_data$bag2_rate,
        rates_data$actual_total_rate
      ),
      `Percentage` = c(
        paste0(round((rates_data$bag1_rate / rates_data$actual_total_rate) * 100, 1), "%"),
        paste0(round((rates_data$bag2_rate / rates_data$actual_total_rate) * 100, 1), "%"),
        "100%"
      ),
      `Dextrose Delivered` = c(
        "0%",
        "10%",
        paste0(rates_data$actual_dextrose_pct, "%")
      ),
      check.names = FALSE
    )
  }, bordered = TRUE, striped = TRUE, hover = TRUE, spacing = "m")
  
  # GIR display
  output$gir_display <- renderText({
    rates_data <- rates()
    if (!is.null(rates_data$gir)) {
      paste0("Glucose Infusion Rate (GIR): ", rates_data$gir, " mg/kg/min")
    } else {
      "Please enter patient weight to calculate GIR"
    }
  })
  
  # Potassium display
  output$potassium_display <- renderText({
    rates_data <- rates()
    if (!is.null(rates_data$potassium_meq_kg_hr)) {
      paste0("Potassium Infusion Rate: ", rates_data$potassium_meq_kg_hr, " mEq/kg/hr")
    } else {
      "Please enter patient weight to calculate potassium rate"
    }
  })
  
  # Bag setup instructions
  output$bag_setup <- renderText({
    rates_data <- rates()
    paste0(
      "Bag 1: 0.9% NaCl + ", rates_data$potassium_meq_l, " mEq/L (KCl + KPhos) - NO dextrose\n",
      "Bag 2: 0.9% NaCl + D10 + ", rates_data$potassium_meq_l, " mEq/L (KCl + KPhos)\n\n",
      "Set Pump 1 (Bag 1) to: ", rates_data$bag1_rate, " mL/hr\n",
      "Set Pump 2 (Bag 2) to: ", rates_data$bag2_rate, " mL/hr\n",
      "Total rate: ", rates_data$actual_total_rate, " mL/hr\n",
      "Target dextrose: ", rates_data$target_dextrose_pct, "%"
    )
  })
  
  # Glucose management guide
  output$glucose_guide <- renderTable({
    data.frame(
      `Glucose (mg/dL)` = c("≥ 300", "275", "250", "225", "200", "175", "150", "< 150"),
      `Target Dextrose` = c("0%", "2.5%", "5%", "6.25%", "7.5%", "8.75%", "10%", "10%"),
      `Bag 1` = c("100%", "75%", "50%", "37.5%", "25%", "12.5%", "0%", "0%"),
      `Bag 2` = c("0%", "25%", "50%", "62.5%", "75%", "87.5%", "100%", "100%"),
      `Rate` = c("Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base × 1.33"),
      check.names = FALSE
    )
  }, bordered = TRUE, striped = TRUE, hover = TRUE)
}

# Run the app
shinyApp(ui = ui, server = server)