# app.R
# IntersectionExplorer Shiny App with dynamic title & model selector
# Author: Daniel Holman (daniel.holman@sheffield.ac.uk)
# Created: June 2025
# This code may be shared and adapted under the MIT license.

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(haven)    # for zap_labels()

# ─── STATIC GLOBALS ───────────────────────────────────────────────────────
gen_levels  <- c("Z", "Y", "X", "boomer", "silent")
ethn_levels <- c(
  "White British","African","Bangladeshi","Caribbean",
  "Indian","Mixed","Other","Other White","Pakistani","White Irish"
)
nssec_levels <- c(
  "Management & professional","Intermediate","Routine","Not in labour market"
)

# Load base to initialize sex_levels (doesn't affect session data)
pred2_base_raw <- readRDS("pred2.rds")

# helper to coerce a data.frame: zap labels, ensure numeric Age/pred, and ensure Strata exists
clean_pred2_df <- function(df) {
  # zap labels safely for Age/pred if they exist
  if ("Age" %in% names(df)) {
    df$Age <- tryCatch(as.numeric(zap_labels(df$Age)), error = function(e) as.numeric(df$Age))
  } else {
    df$Age <- NA_real_
  }
  if ("pred" %in% names(df)) {
    df$pred <- tryCatch(as.numeric(zap_labels(df$pred)), error = function(e) as.numeric(df$pred))
  } else {
    df$pred <- NA_real_
  }
  
  # create Strata column if missing, prefer existing Strata, else strata_v5, else NA
  if (!("Strata" %in% names(df))) {
    if ("strata_v5" %in% names(df)) {
      df$Strata <- df$strata_v5
    } else {
      df$Strata <- NA_character_
    }
  }
  
  # ensure final_nssec is a factor with the expected levels (if present)
  if ("final_nssec" %in% names(df)) {
    df$final_nssec <- factor(df$final_nssec, levels = nssec_levels)
  }
  
  df
}

pred2_base <- clean_pred2_df(pred2_base_raw)

sex_levels   <- sort(unique(as.character(pred2_base$sex)))
min_age_base <- min(pred2_base$Age, na.rm = TRUE)
max_age_base <- max(pred2_base$Age, na.rm = TRUE)

# ─── UI ───────────────────────────────────────────────────────────────────
ui <- fluidPage(
  
  # dynamic title
  uiOutput("dynamicTitle"),
  
  # model selector
  fluidRow(
    column(12,
           radioButtons(
             inputId  = "modelSelect",
             label    = NULL,
             choices  = c("PCS" = "pcs", "MCS" = "mcs"),
             selected = "pcs",
             inline   = TRUE
           )
    )
  ),
  
  # plot
  fluidRow(
    column(12, plotlyOutput("interactivePlot", height = "500px"))
  ),
  
  # filters
  fluidRow(
    column(12,
           wellPanel(
             style = "padding:15px; margin-bottom:20px;",
             fluidRow(
               column(3,
                      tags$div(tags$b("Generation"), actionLink("toggleGen","(All/None)")),
                      checkboxGroupInput("genSelect", NULL, choices = gen_levels,  selected = gen_levels)
               ),
               column(3,
                      tags$div(tags$b("Ethnicity"),  actionLink("toggleEthn","(All/None)")),
                      checkboxGroupInput("ethnSelect", NULL, choices = ethn_levels, selected = ethn_levels[1])
               ),
               column(3,
                      tags$div(tags$b("Sex"),        actionLink("toggleSex","(All/None)")),
                      checkboxGroupInput("sexSelect", NULL, choices = sex_levels,  selected = sex_levels[1])
               ),
               column(3,
                      tags$div(tags$b("NS-SEC"),     actionLink("toggleNssec","(All/None)")),
                      checkboxGroupInput("nssecSelect", NULL, choices = nssec_levels, selected = nssec_levels[1])
               )
             )
           )
    )
  )
)

# ─── SERVER ───────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # dynamic title render
  output$dynamicTitle <- renderUI({
    title_text <- if (is.null(input$modelSelect) || input$modelSelect == "pcs") {
      "SF-12 PCS Score by Intersectional Strata"
    } else {
      "SF-12 MCS Score by Intersectional Strata"
    }
    titlePanel(title_text)
  })
  
  # helper reactive: chosen pred2 file loaded & cleaned
  pred2 <- reactive({
    file <- if (is.null(input$modelSelect) || input$modelSelect == "pcs") "pred2.rds" else "pred2_mcs.rds"
    # safe read: if file missing, fallback to pred2_base
    df <- tryCatch(readRDS(file), error = function(e) {
      warning("Could not read ", file, " — falling back to pred2.rds base")
      pred2_base_raw
    })
    clean_pred2_df(df)
  })
  
  # session‐specific age range (use reactive so layout updates when model changes)
  min_age <- reactive(min(pred2()$Age, na.rm = TRUE))
  max_age <- reactive(max(pred2()$Age, na.rm = TRUE))
  
  # reactive y-axis label (PCS/MCS)
  y_label <- reactive({
    if (is.null(input$modelSelect) || input$modelSelect == "pcs") {
      "Predicted SF-12 PCS Score"
    } else {
      "Predicted SF-12 MCS Score"
    }
  })
  
  # “All/None” toggles
  observeEvent(input$toggleGen, {
    sel <- if (length(input$genSelect) < length(gen_levels)) gen_levels else character(0)
    updateCheckboxGroupInput(session, "genSelect", selected = sel)
  })
  observeEvent(input$toggleEthn, {
    sel <- if (length(input$ethnSelect) < length(ethn_levels)) ethn_levels else character(0)
    updateCheckboxGroupInput(session, "ethnSelect", selected = sel)
  })
  observeEvent(input$toggleSex, {
    sel <- if (length(input$sexSelect) < length(sex_levels)) sex_levels else character(0)
    updateCheckboxGroupInput(session, "sexSelect", selected = sel)
  })
  observeEvent(input$toggleNssec, {
    sel <- if (length(input$nssecSelect) < length(nssec_levels)) nssec_levels else character(0)
    updateCheckboxGroupInput(session, "nssecSelect", selected = sel)
  })
  
  # filtered data reactive
  filteredData <- reactive({
    df <- pred2()
    # if any filter empty return zero-row df
    if (any(sapply(list(input$genSelect,
                        input$ethnSelect,
                        input$sexSelect,
                        input$nssecSelect), function(x) length(x) == 0))) {
      return(df[0, ])
    }
    df %>%
      filter(
        generation    %in% input$genSelect,
        ethn_short_v2 %in% input$ethnSelect,
        sex           %in% input$sexSelect,
        final_nssec   %in% input$nssecSelect
      )
  })
  
  # render plotly
  output$interactivePlot <- renderPlotly({
    df_sel <- filteredData()
    
    # base layer (gray faint lines from the full model data)
    p_base <- ggplot() +
      geom_line(
        data      = pred2(),
        aes(x=Age, y=pred, group=Strata),
        color     = "gray90",
        linewidth = 0.3,
        alpha     = 0.4
      )
    
    # if nothing selected: just show base with dynamic y-label and fixed axes
    if (nrow(df_sel) == 0) {
      p <- p_base +
        theme_minimal(base_size=14) +
        ylab(y_label()) + xlab("Age") +
        scale_x_continuous(
          breaks = seq(floor(min_age()/5)*5,
                       ceiling(max_age()/5)*5,
                       by = 5),
          limits = c(min_age(), max_age())
        ) +
        ylim(10,60) +
        theme(
          plot.title      = element_text(face="bold",size=18),
          axis.title      = element_text(size=16),
          axis.text       = element_text(size=14),
          legend.position = "none",
          panel.grid.major = element_line(color="gray90"),
          panel.grid.minor  = element_blank()
        )
      
      return(
        ggplotly(p, tooltip = NULL) %>%
          layout(
            yaxis = list(title = y_label(), range = c(10, 60)),
            xaxis = list(range = c(min_age(), max_age())),
            margin = list(t = 50, b = 50, l = 60, r = 20)
          )
      )
    }
    
    # overlay selected
    p_sel <- geom_line(
      data = df_sel,
      aes(
        x=Age, y=pred, group=Strata, color=strata_v5,
        text=paste0("Generation: ",generation,"<br>",
                    "Intersection: ",strata_v5,"<br>",
                    "Age: ",Age)
      ),
      linewidth=0.4, alpha=0.9
    )
    
    p <- p_base + p_sel +
      theme_minimal(base_size=14) +
      ylab(y_label()) + xlab("Age") +
      scale_x_continuous(
        breaks = seq(floor(min_age()/5)*5,
                     ceiling(max_age()/5)*5,
                     by = 5),
        limits = c(min_age(), max_age())
      ) +
      ylim(10,60) +
      theme(
        plot.title      = element_text(face="bold",size=18),
        axis.title      = element_text(size=16),
        axis.text       = element_text(size=14),
        legend.position = "none",
        panel.grid.major = element_line(color="gray90"),
        panel.grid.minor  = element_blank()
      )
    
    suppressWarnings(
      ggplotly(p, tooltip="text") %>%
        layout(
          yaxis = list(title = y_label(), range = c(10, 60)),
          xaxis = list(range = c(min_age(), max_age())),
          margin = list(t=50,b=50,l=60,r=20)
        )
    )
  })
}

# ─── RUN THE APP ─────────────────────────────────────────────────────────
shinyApp(ui, server)
