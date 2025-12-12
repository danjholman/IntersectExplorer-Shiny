# app.R
# IntersectionExplorer Shiny App with dynamic title & model selector
# Author: Daniel Holman (daniel.holman@sheffield.ac.uk)
# Created: June 2025
# MIT license.

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(haven)       # for zap_labels()
# install.packages("Polychrome") # <- run once if needed
suppressPackageStartupMessages(
  { have_poly <- requireNamespace("Polychrome", quietly = TRUE) }
)

# ─── STATIC GLOBALS ───────────────────────────────────────────────────────
gen_levels  <- c("Z", "Y", "X", "boomer", "silent")
ethn_levels <- c(
  "White British","African","Bangladeshi","Caribbean",
  "Indian","Mixed","Other","Other White","Pakistani","White Irish"
)
nssec_levels <- c(
  "Managerial & professional","Intermediate","Routine & manual","No work history"
)

# Load base to initialize levels (doesn't affect session data)
pred2_base_raw <- readRDS("pred2.rds")

# helper to coerce a data.frame
clean_pred2_df <- function(df) {
  if ("Age" %in% names(df)) {
    df$Age <- tryCatch(as.numeric(zap_labels(df$Age)), error = function(e) as.numeric(df$Age))
  } else df$Age <- NA_real_
  if ("pred" %in% names(df)) {
    df$pred <- tryCatch(as.numeric(zap_labels(df$pred)), error = function(e) as.numeric(df$pred))
  } else df$pred <- NA_real_
  if (!("Strata" %in% names(df))) {
    df$Strata <- if ("strata_v5" %in% names(df)) df$strata_v5 else NA_character_
  }
  if ("final_nssec" %in% names(df)) {
    # Don't convert to factor yet - keep as character for flexible matching
    df$final_nssec <- as.character(df$final_nssec)
  }
  df
}

pred2_base <- clean_pred2_df(pred2_base_raw)

# Function to normalize NS-SEC values to standard format (vectorized)
normalize_nssec <- function(x) {
  x <- as.character(x)
  x <- gsub(" and ", " & ", x, ignore.case = TRUE)
  x <- gsub("&", "& ", x)
  x <- gsub("  ", " ", x)
  x <- trimws(x)
  
  # Vectorized classification
  x_lower <- tolower(x)
  result <- x  # default to original
  result[grepl("managerial|professional", x_lower)] <- "Managerial & professional"
  result[grepl("intermediate", x_lower)] <- "Intermediate"
  result[grepl("routine|manual", x_lower)] <- "Routine & manual"
  result[grepl("no work|never work", x_lower)] <- "No work history"
  
  result
}

# Get unique sex values from data and convert to proper case
sex_levels_raw <- sort(unique(as.character(pred2_base$sex)))
sex_levels <- sapply(sex_levels_raw, function(x) paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x)))))
names(sex_levels) <- NULL  # Remove names from the vector
min_age_base <- min(pred2_base$Age, na.rm = TRUE)
max_age_base <- max(pred2_base$Age, na.rm = TRUE)

# ─── FIXED COLOUR MAPS (stable, named) ───────────────────────────────────
ethn_pal <- c(
  "White British"="#E41A1C","African"="#377EB8","Bangladeshi"="#4DAF4A",
  "Caribbean"="#984EA3","Indian"="#FF7F00","Mixed"="#A65628",
  "Other"="#F781BF","Other White"="#999999","Pakistani"="#66C2A5","White Irish"="#FFD92F"
)
sex_pal <- c("Female"="#E41A1C","Male"="#377EB8")
gen_pal <- c("Z"="#1F77B4","Y"="#FF7F0E","X"="#2CA02C","boomer"="#9467BD","silent"="#8C564B")
nssec_pal <- c(
  "Managerial & professional"="#1B9E77",
  "Intermediate"="#D95F02",
  "Routine & manual"="#7570B3",
  "No work history"="#E7298A"
)

# ─── DISTINCT PALETTE FOR INTERSECTIONS (strata_v5) ──────────────────────
strata_levels_raw <- sort(unique(as.character(pred2_base$strata_v5)))
# Normalize strata levels to have proper case for sex
strata_levels <- gsub("\\bfemale\\b", "Female", strata_levels_raw, ignore.case = TRUE)
strata_levels <- gsub("\\bmale\\b", "Male", strata_levels, ignore.case = TRUE)

# Make palette generation deterministic across sessions
set.seed(20250701)

make_hue_palette <- function(n, c = 80, l = 60) {
  hues <- seq(0, 360, length.out = n + 1)[-1]
  grDevices::hcl(h = hues, c = c, l = l)
}

if (have_poly) {
  # Vivid, perceptually distinct seed colors for better Polychrome output
  seed <- c("#1F78B4", "#E7810E", "#33A02C", "#E31A1C", "#B2186B", 
            "#40BBD5", "#6A3D9A", "#A65628")
  strata_cols <- Polychrome::createPalette(length(strata_levels), seedcolors = seed)
} else {
  strata_cols <- make_hue_palette(length(strata_levels), c = 100, l = 60)  # Higher chroma
}

# Generate base palette first (without cache for now)
strata_pal <- setNames(strata_cols, strata_levels)

# ── EXPANDED MANUAL OVERRIDES (~32 high-priority intersections) ──
# Using the 8 colors from the provided color palette
strata_manual <- c(
  # MANAGERIAL & PROFESSIONAL - Using distinct colors for each visible intersection
  "Male Managerial & professional African"         = "#4CAF50",  # green
  "Male Managerial & professional Indian"          = "#FF9800",  # orange (changed from blue)
  "Male Managerial & professional Other White"     = "#00BCD4",  # cyan
  "Male Managerial & professional White British"   = "#DC143C",  # crimson red
  "Male Managerial & professional White Irish"     = "#9C27B0",  # purple
  
  # FEMALES - Managerial & professional
  "Female Managerial & professional Mixed"         = "#B71C1C",  # dark red
  "Female Managerial & professional Other White"   = "#E64A19",  # red-orange
  "Female Managerial & professional White British" = "#FF9800",  # orange
  "Female Managerial & professional White Irish"   = "#FFEB3B",  # yellow
  "Male Managerial & professional Mixed"           = "#8B4513",  # saddle brown
  "Female Managerial & professional Indian"        = "#FF5722",  # deep orange
  "Female Managerial & professional African"       = "#FFC107",  # amber
  "Female Managerial & professional Pakistani"     = "#95E1D3",  # mint
  "Male Managerial & professional Pakistani"       = "#F38181",  # salmon
  
  # INTERMEDIATE
  "Female Intermediate White British"              = "#45B7D1",  # sky blue
  "Male Intermediate White British"                = "#FFA07A",  # light salmon
  "Female Intermediate Indian"                     = "#98D8C8",  # seafoam
  "Male Intermediate Indian"                       = "#F7DC6F",  # light gold
  
  # ROUTINE & MANUAL
  "Female Routine & manual White British"          = "#BB8FCE",  # lavender
  "Male Routine & manual White British"            = "#F8B739",  # golden
  "Female Routine & manual Pakistani"              = "#85C1E2",  # powder blue
  "Male Routine & manual Pakistani"                = "#FF8364",  # coral
  
  # NO WORK HISTORY - First 6 colors from palette for the 6 Female intersections
  "Female No work history Bangladeshi"             = "#B71C1C",  # dark red (1)
  "Female No work history Mixed"                   = "#E64A19",  # red-orange (2)
  "Female No work history Other"                   = "#FF9800",  # orange (3)
  "Female No work history Pakistani"               = "#FFEB3B",  # yellow (4)
  "Female No work history White British"           = "#4CAF50",  # green (5)
  "Female No work history White Irish"             = "#00BCD4",  # cyan (6)
  "Male No work history Bangladeshi"               = "#2196F3",  # blue
  "Male No work history Pakistani"                 = "#9C27B0",  # purple
  "Male No work history White British"             = "#2196F3",  # blue
  "Male No work history White Irish"               = "#9C27B0",  # purple
  "Female No work history Caribbean"               = "#DFE6E9",  # light grey
  "Male No work history Caribbean"                 = "#2D3436"   # dark grey
)

# Apply manual overrides to palette
common <- intersect(names(strata_manual), names(strata_pal))
strata_pal[common] <- unname(strata_manual[common])

# Save the palette with overrides applied
palette_cache <- "strata_palette.rds"
saveRDS(strata_pal, palette_cache)


scale_for <- function(aes_var) {
  switch(aes_var,
         "ethn_short_v2" = scale_colour_manual(values = ethn_pal,  breaks = names(ethn_pal),  drop = TRUE),
         "sex"           = scale_colour_manual(values = sex_pal,   breaks = names(sex_pal),   drop = TRUE),
         "generation"    = scale_colour_manual(values = gen_pal,   breaks = names(gen_pal),   drop = TRUE),
         "final_nssec"   = scale_colour_manual(values = nssec_pal, breaks = names(nssec_pal), drop = TRUE),
         "strata_v5"     = scale_colour_manual(values = strata_pal,breaks = names(strata_pal),drop = TRUE),
         scale_colour_discrete(drop = TRUE)
  )
}

# ─── UI ───────────────────────────────────────────────────────────────────
ui <- fluidPage(
  uiOutput("dynamicTitle"),
  fluidRow(
    column(
      12,
      div(
        style = "display:flex; gap:16px; align-items:center; flex-wrap:wrap;",
        radioButtons("modelSelect", NULL, c("PCS"="pcs","MCS"="mcs"), selected="pcs", inline=TRUE),
        selectInput(
          "colourBy","Colour by",
          choices=c("Intersection"="strata_v5","Ethnicity"="ethn_short_v2","Sex"="sex","Generation"="generation","NS-SEC"="final_nssec"),
          selected="strata_v5", width="220px"
        ),
        checkboxInput("showLegend","Show legend overlay", TRUE),
        actionButton("randomizeColors", "Randomise Colors", icon = icon("shuffle"))
      )
    )
  ),
  fluidRow(column(12, plotlyOutput("interactivePlot", height="500px"))),
  fluidRow(
    column(12,
           wellPanel(
             style="padding:15px; margin-bottom:20px;",
             fluidRow(
               column(3, tags$div(tags$b("Generation"), actionLink("toggleGen","(All/None)")),
                      checkboxGroupInput("genSelect", NULL, choices=gen_levels, selected=gen_levels)),
               column(3, tags$div(tags$b("Ethnicity"), actionLink("toggleEthn","(All/None)")),
                      checkboxGroupInput("ethnSelect", NULL, choices=ethn_levels, selected=ethn_levels[1])),
               column(3, tags$div(tags$b("Sex"), actionLink("toggleSex","(All/None)")),
                      checkboxGroupInput("sexSelect", NULL, choices=sex_levels, selected=sex_levels[1])),
               column(3, tags$div(tags$b("NS-SEC"), actionLink("toggleNssec","(All/None)")),
                      checkboxGroupInput("nssecSelect", NULL, choices=nssec_levels, selected=nssec_levels[1]))
             )
           )
    )
  )
)

# ─── SERVER ───────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  output$dynamicTitle <- renderUI({
    title_text <- if (is.null(input$modelSelect) || input$modelSelect == "pcs")
      "SF-12 PCS Score by Intersectional Strata"
    else "SF-12 MCS Score by Intersectional Strata"
    titlePanel(title_text)
  })
  
  # Reactive palette for intersection colors
  current_strata_pal <- reactiveVal(strata_pal)
  
  # Randomize intersection colors (use predefined distinct colors, shuffled)
  observeEvent(input$randomizeColors, {
    new_seed <- as.integer(Sys.time())
    set.seed(new_seed)
    
    n <- length(strata_levels)
    
    # Base set of highly distinct colors (expanded palette)
    distinct_colors <- c(
      "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF",
      "#8DD3C7", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9",
      "#BC80BD", "#CCEBC5", "#FFED6F", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
      "#E6AB02", "#A6761D", "#666666", "#E31A1C", "#1F78B4", "#33A02C", "#FB9A99", "#CAB2D6",
      "#6A3D9A", "#B15928", "#FDBF6F", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B"
    )
    
    # If we need more colors than we have, cycle through with variations
    if (n > length(distinct_colors)) {
      distinct_colors <- rep(distinct_colors, ceiling(n / length(distinct_colors)))
    }
    
    # Randomly sample from the distinct colors
    new_cols <- sample(distinct_colors, n)
    
    new_pal <- setNames(new_cols, strata_levels)
    # Apply manual overrides for key intersections
    common <- intersect(names(strata_manual), names(new_pal))
    new_pal[common] <- unname(strata_manual[common])
    
    current_strata_pal(new_pal)
  })
  
  pred2 <- reactive({
    file <- if (is.null(input$modelSelect) || input$modelSelect == "pcs") "pred2.rds" else "pred2_mcs.rds"
    df <- tryCatch(readRDS(file), error = function(e) { warning("Could not read ", file, " — falling back to pred2.rds base"); pred2_base_raw })
    clean_pred2_df(df)
  })
  
  min_age <- reactive(min(pred2()$Age, na.rm = TRUE))
  max_age <- reactive(max(pred2()$Age, na.rm = TRUE))
  
  y_label <- reactive(if (is.null(input$modelSelect) || input$modelSelect == "pcs")
    "Predicted SF-12 PCS Score" else "Predicted SF-12 MCS Score")
  
  legend_title <- reactive({
    switch(input$colourBy,
           "strata_v5"="Intersection","ethn_short_v2"="Ethnicity","sex"="Sex","generation"="Generation","final_nssec"="NS-SEC","Legend")
  })
  
  # Helper to get the right scale based on colour variable (non-reactive function)
  get_color_scale <- function(aes_var, strata_pal_current) {
    switch(aes_var,
           "ethn_short_v2" = scale_colour_manual(values = ethn_pal,  breaks = names(ethn_pal),  drop = TRUE),
           "sex"           = scale_colour_manual(values = sex_pal,   breaks = names(sex_pal),   drop = TRUE),
           "generation"    = scale_colour_manual(values = gen_pal,   breaks = names(gen_pal),   drop = TRUE),
           "final_nssec"   = scale_colour_manual(values = nssec_pal, breaks = names(nssec_pal), drop = TRUE),
           "strata_v5"     = scale_colour_manual(values = strata_pal_current, breaks = names(strata_pal_current), drop = TRUE),
           scale_colour_discrete(drop = TRUE)
    )
  }
  
  observeEvent(input$toggleGen, {
    sel <- if (length(input$genSelect) < length(gen_levels)) gen_levels else character(0)
    updateCheckboxGroupInput(session,"genSelect",selected=sel)
  })
  observeEvent(input$toggleEthn, {
    sel <- if (length(input$ethnSelect) < length(ethn_levels)) ethn_levels else character(0)
    updateCheckboxGroupInput(session,"ethnSelect",selected=sel)
  })
  observeEvent(input$toggleSex, {
    sel <- if (length(input$sexSelect) < length(sex_levels)) sex_levels else character(0)
    updateCheckboxGroupInput(session,"sexSelect",selected=sel)
  })
  observeEvent(input$toggleNssec, {
    sel <- if (length(input$nssecSelect) < length(nssec_levels)) nssec_levels else character(0)
    updateCheckboxGroupInput(session,"nssecSelect",selected=sel)
  })
  
  filteredData <- reactive({
    df <- pred2()
    if (any(sapply(list(input$genSelect,input$ethnSelect,input$sexSelect,input$nssecSelect), function(x) length(x)==0))) {
      return(df[0, ])
    }
    # Normalize sex and NS-SEC values for filtering
    df <- df %>%
      mutate(
        sex_normalized = paste0(toupper(substr(as.character(sex), 1, 1)), tolower(substring(as.character(sex), 2))),
        nssec_normalized = normalize_nssec(final_nssec),
        # Normalize strata_v5 to have proper case for sex
        strata_v5_normalized = gsub("\\bfemale\\b", "Female", strata_v5, ignore.case = TRUE),
        strata_v5_normalized = gsub("\\bmale\\b", "Male", strata_v5_normalized, ignore.case = TRUE)
      )
    
    df %>%
      filter(
        generation       %in% input$genSelect,
        ethn_short_v2    %in% input$ethnSelect,
        sex_normalized   %in% input$sexSelect,
        nssec_normalized %in% input$nssecSelect
      ) %>%
      mutate(
        generation    = factor(generation,    levels=gen_levels),
        ethn_short_v2 = factor(ethn_short_v2, levels=ethn_levels),
        sex           = factor(sex_normalized, levels=c("Female","Male")),
        final_nssec   = factor(nssec_normalized, levels=nssec_levels),
        strata_v5     = factor(strata_v5_normalized, levels=strata_levels)
      ) %>%
      select(-sex_normalized, -nssec_normalized, -strata_v5_normalized)  # Remove temporary columns
  })
  
  # ─── PLOT (with high-DPI export) ────────────────────────────────────────
  output$interactivePlot <- renderPlotly({
    df_all <- pred2()
    df_sel <- filteredData()
    
    # Get current palette (creates reactive dependency)
    strata_colors <- current_strata_pal()
    
    p_base <- ggplot() +
      geom_line(
        data      = df_all,
        aes(x = Age, y = pred, group = Strata),
        color     = "gray80",
        linewidth = 0.3,
        alpha     = 0.35
      )
    
    # Empty selection: base only
    if (nrow(df_sel) == 0) {
      p <- p_base +
        theme_minimal(base_size = 14) +
        ylab(y_label()) + xlab("Age") +
        scale_x_continuous(
          breaks = seq(floor(min_age()/5)*5, ceiling(max_age()/5)*5, by = 5),
          limits = c(min_age(), max_age())
        ) + ylim(10, 60) +
        theme(
          plot.title       = element_text(face = "bold", size = 18),
          axis.title       = element_text(size = 16),
          axis.text        = element_text(size = 14),
          legend.position  = "none",
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank()
        )
      
      return(
        ggplotly(p, tooltip = NULL) %>%
          layout(
            yaxis  = list(title = y_label(), range = c(10, 60)),
            xaxis  = list(range = c(min_age(), max_age())),
            margin = list(t = 50, b = 50, l = 60, r = 20),
            showlegend = FALSE
          ) %>%
          config(
            toImageButtonOptions = list(
              format   = "png",
              filename = "plot_export",
              scale    = 3           # hi-DPI, keeps on-screen aspect
            ),
            displaylogo = FALSE
          )
      )
    }
    
    # Selected overlay; colour is dynamic
    colour_var <- input$colourBy
    p <- p_base +
      geom_line(
        data = df_sel,
        aes(
          x = Age, y = pred, group = Strata, color = .data[[colour_var]],
          text = paste0(
            "Generation: ", generation, "<br>",
            "Intersection: ", strata_v5, "<br>",
            "Age: ", Age, "<br>",
            legend_title(), ": ", .data[[colour_var]]
          )
        ),
        linewidth = 0.5, alpha = 0.95
      ) +
      theme_minimal(base_size = 14) +
      ylab(y_label()) + xlab("Age") +
      scale_x_continuous(
        breaks = seq(floor(min_age()/5)*5, ceiling(max_age()/5)*5, by = 5),
        limits = c(min_age(), max_age())
      ) +
      ylim(10, 60) +
      get_color_scale(colour_var, strata_colors) +
      guides(color = guide_legend(title = legend_title())) +
      theme(
        plot.title       = element_text(face = "bold", size = 18),
        axis.title       = element_text(size = 16),
        axis.text        = element_text(size = 14),
        legend.position  = "right",
        legend.title     = element_text(size = 11, face = "bold"),
        legend.text      = element_text(size = 10),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
      )
    
    suppressWarnings(
      ggplotly(p, tooltip = "text") %>%
        layout(
          yaxis  = list(title = y_label(), range = c(10, 60)),
          xaxis  = list(range = c(min_age(), max_age())),
          margin = list(t = 50, b = 50, l = 60, r = 20),
          showlegend = isTRUE(input$showLegend),
          legend = list(
            x = 0.02, y = 0.60, xanchor = "left", yanchor = "top",
            bgcolor = "rgba(255,255,255,0.80)", bordercolor = "rgba(0,0,0,0.25)",
            borderwidth = 1, font = list(size = 10), tracegroupgap = 0,
            itemclick = "toggle", itemdoubleclick = "toggleothers"
          )
        ) %>%
        config(
          editable = TRUE,   
          toImageButtonOptions = list(
            format   = "png",
            filename = "plot_export",
            scale    = 3
          ),
          displaylogo = FALSE
        )
    )
  })
}

# ─── RUN THE APP ─────────────────────────────────────────────────────────
shinyApp(ui, server)

