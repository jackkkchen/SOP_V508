library(shiny)
library(bslib)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(tidyr)
library(bsicons)

# Read data
health_data <- read.csv("us_health_states.csv", sep = ";", check.names = FALSE)

# Rename columns and transform data
health_data <- health_data %>%
  rename(
    "Adult.obesity..in..." = "Adult obesity [in %]",
    "Adult.smoking..in..." = "Adult smoking [in %]",
    "Physical.unhealthy.days" = "Physically Unhealthy Days",
    "Mental.unhealthy.days" = "Mentally Unhealthy Days"
  ) %>%
  mutate(
    across(c("Adult.obesity..in...", "Adult.smoking..in..."), 
           ~as.numeric(gsub(",", ".", .x))),
    across(c("Physical.unhealthy.days", "Mental.unhealthy.days"), 
           ~as.numeric(.x))
  )

# Print column names for debugging
print(names(health_data))

# UI part
ui <- function(request) {
  page_fillable(
    theme = bs_theme(
      version = 5,
      bg = "#ffffff",
      fg = "#1b1b1b",
      primary = "#004795"
    ),
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$script(src = "accessibility.js")
    ),
    
    # Main layout with sidebar
    layout_sidebar(
      # Sidebar with all controls
      sidebar = sidebar(
        title = "Control Panel",
        padding = 3,
        open = TRUE,
        position = "left",
        bg = "#e9ecef",
        style = "color: #212529;",
        
        # Search and state selection
        div(
          class = "sidebar-panel",
          accordion(
            open = TRUE,
            accordion_panel(
              "State Selection",
              icon = bsicons::bs_icon("geo-alt"),
              style = "color: #212529; background-color: #c9d5e6;",
              div(
                class = "mb-3",
                textInput("state_search", 
                  label = NULL,
                  placeholder = "Search states...",
                  width = "100%"
                )
              ),
              div(
                class = "mb-3",
                selectInput("state", 
                  label = "Select States",
                  choices = unique(health_data$State),
                  selected = "Alabama",
                  multiple = TRUE,
                  width = "100%"
                )
              )
            ),
            
            # Year selection
            accordion_panel(
              "Time Period",
              icon = bsicons::bs_icon("calendar"),
              style = "color: #212529; background-color: #c9d5e6;",
              div(
                class = "mb-3",
                selectInput("year",
                  label = "Select Year",
                  choices = unique(health_data$year),
                  selected = max(unique(health_data$year)),
                  width = "100%"
                )
              )
            ),
            
            # Health indicator selection
            accordion_panel(
              "Health Metrics",
              icon = bsicons::bs_icon("heart-pulse"),
              style = "color: #212529; background-color: #c9d5e6;",
              div(
                class = "mb-3",
                selectInput("primary_var",
                  label = "Select Indicator",
                  choices = c(
                    "Obesity Rate" = "Adult.obesity..in...",
                    "Smoking Rate" = "Adult.smoking..in..."
                  ),
                  selected = "Adult.obesity..in...",
                  width = "100%"
                )
              )
            )
          )
        )
      ),
      
      # Main content
      # Accessibility controls
      div(
        class = "accessibility-box",
        style = "background-color: rgba(0, 71, 149, 0.1) !important; border-bottom: 1px solid rgba(255, 255, 255, 0.1);",
        div(
          class = "accessibility-controls",
          style = "display: flex; justify-content: space-between; align-items: center; padding: 8px 16px;",
          # Dark mode toggle
          div(
            class = "accessibility-control",
            style = "color: #112e51;",
            bsicons::bs_icon("moon-stars", title = "Toggle dark/light mode", size = "1.2rem"),
            tooltip(
              actionButton(
                "dark_mode",
                "Light",
                class = "btn btn-outline-primary btn-accessibility",
                style = "height: 32px; padding: 4px 12px;"
              ),
              "Toggle dark/light mode (Alt+T)"
            )
          ),
          # Contrast toggle
          div(
            class = "accessibility-control",
            style = "color: #112e51;",
            bsicons::bs_icon("circle-half", title = "Toggle contrast mode", size = "1.2rem"),
            tooltip(
              actionButton(
                "contrast_toggle",
                "Standard",
                class = "btn btn-outline-primary btn-accessibility",
                style = "height: 32px; padding: 4px 12px;"
              ),
              "Toggle high contrast mode (Alt+H)"
            )
          ),
          # Font size control
          div(
            class = "accessibility-control",
            style = "color: #112e51;",
            bsicons::bs_icon("type", title = "Adjust font size", size = "1.2rem"),
            tooltip(
              sliderInput(
                "font_size",
                label = NULL,
                min = 80,
                max = 150,
                value = 100,
                step = 10,
                post = "%",
                width = "150px",
                ticks = FALSE
              ),
              "Adjust font size (Alt+Up/Down)"
            )
          )
        )
      ),
      
      # Stats boxes
      layout_column_wrap(
        width = 1/3,
        value_box(
          title = "Selected States",
          value = textOutput("state_count"),
          showcase = bsicons::bs_icon("geo-alt"),
          theme = "primary"
        ),
        value_box(
          title = "Year",
          value = textOutput("selected_year"),
          showcase = bsicons::bs_icon("calendar"),
          theme = "secondary"
        ),
        value_box(
          title = "Health Indicator",
          value = textOutput("selected_indicator"),
          showcase = bsicons::bs_icon("heart-pulse"),
          theme = "success"
        )
      ),
      
      # Visualization cards
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Trend Plot"),
          card_body(
            plotOutput("trend_plot")
          )
        ),
        card(
          card_header("Bar Plot"),
          card_body(
            plotOutput("bar_plot")
          )
        )
      ),
      
      # Data table
      card(
        card_header("Data Table"),
        card_body(
          div(
            style = "overflow-x: auto;",
            tableOutput("data_table")
          )
        )
      )
    )
  )
}

# Server logic
server <- function(input, output, session) {
  # Theme toggle
  dark_mode_state <- reactiveVal(FALSE)
  
  observeEvent(input$dark_mode, {
    is_dark <- !dark_mode_state()
    dark_mode_state(is_dark)
    
    if (is_dark) {
      updateActionButton(session, "dark_mode", label = "Dark")
      session$setCurrentTheme(
        bs_theme_update(getDefaultReactiveDomain()$getCurrentTheme(),
          bg = "#1a1a1a",
          fg = "#ffffff",
          primary = "#004795",
          "body-color" = "#ffffff",
          "body-bg" = "#1a1a1a"
        )
      )
      shinyjs::addClass(selector = "body", class = "dark-mode")
    } else {
      updateActionButton(session, "dark_mode", label = "Light")
      session$setCurrentTheme(
        bs_theme_update(getDefaultReactiveDomain()$getCurrentTheme(),
          bg = "#ffffff",
          fg = "#1b1b1b",
          primary = "#004795",
          "body-color" = "#1b1b1b",
          "body-bg" = "#ffffff"
        )
      )
      shinyjs::removeClass(selector = "body", class = "dark-mode")
    }
  })
  
  # Font size change
  observeEvent(input$font_size, {
    scale <- input$font_size / 100
    # Update font size
    js_code <- sprintf('
      document.documentElement.style.fontSize = "%d%%";
      var elements = document.querySelectorAll("body, body *");
      elements.forEach(function(el) {
        if (!el.classList.contains("irs-single")) {
          el.style.fontSize = "inherit";
        }
      });
    ', input$font_size)
    runjs(js_code)
  })
  
  # Contrast toggle
  contrast_state <- reactiveVal("Standard")
  
  observeEvent(input$contrast_toggle, {
    new_state <- if(contrast_state() == "Standard") "High" else "Standard"
    contrast_state(new_state)
    updateActionButton(session, "contrast_toggle", label = new_state)
    
    if (new_state == "High") {
      # Apply high contrast mode
      js_code <- sprintf("
        if (document.body.classList.contains('dark-mode')) {
          document.body.className = 'high-contrast-dark';
        } else {
          document.body.className = 'high-contrast-light';
        }
      ")
      runjs(js_code)
    } else {
      # Remove high contrast mode
      js_code <- sprintf("
        if (document.body.classList.contains('high-contrast-dark') || 
            document.body.classList.contains('high-contrast-light')) {
          document.body.className = document.body.classList.contains('dark-mode') ? 'dark-mode' : '';
        }
      ")
      runjs(js_code)
    }
  })
  
  # Filter states based on search
  observe({
    search_term <- tolower(input$state_search)
    states <- unique(health_data$State)
    current_selected <- input$state
    
    if (!is.null(search_term) && search_term != "") {
      # Match state name or abbreviation
      states <- states[grepl(search_term, tolower(states)) | 
                      grepl(search_term, tolower(health_data$ST[match(states, health_data$State)]))]
      
      if (length(states) == 0) {
        states <- unique(health_data$State)
      }
    }
    
    # Keep selected states
    if (!is.null(current_selected)) {
      current_selected <- current_selected[current_selected %in% states]
    } else {
      current_selected <- "Alabama"  # Ensure default selection when none selected
    }
    
    updateSelectInput(session, "state",
      choices = states,
      selected = current_selected
    )
  })
  
  # Update year selection with 2020 as default
  observe({
    years <- unique(health_data$year)
    updateSelectInput(session, "year",
      choices = years,
      selected = max(years)  # Set default to latest year
    )
  })
  
  # Set default health indicator to Obesity Rate
  observe({
    updateSelectInput(session, "primary_var",
      selected = "Adult.obesity..in..."  # Set default to Obesity Rate
    )
  })
  
  # Check if all necessary inputs are selected
  is_ready <- reactive({
    !is.null(input$state) && length(input$state) > 0 &&
    !is.null(input$year) &&
    !is.null(input$primary_var)
  })
  
  # Get trend data
  trend_data <- reactive({
    req(input$state, input$primary_var)
    
    health_data %>%
      filter(State %in% input$state) %>%
      select(
        State,
        year,
        value = !!sym(input$primary_var)
      )
  })
  
  # Get current year data
  current_data <- reactive({
    req(input$state, input$year, input$primary_var)
    
    health_data %>%
      filter(
        State %in% input$state,
        year == input$year
      ) %>%
      select(
        State,
        year,
        value = !!sym(input$primary_var)
      )
  })
  
  # Render trend plot
  output$trend_plot <- renderPlot({
    req(trend_data())
    
    var_name <- switch(input$primary_var,
      "Adult.obesity..in..." = "Obesity Rate",
      "Adult.smoking..in..." = "Smoking Rate",
      "Physical.unhealthy.days" = "Physically Unhealthy Days",
      "Mental.unhealthy.days" = "Mentally Unhealthy Days"
    )
    
    # 创建基础主题
    base_theme <- theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()
      )
    
    # 根据深色模式状态添加额外的主题设置
    if (dark_mode_state()) {
      base_theme <- base_theme +
        theme(
          plot.background = element_rect(fill = "#1a1a1a", color = NA),
          panel.background = element_rect(fill = "#1a1a1a", color = NA),
          text = element_text(color = "#ffffff"),
          axis.text = element_text(color = "#ffffff"),
          panel.grid.major = element_line(color = "#2d2d2d"),
          legend.background = element_rect(fill = "#1a1a1a", color = NA),
          legend.text = element_text(color = "#ffffff")
        )
    }
    
    ggplot(trend_data(), aes(x = year, y = value, color = State, group = State)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_color_brewer(palette = "Set2") +
      labs(
        title = paste("Trend of", var_name, "by State"),
        x = "Year",
        y = var_name
      ) +
      base_theme
  })
  
  # Render bar plot
  output$bar_plot <- renderPlot({
    req(current_data())
    
    var_name <- switch(input$primary_var,
      "Adult.obesity..in..." = "Obesity Rate",
      "Adult.smoking..in..." = "Smoking Rate",
      "Physical.unhealthy.days" = "Physically Unhealthy Days",
      "Mental.unhealthy.days" = "Mentally Unhealthy Days"
    )
    
    # 创建基础主题
    base_theme <- theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    # 根据深色模式状态添加额外的主题设置
    if (dark_mode_state()) {
      base_theme <- base_theme +
        theme(
          plot.background = element_rect(fill = "#1a1a1a", color = NA),
          panel.background = element_rect(fill = "#1a1a1a", color = NA),
          text = element_text(color = "#ffffff"),
          axis.text = element_text(color = "#ffffff"),
          panel.grid.major = element_line(color = "#2d2d2d")
        )
    }
    
    ggplot(current_data(), aes(x = reorder(State, value), y = value)) +
      geom_bar(stat = "identity", fill = if(dark_mode_state()) "#005ea2" else "#004795") +
      coord_flip() +
      labs(
        title = paste(var_name, "Comparison", input$year),
        x = "State",
        y = var_name
      ) +
      base_theme
  })
  
  # Render data table
  output$data_table <- renderTable({
    req(current_data())
    
    var_name <- switch(input$primary_var,
      "Adult.obesity..in..." = "Obesity Rate",
      "Adult.smoking..in..." = "Smoking Rate",
      "Physical.unhealthy.days" = "Physically Unhealthy Days",
      "Mental.unhealthy.days" = "Mentally Unhealthy Days"
    )
    
    current_data() %>%
      arrange(desc(value)) %>%
      rename(
        "State" = State,
        "Year" = year,
        !!var_name := value
      )
  })
  
  # New reactive outputs for value boxes
  output$state_count <- renderText({
    req(input$state)
    paste(length(input$state), "states")
  })
  
  output$selected_year <- renderText({
    req(input$year)
    input$year
  })
  
  output$selected_indicator <- renderText({
    req(input$primary_var)
    switch(input$primary_var,
      "Adult.obesity..in..." = "Obesity Rate",
      "Adult.smoking..in..." = "Smoking Rate"
    )
  })
}

# Run application
shinyApp(ui = ui, server = server) 