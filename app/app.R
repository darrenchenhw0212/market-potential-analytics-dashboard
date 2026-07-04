# app.R

library(shiny)
library(tidyverse)
library(plotly)
library(scales)
library(readxl)
library(countrycode)

my_data <- readxl::read_excel("data/raw/superstore.xlsx")

gdp <- readr::read_csv("data/raw/gdp_per_capita.csv")
internet <- readr::read_csv("data/raw/population_by_country.csv")
population <- readr::read_csv("data/raw/internet_user_by_country.csv")

gdp_2014 <- gdp %>%
  select(Country = `Country Code`, GDP_per_Capita = `2014`) %>%
  filter(!is.na(GDP_per_Capita))

population_2014 <- population %>%
  select(Country = `Country Code`, Population = `2014`) %>%
  filter(!is.na(Population))

internet_2014 <- internet %>%
  select(Country = `Country Code`, Internet_Usage = `2014`) %>%
  filter(!is.na(Internet_Usage))

external_2014 <- gdp_2014 %>%
  left_join(population_2014, by = "Country") %>%
  left_join(internet_2014, by = "Country")

my_data <- my_data %>%
  mutate(
    Country_Code = countrycode(
      Country,
      origin = "country.name",
      destination = "iso3c"
    )
  )

market_data <- my_data %>%
  left_join(
    external_2014,
    by = c("Country_Code" = "Country")
  )

market_data_plot <- market_data %>%
  filter(
    !is.na(GDP_per_Capita),
    !is.na(Population),
    !is.na(Internet_Usage),
    !is.na(Profit),
    !is.na(Sales)
  )

country_market <- market_data_plot %>%
  group_by(Country) %>%
  summarise(
    Country_Code = first(Country_Code),
    TotalSales = sum(Sales, na.rm = TRUE),
    TotalProfit = sum(Profit, na.rm = TRUE),
    ProfitMargin = TotalProfit / TotalSales,
    GDP_per_Capita = first(GDP_per_Capita),
    Population = first(Population),
    Internet_Usage = first(Internet_Usage),
    .groups = "drop"
  ) %>%
  mutate(
    Region = countrycode(
      Country_Code,
      origin = "iso3c",
      destination = "region"
    ),
    
    GDP_Score = (GDP_per_Capita - min(GDP_per_Capita)) /
      (max(GDP_per_Capita) - min(GDP_per_Capita)),
    
    Population_Score = (Population - min(Population)) /
      (max(Population) - min(Population)),
    
    Internet_Score = (Internet_Usage - min(Internet_Usage)) /
      (max(Internet_Usage) - min(Internet_Usage)),
    
    ProfitMargin_Score_Raw = (ProfitMargin - min(ProfitMargin)) /
      (max(ProfitMargin) - min(ProfitMargin)),
    
    PotentialScore_Raw =
      0.4 * GDP_Score +
      0.4 * Population_Score +
      0.2 * Internet_Score,
    
    OpportunityGap_Raw =
      PotentialScore_Raw - ProfitMargin_Score_Raw,
    
    PotentialScore =
      as.numeric(scale(PotentialScore_Raw)),
    
    ProfitMargin_Score =
      as.numeric(scale(ProfitMargin)),
    
    OpportunityGap =
      PotentialScore - ProfitMargin_Score,
    
    Profitability = case_when(
      ProfitMargin < 0 ~ "Loss-Making",
      ProfitMargin < 0.15 ~ "Low Profitability",
      ProfitMargin < 0.25 ~ "Moderate Profitability",
      TRUE ~ "High Profitability"
    ),
    
    Profitability = factor(
      Profitability,
      levels = c(
        "Loss-Making",
        "Low Profitability",
        "Moderate Profitability",
        "High Profitability"
      )
    )
  ) %>%
  filter(!is.na(Region))

profitability_colours <- c(
  "Loss-Making" = "red",
  "Low Profitability" = "orange",
  "Moderate Profitability" = "gold",
  "High Profitability" = "darkgreen"
)

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      @page {
        size: A4 landscape;
        margin: 8mm;
      }

      body {
        font-size: 12px;
      }

      h2 {
        font-size: 24px;
        font-weight: bold;
        margin-top: 5px;
        margin-bottom: 8px;
      }

      h4 {
        font-size: 14px;
        font-weight: bold;
        margin-top: 8px;
        margin-bottom: 4px;
      }

      .well {
        padding: 8px;
        margin-bottom: 8px;
      }

      .form-group {
        margin-bottom: 6px;
      }

      .selectize-input {
        min-height: 28px;
        padding: 4px 8px;
        font-size: 12px;
      }

      .irs {
        height: 45px;
      }

      .kpi-box {
        padding: 6px;
        border-bottom: 1px solid #ddd;
        margin-bottom: 6px;
      }

      .kpi-title {
        font-size: 13px;
        font-weight: bold;
      }

      .kpi-value {
        font-size: 13px;
      }
    "))
  ),
  
  titlePanel("Market Potential vs Profitability Dashboard"),
  
  wellPanel(
    fluidRow(
      column(
        3,
        selectInput(
          "profitability",
          "Profitability Category",
          choices = c("All", levels(country_market$Profitability))
        )
      ),
      
      column(
        3,
        selectInput(
          "region",
          "Region",
          choices = c("All", sort(unique(country_market$Region)))
        )
      ),
      
      column(
        3,
        sliderInput(
          "gdp_range",
          "GDP per Capita Range",
          min = floor(min(country_market$GDP_per_Capita, na.rm = TRUE)),
          max = ceiling(max(country_market$GDP_per_Capita, na.rm = TRUE)),
          value = c(
            floor(min(country_market$GDP_per_Capita, na.rm = TRUE)),
            ceiling(max(country_market$GDP_per_Capita, na.rm = TRUE))
          )
        )
      ),
      
      column(
        3,
        sliderInput(
          "gap_range",
          "Relative Opportunity Gap Range",
          min = floor(min(country_market$OpportunityGap, na.rm = TRUE)),
          max = ceiling(max(country_market$OpportunityGap, na.rm = TRUE)),
          value = c(
            floor(min(country_market$OpportunityGap, na.rm = TRUE)),
            ceiling(max(country_market$OpportunityGap, na.rm = TRUE))
          ),
          step = 0.1
        )
      )
    )
  ),
  
  fluidRow(
    column(
      4,
      div(class = "kpi-box",
          div(class = "kpi-title", "Total Countries"),
          div(class = "kpi-value", textOutput("total_countries")))
    ),
    column(
      4,
      div(class = "kpi-box",
          div(class = "kpi-title", "Average Profit Margin"),
          div(class = "kpi-value", textOutput("avg_margin")))
    ),
    column(
      4,
      div(class = "kpi-box",
          div(class = "kpi-title", "Total Profit"),
          div(class = "kpi-value", textOutput("total_profit")))
    )
  ),
  
  fluidRow(
    column(
      12,
      h4("Market Potential vs Profitability Performance"),
      plotlyOutput("market_scatter", height = "350px")
    )
  ),
  
  fluidRow(
    column(
      6,
      h4("Top Underperforming and Overperforming Markets"),
      plotlyOutput("opportunity_gap", height = "245px")
    ),
    
    column(
      6,
      h4("Market Profitability Composition"),
      plotlyOutput("profitability_pie", height = "245px")
    )
  ),
  
  fluidRow(
    column(
      6,
      h4("Relative Opportunity Gap by Profitability Category"),
      plotlyOutput("profit_boxplot", height = "245px")
    ),
    
    column(
      6,
      h4("Correlation Heatmap of Market and Performance Indicators"),
      plotlyOutput("correlation_heatmap", height = "245px")
    )
  )
)

server <- function(input, output) {
  
  filtered_country <- reactive({
    data <- country_market
    
    if (input$profitability != "All") {
      data <- data %>% filter(Profitability == input$profitability)
    }
    
    if (input$region != "All") {
      data <- data %>% filter(Region == input$region)
    }
    
    data %>%
      filter(
        GDP_per_Capita >= input$gdp_range[1],
        GDP_per_Capita <= input$gdp_range[2],
        OpportunityGap >= input$gap_range[1],
        OpportunityGap <= input$gap_range[2]
      )
  })
  
  output$total_countries <- renderText({
    comma(nrow(filtered_country()))
  })
  
  output$avg_margin <- renderText({
    percent(mean(filtered_country()$ProfitMargin, na.rm = TRUE), accuracy = 0.1)
  })
  
  output$total_profit <- renderText({
    dollar(sum(filtered_country()$TotalProfit, na.rm = TRUE))
  })
  
  output$market_scatter <- renderPlotly({
    data <- filtered_country()
    
    median_gdp <- median(country_market$GDP_per_Capita, na.rm = TRUE)
    median_population <- median(country_market$Population, na.rm = TRUE)
    
    p <- ggplot(
      data,
      aes(
        x = GDP_per_Capita,
        y = Population,
        colour = Profitability,
        text = paste(
          "<b>", Country, "</b>",
          "<br>Region: ", Region,
          "<br>GDP per Capita: $", comma(round(GDP_per_Capita)),
          "<br>Population: ", comma(Population),
          "<br>Internet Usage: ", round(Internet_Usage, 1), "%",
          "<br>Total Sales: $", comma(round(TotalSales)),
          "<br>Total Profit: $", comma(round(TotalProfit)),
          "<br>Profit Margin: ", percent(ProfitMargin, accuracy = 0.1),
          "<br>Opportunity Gap Score: ", round(OpportunityGap_Raw, 2),
          "<br>Relative Opportunity Gap: ", round(OpportunityGap, 2)
        )
      )
    ) +
      geom_point(size = 4, alpha = 0.8) +
      geom_vline(
        xintercept = median_gdp,
        linetype = "dashed",
        colour = "grey40"
      ) +
      geom_hline(
        yintercept = median_population,
        linetype = "dashed",
        colour = "grey40"
      ) +
      scale_x_log10(labels = comma) +
      scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) +
      scale_colour_manual(values = profitability_colours) +
      labs(
        title = "",
        x = "GDP per Capita (USD, 2014)",
        y = "Population (2014)",
        colour = "Profitability"
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(
          orientation = "v",
          x = 1.02,
          y = 1
        ),
        margin = list(
          l = 55,
          r = 140,
          t = 10,
          b = 55
        )
      )
  })
  
  output$opportunity_gap <- renderPlotly({
    positive_gap <- filtered_country() %>%
      arrange(desc(OpportunityGap_Raw)) %>%
      slice_head(n = 5)
    
    negative_gap <- filtered_country() %>%
      arrange(OpportunityGap_Raw) %>%
      slice_head(n = 5)
    
    gap_data <- bind_rows(positive_gap, negative_gap) %>%
      distinct(Country, .keep_all = TRUE) %>%
      arrange(OpportunityGap_Raw)
    
    plot_ly(
      data = gap_data,
      x = ~OpportunityGap_Raw,
      y = ~reorder(Country, OpportunityGap_Raw),
      type = "bar",
      orientation = "h",
      hovertemplate = paste(
        "Country: %{y}",
        "<br>Opportunity Gap Score: %{x:.2f}",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = "",
        xaxis = list(
          title = "Opportunity Gap Score",
          zeroline = TRUE,
          zerolinewidth = 2,
          zerolinecolor = "black"
        ),
        yaxis = list(title = ""),
        margin = list(l = 90, r = 20, t = 10, b = 45)
      )
  })
  
  output$profitability_pie <- renderPlotly({
    pie_data <- filtered_country() %>%
      count(Profitability) %>%
      mutate(
        PieColour = profitability_colours[as.character(Profitability)]
      )
    
    plot_ly(
      data = pie_data,
      labels = ~Profitability,
      values = ~n,
      type = "pie",
      textinfo = "label+percent",
      marker = list(colors = ~PieColour),
      showlegend = FALSE,
      hovertemplate = paste(
        "Category: %{label}",
        "<br>Countries: %{value}",
        "<br>Share: %{percent}",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = "",
        margin = list(l = 20, r = 20, t = 10, b = 20)
      )
  })
  
  output$profit_boxplot <- renderPlotly({
    data <- filtered_country()
    
    plot_ly(
      data = data,
      x = ~Profitability,
      y = ~OpportunityGap,
      color = ~Profitability,
      colors = c("red", "orange", "gold", "darkgreen"),
      type = "box",
      boxpoints = "all",
      jitter = 0.3,
      pointpos = 0,
      hovertemplate = paste(
        "Profitability: %{x}",
        "<br>Relative Opportunity Gap: %{y:.2f}",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = "",
        showlegend = FALSE,
        xaxis = list(title = ""),
        yaxis = list(title = "Relative Opportunity Gap"),
        margin = list(l = 55, r = 20, t = 10, b = 55)
      )
  })
  
  output$correlation_heatmap <- renderPlotly({
    corr_data <- filtered_country() %>%
      select(
        GDP_per_Capita,
        Population,
        Internet_Usage,
        TotalSales,
        TotalProfit,
        ProfitMargin,
        OpportunityGap_Raw,
        OpportunityGap
      ) %>%
      rename(
        GDP = GDP_per_Capita,
        Population = Population,
        Internet = Internet_Usage,
        Sales = TotalSales,
        Profit = TotalProfit,
        Margin = ProfitMargin,
        Opportunity = OpportunityGap_Raw,
        RelativeGap = OpportunityGap
      )
    
    corr_matrix <- cor(
      corr_data,
      use = "pairwise.complete.obs"
    )
    
    plot_ly(
      x = colnames(corr_matrix),
      y = rownames(corr_matrix),
      z = round(corr_matrix, 2),
      type = "heatmap",
      colorscale = "RdBu",
      reversescale = TRUE,
      zmin = -1,
      zmax = 1,
      hovertemplate = paste(
        "X: %{x}",
        "<br>Y: %{y}",
        "<br>Correlation: %{z}",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = "",
        xaxis = list(title = ""),
        yaxis = list(title = ""),
        margin = list(l = 65, r = 20, t = 10, b = 55)
      )
  })
}

shinyApp(ui = ui, server = server)