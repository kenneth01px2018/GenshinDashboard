library(tidyverse)
library(shinydashboard)
library(DT)
library(ggimage)
library(ggthemes)
library(scales)
library(ggrepel)

colwidth = 6
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(
    title = "Genshin Pull Dashboard", 
    titleWidth = 300
  ),
  dashboardSidebar(
    width = 300,
    fileInput("file3", "Input Banner Pulls"),
    selectInput(
      "select3",
      "Select Rarity (Data Table)",
      list("All", "4 Star and Above", "5 Star"),
      selected = "5 Star"
    )
  ),
  dashboardBody(
    fluidRow(
      column(width = 6,
             box(title = "Quantify Your Simping", status = "warning", solidHeader = TRUE,
                 width = 12,
                 plotOutput("barplot", height = 500)),
             box(status = "warning", solidHeader = TRUE,
                 width = 12,
                 plotOutput("timeplot", height = 500))
      ),
      column(width = 6,
             valueBoxOutput("stat1", width = 12),
             valueBoxOutput("stat2", width = 12),
             valueBoxOutput("stat3", width = 12),
             valueBoxOutput("stat4", width = 12)
      ),
      box(status = "warning", solidHeader = TRUE, width = 6,
          dataTableOutput("table3"))
    )
  )
)

server <- function(input, output) {
  df3 <- reactive({
    if(is.null(input$file3))     return(NULL) 
    inFile <- input$file3
    df <- read.csv(inFile$datapath) %>%
      rename(pity = `within.pity`) %>% 
      select(-remark)
    if (input$select3 == "4 Star and Above") {
      df %>%
        filter(rarity >= 4)
    } else {
      if (input$select3 == "5 Star") {
        df %>%
          filter(rarity >= 5)
      } else {
        df
      }
    }
  })
  df_stat <- reactive({
    if(is.null(input$file3))     return(NULL) 
    inFile <- input$file3
    read.csv(inFile$datapath) %>%
      rename(pity = `within.pity`) %>% 
      select(-remark)
  })
  output$table3 <- renderDataTable({
    if (is.null(df3()))    return(NULL)
    df3()
  })
  output$barplot <- renderPlot({
    if (is.null(df3())) {
      return(NULL)
    } else {
      df_stat() %>%
        filter(rarity >= 5) %>%
        group_by(name) %>%
        summarize(Count = sum(pity)) %>%
        rename(Character = name) %>%
        ggplot(aes(x = reorder(Character, Count), y = Count, fill = Count)) + geom_bar(stat = "identity") +
        scale_fill_steps(low = "wheat2", high = "orange3") + 
        geom_text(aes(label = Count),  hjust = 1.5, color = "white", size = 5) + coord_flip() +
        labs(subtitle = "Regardless of 50/50 result",
             title = "Characters by Descending Pull Count") + theme_fivethirtyeight() +
        theme(
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.position = "none"
        )
    }
  })
  output$timeplot <- renderPlot({
    if (is.null(df3())) {
      return(NULL)
    } else {
      df_stat() %>%
        mutate(time = format(as.Date(time), "%Y-%m")) %>%
        group_by(time) %>%
        summarize(count = n()) %>%
        ggplot(aes(x = time, y = count, group = 1)) + 
        geom_segment(aes(x = time, xend = time, y = 0, yend = count), lwd = 3.5, color = "grey") +
        geom_point(size = 13, color = "orange2", alpha = 1) + 
        geom_text(aes(label = count),  color = "white", size = 5.5) + 
        theme_fivethirtyeight() + 
        labs(
          title = "Total Pull Count Per Month",
          subtitle = "Track your simping monthly",
        ) + 
        theme(
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12)
        )
    }
  })
  output$stat1 <- renderValueBox( {
    if (is.null(df_stat())) {
      pity <- 0
    } else {
      df <- df_stat()
      last_pull <- df %>%
        tail(1) %>%
        select(rarity, pity)
      if (last_pull$rarity == 5) {
        pity <- 0
      } else {
        pity <- last_pull$pity
      }
    }
    valueBox(
      as.character(pity), "Current Pity Count (Pulls Since Last 5 Star)",
      icon = icon("hourglass", lib = "glyphicon"), color = "yellow"
    )
  })
  output$stat2 <- renderValueBox( {
    if (is.null(df_stat())) {
      avg_val <- 0
    } else {
      df3 <- df_stat()
      avg_val <- df3 %>%
        filter(rarity >= 5) %>%
        summarize(mean(pity)) %>%
        pull() %>% round(2) %>% as.character()
    }
    valueBox(
      avg_val, "Average Pulls Per 5 Star",
      icon = icon("stats", lib = "glyphicon"), color = "yellow"
    )
  })
  output$stat3 <- renderValueBox( {
    if (is.null(df_stat())) {
      luck_val <- 0
    } else {
      df3 <- df_stat()
      luck_val <- df3 %>%
        filter(rarity >= 5) %>%
        summarize(mean(pity < 70) * 100) %>%
        pull() %>% round(1) %>% as.character() %>% paste0("%")
    }
    valueBox(
      luck_val, "Percent of 5 Stars Pulled Before Soft Pity (<70 Pulls)",
      icon = icon("sunglasses", lib = "glyphicon"), color = "yellow"
    )
  })
  output$stat4 <- renderValueBox( {
    if (is.null(df_stat())) {
      max_count <- 0
      max_date <- "TBD"
    } else {
      df3 <- df_stat()
      pull_counts <- as.Date(df3$time) %>% 
        table() 
      max_count <- max(pull_counts)
      pull_dates <- as.Date(df3$time) %>% 
        table() %>% names()
      max_date <- pull_dates[pull_counts == max_count] %>% as.character()
      max_count <- max_count %>% as.character()
    }
    valueBox(
      max_count, paste0(c("Pulls on ", max_date, " (Most Pulls in a Day)"), collapse = ""),
      icon = icon("jpy", lib = "glyphicon"), color = "yellow"
    )
  })
}

shinyApp(ui, server)
