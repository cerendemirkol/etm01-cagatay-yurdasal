library(shiny)
library(data.table)
library(sp)
library(shinydashboard)
library(data.table)
library(tidyverse)

# Prepare Data
setwd('./HW3')
source('read_matches.R')
setDT(matches)

# 2 page UI
ui <- 
  dashboardPage(
    dashboardHeader(title = "Visualization HW"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Match Information", tabName = "matches", icon = icon("dashboard")),
        menuItem("Team Statistics", tabName = "stats", icon = icon("th"))
      )
    ),
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(
          tabName = "matches",
          fluidRow(
            column(
              width = 12,
              selectInput("m_season","Seasons",c('All',seasons)),
              selectInput("m_team","Teams",c('All',teams)),
              dataTableOutput("table_matches")
            )
          )
        ),
        
        # Second tab content
        tabItem(
          tabName = "stats",
          fluidRow(
            column(
              width = 12,
              selectInput("s_season","Seasons",seasons),
              selectInput("s_team","Teams",teams),
              plotOutput("plot_stats")
            ) 
          )
        )
      )
    )
  )


# server side
server <- function(input, output){
  output$table_matches <-renderDataTable({
    inputm_season <- input$m_season
    inputm_team <- input$m_team
    if(inputm_season=='All'){inputm_season<-seasons}
    if(inputm_team=='All'){inputm_team<-teams}
    
    table_to_show=matches[(season==inputm_season)&((home==inputm_team)|(away==inputm_team)),c("home","away","score","date")]
    table_to_show
  })
  
  output$plot_stats <- renderPlot({
    inputs_season <- input$s_season
    inputs_team <- input$s_team
    calculate <- matches[(season==inputs_season)&((home==inputs_team)|(away==inputs_team)),c("score_home","score_away","home","away","season")]
    summary <- calculate[home==inputs_team, .(base="HOME",scored=mean(score_home),conceded=mean(score_away))]
    summary <- rbind(summary,calculate[away==inputs_team, .(base="AWAY",scored=mean(score_away),conceded=mean(score_home))])
    summary <- melt(summary, id.vars = "base", measure.vars = c("scored","conceded"))
    ggplot(summary) +
    geom_bar(aes(base,value,fill=variable),stat="identity",position="dodge") +
    xlab("base") +
    ylab("Average Goals") +
    theme_minimal() 
      
  }) 
}

# build shiny application
shinyApp(ui = ui, server = server)