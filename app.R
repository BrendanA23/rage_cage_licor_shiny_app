
library(shiny)
library(tidyverse)

#read in combined li-cor 6400xt data file, filtering out by unreasonable A values removes most of the crud
lc <- read.csv("rage_cage_licor - Sheet1.csv") %>%
  filter(include_data == "woo" & Photo < 20 & Photo > -1 & Ci < 4000 & Ci > -10) %>%
  select(-c(include_data,time_year,time_month,time_day,time_hour,time_minute,time_second,Obs,FTime, Ebal,
            FCnt, DCnt, PhiPS2, Adark , RedAbs, BlueAbs,BlueAbs,LeafAbs, Area, BLC_1, StmRat, 
            BLCond, BLCslope, BLCoffst, f_parin, f_parout, alphaK, Status, fda, Trans))
#filter out most of the crap variables
#names(lc)
cols <- names(lc)
ui <- fluidPage(
  #title
    titlePanel("Li-COR 6400xt Gas Exchange System File Viewer"),
    sidebarLayout(
      mainPanel(plotOutput("multi_var_Plot")),
        sidebarPanel(
          sliderInput("days_elapsed","days elapsed:",min = 0, max = 26, value = c(1, 26)),
          selectInput("x_variable", "X Variable", cols, selected = "days_elapsed"),
          selectInput("y_variable", "Y Variable", cols, selected = "Photo"),
          selectInput("color_variable", "Color By", cols, selected = "leaf_id"),
          selectInput("cohort", "Select Plant",choices = c("a1", "d1", "h1"),  selected = "a1"),
        )))      

server <- function(input, output){
  lc_out <- reactive({
    dplyr::filter(lc, plant_id == input$cohort)
  })
  
  output$multi_var_Plot <- renderPlot({
    ggplot(data = lc_out(), aes_string(x = input$x_variable, y = input$y_variable, col = input$color_variable)) +
      geom_point(size=4) +
      xlim(input$days_elapsed[1],input$days_elapsed[2]) +
      theme_classic()
  })
}
# Run!!!
shinyApp(ui = ui, server = server)
