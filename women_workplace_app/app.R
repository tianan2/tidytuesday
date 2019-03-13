library(shiny)
library(tidyverse)
options(scipen=999)

## data load in
jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Gender Disparity Explorer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("major_category",
                     "Major occupation:",
                     choices = unique(jobs_gender$major_category))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("jobs_scatter")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$jobs_scatter <- renderPlotly({
     q <- jobs_gender %>%
       filter(year == 2016, 
              total_workers >= 2000) %>% 
       filter(major_category == input$major_category) %>% 
       arrange(desc(wage_percent_of_male)) %>% 
       ggplot(aes(workers_female / total_workers, 
                  total_earnings_female / total_earnings_male,
                  size = total_workers,
                  label = occupation)) + 
       geom_point() + 
       scale_size_continuous(range = c(1, 10)) +
       labs(size = "Total # of workers", 
            x = "% of workers reported as female", 
            y = "% of female median salary out of male") +
       scale_x_continuous(labels = scales::percent_format()) +
       scale_y_continuous(labels = scales::percent_format())
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

