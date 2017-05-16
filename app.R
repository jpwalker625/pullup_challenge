#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("pullup_challenge.R")


# Define UI for application that draws a histogram
ui <- navbarPage(title = "Amyris Pullup Challenge",
   # tab1
   tabPanel("Overall Results",
               tabsetPanel(
                 tabPanel("Individual", plotOutput(outputId = "plot1")),
                 tabPanel("Group", plotOutput(outputId = "plot2"))
               ) # end tabsetPanel
   ), # end tabPanel 1
                   
   
   # tab2
   tabPanel("Individual Results",
            sidebarLayout(
              sidebarPanel(
                selectInput(inputId = "person",
                     label = "Select Challenger", 
                     choices = levels(pullups$name))
                ), #end of sidebar Panel
              mainPanel(
                tableOutput(outputId = "summary_table"),
                plotOutput(outputId = "plot3")
              ) #end of Main Panel
            ) # end of sidebar Layout
   ), #end of Tab Panel 2
   
   # tab3
   tabPanel("Group Results",
            sidebarLayout(
              sidebarPanel(
                selectInput(inputId = "group",
                            label = "Select Group", 
                            choices = levels(group_total_by_day$group))
              ), #end of sidebar Panel
            mainPanel(
              plotOutput(outputId = "plot4")
            ) #end of mainPanel
   ) # end of sidebar layout
   )# end of tab panel 3
) # end of Navbar Page

########### Server

# Define server logic required to draw a histogram
server <- function(input, output) {
 
  # overall_results
  output$plot1 <- renderPlot({
      ggplot(data = sum_per_person, aes(x = fct_reorder(name, total), y = total, fill = total))+
      geom_bar(stat = "identity")+
      coord_flip() +
      scale_fill_gradientn(colours = viridis(25, begin = .25,end = .9, direction = -1), guide = FALSE) +
      labs(x = "Name",  y = "Pullups", title = "Total Pullups for April's Pullup Challenge") +
      geom_label(aes(label = total)) +
      theme(text = element_text(size = 16))
  }, width = 600, height = 800)
  
  output$plot2 <- renderPlot({
    ggplot(data = group_total, aes(x = fct_reorder(group, total), y = total, fill = total))+
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_y_continuous(breaks = c(0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000)) +
      scale_fill_gradientn(colours = viridis(9, begin = .25, end = .9,direction = -1), guide = FALSE) +
      labs(title = "Total Pullups by Group", x = "Group", y = "Pullups") +
      geom_label(aes(label = total)) +
      theme(text = element_text(size = 16))
  }, width = 600, height = 600)
  
  # individual results  
  by_person <- reactive({
    pullups %>% filter(name == input$person)
  })
  
  total <- reactive({
    by_person() %>%
    summarise(total_pullups = sum(pullups))
  })
  
  summary <- reactive({
    by_person() %>%
    select(`starting max set` = initial_max_set, `final max set ` = final_max_set) %>%
      distinct() %>%
      mutate(`total pullups` = total()$total_pullups)
  })
  
  output$summary_table <- renderTable(summary(), bordered = TRUE, align = 'c', 
                                      caption = "Summary Stats", caption.placement = getOption("xtable.caption.placement", "top")) 
  
  
  output$plot3 <- renderPlot({
    ggplot(data = by_person(), aes(x = date, y = pullups)) +
      geom_bar(stat= "identity", color = "grey", fill = 'steelblue') +
      scale_x_date(date_labels = "%b-%d", date_breaks = "day") +
      labs(title = "Pullups per Day", subtitle = "weekends excluded", x = "Date", y = 'Pullups') +
      theme_grey() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11), text = element_text(size = 16))
  }, width = 600, height = 600)
  
  # Group Results
  
  by_group <- reactive({
    group_total_by_day %>%
      filter(group == input$group)
    
  })
  
  output$plot4 <- renderPlot({
    ggplot(data = by_group(), aes(x = date, y = total)) +
      geom_bar(stat = "identity", color ="grey", fill = 'olivedrab3') +
      geom_point(aes(y = average_per_person), shape = 8) +
      scale_x_date(date_labels = "%b-%d", date_breaks = "day") +
      labs(title = "Pullups per Day by Group", subtitle = "weekends excluded, * denotes average pullups/person within specified group", x = "Date", y = 'Pullups') +
      theme_grey() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11), text = element_text(size = 16))
  }, width = 600, height = 600)
}

# Run the application 
shinyApp(ui = ui, server = server)

