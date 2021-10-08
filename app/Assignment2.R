library(shiny)
library(shinyjs)
library(DT)
library(shinydashboard)
library(data.table)
library(plotly)
ui <- fluidPage(
    titlePanel(h4(strong("ALL ABOUT CYLINDERS"), 
                  style = "font-size:30px;", align = "center",)),
    box(actionButton(inputId = "show", label = "Show", icon = icon("eye")), width = 0),
    column(4,box(dataTableOutput("showdata"))),
    column(12,plotlyOutput("plot"))
)


server <- function(input, output){
    
    observeEvent(input$show, 
                 {
                     cars <- data.frame(mtcars)
                     setDT(cars, keep.rownames = "Car_Names")[]
                     
                     by_cyl <- mtcars %>% group_by(cyl)
                     
                     cyl_summary <<- by_cyl %>%
                         summarise(
                             mean_disp = mean(disp),
                             min_disp = min(disp),
                             max_disp = max(disp),
                             sum_disp = sum(disp)
                         )
                     
                     output$showdata <- DT::renderDT({
                         DT::datatable(cyl_summary, 
                                       selection = "single") %>% formatStyle(
                             0,
                             target = 'row',
                             backgroundColor = styleEqual(c(1,2,3), c('red', 'yellow','green'))
                         )
                     })
                     
                     
                 })
    observeEvent(input$showdata_rows_selected,
                 {
                     if(length(input$showdata_rows_selected)>0)
                      {
                         df <- data.frame(cyl_summary[input$showdata_rows_selected,])
                         rownames(df) <- df$cyl
                         df$cyl <- NULL
                         df_transpose <- as.data.frame(t(as.matrix(df)))
                         df_final<- setDT(df_transpose, keep.rownames = "X")
                         df_final <- data.frame(df_final)
                         output$plot <- renderPlotly(
                             {
                                 fig <- plot_ly(x = df_final$X,
                                              y = df_final[,2],
                                              name = "Trend",
                                              type = "bar")
                                 
                             }
                         )
                         
                      }
                     
                 })
    
    
}


shinyApp(ui, server)

