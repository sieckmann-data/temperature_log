library(shiny)
library(ggplot2)
library(dbplyr)
library(dplyr)

ui <- page_sidebar(
  title = "GUI Temperature Sensor",
  sidebar = sidebar(
    helpText("Upload the thermosensor data below to assess relevant findings:"),
    fileInput("file", label = "File input:"),
    hr(),
    sliderInput("cutoff", label = "Temperature cutoff:", min = 10, max = 40, value = 27),
    hr(),
    dateRangeInput("dates", label = "Date range:", start = "2024-04-15"),
    hr(),
    sliderInput("hr_slider", label = "Hour range:", min = 0, max = 24, value = c(8, 18)),
    hr(),
    checkboxInput("checkbox", label = "Exclude weekends:", value = TRUE)
  ),
  page_fillable(
    layout_columns(
      card(card_header("Daily Average, Min and Max Temperatures:"), card_body(plotOutput("meanminmax_plot"))),
      card(card_header(textOutput("cutoff_txt")),card_body(plotOutput("donut"))),
      card(card_header(textOutput("min_cutoff")), card_body(h3(textOutput("minutes"), style="color:red; font-size:36px; text-align:center"))),
      card(card_header(textOutput("hrs_cutoff")), card_body(h3(textOutput("hours"), style="color:red; font-size:36px; text-align:center"))),
      col_widths = c(12, 6, 3, 3),
      row_heights = c(1, 1)
    )
  )
)


server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    
    raw_data <- read.csv(input$file$datapath)
 
    temperatures_df_new <-  raw_data %>% 
      mutate(DateTime = as.POSIXct(Zeitstempel.für.Abtastfrequenz.alle.1.min, format="%d-%m-%Y %H:%M")) %>%
      mutate(Day = as.Date(DateTime, format="%d"))
    
    temperatures_df_new$weekday <- weekdays(temperatures_df_new$Day)
    
    temperatures_df_new <- subset(temperatures_df_new, Day >= as.Date(input$dates[1]) & Day <= as.Date(input$dates[2]))
    
    temperatures_df_new <- subset(temperatures_df_new, as.integer(format(DateTime, "%H")) >= input$hr_slider[1])
    temperatures_df_new <- subset(temperatures_df_new, as.integer(format(DateTime, "%H")) <= input$hr_slider[2]-1)
    
    if(input$checkbox == TRUE){
      temperatures_df_new <- temperatures_df_new[temperatures_df_new$weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),]
      return(temperatures_df_new)}
      else{
        return(temperatures_df_new)
    }
  })
  
  daily_df <- reactive({
    Avg_temperatures <- data() %>%
      group_by(Day) %>%
      summarise(sum_T = mean(Temperatur_Celsius)) 
    
    Max_temperatures <- data() %>%
      group_by(Day) %>%
      summarise(sum_T = max(Temperatur_Celsius))
    
    Min_temperatures <- data() %>%
      group_by(Day) %>%
      summarise(sum_T = min(Temperatur_Celsius))
    
    daily_temp <- data.frame(Avg_temperatures)
    daily_temp$Max <- Max_temperatures$sum_T 
    daily_temp$Min <- Min_temperatures$sum_T 
    daily_temp$weekday <- weekdays(daily_temp$Day)
    colnames(daily_temp) <- c("Day", "mean_T", "max_T", "min_T", "weekday")
    
    return(daily_temp)
  })
  
  pie_data <- reactive({
    d_data <- data.frame(category=c("Above", "Below"), count=c(sum(daily_df()$max_T >= input$cutoff), sum(daily_df()$max_T < input$cutoff)))
    d_data$fraction <- d_data$count / sum(d_data$count)
    d_data$ymax <- cumsum(d_data$fraction)
    d_data$ymin <- c(0, head(d_data$ymax, n=-1))
    d_data$labelPosition <- (d_data$ymax + d_data$ymin) / 2
    d_data$label <- paste0(d_data$category, "\n days: ", d_data$count)
    
    return(d_data)
  })
  
  
    output$meanminmax_plot <- renderPlot({
      ggplot(daily_df(), aes(x=Day))+
        geom_line(aes(y=mean_T, colour = "mean_T")) + 
        geom_line(aes(y=min_T, colour = "min_T")) + 
        geom_line(aes(y=max_T, colour = "max_T")) + 
        scale_colour_manual("", breaks = c("max_T", "mean_T", "min_T"), values = c("red", "black", "blue")) +
        geom_hline(yintercept=input$cutoff, linetype=2, col = 'black')+
        theme_minimal() + 
        labs(x = "Date", y = "Temperature [°C]")
  })
  
    output$donut <- renderPlot({
      ggplot(pie_data(), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
        geom_rect() +
        geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
        scale_fill_manual(values = c("Above" = "firebrick1", "Below" = "deepskyblue")) +
        coord_polar(theta="y") +
        xlim(c(2, 4)) +
        theme_void() +
        theme(legend.position = "none") 
    })

    mins <- reactive(sum(data()$Temperatur_Celsius >= input$cutoff))
    hrs <- reactive(mins()/60)
    
    output$min_cutoff <- renderText(paste("Minutes above ", input$cutoff, "°C: ", sep = ""))
    output$minutes <- renderText(mins())
    output$hrs_cutoff <- renderText(paste("Hours above ", input$cutoff, "°C: ", sep = ""))
    output$hours <- renderText(format(round(hrs(), 0), nsmall = 0))
    
    output$cutoff_txt <- reactive(paste("Number of days above and below ", input$cutoff, "°C:", sep=""))
}

shinyApp(ui = ui, server = server)