#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(xlsx)
library(readxl)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(xts)


# getwd()
# setwd("/Users/cvoisin/Repos/Rproject/rproject/project")
# excel <- read_excel("./surveydataece.xlsx", 1)
# df <- data.frame(excel)
# head(df)
# 
# csv <- read.csv("./logs.csv", sep = ";")
# df_logs <- data.frame(csv)
# head(df_logs)



# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Lighter data and customer survey analysis"),
  fileInput('file1', 'Upload survey data',
            accept = c(".xlsx")),
  fileInput('file2', 'Upload logs data',
            accept = c(".csv")),
  tabsetPanel(
    tabPanel('All users mode',
             navlistPanel(
               tabPanel(
                 "Information",
                 h3("Total number of cigarettes saved"),
                 verbatimTextOutput("all_cigarettes_saved"),
                 h3("Total number of cigarettes saved"),
                 verbatimTextOutput("all_money_saved"),
                 h3("Average amount of cigarettes saved"),
                 verbatimTextOutput("mean_cigarettes_saved"),
                 h3("Average amount of money saved"),
                 verbatimTextOutput("mean_money_saved")
               ),
               tabPanel(
                 "Classic",
                 h4("Mean and std of cigarette consumption per weekday"),
                 verbatimTextOutput("all_mean_std"),
                 h4("Average progress of all users"),
                 verbatimTextOutput("mean_progress"),
                 h4("Cigarettes per weekday per time slots"),
                 plotOutput("all_cigarettes_weekday"),
                 h4("Average rate of progress of all users"),
                 plotOutput("mean_progress_rate")
               ),
               tabPanel(
                 "Engagement",
                 h4("Engagement over all period"),
                 plotOutput("all_engagement")
               )
             )
             ),
    tabPanel(
      'Single user mode',
      selectInput("user_name", "Please choose a user:" , choices = NULL),
      navlistPanel(
        tabPanel(
          "Information",
          h4("Age & Basic Information"),
          verbatimTextOutput("age"),
          h4("Age category"),
          verbatimTextOutput("age_category"),
          h4("Money saved"),
          verbatimTextOutput("money_saved"),
          h4("Cigarettes saved"),
          verbatimTextOutput("cigarettes_saved"),
          h4("Overall progress"),
          verbatimTextOutput("overall_progress"),
          h4("Overall progress category"),
          verbatimTextOutput("overall_progress_cat"),
          h4("Overall engagement"),
          verbatimTextOutput("overall_engagement"),
          h4("Best rate of progress"),
          verbatimTextOutput("max_progress"),
          h4("Mean of consumed cigarettes per day"),
          verbatimTextOutput("mean_consumed"),
          h4("Mean of consumed cigarettes in weekdays"),
          verbatimTextOutput("mean_consumed_weekdays"),
          h4("Mean of consumed cigarettes on weekends"),
          verbatimTextOutput("mean_consumed_weekends"),
          h4("Most smoking intensity slot"),
          verbatimTextOutput("worst_day")
        ),
        tabPanel(
          "Classic",
          h4("Cigarette consumption per weekday"),
          plotOutput("consumed_weekdays"),
          h4("Cigarette consumption per weekend day"),
          plotOutput("consumed_weekends"),
          h4("Cigarettes consumption in last seven days"),
          plotOutput("last_7"),
          h4("Mean and std cigarette consumption per weekday v1"),
          plotOutput("mean_std_v1"),
          h4("Mean and std cigarette consumption per weekday v2"),
          verbatimTextOutput("mean_std_v2"),
          h4("Mean and std cigarette consumption per weekday v3"),
          verbatimTextOutput("mean_std_v3"),
          h4("Progress over all period"),
          plotOutput("weekly_progress"),
          h4("Rate of progress"),
          plotOutput("rate_progress")
          
        ),
        tabPanel(
          "Week",
          h4("Comparison of cigarettes consumption between weeks"),
          plotOutput("weekly_comparison")
        ),
        tabPanel(
          "Engagement",
          h4('Enagement per day'),
          plotOutput("daily_engagement"),
          h4("Engagement per week"),
          plotOutput("weekly_engagement")
        ),
        tabPanel(
          "All days",
          h4('Cigarettes consumption over all period'),
          plotOutput("daily_usage"),
          h4('Mode usage over all period'),
          plotOutput("mode_usage_behaviour"),
          plotOutput("mode_usage_ontime"),
          plotOutput("mode_usage_cheated"),
          plotOutput("mode_usage_skipped"),
          plotOutput("mode_usage_autoskipped"),
          plotOutput("mode_usage_friend"),
          plotOutput("mode_usage_snoozed")
        )
        
      )
      
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #######################################################################################################################################################
  #################################################################### Single user ######################################################################
  
  ##################### Information tab ##############################
  observe({
    inFile <- input$file1
    req(inFile)
    excel <- read_excel(inFile$datapath, 1)
    df <- data.frame(excel)
    updateSelectInput(session, "user_name", choices = df$Name)
  })
  
  mySurveyData <- reactive({
    inFile <- input$file1
    req(inFile)
    excel <- read_excel(inFile$datapath, 1)
    df <- data.frame(excel)
    return (df)
  })
  myLogsData <- reactive({
    inFile <- input$file2
    req(inFile)
    csv <- read.csv(inFile$datapath, sep = ";")
    df_logs <- data.frame(csv)
    return(df_logs)
  })
  #Number of cigs smoked in behaviour week
  n <- reactive({
    df_logs <- myLogsData()
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    n <- nrow(newdata_xts[newdata_xts$Type == "Behaviour"])
    n
  })
  WeeklyConsumptionData <- reactive({
    df_logs <- myLogsData()
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-
      as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only smoked
    newdata_xts_smoked <-
      newdata_xts[newdata_xts$Type == "On time" | newdata_xts$Type == "Cheated"]
    #Number of cigs smoked in behaviour week
    n <- nrow(newdata_xts[newdata_xts$Type == "Behaviour"])
    f <- function(week) {
      return (nrow(week))
    }
    index <- endpoints(newdata_xts_smoked, on = "weeks")
    weekly_progress <- period.apply(newdata_xts_smoked, index, f)
    return(weekly_progress)
  })
  WeeklyProgressData <- reactive ({
    weekly_progress <- WeeklyConsumptionData()
    f2 <- function(week) {
      avg <- mean(week[1, 1], week[2, 1], week[1, 1])
      return ((avg - week[4, 1]) / avg)
    }
    n <- n()
    f3 <- function(week) {
      return ((n - week) / n)
    }
    #Progress week 1 to 3
    progress_2 <- lapply(weekly_progress[1:3, 1], f3)
    progress_2 <- do.call(rbind, progress_2)
    #Progress week 3 and on
    progress_1 <- rollapply(weekly_progress[, 1], width = 4, f2)
    progress_1[1:3, 1] <- progress_2[1:3, 1]
    return(progress_1)
  })
  # Age
  output$age <- renderPrint({
    df <- mySurveyData()
    newdata <- subset(df, df$Name == input$user_name)
    print(paste("Age: ", newdata$Age))
    print(paste("Gender: ", newdata$Gender))
    print(paste("Education: ", newdata$Education))
    print(paste("Family status: ", newdata$Family.status))
  })
  # Age category
  output$age_category <- renderPrint({
    df <- mySurveyData()
    newdata <- subset(df, df$Name == input$user_name)
    if (newdata$Age < 30)
      print(paste("Young"))
    if (newdata$Age >= 30 && newdata$Age < 50)
      print(paste("Young"))
    if (newdata$Age >= 50)
      print(paste("Old"))
  })
  
  # Money saved
  ## Modify
  output$money_saved <- renderPrint({
    df_logs <- myLogsData()
    #newdata <- subset(df_logs, df_logs$User == "Renaud Courbis")
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    if (nrow(newdata) != 0) {
      #Transforming to xts
      newdata$Time <-
        as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
      newdata_xts <-
        as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
      #Keeping only smoked
      newdata_xts_smoked <-
        newdata_xts[newdata_xts$Type == "On time" |
                      newdata_xts$Type == "Cheated"]
      #Adjusting periodicity to week
      newdata_weeks <- split(newdata_xts_smoked$Type, f = "weeks")
      #Number of cigs smoked in behaviour week
      n <- nrow(newdata_xts[newdata_xts$Type == "Behaviour"])
      #Removing behaviour week
      newdata_normal_weeks <- newdata_weeks
      f <- function(week) {
        return (n - nrow(week))
      }
      newdata_weekly_savings <- lapply(newdata_normal_weeks, f)
      weekly_saving <- do.call(rbind, newdata_weekly_savings)
      fsum <- cumsum(weekly_saving[, 1])
      print(paste(tail(fsum, n = 1), " € saved"))
    }
    else
      print(paste("0 € saved"))
  })
  # Cigarettes_saved
  output$cigarettes_saved <- renderPrint({
    df_logs <- myLogsData()
    #newdata <- subset(df_logs, df_logs$User == "Renaud Courbis")
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    if (nrow(newdata) != 0) {
      #Transforming to xts
      newdata$Time <-
        as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
      newdata_xts <-
        as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
      #Keeping only smoked
      newdata_xts_smoked <-
        newdata_xts[newdata_xts$Type == "On time" |
                      newdata_xts$Type == "Cheated"]
      #Adjusting periodicity to week
      newdata_weeks <- split(newdata_xts_smoked$Type, f = "weeks")
      #Number of cigs smoked in behaviour week
      n <- nrow(newdata_xts[newdata_xts$Type == "Behaviour"])
      #Removing behaviour week
      newdata_normal_weeks <- newdata_weeks
      f <- function(week) {
        return (n - nrow(week))
      }
      newdata_weekly_savings <- lapply(newdata_normal_weeks, f)
      weekly_saving <- do.call(rbind, newdata_weekly_savings)
      fsum <- cumsum(weekly_saving[, 1])
      print(paste(tail(fsum, n = 1), " Cigarettes saved"))
    }
    else
      print(paste("0 Cigarettes saved"))
  })
  # Overall progress
  output$overall_progress<-renderPrint({
    progress_1 <- WeeklyProgressData()
    prg<-mean(progress_1)
    print(paste("Overall progress =", prg))
  })
  # Overall Progress Category
  output$overall_progress_cat<-renderPrint({
    progress_1 <- WeeklyProgressData()
    prg<-mean(progress_1)
    if(prg<=0.2) print("Low overall progress")
    else if(prg>=0.5) print("High overall progress")
    else print("Medium overall progres")
  })
  # Overall Engagement
  output$overall_engagement<-renderPrint({
    df_logs <- myLogsData()
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only engagement data
    newdata_engaged <-
      newdata_xts[newdata_xts$Type == "Skipped" | newdata_xts$Type == "On time" | newdata_xts$Type == "Auto skipped" | newdata_xts$Type == "Snoozed"]
    f<-function(day){
      1-(nrow(day[day$Type=="Auto skipped"])/nrow(day))
    }
    index <- endpoints(newdata_engaged, on = "weeks")
    weekly_engagement <- period.apply(newdata_engaged, index, f)
    eng<-mean(weekly_engagement)
    #print("Overall engagement: ", eng)
    print(paste("Overall engagement: ", eng))
  })
  #Best Rate of Progress
  output$max_progress<-renderPrint({
    progress_1 <- WeeklyProgressData()
    max_prg<-which.max(progress_1)
    print(paste("Max progress week was week n°",max_prg))
  })
  # Mean of consumed cigarettes per day
  output$mean_consumed <- renderPrint({
    df_logs <- myLogsData()
    #newdata <- subset(df_logs, df_logs$User == "Renaud Courbis")
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    if (nrow(newdata) != 0) {
      #Transforming to xts
      newdata$Time <-
        as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
      newdata_xts <-
        as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
      #Keeping only smoked
      newdata_xts_smoked <-
        newdata_xts[newdata_xts$Type == "On time" |
                      newdata_xts$Type == "Cheated"]
      #Adjusting periodicity to days
      newdata_days <- split(newdata_xts_smoked$Type, f = "days")
      summary(newdata_days)
      f <- function(day) {
        return (nrow(day))
      }
      newdata_daily_sum <- lapply(newdata_days, f)
      daily_avg <- do.call(rbind, newdata_daily_sum)
      
      print(paste(
        "Smoked ",
        mean(daily_avg[, 1]),
        " cigarettes per day on average"
      ))
    }
    else
      print(paste("0 cigarettes per day on average"))
  })
  # Mean of consumed cigarettes in weekdays
  output$mean_consumed_weekdays <- renderPrint({
    df_logs <- myLogsData()
    #newdata <- subset(df_logs, df_logs$User == "Renaud Courbis")
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    if (nrow(newdata) != 0) {
      #Transforming to xts
      newdata$Time <-
        as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
      newdata_xts <-
        as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
      #Keeping only smoked
      newdata_xts_smoked <-
        newdata_xts[newdata_xts$Type == "On time" |
                      newdata_xts$Type == "Cheated"]
      #Indexing weekdays
      weekdays <-
        which(
          .indexwday(newdata_xts_smoked) == 1 |
            .indexwday(newdata_xts_smoked) == 2 |
            .indexwday(newdata_xts_smoked) == 3 |
            .indexwday(newdata_xts_smoked) == 4 |
            .indexwday(newdata_xts_smoked) == 5
        )
      smoked_weekdays <- newdata_xts[weekdays]
      #Adjusting periodicity to days
      newdata_days <- split(smoked_weekdays$Type, f = "days")
      head(newdata_days)
      f <- function(day) {
        return (nrow(day))
      }
      newdata_daily_sum <- lapply(newdata_days, f)
      daily_avg <- do.call(rbind, newdata_daily_sum)
      
      print(paste(
        "Smoked ",
        mean(daily_avg[, 1]),
        " cigarettes per day on the average weekday"
      ))
    }
    else
      print(paste("0 cigarettes per day on the average weekday"))
  })
  # Mean of consumed cigarettes on weekend
  output$mean_consumed_weekends <- renderPrint({
    df_logs <- myLogsData()
    #newdata <- subset(df_logs, df_logs$User == "Renaud Courbis")
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    if (nrow(newdata) != 0) {
      #Transforming to xts
      newdata$Time <-
        as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
      newdata_xts <-
        as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
      #Keeping only smoked
      newdata_xts_smoked <-
        newdata_xts[newdata_xts$Type == "On time" |
                      newdata_xts$Type == "Cheated"]
      #Indexing weekdays
      weekdays <-
        which(.indexwday(newdata_xts_smoked) == 0 |
                .indexwday(newdata_xts_smoked) == 6)
      smoked_weekdays <- newdata_xts[weekdays]
      #Adjusting periodicity to days
      newdata_days <- split(smoked_weekdays$Type, f = "days")
      head(newdata_days)
      f <- function(day) {
        return (nrow(day))
      }
      newdata_daily_sum <- lapply(newdata_days, f)
      daily_avg <- do.call(rbind, newdata_daily_sum)
      
      print(paste(
        "Smoked ",
        mean(daily_avg[, 1]),
        " cigarettes per day on the average weekend day"
      ))
    }
    else
      print(paste("0 cigarettes per day on the average weekend day"))
  })
  # Most Smoking Intensity Slot
  output$worst_day <- renderPrint({
    df_logs <- myLogsData()
    #newdata <- subset(df_logs, df_logs$User == "Renaud Courbis")
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    if (nrow(newdata) != 0) {
      #Transforming to xts
      newdata$Time <-
        as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
      newdata_xts <-
        as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
      #Keeping only smoked
      newdata_xts_smoked <-
        newdata_xts[newdata_xts$Type == "Behaviour" |
                      newdata_xts$Type == "On time" |
                      newdata_xts$Type == "Cheated"]
      #Indexing weekdays
      weekdays <-
        which(.indexwday(newdata_xts_smoked) == 0 |
                .indexwday(newdata_xts_smoked) == 6)
      smoked_weekdays <- newdata_xts[weekdays]
      #Adjusting periodicity to days
      newdata_days <- split(smoked_weekdays$Type, f = "days")
      head(newdata_days)
      f <- function(day) {
        return (nrow(day))
      }
      newdata_daily_sum <- lapply(newdata_days, f)
      daily_avg <- do.call(rbind, newdata_daily_sum)
      print(
        paste(
          "Smoked",
          max(daily_avg[, 1]),
          "cigarettes on the",
          which.max(daily_avg[, 1]),
          "st/nd/rd/th day of using the iLighter"
        )
      )
    }
  })
  ##################### Classics tab ##############################
  # Consumed cigarettes weekdays
  output$consumed_weekdays <- renderPlot({
    df_logs <- myLogsData()
    #newdata <- subset(df_logs, df_logs$User == "Renaud Courbis")
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    if (nrow(newdata) != 0) {
      #Transforming to xts
      newdata$Time <-
        as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
      newdata_xts <-
        as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
      #Keeping only smoked
      newdata_xts_smoked <-
        newdata_xts[newdata_xts$Type == "On time" |
                      newdata_xts$Type == "Cheated"]
      #Indexing weekdays
      weekdays <-
        which(.indexwday(newdata_xts_smoked) == 0 |
                .indexwday(newdata_xts_smoked) == 6)
      smoked_weekdays <- newdata_xts[weekdays]
      f <- function(day) {
        nrow(day)
      }
      index <- endpoints(smoked_weekdays, on = "days")
      daily_sum <- period.apply(smoked_weekdays, index, f)
      colnames(daily_sum) <- c("Smoked")
      plot.xts(daily_sum, type = "h")
    }
  })
  output$consumed_weekends <- renderPlot({
    df_logs <- myLogsData()
    #newdata <- subset(df_logs, df_logs$User == "Renaud Courbis")
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    if (nrow(newdata) != 0) {
      #Transforming to xts
      newdata$Time <-
        as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
      newdata_xts <-
        as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
      #Keeping only smoked
      newdata_xts_smoked <-
        newdata_xts[newdata_xts$Type == "On time" |
                      newdata_xts$Type == "Cheated"]
      #Indexing weekdays
      weekdays <-
        which(
          .indexwday(newdata_xts_smoked) == 1 |
            .indexwday(newdata_xts_smoked) == 2 |
            .indexwday(newdata_xts_smoked) == 3 |
            .indexwday(newdata_xts_smoked) == 4 |
            .indexwday(newdata_xts_smoked) == 5
        )
      smoked_weekdays <- newdata_xts[weekdays]
      f <- function(day) {
        nrow(day)
      }
      index <- endpoints(smoked_weekdays, on = "days")
      daily_sum <- period.apply(smoked_weekdays, index, f)
      colnames(daily_sum) <- c("Smoked")
      plot.xts(daily_sum$Smoked, type = "h")
    }
  })
  output$last_7 <- renderPlot({
    df_logs <- myLogsData()
    #newdata <- subset(df_logs, df_logs$User == "Renaud Courbis")
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    if (nrow(newdata) != 0) {
      #Transforming to xts
      newdata$Time <-
        as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
      newdata_xts <-
        as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
      #Keeping only smoked
      newdata_xts_smoked <-
        newdata_xts[newdata_xts$Type == "On time" |
                      newdata_xts$Type == "Cheated"]
      f <- function(day) {
        nrow(day)
      }
      index <- endpoints(newdata_xts_smoked, on = "days")
      daily_sum <- period.apply(newdata_xts_smoked, index, f)
      colnames(daily_sum) <- c("Smoked")
      plot.xts(tail(daily_sum$Smoked, n = 7), type = "h")
    }
  })
  #Literall mean of the 3 per day
  output$mean_std_v1 <- renderPlot({
    df_logs <- myLogsData()
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-
      as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only smoked
    newdata_xts_smoked <-
      newdata_xts[newdata_xts$Type == "Behaviour" | newdata_xts$Type == "On time" | newdata_xts$Type == "Cheated"]
    f <- function(day) {
      ret<-mean(nrow(day[day$Type=="Cheated"]),nrow(day[day$Type=="On time"]),nrow(day[day$Type=="Behaviour"]))
      return (ret)
    }
    weekdays <-
      which(
        .indexwday(newdata_xts_smoked) == 1 |
          .indexwday(newdata_xts_smoked) == 2 |
          .indexwday(newdata_xts_smoked) == 3 |
          .indexwday(newdata_xts_smoked) == 4 |
          .indexwday(newdata_xts_smoked) == 5
      )
    smoked_weekdays <- newdata_xts[weekdays]
    index <- endpoints(smoked_weekdays, on = "days")
    mean_weekday <- period.apply(smoked_weekdays, index, f)
    plot.xts(mean_weekday)
    #print(paste("Mean:", round(mean(daily_consumption), digits=2),"Std:",round(sd(daily_consumption), digits = 2)))
  })
  #Literal interpretation this time mean of mean and std of mean
  output$mean_std_v2<-renderPrint({
    df_logs <- myLogsData()
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-
      as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only smoked
    newdata_xts_smoked <-
      newdata_xts[newdata_xts$Type == "Behaviour" | newdata_xts$Type == "On time" | newdata_xts$Type == "Cheated"]
    f <- function(day) {
      ret<-mean(nrow(day[day$Type=="Cheated"]),nrow(day[day$Type=="On time"]),nrow(day[day$Type=="Behaviour"]))
      return (ret)
    }
    weekdays <-
      which(
        .indexwday(newdata_xts_smoked) == 1 |
          .indexwday(newdata_xts_smoked) == 2 |
          .indexwday(newdata_xts_smoked) == 3 |
          .indexwday(newdata_xts_smoked) == 4 |
          .indexwday(newdata_xts_smoked) == 5
      )
    smoked_weekdays <- newdata_xts[weekdays]
    index <- endpoints(smoked_weekdays, on = "days")
    mean_weekday <- period.apply(smoked_weekdays, index, f)
    print(paste("Mean:", round(mean(mean_weekday), digits=2),"Std:",round(sd(mean_weekday), digits = 2)))
  })
  #Simple mean and std of all 
  output$mean_std_v3<-renderPrint({
    df_logs <- myLogsData()
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-
      as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only smoked
    newdata_xts_smoked <-
      newdata_xts[newdata_xts$Type == "Behaviour" | newdata_xts$Type == "On time" | newdata_xts$Type == "Cheated"]
    f <- function(day) {
      ret<-nrow(day)
      return (ret)
    }
    weekdays <-
      which(
        .indexwday(newdata_xts_smoked) == 1 |
          .indexwday(newdata_xts_smoked) == 2 |
          .indexwday(newdata_xts_smoked) == 3 |
          .indexwday(newdata_xts_smoked) == 4 |
          .indexwday(newdata_xts_smoked) == 5
      )
    smoked_weekdays <- newdata_xts[weekdays]
    index <- endpoints(smoked_weekdays, on = "days")
    mean_weekday <- period.apply(smoked_weekdays, index, f)
    plot.xts(mean_weekday)
    print(paste("Mean:", round(mean(mean_weekday), digits=2),"Std:",round(sd(mean_weekday), digits = 2)))
  })
  #Weekly progress
  output$weekly_progress <- renderPlot({
    progress_1 <- WeeklyProgressData()
    plot.xts(progress_1)
  })
  # Rate of progress
  output$rate_progress <- renderPlot({
    progress_1 <- WeeklyProgressData()
    #weekly_progress<-progress_1
    #progress_1
    f4 <- function(week) {
      if (week[2, 1] == 0) {
        res <- (as.numeric(week[3, 1]) - as.numeric(week[1, 1])) / abs(week[2, 1])
      }
      res <-
        ((as.numeric(week[3, 1]) - as.numeric(week[2, 1])) / abs(week[2, 1]))
      return (res)
    }
    rate_progress <- rollapply(progress_1[, 1], width = 4, f4)
    plot.xts(rate_progress)
  })
  ##################### Weeks tab ##############################
  #Comparison of cigarettes consumption between weeks
  output$weekly_comparison <-renderPlot({
    df_logs <- myLogsData()
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-
      as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only smoked
    newdata_xts_smoked <-
      newdata_xts[newdata_xts$Type == "Behaviour" | newdata_xts$Type == "On time" | newdata_xts$Type == "Cheated"]
    f <- function(day) {
      return (nrow(day))
    }
    f2 <- function(week) {
      return (mean(week))
    }
    index <- endpoints(newdata_xts_smoked, on = "days")
    daily_consumption <- period.apply(newdata_xts_smoked, index, f)
    daily_consumption
    index <- endpoints(daily_consumption, on = "weeks")
    avg_per_week_per_day <- period.apply(daily_consumption, index, f2)
    plot.xts(avg_per_week_per_day, type="h")
  })
  #Mode usage per week 
  #Cigarettes per weekday per time slots
  # output$usage_2h<-renderPlot({
  #   index <- endpoints(newdata_xts_smoked, on = "hours")
  #   daily_consumption <- period.apply(newdata_xts_smoked, index, f)
  # })
  ##################### Engagement tab ##############################
  output$daily_engagement<- renderPlot({
    df_logs <- myLogsData()
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only engagement data
    newdata_engaged <-
      newdata_xts[newdata_xts$Type == "Skipped" | newdata_xts$Type == "On time" | newdata_xts$Type == "Auto skipped" | newdata_xts$Type == "Snoozed"]
    f<-function(day){
      1-(nrow(day[day$Type=="Auto skipped"])/nrow(day))
    }
    index <- endpoints(newdata_engaged, on = "days")
    daily_engagement <- period.apply(newdata_engaged, index, f)
    plot.xts(daily_engagement)
  })
  output$weekly_engagement<- renderPlot({
    df_logs <- myLogsData()
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only engagement data
    newdata_engaged <-
      newdata_xts[newdata_xts$Type == "Skipped" | newdata_xts$Type == "On time" | newdata_xts$Type == "Auto skipped" | newdata_xts$Type == "Snoozed"]
    f<-function(day){
      1-(nrow(day[day$Type=="Auto skipped"])/nrow(day))
    }
    index <- endpoints(newdata_engaged, on = "weeks")
    weekly_engagement <- period.apply(newdata_engaged, index, f)
    plot.xts(weekly_engagement, type="h")
  })
  output$daily_usage<- renderPlot({
    df_logs <- myLogsData()
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-
      as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only smoked
    newdata_xts_smoked <-
      newdata_xts[newdata_xts$Type == "Behaviour" | newdata_xts$Type == "On time" | newdata_xts$Type == "Cheated"]
    f <- function(day) {
      return (nrow(day))
    }
    smoked_weekdays <- newdata_xts
    index <- endpoints(smoked_weekdays, on = "days")
    daily_consumption <- period.apply(smoked_weekdays, index, f)
    plot.xts(daily_consumption)
  })
  output$mode_usage_behaviour<-renderPlot({
    df_logs <- myLogsData()
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-
      as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only smoked
    f <- function(day) {
      return (nrow(day))
    }
    smoked_weekdays <- newdata_xts
    type<-newdata_xts[newdata_xts$Type=="Behaviour"]
    index <- endpoints(type, on = "days")
    Behaviour <- period.apply(type, index, f)
    plot.xts(Behaviour)
  })
  output$mode_usage_ontime<-renderPlot({
    df_logs <- myLogsData()
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-
      as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only smoked
    f <- function(day) {
      return (nrow(day))
    }
    smoked_weekdays <- newdata_xts
    type<-newdata_xts[newdata_xts$Type=="On time"]
    index <- endpoints(type, on = "days")
    OnTime <- period.apply(type, index, f)
    plot.xts(OnTime)
  })
  output$mode_usage_cheated<-renderPlot({
    df_logs <- myLogsData()
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-
      as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only smoked
    f <- function(day) {
      return (nrow(day))
    }
    smoked_weekdays <- newdata_xts
    type<-newdata_xts[newdata_xts$Type=="Cheated"]
    index <- endpoints(type, on = "days")
    Cheated <- period.apply(type, index, f)
    plot.xts(Cheated)
  })
  output$mode_usage_skipped<-renderPlot({
    df_logs <- myLogsData()
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-
      as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only smoked
    f <- function(day) {
      return (nrow(day))
    }
    smoked_weekdays <- newdata_xts
    type<-newdata_xts[newdata_xts$Type=="Skipped"]
    index <- endpoints(type, on = "days")
    Skipped <- period.apply(type, index, f)
    plot.xts(Skipped)
  })
  output$mode_usage_autoskipped<-renderPlot({
    df_logs <- myLogsData()
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-
      as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only smoked
    f <- function(day) {
      return (nrow(day))
    }
    smoked_weekdays <- newdata_xts
    type<-newdata_xts[newdata_xts$Type=="Auto skipped"]
    index <- endpoints(type, on = "days")
    Autoskipped <- period.apply(type, index, f)
    plot.xts(Autoskipped)
  })
  output$mode_usage_snoozed<-renderPlot({
    df_logs <- myLogsData()
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-
      as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only smoked
    f <- function(day) {
      return (nrow(day))
    }
    smoked_weekdays <- newdata_xts
    type<-newdata_xts[newdata_xts$Type=="Snoozed"]
    index <- endpoints(type, on = "days")
    Snoozed <- period.apply(type, index, f)
    plot.xts(Snoozed)
  })
  output$mode_usage_friend<-renderPlot({
    df_logs <- myLogsData()
    newdata <- subset(df_logs, df_logs$User == input$user_name)
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-
      as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only smoked
    f <- function(day) {
      return (nrow(day))
    }
    smoked_weekdays <- newdata_xts
    type<-newdata_xts[newdata_xts$Type=="Friend"]
    index <- endpoints(type, on = "days")
    Friend <- period.apply(type, index, f)
    plot.xts(Friend)
  })
  
  
  
  ######################################################################################################################################################
  #################################################################### All users #######################################################################
  ################################## Information tab ###########################################
  myUsefulData <- reactive({
    df_logs <- myLogsData()
    newdata <- df_logs
    #Transforming to xts
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-
      as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only smoked
    newdata_xts_smoked <-
      newdata_xts[newdata_xts$Type == "On time" | newdata_xts$Type == "Cheated"]
    #Adjusting periodicity to week
    newdata_weeks <- split(newdata_xts_smoked$Type, f = "weeks")
    #Number of cigs smoked in behaviour week
    n <- nrow(newdata_xts[newdata_xts$Type == "Behaviour"])
    #Removing behaviour week
    newdata_normal_weeks <- newdata_weeks
    f <- function(week) {
      return (n - nrow(week))
    }
    newdata_weekly_savings <- lapply(newdata_normal_weeks, f)
    weekly_saving <- do.call(rbind, newdata_weekly_savings)
    return(weekly_saving)
  })
  
  output$all_cigarettes_saved <- renderPrint({
    weekly_saving <- myUsefulData()
    fsum <- cumsum(weekly_saving[, 1])
    print(paste(tail(fsum, n = 1), "Cigarettes saved"))
    
  })
  output$all_money_saved <- renderPrint({
    weekly_saving <- myUsefulData()
    fsum <- cumsum(weekly_saving[, 1])
    print(paste(tail(fsum, n = 1), "€ saved"))
  })
  output$mean_cigarettes_saved <- renderPrint({
    weekly_saving <- myUsefulData()
    fsum <- cumsum(weekly_saving[, 1])
    print(paste(round(tail(fsum, n = 1) / 36, digits=2), "Cigarettes saved"))
  })
  output$mean_money_saved <- renderPrint({
    weekly_saving <- myUsefulData()
    fsum <- cumsum(weekly_saving[, 1])
    print(paste(round(tail(fsum, n = 1) / 36, digits=2), "€ saved"))
  })
  ##################### Classic tab ##############################
  #Mean and std of cigarette consumption per weekday
  output$all_mean_std <- renderPrint({
    df_logs <- myLogsData()
    newdata <- df_logs
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-
      as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only smoked
    newdata_xts_smoked <-
      newdata_xts[newdata_xts$Type == "Behaviour" | newdata_xts$Type == "On time" | newdata_xts$Type == "Cheated"]
    f <- function(day) {
      return (nrow(day))
    }
    weekdays <-
      which(
        .indexwday(newdata_xts_smoked) == 1 |
          .indexwday(newdata_xts_smoked) == 2 |
          .indexwday(newdata_xts_smoked) == 3 |
          .indexwday(newdata_xts_smoked) == 4 |
          .indexwday(newdata_xts_smoked) == 5
      )
    smoked_weekdays <- newdata_xts[weekdays]
    index <- endpoints(smoked_weekdays, on = "days")
    daily_consumption <- period.apply(smoked_weekdays, index, f)
   
    print(paste("Mean:", round(mean(daily_consumption), digits =2),"Standard deviation:",round(sd(daily_consumption), digits=2)))
    })
  FullProgress<-reactive({
    df_logs <- myLogsData()
    newdata <- df_logs
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-
      as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only smoked
    newdata_xts_smoked <-
      newdata_xts[newdata_xts$Type == "On time" | newdata_xts$Type == "Cheated"]
    f <- function(week) {
      return (nrow(week))
    }
    index <- endpoints(newdata_xts_smoked, on = "weeks")
    weekly_progress <- period.apply(newdata_xts_smoked, index, f)
    f2 <- function(week) {
      avg <- mean(week[1, 1], week[2, 1], week[1, 1])
      return ((avg - week[4, 1]) / avg)
    }
    n <- nrow(newdata_xts[newdata_xts$Type == "Behaviour"])
    f3 <- function(week) {
      return ((n - week) / n)
    }
    #Progress week 1 to 3
    progress_2 <- lapply(weekly_progress[1:3, 1], f3)
    progress_2 <- do.call(rbind, progress_2)
    #Progress week 3 and on
    progress_1 <- rollapply(weekly_progress[, 1], width = 4, f2)
    progress_1[1:3, 1] <- progress_2[1:3, 1]
    return(progress_1)
  })
  
  #Average progress of all users 
  output$mean_progress<-renderPrint({
    progress_1<-FullProgress()
    print(paste("Mean progress for all users:", mean(progress_1)))
  })
  #Average rate of progress of all users 
  output$mean_progress_rate<-renderPlot({
    progress_1<-FullProgress()
    f4 <- function(week) {
      if (week[2, 1] == 0) {
        res <- (as.numeric(week[3, 1]) - as.numeric(week[1, 1])) / abs(week[2, 1])
      }
      res <-
        ((as.numeric(week[3, 1]) - as.numeric(week[2, 1])) / abs(week[2, 1]))
      return (res)
    }
    rate_progress <- rollapply(progress_1[, 1], width = 4, f4)
    plot.xts(rate_progress)
  })
  #Cigarettes per weekday per time slots
  output$all_cigarettes_weekday<-renderPlot({
    df_logs <- myLogsData()
    newdata <- df_logs
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-
      as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only smoked
    newdata_xts_smoked <-
      newdata_xts[newdata_xts$Type == "Behaviour" | newdata_xts$Type == "On time" | newdata_xts$Type == "Cheated"]
    f <- function(day) {
      return (nrow(day))
    }
    weekdays <-
      which(
        .indexwday(newdata_xts_smoked) == 1 |
          .indexwday(newdata_xts_smoked) == 2 |
          .indexwday(newdata_xts_smoked) == 3 |
          .indexwday(newdata_xts_smoked) == 4 |
          .indexwday(newdata_xts_smoked) == 5
      )
    smoked_weekdays <- newdata_xts[weekdays]
    index <- endpoints(smoked_weekdays, on = "days")
    daily_consumption <- period.apply(smoked_weekdays, index, f)
    plot.xts(daily_consumption)
  })
  ##################### Engagement tab ##############################
  #Engagement over all period
  output$all_engagement<-renderPlot({
    df_logs <- myLogsData()
    newdata <- df_logs
    newdata$Time <- as.Date(newdata$Time, format = "%d/%m/%Y %H:%M")
    newdata_xts <-as.xts(newdata[, c("User", "Type", "Latitude", "Longitude")], order.by = newdata$Time)
    #Keeping only engagement data
    newdata_engaged <-
      newdata_xts[newdata_xts$Type == "Skipped" | newdata_xts$Type == "On time" | newdata_xts$Type == "Auto skipped" | newdata_xts$Type == "Snoozed"]
    f<-function(day){
      1-(nrow(day[day$Type=="Auto skipped"])/nrow(day))
    }
    index <- endpoints(newdata_engaged, on = "weeks")
    weekly_engagement <- period.apply(newdata_engaged, index, f)
    plot.xts(weekly_engagement, type="h")
  })
}
shinyApp(ui = ui, server = server)