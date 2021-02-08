#### Load Libraries ####

library(shiny)
library(shinythemes)
library(shinyjs)
library(ggplot2)
library(psych)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)

#### Load Data ####

data <- read.csv("quant_data_clean.csv", stringsAsFactors = FALSE)
trainingQuestions <- data.frame(item = paste0("trainingPathway_",1:29), 
                                question = readLines(con = "trainingPerspectives.txt"), 
                                stringsAsFactors = FALSE)
endorsementsQuestions <- data.frame(item = paste0("endorsements_",1:13),
                                    question = readLines(con = "aopePerspectives.txt"),
                                    stringsAsFactors = FALSE)
betterAccessQuestions <- data.frame(item = paste0("betterAccess_",1:17),
                                    question = readLines(con = "betterAccessPerspectives.txt"),
                                    stringsAsFactors = FALSE)
questions <- rbind(trainingQuestions, endorsementsQuestions, betterAccessQuestions)
agreements <- c("1" = "Strongly disagree",
                "2" = "Somewhat disagree",
                "3" = "Neither agree nor disagree",
                "4" = "Somewhat agree",
                "5" = "Strongly agree")

data_long <- pivot_longer(data = data, cols = all_of(questions$item), names_to = "Item", values_to = "Response")
data_long <- merge(x = data_long, y = questions, by.x = "Item", by.y = "item")
colnames(data_long)[12] <- "Question"

data_summary <- data_long %>% group_by(Item, Response) %>% summarise(N = n(), Percentage = round(N/340*100, 2))
data_summary <- merge(x = data_summary, y = questions, by.x = "Item", by.y = "item")
colnames(data_summary)[5] <- "Question"

colours <- c("darkred","red","orange","green","darkgreen","white")

#### Clean and Recode Data ####

questions <- setNames(questions$item, questions$question) # change to named vector
betterAccessQuestions <- setNames(betterAccessQuestions$item, betterAccessQuestions$question) # change to named vector
endorsementsQuestions <- setNames(endorsementsQuestions$item, endorsementsQuestions$question) # change to named vector
trainingQuestions <- setNames(trainingQuestions$item, trainingQuestions$question) # change to named vector

#### Functions ####

s1.demographics <- function(variable, variablename) {
  ### "variable" is usually the x axis, entered as a string and evaluated into an object
  ### "variablename" is a string
  ggplot(data = separate_rows(data, all_of(variable), sep = ","), 
         aes(x = eval(parse(text = variable)))) +
    geom_bar(aes(fill = eval(parse(text = variable)))) +
    geom_text(stat = "count", aes(label = paste0("n = ", ..count..)), position = position_stack(vjust = 0.5)) +
    scale_y_continuous(name = "Number of Participants") +
    scale_x_discrete(name = variablename) +
    scale_fill_discrete(name = variablename) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, vjust = .2, hjust = .95),
          axis.text.y = element_text(colour = "grey20", size = 12, angle = 0),
          axis.title.x = element_blank(),
          axis.title.y = element_text(colour = "grey20", size = 12, angle = 90))
}

s1.analysis <- function(variable, question, variablename, type) {
  ### "variable" is usually the x axis, entered as a string and evaluated into an object
  ### "question" is the question that you want, usually "input$question"
  ### "variablename" is a string
  ### "type" is the plot type
  if(type == "Boxplot") {
    ggplot(data = separate_rows(data, variable, sep = ","), 
           aes(x = eval(parse(text = variable)), 
               colour = eval(parse(text = variable)))) +
      geom_boxplot(aes_string(y = question)) +
      # geom_text(stat = "summary", fun.y = mean, aes_string(y = question, label = "paste0('mean = ', round(..y.., 2))"), size = 3) +
      scale_y_continuous(labels = agreements) +
      scale_x_discrete(name = variablename) +
      theme(legend.position = "none",
            axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, vjust = .2, hjust = .95),
            axis.text.y = element_text(colour = "grey20", size = 12, angle = 0),
            axis.title.x = element_text(colour = "grey20", size = 12),
            axis.title.y = element_blank())
  } else if (type == "Likert") {
    data_long %>%
      separate_rows(variable, sep = ",") %>%
      filter(Item == question) %>%
      group_by(Response, variable = eval(parse(text = variable))) %>%
      summarise(N = n(), Percentage = round(N/340*100, 2)) %>%
      ggplot() +
      geom_bar(stat = "identity",
               aes(x = variable, y = Percentage, fill = as.factor(Response)),
               width = 0.7, position = position_fill(reverse = TRUE)) + 
      geom_text(stat = "count", 
                aes(x = variable, fill = as.factor(Response), label = Percentage),
                position = position_fill(reverse = TRUE)) +
      scale_fill_manual(values = colours, name = "Response", labels = agreements) +
      scale_x_discrete(name = variablename) +
      scale_y_continuous(name = "Percentage") +
      coord_flip()
  } else {
    ""
  }
}

s1.table <- function(variable, input, variablename) {
  separate_rows(data, variable, sep = ",") %>%
    group_by(Group = eval(parse(text = variable))) %>%
    summarise("Missing" = sum(is.na(eval(parse(text = input)))),
              "N" = n(),
              "Mean" = round(mean(eval(parse(text = input)), na.rm = TRUE), 2),
              "SD" = round(sd(eval(parse(text = input)), na.rm = TRUE), 2),
              "Median" = round(median(eval(parse(text = input)), na.rm = TRUE), 2),
              "Skew" = round(skew(eval(parse(text = input)), na.rm = TRUE), 2),
              "Kurtosis" = round(kurtosi(eval(parse(text = input)), na.rm = TRUE), 2),
              "Min." = round(min(eval(parse(text = input)), na.rm = TRUE), 2),
              "Max." = round(max(eval(parse(text = input)), na.rm = TRUE), 2))
}

s1.summary.plot <- function(input) {
  
  itemset <- if(input == "Training") {
    trainingQuestions
  } else if (input == "Endorsement") {
    endorsementsQuestions
  } else if (input == "Better Access") {
    betterAccessQuestions
  } else { "" }
  
  ggplot(data = filter(data_summary, Item %in% itemset)) +
    geom_bar(stat = "identity",
             aes(x = Question, y = Percentage, fill = as.factor(Response)),
             width = 0.7, position = position_fill(reverse = TRUE)) + 
    geom_text(stat = "count", 
              aes(x = Question, fill = as.factor(Response), label = Percentage),
              position = position_fill(reverse = TRUE)) +
    scale_fill_manual(values = colours, name = "Response", labels = agreements) +
    scale_x_discrete(name = "Question") +
    scale_y_continuous(name = "Percentage") +
    coord_flip()
}

s1.summary.table <- function(input) {
  
  itemset <- if(input == "Training") {
    trainingQuestions
  } else if (input == "Endorsement") {
    endorsementsQuestions
  } else if (input == "Better Access") {
    betterAccessQuestions
  } else { "" }
  
  data_long %>% 
    group_by(Question) %>% 
    filter(Item %in% itemset) %>%
    summarise("Missing" = sum(is.na(Response)),
              "N" = n(),
              "Mean" = round(mean(Response, na.rm = TRUE), 2),
              "SD" = round(sd(Response, na.rm = TRUE), 2),
              "Median" = round(median(Response, na.rm = TRUE), 2),
              "Skew" = round(skew(Response, na.rm = TRUE), 2),
              "Kurtosis" = round(kurtosi(Response, na.rm = TRUE), 2),
              "Min." = round(min(Response, na.rm = TRUE), 2),
              "Max." = round(max(Response, na.rm = TRUE), 2))
}

#### User Interface ####

ui <- navbarPage(
  
  #### Initial JavaScript ####
  
  useShinyjs(),
  
  tags$script("$(document).on('shiny:connected', function(event) {var myWidth = $(window).width();Shiny.onInputChange('shiny_width',myWidth)});"),
  
  #### Title and Page Details ####
  
  title = "Australian Psychologists' Training and Pathways to Registration", 
  selected = "About", 
  fluid = TRUE, 
  theme = shinytheme("flatly"),
  
  #### Home Page UI ####
  
  tabPanel("About", 
           fluidRow(column(width = 10, offset = 1, 
                           includeHTML("about.html"),
                           hr(),
                           includeHTML("footer.html")))),
  
  #### Study One Demographics UI ####
  
  tabPanel("Study One - Demographics",
           fluidRow(column(width = 10, offset = 1, 
                           tags$h3("Psychologists' attitudes towards their training, AoPEs and the Better Access Scheme"),
                           hr()
                           )
                    ),
           fluidRow(column(width = 5, offset = 1,
                           tags$h4("Gender"),
                           plotOutput("genderDistributionPlot"),
                           tags$h4("Age"),
                           plotOutput("ageDistributionPlot"),
                           tags$h4("Current Registration"),
                           plotOutput("currentRegistrationDistributionPlot"),
                           tags$h4("Area of Practice Endorsement"),
                           plotOutput("aopeDistributionPlot")),
                    column(width = 5,
                           tags$h4("Training Pathway"),
                           plotOutput("trainingPathwayDistributionPlot"),
                           tags$h4("Years of Experience"),
                           plotOutput("yearsExperienceDistributionPlot"),
                           tags$h4("Client Age"),
                           plotOutput("clientAgeDistributionPlot"),
                           tags$h4("Work Setting"),
                           plotOutput("workSettingDistributionPlot"))
                    ),
           fluidRow(column(width = 10, offset = 1,
                           tags$p("AoPE, Work Setting and Client Age counts 
                                  higher than participant counts due to multiple category selection")))
        
  ),
  
  #### Study One Analysis UI ####
  
  tabPanel("Study One - Analysis",
           fluidRow(column(width = 10, offset = 1, 
                           tags$h3("Psychologists' attitudes towards their training, AoPEs and the Better Access Scheme"),
                           hr(),
                           selectInput(inputId = "question",
                                       label = "Question", 
                                       list(`Training Perspectives` = trainingQuestions,
                                            `Endorsement Perspectives` = endorsementsQuestions,
                                            `Better Access Perspectives` = betterAccessQuestions),
                                       width = "100%"),
                           radioButtons(inputId = "analysisType",
                                        label = "Type:", 
                                        choices = c("Boxplot", "Likert", "Table"),
                                        selected = "Boxplot",
                                        inline = TRUE))
                    ),
           
           fluidRow(column(width = 5, offset = 1,
                           tags$h4("Gender"),
                           plotOutput("genderPlot"),
                           tableOutput("genderTable"),
                           hr(),
                           tags$h4("Age"),
                           plotOutput("agePlot"),
                           tableOutput("ageTable"),
                           hr(),
                           tags$h4("Registration"),
                           plotOutput("currentRegistrationPlot"),
                           tableOutput("currentRegistrationTable"),
                           hr(),
                           tags$h4("Area of Practice Endorsement"),
                           plotOutput("aopePlot"),
                           tableOutput("aopeTable")),
                    column(width = 5,
                           tags$h4("Training Pathway"),
                           plotOutput("trainingPathwayPlot"),
                           tableOutput("trainingPathwayTable"),
                           hr(),
                           tags$h4("Years of Experience"),
                           plotOutput("yearsExperiencePlot"),
                           tableOutput("yearsExperienceTable"),
                           hr(),
                           tags$h4("Client Age"),
                           plotOutput("clientAgePlot"),
                           tableOutput("clientAgeTable"),
                           hr(),
                           tags$h4("Work Setting"),
                           plotOutput("workSettingPlot"),
                           tableOutput("workSettingTable"))
                    )
           ),
  
  #### Study One Summary UI ####
  
  tabPanel("Study One - Summary", 
           fluidRow(column(width = 10, offset = 1, 
                           tags$h3("Psychologists' attitudes towards their training, AoPEs and the Better Access Scheme"),
                           hr(),
                           selectInput(inputId = "category",
                                       label = "Category", 
                                       list(`Training` = "Training",
                                            `Endorsement` = "Endorsement",
                                            `Better Access` = "Better Access"),
                                       width = "100%"),
                           radioButtons(inputId = "summaryType",
                                        label = "Type:", 
                                        choices = c("Likert", "Table"),
                                        selected = "Likert",
                                        inline = TRUE))
                    ),
           
           fluidRow(column(width = 10, offset = 1,
                           tags$h4("Summary Data (n = 340)"),
                           plotOutput("summaryPlot"),
                           tableOutput("summaryTable"))
                    )
           )
)

server <- function(input, output, session) {
  
  #### Study One Demographics ####
  
  output$genderDistributionPlot <- renderPlot({
    s1.demographics("gender","Gender")
  })
  
  output$ageDistributionPlot <- renderPlot({
    s1.demographics("age","Age")
  })
  
  output$currentRegistrationDistributionPlot <- renderPlot({
    s1.demographics("currentRegistration","Current Registration")
  })
  
  output$aopeDistributionPlot <- renderPlot({
    s1.demographics("aope","AoPE")
  })
  
  output$trainingPathwayDistributionPlot <- renderPlot({
    s1.demographics("trainingPathway","Training Pathway")
  })
  
  output$yearsExperienceDistributionPlot <- renderPlot({
    s1.demographics("yearsExperience","Years of Experience")
  })
  
  output$clientAgeDistributionPlot <- renderPlot({
    s1.demographics("clientAge","Client Age")
  })
  
  output$workSettingDistributionPlot <- renderPlot({
    s1.demographics("workSetting","Work Setting")
  })
  
  #### Study One Analysis ####
  
  observeEvent(input$analysisType, {
    req(input$analysisType)
    if(input$analysisType == "Boxplot" | input$analysisType == "Likert") {
      hide("genderTable")
      hide("ageTable")
      hide("currentRegistrationTable")
      hide("aopeTable")
      hide("trainingPathwayTable")
      hide("yearsExperienceTable")
      hide("clientAgeTable")
      hide("workSettingTable")
      show("genderPlot")
      show("agePlot")
      show("currentRegistrationPlot")
      show("aopePlot")
      show("trainingPathwayPlot")
      show("yearsExperiencePlot")
      show("clientAgePlot")
      show("workSettingPlot")
    } else {
      show("genderTable")
      show("ageTable")
      show("currentRegistrationTable")
      show("aopeTable")
      show("trainingPathwayTable")
      show("yearsExperienceTable")
      show("clientAgeTable")
      show("workSettingTable")
      hide("genderPlot")
      hide("agePlot")
      hide("currentRegistrationPlot")
      hide("aopePlot")
      hide("trainingPathwayPlot")
      hide("yearsExperiencePlot")
      hide("clientAgePlot")
      hide("workSettingPlot")
    }
  })
  
  #### Study One Analysis Plots ####
  
  output$genderPlot <- renderPlot({
    s1.analysis("gender",input$question,"Gender",input$analysisType)
  })
  
  output$agePlot <- renderPlot({
    s1.analysis("age",input$question,"Age",input$analysisType)
  })
  
  output$currentRegistrationPlot <- renderPlot({
    s1.analysis("currentRegistration",input$question,"Registration",input$analysisType)
  })
  
  output$aopePlot <- renderPlot({
    s1.analysis("aope",input$question,"AoPE",input$analysisType)
  })
  
  output$trainingPathwayPlot <- renderPlot({
    s1.analysis("trainingPathway",input$question,"Training pathway",input$analysisType)
  })
  
  output$yearsExperiencePlot <- renderPlot({
    s1.analysis("yearsExperience",input$question,"Years of Experience",input$analysisType)
  })
  
  output$clientAgePlot <- renderPlot({
    s1.analysis("clientAge",input$question,"Client Age",input$analysisType)
  })
  
  output$workSettingPlot <- renderPlot({
    s1.analysis("workSetting",input$question,"Work Setting",input$analysisType)
  })
  
  #### Study One Analysis Tables ####
  
  output$genderTable <- renderTable({
    s1.table("gender",input$question,"Gender")
  })
  
  output$ageTable <- renderTable({
    s1.table("age",input$question, "Age")
  })
  
  output$currentRegistrationTable <- renderTable({
    s1.table("currentRegistration",input$question,"Registration")
  })
  
  output$aopeTable <- renderTable({
    s1.table("aope",input$question,"AoPE")
  })
  
  output$trainingPathwayTable <- renderTable({
    s1.table("trainingPathway",input$question,"Training Pathway")
  })
  
  output$yearsExperienceTable <- renderTable({
    s1.table("yearsExperience",input$question,"Years of Experience")
  })
  
  output$clientAgeTable <- renderTable({
    s1.table("clientAge",input$question,"Client Age")
  })
  
  output$workSettingTable <- renderTable({
    s1.table("workSetting",input$question,"Work Setting")
  })
  
  #### Study One Summary ####
  
  observeEvent(input$summaryType, {
    req(input$summaryType)
    if(input$summaryType == "Likert") {
      hide("summaryTable")
      show("summaryPlot")
    } else {
      show("summaryTable")
      hide("summaryPlot")
    }
  })
  
  output$summaryPlot <- renderPlot({
    s1.summary.plot(input$category)
    }, height = 1000)
  
  output$summaryTable <- renderTable({
    s1.summary.table(input$category)
    })

  #### Close Session on Browser Exit ####
  
  session$onSessionEnded(stopApp)
  
}

shinyApp(ui, server, options = "display.mode")