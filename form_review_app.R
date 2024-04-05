library(shiny)
library(dplyr)
library(readxl)
library(tidyr)
library(plotly)
library(shinydashboard)
library(shinythemes)
library(readr)

source('C:/Users/jichen/Documents/GitHub/Form-Review-Tool/TIFTCC.R')


library('shinythemes')
rm(list = ls())
graphics.off()
currentDate <- Sys.Date()

hline <- function(y = 0, color = "green") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color,dash="dot")
  )
}







####################################################################################
data_dir <- 'C:/Users/jichen/Documents/GitHub/Form-Review-Tool/'
df<- read.csv(paste0(data_dir,'items_simulated.csv'))

a = 1
c = 0
D = 1
thetas <- round(seq(-5, 5, by = 0.1), 2)


# df <- read.csv(paste0(data_dir,'df.csv'))
# 
# # Generate 900 numbers with ~20% 2s and ~80% 1s
# set.seed(123) # Set a seed for reproducibility
# numbers <- sample(c(1, 2), size = dim(df)[1], replace = TRUE, prob = c(0.8, 0.2))
# df$ScorePoints <- numbers
# df[df$ScorePoints==1,'model'] <- 'logit'
# df[df$ScorePoints==2,'model'] <- 'adjacent logit'
# 
# #-----------------generate P0, P1, and P2------------------------------------------------
# # Assuming C is a given positive vector
# C <- df$difficulty
# 
# # Generate A as uniform random numbers, with an upper limit of C/2 to ensure A will be smaller than B
# A <- runif(length(C), min=-4, max=2*C/2)
# 
# # Calculate B as C - A
# B <- 2*C - A
# 
# # Check to ensure A is smaller than B (this should always be true by construction)
# all(A < B) # Expected to be TRUE
# 
# # Showing the variables
# data.frame(A, B, C)
# df$P0<-A
# df$P1<-B
# df[df$ScorePoints==1,'P0'] = 1
# df[df$ScorePoints==1,'P1'] = df[df$ScorePoints==1,'difficulty']
# df[df$ScorePoints==1,'P2'] = 0
# df[df$ScorePoints==2,'P2'] = NA
# write.csv(df,'C:/Users/jichen/Documents/GitHub/Form-Review-Tool/items_simulated.csv',row.names = FALSE)




##############################################################################
ui<-
  navbarPage("Form Evaluator",theme = shinytheme("cerulean"),
             tabPanel("Blue Print", uiOutput('page1')),
             tabPanel("Item Difficulty",uiOutput('page2')),
             tabPanel("Test Information",uiOutput('page3'))#,
             # tabPanel("Graphs",uiOutput('page4'))
      
  )

###################################################################################
#--------page 1----------------------------------------------------
  
  server<-function(input, output, session) {
    output$page1 <- renderUI({
      # Sidebar layout with a input and output definitions ----
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          
          selectInput(inputId = "Oppor",
                      label = "Choose an opportunity:",
                      choices = c("Opportunity 1","Opportunity 2", "Opportunity 3")),#"Opp1",
          
          # Input: Selector for choosing dataset (Subject) ----
          selectInput(inputId = "Subj",
                      label = "Choose a subject/grade:",
                      choices = c("Grade 3 Math", "Grade 4 Language Arts","Grade 5 Natural Science")),
          
          
          selectInput(inputId = "form",
                      label = "Choose a form:",
                      choices = c("Easy","Medium","Hard")),
          width = 2
          
        ),#siddebarpanel
        # Main panel for displaying outputs ----
        mainPanel(
          
          
          # Output: Verbatim text for data summary ----
          h3("Number of Items in Each Reporting Category"),
          tableOutput("summary_rep"),
          br(),
          
          # Output: Verbatim text for data summary ----
          h3("Number of Items for Each Knowledge and Skill Standard"),
          tableOutput("summary_ks"),
          br(),
          
          # Output: HTML table with requested number of observations ----
          h3("Form Preview"),
          tableOutput("view")

        )#mainpanel
      )#sidebarlayout
      
      
    })#renderUI
    
#------------------------------------------------------------------------------
    datasetInput <- reactive({
      
      df_opp <- df %>% filter(Opps == input$Oppor)
      
      if (grepl("Math", input$Subj)){sub_selected <-'Math'}
      else if (grepl("Science", input$Subj)){sub_selected <-'Natural Science'}
      else {sub_selected <-'Language Arts'}
      
      df_opp_sub <- df_opp %>% filter(Subject == sub_selected)
      df_opp_sub_form <- df_opp_sub %>% filter(Form == input$form)

      df_opp_sub_form
      
      
    })
 
    

############################################################################
    

#------------------------------------------------------------------------------  
#- Generate a summary of the rep_cat ----

  output$summary_rep <- renderTable({
    dataset <- datasetInput()
    rep_table <- as.data.frame(table(dataset["Reporting_Category"]))
    rc_table <- rep_table %>%
      bind_rows(summarise_all(., ~if(is.numeric(.)) {sum(.)} else {"Total"}))

    rc_table
  })

  output$summary_ks <- renderTable({
    dataset <- datasetInput()
    know_table <- as.data.frame(table(dataset["Knowledge_and_Skill"]))
    ks_table <- know_table %>%
      bind_rows(summarise_all(., ~if(is.numeric(.)) {sum(.)} else {"Total"}))
    
    ks_table
  })
  
 

  
  
#-------------------------------------------  

  output$view <- renderTable({
    #head(datasetInput(), n = input$obs)
    #datasetInput()
    
    datasetInput()[ , -which(names(datasetInput()) %in% c("Subject_Grade","model"))]
  })

###########################################################################  
#--------page 2----------------------------------------------------
  
  
  output$page2 <- renderUI({
    # Sidebar layout with a input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        selectInput(inputId = "OpporP2",
                    label = "Choose an opportunity:",
                    choices = c("Opportunity 1","Opportunity 2", "Opportunity 3")),
        
        # Input: Selector for choosing dataset (Subject) ----
        selectInput(inputId = "SubjP2",
                    label = "Choose a subject/grade:",
                    choices = c("Grade 3 Math", "Grade 4 Language Arts","Grade 5 Natural Science")),
        
        # selectInput(inputId = "form",
        #             label = "Choose a form:",
        #             choices = c("Easy","Medium","Hard")),
        
        width = 2
        
      ),#siddebarpanel
      # Main panel for displaying outputs ----
      mainPanel(
        
        
        # Output: Verbatim text for data summary ----
        h3("Summary of Form Difficulties"),
        tableOutput("summary_dif"),
        br(),
        
        # Output: Verbatim text for data summary ----
        h3("Item P-Value Scatter Plot"),
        plotlyOutput(outputId = "PVPlot"),
        br()
        
        
        
      )#mainpanel
    )#sidebarlayout
    
    
  })#renderUI
  
  #------------------------------------------------------------------------------
  datasetInputP2 <- reactive({
    
    df_opp <- df %>% filter(Opps == input$OpporP2)
    
    if (grepl("Math", input$SubjP2)){sub_selected <-'Math'}
    else if (grepl("Science", input$SubjP2)){sub_selected <-'Natural Science'}
    else {sub_selected <-'Language Arts'}
    
    df_opp_sub <- df_opp %>% filter(Subject == sub_selected)
    df_opp_sub
    
    
  })
  
  output$summary_dif <- renderTable({
    dataset <- datasetInputP2()
    #   
    
    dif_table <- dataset %>%                               # Summary by group using dplyr
      group_by(Form) %>% 
      summarise(count = length(Form),
                mean = mean(difficulty),
                std = sd(difficulty),
                median = median(difficulty),
                min = min(difficulty),
                max = max(difficulty))
    dif_table<-as.data.frame(dif_table)
    
    
    if (input$SubjP2 == 'Grade 3 Math'){sheetname<-'G3' }
    if (input$SubjP2 == 'Grade 4 Language Arts'){sheetname<-'G4' }
    if (input$SubjP2 == 'Grade 5 Natural Science'){sheetname<-'G5' }

    
    targ <- read_excel('C:/Users/jichen/Documents/GitHub/Form-Review-Tool/targets.xlsx',sheet=sheetname)
    targ <- targ[which(targ$Opp==input$OpporP2),]
    targ <- subset(targ, select=-Opp)
    dif_table <- merge(x=dif_table,y=targ, by = 'Form')
    dif_table
    
  })
  
  #------------
  output$PVPlot <- renderPlotly({
    dataset <- datasetInputP2()
    
    dataset['form_id'] = paste0(dataset$Form,'_',dataset$ItemId)
    fig <- plot_ly(data = dataset, x = ~form_id, y = ~P.Value,type = "scatter",mode = 'markers',
                   marker = list(size = 6,color = 'rgba(255, 182, 193, .9)',
                                 line = list(color = 'rgba(152, 0, 0, .8)',width = 2)),
                   text = ~ItemId,
                   hovertemplate = paste("<b>ItemId</b>: %{text}",  # text = itemid
                                         "<b>P-value</b>: %{y}", 
                                         '<extra></extra>'))%>%
      layout(shapes = list(hline(0.9),hline(0.2)))
    fig
    
    
  })



########################################################################
#--------page 3----------------------------------------------------


  output$page3 <- renderUI({
    # Sidebar layout with a input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        selectInput(inputId = "OpporP3",
                    label = "Choose an opportunity:",
                    choices = c("Opportunity 1","Opportunity 2", "Opportunity 3")),
        
        # Input: Selector for choosing dataset (Subject) ----
        selectInput(inputId = "SubjP3",
                    label = "Choose a subject/grade:",
                    choices = c("Grade 3 Math", "Grade 4 Language Arts","Grade 5 Natural Science")),
        
        
        width = 2
        
      ),#siddebarpanel
      # Main panel for displaying outputs ----
      mainPanel(
        
  
        
        # Output: Verbatim text for data summary ----
        h3("Test Information Curve"),
        plotlyOutput(outputId = "TICPlot"),
        br(),
        
        # Output: Verbatim text for data summary ----
        h3("Test Characteristic Curve"),
        plotlyOutput(outputId = "TCCPlot"),
        br()
    
        
   
        
      )#mainpanel
    )#sidebarlayout
    
    
  })#renderUI
  
  #------------------------------------------------------------------------------
  
  
  
  datasetInputP3 <- reactive({
    
    df_opp <- df %>% filter(Opps == input$OpporP3)
    
    if (grepl("Math", input$SubjP3)){sub_selected <-'Math'}
    else if (grepl("Science", input$SubjP3)){sub_selected <-'Natural Science'}
    else {sub_selected <-'Language Arts'}
    
    df_opp_sub <- df_opp %>% filter(Subject == sub_selected)
    df_opp_sub
    
    
  })
  
  
  
  
  #------------
  output$TCCPlot <- renderPlotly({
    dataset <- datasetInputP3()
    
    easy_form <- dataset[dataset$Form=='Easy',]
    medium_form <- dataset[dataset$Form=='Medium',]
    hard_form <- dataset[dataset$Form=='Hard',]
    
    easy_tcc<- lapply(thetas, function(theta) test_ExpectedScore(theta, easy_form, 1))
    medium_tcc<- lapply(thetas, function(theta) test_ExpectedScore(theta, medium_form, 1))
    hard_tcc<- lapply(thetas, function(theta) test_ExpectedScore(theta, hard_form, 1))
    
  
    fig <- plot_ly() %>%
      add_lines(x = thetas, y = easy_tcc, name = 'Easy', line = list(color = 'green')) %>%
      add_lines(x = thetas, y = medium_tcc, name = 'Medium', line = list(color = 'blue')) %>%
      add_lines(x = thetas, y = hard_tcc, name = 'Hard', line = list(color = 'red')) %>%
      layout(title = 'Test Characteristic Curve by Theta',
             xaxis = list(title = 'Theta'),
             yaxis = list(title = 'Test Characteristic Curve'),
             width = 600,  # Narrower width in pixels
             height = 400)  # Taller height in pixels)
    
    
    fig
    
    
  })
  
  
  #------------
  output$TICPlot <- renderPlotly({
    dataset <- datasetInputP3()
    
    easy_form <- dataset[dataset$Form=='Easy',]
    medium_form <- dataset[dataset$Form=='Medium',]
    hard_form <- dataset[dataset$Form=='Hard',]
    
    easy_tic<- lapply(thetas, function(theta) test_Information(theta, easy_form, 0))
    medium_tic<- lapply(thetas, function(theta) test_Information(theta, medium_form, 0))
    hard_tic<- lapply(thetas, function(theta) test_Information(theta, hard_form, 0))
    
    
    fig <- plot_ly() %>%
      add_lines(x = thetas, y = easy_tic, name = 'Easy', line = list(color = 'green')) %>%
      add_lines(x = thetas, y = medium_tic, name = 'Medium', line = list(color = 'blue')) %>%
      add_lines(x = thetas, y = hard_tic, name = 'Hard', line = list(color = 'red')) %>%
      layout(title = 'Test Information Curve by Theta',
             xaxis = list(title = 'Theta'),
             yaxis = list(title = 'Test Information Curve'),
             width = 600,  # Narrower width in pixels
             height = 400)  # Taller height in pixels)
    
    
    fig
    
    
  })
  
 
  
  
  
  
  
  
} # server
  
  ############################################################################
  





############################################################################

# Create Shiny app ----
shinyApp(ui = ui, server = server)


##############################################################################







