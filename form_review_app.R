library(shiny)
library(dplyr)
library(readxl)
library(tidyr)
library(plotly)
library(shinydashboard)
library(shinythemes)
library(readr)

#install.packages("shinythemes")
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
df <- read.csv(paste0(data_dir,'df.csv'))


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

      res <- df_opp_sub_form
      res
      
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
    datasetInput()
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
    res <- df_opp_sub
    res
    
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
        h3("Dichotomous Item P-value Scatter Plot"),
        plotlyOutput(outputId = "DiPVPlot"),
        br(),
        
        # Output: Verbatim text for data summary ----
        h3("Polytomous Item P-Value Scatter Plot"),
        plotlyOutput(outputId = "PoPVPlot"),
        br(),
        
        # Output: Verbatim text for data summary ----
        h3("Item Point Biserial Plot"),
        plotlyOutput(outputId = "PBPlot"),
        br(),
        
        
        # Output: Verbatim text for data summary ----
        h3("Item Rasch Fit Stat Plot"),
        plotlyOutput(outputId = "FITPlot"),
        br()
        
   
        
      )#mainpanel
    )#sidebarlayout
    
    
  })#renderUI
  
  #------------------------------------------------------------------------------
  datasetInputP3 <- reactive({
    
    df_opp <- df %>% filter(Opp == input$OpporP3)
    
    if (grepl("Math", input$SubjP3)){sub_selected <-'Math'}
    else if (grepl("Science", input$SubjP3)){sub_selected <-'Science'}
    else {sub_selected <-'Social Studies'}
    grade_selected <- parse_number(input$SubjP3)
    df_opp_sub <- df_opp %>% filter(Subject == sub_selected & Grade == grade_selected)
    res <- df_opp_sub
    res

  })
  
  

  
  
  
  
  
  
  
  
  
  
  
  
} # server
  
  ############################################################################
  





############################################################################

# Create Shiny app ----
shinyApp(ui = ui, server = server)


##############################################################################







