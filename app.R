
library(shiny)
library(tidyverse)
library(kableExtra)
library(ggplot2)
library(forcats)
library(RColorBrewer)
library(plotly)
library(DT)
library(shinythemes)
library(ggthemes)

thailand <- read_csv("data/thailand_province_amphoe.csv")

ui <- navbarPage(
  "HP Clinic",
  theme = shinytheme("flatly"),
  tabPanel(
    "Patient Info",
    fluidRow(
      # Left column (width = 6 out of 12)
      column(6,
             h3("Personal Infomation"),
             textInput("name", "Name", ""),
             textInput("email", "Email", ""),
             dateInput('dob',
                         label = 'Date input: dd-mm-yyyy',
                         value = Sys.Date()
               ),
              tags$label("Your Age:"),
              textOutput("age_text")  # Styled as a textInput-style box
             ,
             textInput("phone", "Phone", ""),
             textInput("email", "Email", ""),
             selectInput("gender", "Gender", 
                         choices = c("Male", "Female", "Other"))
      ),

      # Right column (width = 6 out of 12)
      column(6,
             textInput("address", "Address", ""),
             selectInput("province", "Province",
                         choices = unique(thailand$ADM1_TH), 
                         selected = NULL),               
             selectInput("amphoe", "Amphoe",
                         choices = NULL),
             selectInput("status", "Status",
                         choices = c("Single", "Marriage", "Other")),
             conditionalPanel(
               condition = "input.status == 'Other'",
               textInput("other_status", "Please Specify Status:", "")
             ),
             selectInput("nationality", "Nationality",
                         choices = c("Thai", "American", "British", "Indian", "Chinese", "Other")),
             # Conditional input for "Other" Nationality
             conditionalPanel(
               condition = "input.nationality == 'Other'",
               textInput("other_nationality", "Please Specify Nationality:", "")
             ),
             # Education selection
             selectInput("education", "Education Level",
                         choices = c("Primary", "Secondary", "Bachelor's Degree", 
                                     "Master's Degree", "PhD", "Other")),
             # Conditional input for "Other" Education
             conditionalPanel(
               condition = "input.education == 'Other'",
               textInput("other_education", "Please Specify Education Level:", "")
             ),
             #Occupation selection
             selectInput("occupation", "Occupation",
                         choices = c("Doctor", "Nurse", "Teacher", "Engineer", 
                                     "Business Owner", "Student", "Other")),
             # Conditional input for "Other" Occupation
             conditionalPanel(
               condition = "input.occupation == 'Other'",
               textInput("other_occupation", "Please Specify Occupation:", "")
             )
             
      )
    )
  )
)


server <- function(input, output, session) {
  # Automatically calculate and display age when Date of Birth is selected
  output$age_text <- renderText({
    if (is.null(input$dob)) {
      return("")
    }
    
    dob <- as.Date(input$dob)
    today <- Sys.Date()
    
    # Calculate age
    age <- as.numeric(floor(difftime(today, dob, units = "days") / 365.25))
    
    paste(age, "years")
  })
  
  # Update amphoe dropdown based on selected province
  observeEvent(input$province, {
    # Filter the dataset for amphoe of the selected province
    amphoe <- thailand %>%
      filter(ADM1_TH == input$province) %>%
      pull(ADM2_TH)  # Extract only the amphoe names
    
    # Update the amphoe dropdown dynamically
    updateSelectInput(session, "amphoe",
                      choices = amphoe,
                      selected = NULL)
  })
  
  # Display the selected province and amphoe
  output$selection <- renderText({
    if (is.null(input$province) || is.null(input$amphoe)) {
      return("กรุณาเลือกจังหวัด") # "Please select a provinc"
    }
    paste("คุณเลือกจังหวัด:", input$province,
          "อำเภอ:", input$amphoe) # "You selected province and amphoe"
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
