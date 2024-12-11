
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
      # Left column (width = 4 out of 12)
      column(4,
             h3("Patient Registration"),
             textOutput("no"),  # Display patient number
             textInput("hn", "Patient Code (HN):", ""),  # User-provided HN
             actionButton("check_hn", "Check HN"),        # Button to check HN
             verbatimTextOutput("hn_status"),  # Display HN status
             h3("Personal Infomation"),
             selectInput("titles", "Titles",
                         choices = c("Mr", "Miss", "Mrs", "Ms", "Dr", "Other")),
             conditionalPanel(
               condition = "input.titles == 'Other'",
               textInput("other_titles", "Please Specify Titles:", "")
             ),
             textInput("name", "Name", ""),
             textInput("dob", label = "Date of Birth", placeholder = "dd-mm-yyyy"),
             tags$label("Your Age:"),
             textOutput("age_text")  # Styled as a textInput-style box
             ,
             textInput("phone", "Phone", ""),
             textInput("email", "Email", ""),
             selectInput("gender", "Gender",
                         choices = c("Male", "Female", "Other"))
      ),
      
      # Middle column (width = 4 out of 12)
      column(4,
             textAreaInput("address", "Address:", "", rows = 10, 
                           placeholder = "Enter your full address here"),
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
             selectInput("ethnicity", "Ethnicity",
                         choices = c("Thai", "American", "British", "Indian", "Chinese", "Other")),
             # Conditional input for "Other" Ethnicity
             conditionalPanel(
               condition = "input.ethnicity == 'Other'",
               textInput("other_ethnicity", "Please Specify Ethnicity:", "")
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
                         choices = c("รับราชการ",
                                     "เกษตรกรรม", 
                                     "ค้าขาย",
                                     "ประกอบอาชีพส่วนตัว",
                                     "ไม่ประกอบอาชีพ",
                                     "แม่บ้าน",
                                     "ว่างงาน",
                                     "รับจ้าง",
                                     "รับราชการบำนาญ",
                                     "นักศึกษา",
                                     "พนักงานมหาวิทยาลัย",
                                     "Other")),
             # Conditional input for "Other" Occupation
             conditionalPanel(
               condition = "input.occupation == 'Other'",
               textInput("other_occupation", "Please Specify Occupation:", "")
             )
             
      ),
      column(4,
             selectInput("medcon", "Medical condition",
                         choices = c("DM", "HT", "Gout",
                                     "CKD", "Thyroid", "DLD",
                                     "DM HT", "DM DLD", "DM HT DLD",
                                     "HT Gout", "None", "Other")),
             conditionalPanel(
               condition = "input.medcon == 'Other'",
               textInput("other_medcon", "Please Specify Medical condition:", "")
             ),
             textInput("ekg", "Latest EKG", ""),
             textInput("echo", "Latest Echo", ""),
             textInput("eye", "Latest eye examination", ""),
             textAreaInput("drugallergy", "Drug allergy", "", rows = 3, 
                           placeholder = "Enter your drug allergy here"),
             selectInput("caregiver", "Caregiver",
                         choices = c("Self", "Child", "Parent", "Spouse (Husband/Wife)",
                                     "Other Family Member", "Other")),
             conditionalPanel(
               condition = "input.caregiver == 'Other'",
               textInput("other_caregiver", "Please Specify Caregiver:", "")
             ),
             selectInput("hbpm", "Home Blood Pressure Mornitor",
                         choices = c("Yes", "No")),
             selectInput("medexpenditure", "Medical expenditure",
                         choices = c("เบิกได้", 
                                     "จ่ายตรง", 
                                     "ประกันสังคม",
                                     "บัตรทอง",
                                     "จ่ายเอง",
                                     "ประกันชีวิต")),
             selectInput("pateintstatus", "Patient status",
                         choices = c("Yes", "No")),
             selectInput("complication", "Complication",
                         choices = c("Stroke", "Cardio", "Kidney",
                                     "Eye", "Other")),
             conditionalPanel(
               condition = "input.complication == 'Other'",
               textInput("other_complication", "Please Specify Complication:", "")
             ),
             textAreaInput("precriptionadjust", "Prescription adjusted", "", rows = 10),
             actionButton("save", "Save Data"), # Save button
             verbatimTextOutput("save_status")  # Save status
      )
    )
  ),
  tabPanel(
    "Doctor form",
    fluidRow(
      column(4,
             textInput("hn", "Patient Code (HN):", ""),  # User-provided HN
             actionButton("check_hn", "Check HN"),        # Button to check HN
             textOutput("pateintname")
        
      )
    )
  )
)


server <- function(input, output, session) {
  # Auto-incrementing No.
  output$no <- renderText({
    file_path <- "patient_data.csv"
    if (file.exists(file_path)) {
      num_rows <- nrow(read.csv(file_path))
      paste("No.:", num_rows + 1)
    } else {
      paste("No.:", 1)
    }
  })
  
  # Automatically calculate and display age when Date of Birth is selected
  output$age_text <- renderText({
    if (is.null(input$dob) || input$dob == "") {
      return("")
    }
    
    tryCatch({
      # Attempt to parse the input as a date in either dd-mm-yyyy or dd/mm/yyyy format
      dob_input <- as.Date(input$dob, tryFormats = c("%d-%m-%Y", "%d/%m/%Y"))
      
      if (is.na(dob_input)) {
        return("Invalid date format. Please use dd-mm-yyyy or dd/mm/yyyy.")
      }
      
      # Convert Buddhist Era (พ.ศ.) to Gregorian calendar
      gregorian_dob <- dob_input - years(543)
      
      # Current date
      today <- Sys.Date()
      
      # Calculate age
      age <- as.numeric(floor(difftime(today, gregorian_dob, units = "days") / 365.25))
      
      paste(age, "years")
    }, error = function(e) {
      return("Invalid date format. Please use dd-mm-yyyy or dd/mm/yyyy.")
    })
  })
  
  # Automatically convert HN to uppercase while typing
  observeEvent(input$hn, {
    updateTextInput(session, "hn", value = toupper(input$hn))  # Convert to uppercase
  })
  
  # Update amphoe (suburb) dropdown based on selected province
  observeEvent(input$province, {
    amphoe <- thailand %>%
      filter(ADM1_TH == input$province) %>%
      pull(ADM2_TH)  # Extract only the amphoe names
    
    updateSelectInput(session, "amphoe",
                      choices = amphoe,
                      selected = NULL)
  })
  
  
  
  # Check if HN exists in the file and populate the fields
  observeEvent(input$check_hn, {
    file_path <- "patient_data.csv"
    if (file.exists(file_path)) {
      all_data <- read.csv(file_path)
      hn_to_search <- toupper(input$hn)  # Convert input HN to uppercase for search
      
      result <- all_data %>% filter(HN == hn_to_search)
      
      if (nrow(result) > 0) {
        updateSelectInput(session, "titles", selected = result$Titles[1])
        updateTextInput(session, "name", value = result$Name[1])
        updateTextInput(session, "email", value = result$Email[1])
        updateDateInput(session, "dob", value = result$DateOfBirth[1])
        updateTextInput(session, "phone", value = result$Phone[1])
        updateSelectInput(session, "gender", selected = result$Gender[1])
        updateTextInput(session, "address", value = result$Address[1])
        updateSelectInput(session, "province", selected = result$Province[1])
        updateSelectInput(session, "amphoe", selected = result$Amphoe[1])
        updateSelectInput(session, "education", selected = result$Education[1])
        updateSelectInput(session, "occupation", selected = result$Occupation[1])
        updateTextInput(session, "medcon", value = result$MedicalCondition[1]) 
        updateTextInput(session, "ekg", value = result$EKG[1])                
        updateTextInput(session, "echo", value = result$Echo[1])              
        updateTextInput(session, "eye", value = result$EyeExamination[1])     
        updateTextAreaInput(session, "drugallergy", value = result$DrugAllergy[1]) 
        updateSelectInput(session, "caregiver", selected = result$Caregiver[1])
        updateSelectInput(session, "hbpm", selected = result$HBPM[1])
        updateSelectInput(session, "medexpenditure", selected = result$MedicalExpenditure[1])
        updateSelectInput(session, "pateintstatus", selected = result$PateintStatus[1])
        updateSelectInput(session, "complication", selected = result$Complication[1])
        updateTextAreaInput(session, "precriptionadjust", value = result$PrecriptionAdjust[1])
        output$hn_status <- renderText("Patient data loaded successfully.")
      } else {
        output$hn_status <- renderText("No patient found with this HN.")
      }
    } else {
      output$hn_status <- renderText("No data file exists yet.")
    }
  })
  
  # Save or replace data in the file when the "Save Data" button is clicked
  observeEvent(input$save, {
    file_path <- "patient_data.csv"
    
    # Calculate Age for saving
    calculated_age <- if (!is.null(input$dob) && input$dob != "") {
      tryCatch({
        dob_input <- as.Date(input$dob, tryFormats = c("%d-%m-%Y", "%d/%m/%Y"))
        if (!is.na(dob_input)) {
          gregorian_dob <- dob_input - years(543)  # Adjust for Buddhist Era
          today <- Sys.Date()
          as.numeric(floor(difftime(today, gregorian_dob, units = "days") / 365.25))
        } else {
          NA  # Invalid date
        }
      }, error = function(e) {
        NA  # Handle errors in date parsing
      })
    } else {
      NA  # Empty DOB
    }
    
    if (file.exists(file_path)) {
      # Load existing data
      all_data <- read.csv(file_path)
      
      # Check if HN exists
      hn_to_save <- toupper(input$hn)  # Convert HN to uppercase for saving
      if (hn_to_save %in% all_data$HN) {
        # Replace the existing data for this HN
        all_data <- all_data %>%
          filter(HN != hn_to_save)  # Remove the old record
      }
      
      # Consolidate updated user inputs into a data frame
      gender <- if (input$gender == "Other") input$other_gender else input$gender
      nationality <- if (input$nationality == "Other") input$other_nationality else input$nationality
      ethnicity <- if (input$ethnicity == "Other") input$other_ethnicity else input$ethnicity
      status <- if (input$status == "Other") input$other_status else input$status
      titles <- if (input$titles == "Other") input$other_titles else input$titles
      education <- if (input$education == "Other") input$other_education else input$education
      occupation <- if (input$occupation == "Other") input$other_occupation else input$occupation
      medcon <- if (input$medcon == "Other") input$other_medcon else input$medcon
      caregiver <- if (input$caregiver == "Other") input$other_caregiver else input$caregiver
      complication <- if (input$complication == "Other") input$other_complication else input$complication
      
      
      user_data <- data.frame(
        No = nrow(all_data) + 1,  # Maintain sequential No.
        HN = hn_to_save,         # Save HN in uppercase
        Titles = input$titles,
        Name = input$name,
        Email = input$email,
        DateOfBirth = input$dob,
        Phone = input$phone,
        Gender = input$gender,
        Age = calculated_age,
        Ethnicity = input$ethnicity,
        Nationality = input$nationality,
        Address = input$address,
        Province = input$province,
        Amphoe = input$amphoe,
        Education = input$education,
        Occupation = input$occupation,
        MedicalCondition = input$medcon,
        EKG = input$ekg,
        Echo = input$echo,
        EyeExamination = input$eye,
        DrugAllergy = input$drugallergy,
        Caregiver = input$caregiver,
        HBPM = input$hbpm,
        MedicalExpenditure = input$medexpenditure,
        PateintStatus = input$pateintstatus,
        Complication = input$complication,
        PrecriptionAdjust = input$precriptionadjust,
        stringsAsFactors = FALSE
      )
      
      # Add the updated or new record
      all_data <- bind_rows(all_data, user_data)
      
      # Save the updated data back to the file
      write.csv(all_data, file_path, row.names = FALSE)
      output$save_status <- renderText("Data saved successfully!")
    } else {
      # If no file exists, save the data as a new file
      user_data <- data.frame(
        No = 1,
        HN = toupper(input$hn),  # Convert HN to uppercase for saving
        Titles = input$titles,
        Name = input$name,
        Email = input$email,
        DateOfBirth = input$dob,
        Phone = input$phone,
        Gender = input$gender,
        Age = calculated_age,
        Ethnicity = input$ethnicity,
        Nationality = input$nationality,
        Address = input$address,
        Province = input$province,
        Amphoe = input$amphoe,
        Education = input$education,
        Occupation = input$occupation,
        MedicalCondition = input$medcon,
        EKG = input$ekg,
        Echo = input$echo,
        EyeExamination = input$eye,
        DrugAllergy = input$drugallergy,
        Caregiver = input$caregiver,
        HBPM = input$hbpm,
        MedicalExpenditure = input$medexpenditure,
        PateintStatus = input$pateintstatus,
        Complication = input$complication,
        PrecriptionAdjust = input$precriptionadjust,
        stringsAsFactors = FALSE
      )
      write.csv(user_data, file_path, row.names = FALSE)
      output$save_status <- renderText("Data saved successfully!")
    }
  })
  

}


# Run the application 
shinyApp(ui = ui, server = server)
