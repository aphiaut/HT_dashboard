
library(shiny)
library(shinyTime)
library(tidyverse)
library(kableExtra)
library(ggplot2)
library(forcats)
library(RColorBrewer)
library(plotly)
library(DT)
library(shinythemes)
library(ggthemes)
library(bslib)

thailand <- read_csv("data/thailand_province_amphoe.csv")

ui <- navbarPage(
  "HT Clinic",
  theme = shinytheme("flatly"),
  tabPanel(
    "Patient Info",
    fluidRow(
      # Left column (width = 4 out of 12)
      column(4,
             h3("Patient Registration"),
             textOutput("no"),  # Display patient number
             textInput("hn_register", "Patient Code (HN):", ""),  # User-provided HN
             actionButton("check_hn_register", "Check HN"),        # Button to check HN
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
             textOutput("age_text"),
             textInput("phone", "Phone", ""),
             textInput("phone2", "Phone 2 (optional)", ""),
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
                         choices = c("Single", 
                                     "Marriage", 
                                     "Divorced",
                                     "Separated",
                                     "Widowed",
                                     "Other")),
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
                         choices = c("Primary/ปวช.",
                                     "Secondary/ปวส.", 
                                     "Bachelor's Degree",
                                     "Master's Degree or Higher", 
                                     "Other")),
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
             textInput("ekg", label = "Latest EKG", placeholder = "dd-mm-yyyy"),
             textInput("echo", label = "Latest Echo", placeholder = "dd-mm-yyyy"),
             textInput("eye", label = "Latest eye examination", placeholder = "dd-mm-yyyy"),
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
                         choices = c("Have", "Don't Have")),
             selectInput("medexpenditure", "Medical expenditure",
                         choices = c("จ่ายตรง",
                                     "จ่ายเอง", 
                                     "บัตรทอง",
                                     "เบิกได้", 
                                     "ประกันสังคม",
                                     "ประกันชีวิต")),
             selectInput("pateintstatus", "Patient status",
                         choices = c("Continuing Treatment"= "continuing_treatment",
                                     "Loss to Follow-Up" = "loss_fu",
                                     "Consult OPD" = "consult_opd",
                                     "Refer to Hospital" = "refer_hospital")
                         ),
             conditionalPanel(
               condition = "input.pateintstatus == 'refer_hospital'",
               textInput("refer_hospital_details", "Specify Hospital Details:", "")
             ),
             conditionalPanel(
               condition = "input.pateintstatus == 'consult_opd'",
               textInput("consult_opd_details", "Specify OPD Details:", "")
             ),     
             selectInput("complication", "Complication",
                         choices = c("Stroke", "Cardio", "Kidney",
                                     "Eye", "Other")),
             conditionalPanel(
               condition = "input.complication == 'Other'",
               textInput("other_complication", "Please Specify Complication:", "")
             ),
             selectInput("precriptionadjust", "Prescription Adjusted",
                         choices = c("Off Medication", "Decrease", "Add",
                                     "Same", "Change : Complication")),
             hr(),
             actionButton("save", "Save Data"), # Save button
             verbatimTextOutput("save_status")  # Save status
      )
    )
  ),
#--------------------------- Doctor Form UI -------------------
  tabPanel(
    "Doctor Form",
    fluidRow(
      column(4,
             textInput("hn_doc", "Patient Code (HN):", ""),  # User-provided HN
             actionButton("check_hn_doc", "Check HN"),   # Button to check HN
             tags$div(
               tags$h4("Patient Name:",
                       style = "display: inline-block; margin-right: 10px;"), # Inline label
               textOutput("patient_name", inline = TRUE)                     # Inline output
             ),
             tags$hr(),
             textInput("num_visit", "Number of Visits:"), # User-provided Number of visits
             textInput("visit_date", label = "Date", placeholder = "dd-mm-yyyy"), # Provided date
             textAreaInput("patient_note", "Patient Notes:", "", rows = 10, 
                           placeholder = "Are there any specific questions or concerns you want to discuss with the doctor?"),
        
      ),
      column(4,
             h3("Patient Symptom Checklist:"),
             h4("Please answer the following questions"),
             # Question 1
             radioButtons(
               inputId = "chest_tightness",
               label = "1. Chest Tightness",
               choices = c("No" = "no", "Yes" = "yes"),
               inline = TRUE
             ),
             # Question 2
             radioButtons(
               inputId = "nervous_system",
               label = "2. Abnormal Nervous System (e.g., facial drooping, weakness in limbs, numbness on one side, or slurred speech)",
               choices = c("No" = "no", "Yes" = "yes"),
               inline = TRUE
             ),
             # Question 3
             radioButtons(
               inputId = "urinal_abnormal",
               label = "3. Abnormal Urination (e.g., frequent urination, pain during urination, urine retention, or inability to urinate)",
               choices = c("No" = "no", "Yes" = "yes"),
               inline = TRUE
             ),
             # Question 4
             radioButtons(
               inputId = "headache",
               label = "4. Headache",
               choices = c("No" = "no", "Occasionally" = "sometimes", "Often" = "often"),
               inline = TRUE
             ),
             
             # Question 5
             radioButtons(
               inputId = "dizziness",
               label = "5. Dizziness",
               choices = c("No" = "no", "Occasionally" = "sometimes", "Often" = "often"),
               inline = TRUE
             ),
             
             # Question 6
             radioButtons(
               inputId = "breath_shortness",
               label = "6. Shortness of Breath",
               choices = c("No" = "no", "Occasionally" = "sometimes", "Often" = "often"),
               inline = TRUE
             ),
             # Question 7
             radioButtons(
               inputId = "leg_swelling",
               label = "7. Leg Swelling",
               choices = c("No" = "no", "Occasionally" = "sometimes", "Often" = "often"),
               inline = TRUE
             ),
             
             # Question 8
             radioButtons(
               inputId = "face_swelling",
               label = "8. Face Swelling",
               choices = c("No" = "no", "Occasionally" = "sometimes", "Often" = "often"),
               inline = TRUE
             )
             
             # hr(),
             # actionButton("submit", "Submit Responses"),
             # verbatimTextOutput("response")
      ),
      column(4,
             h3("Lab Results:"),
             textInput("cr", "Cr:", ""),
             textInput("na", "Na:", ""),
             textInput("fbs", "FBS:", ""),
             textInput("hba1c", "HBA1C:", ""),
             textInput("cho", "CHO:", ""),
             textInput("ldl", "LDL:", ""),
             textInput("tg", "TG:", ""),
             textInput("hdl", "HDL:", ""),
             textInput("ast", "AST:", ""),
             textInput("alt", "ALT:", ""),
      )
    ),
    hr(),
    fluidRow(
    column(4,
           #CC and PI Section
           h4("CC and PI"),
           radioButtons("cc", "CC:", 
                        choices = c("Follow-up Visit" = "follow_up", 
                                    "Early Visit" = "early_visit", 
                                    "Late Visit" = "late_visit"), 
                        inline = TRUE),
           radioButtons(
             inputId = "pi",
             label = "PI:",
             choices = c(
               "Normal" = "normal", 
               "Abnormal" = "abnormal"
             ),
             inline = TRUE
           ),
           conditionalPanel(
             condition = "input.pi == 'abnormal'",
             textAreaInput("pi_abnormal", "Please Specify Symptoms:", "", rows = 3)
           ),
           checkboxGroupInput("additional_activities", "Pateint Additional Activities:", 
                              choices = c("Always take medicines" = "alway_take_medicine", 
                                          "Control salty taste" = "salty_control",
                                          "Exercise" = "excercise"), 
                              inline = TRUE),
           textAreaInput("allergic_history", "Drug Allergic History:", "", rows = 3),
           fluidRow(
              column(6, h4("Blood Pressure")),
                   ),
           fluidRow(
              column(6, textInput("bp_sys", "Blood Pressure (Sys):", "")),
              column(6, textInput("bp_dia", "Blood Pressure (Dia):", "")),
           ),
           fluidRow(
              column(6, h6("BP Target: Less than 140/90 mmHg"))
           ),
           fluidRow(
              column(6,textInput("pulse", "Pulse Rate:", ""),
                     h6("Normal value: 60-100 beats per min")),
              column(6,textInput("resp", "Respiration Rate:", ""),
                     h6("Normal value 12-20 breaths per minute")),
                   ),
           fluidRow(
              column(6,textInput("height", "Height (cm):", "")),
              column(6,textInput("weight", "Weight (kg):", "")),
              tags$label("Your BMI:"),
              textOutput("bmi_text"),
              h6("BMI Target: 18.5-23.0 kg/m2")
                   )
           ),
    column(4,
           h4("Physical Examination:"),
           fluidRow(
             column(6, 
                    radioButtons(inputId = "heent", 
                                    label = "HEENT:", 
                                    choices = c("WNL" = "wnl", "Abnormal" = "abnormal"), 
                                    inline = TRUE),
                    conditionalPanel(
                      condition = "input.heent == 'abnormal'",
                      textAreaInput("heent_abnormal", "Please Specify Symptoms:", "", rows = 3)
                    )),
             column(6, 
                    radioButtons(inputId = "heart", 
                                    label = "Heart:", 
                                    choices = c("WNL" = "wnl", "Abnormal" = "abnormal"), 
                                    inline = TRUE),
                    conditionalPanel(
                      condition = "input.heart == 'abnormal'",
                      textAreaInput("heart_abnormal", "Please Specify Symptoms:", "", rows = 3)
                    ))
                   ),
           fluidRow(
             column(6, 
                    radioButtons(inputId = "lungs", 
                                    label = "Lungs:", 
                                    choices = c("WNL" = "wnl", "Abnormal" = "abnormal"), 
                                    inline = TRUE),
                    conditionalPanel(
                      condition = "input.lungs == 'abnormal'",
                      textAreaInput("lungs_abnormal", "Please Specify Symptoms:", "", rows = 3)
                    )),
             column(6, 
                    radioButtons(inputId = "abd", 
                                    label = "Abdomen:", 
                                    choices = c("WNL" = "wnl", "Abnormal" = "abnormal"), 
                                    inline = TRUE),
                    conditionalPanel(
                      condition = "input.abd == 'abnormal'",
                      textAreaInput("abd_abnormal", "Please Specify Symptoms:", "", rows = 3)
                    ))
           ),
           fluidRow(
             column(6, 
                    radioButtons(inputId = "ext", 
                                    label = "Extremities:", 
                                    choices = c("WNL" = "wnl", "Abnormal" = "abnormal"), 
                                    inline = TRUE),
                    conditionalPanel(
                      condition = "input.ext == 'abnormal'",
                      textAreaInput("ext_abnormal", "Please Specify Symptoms:", "", rows = 3)
                    )),
             column(6, 
                    radioButtons(inputId = "ns", 
                                    label = "N/S:", 
                                    choices = c("WNL" = "wnl", "Abnormal" = "abnormal"), 
                                    inline = TRUE),
                    conditionalPanel(
                      condition = "input.ns == 'abnormal'",
                      textAreaInput("ns_abnormal", "Please Specify Symptoms:", "", rows = 3)
                    ))
           ),
           # Diagnosis
           textAreaInput("diagnosis", "Diagnosis:", "", rows = 10)
           ),
    column(4,
           h4("Follow-up Schedule"),
           radioButtons("follow_up", "Follow-up:", 
                        choices = c("1 week" = "1_week", 
                                    "2 weeks" = "2_weeks", 
                                    "4 weeks" = "4_weeks", 
                                    "8 weeks" = "8_weeks", 
                                    "12 weeks" = "12_weeks", 
                                    "24 weeks" = "24_weeks"),
                        inline = TRUE
           ),
           # Date and Time Input
           textInput("follow_up_date", label = "Date:", placeholder = "dd-mm-yyyy"),
           textInput("follow_up_time", label = "Time:", value = format(Sys.time(), "%H:%M")),
           textInput("follow_up_location", label = "Location:", ""),
           #Lab Tests Section
           h4("Laboratory Tests:"),
           checkboxGroupInput(
             inputId = "lab_tests",
             label = NULL,
             choices = c(
               "UA (Urinalysis)" = "ua",
               "BUN, Cr (Blood Urea Nitrogen, Creatinine)" = "bun_cr",
               "HbA1C" = "hba1c",
               "FBS (Fasting Blood Sugar)" = "fbs",
               "Lipid Profiles" = "lipid_profiles",
               "CXR (Chest X-ray)" = "cxr",
               "ECG (Electrocardiogram)" = "ecg",
               "Other" = "other"
             ),
             inline = FALSE
           ),
           conditionalPanel(
             condition = "input.lab_tests == 'other'",
             textInput("other_lab_test", "Specify Other Tests:", "",)
           ))
           
    ),
    hr(),
    fluidRow(
      # Nursing Activities Section
      column(6,
          h4("Nurse Activities"),
          checkboxGroupInput(
            inputId = "nursing_activities",
            label = "Please select the nursing activities provided to the patient:",
            choices = c(
              "Hypertension management and prevention of complications" = "hypertension_management",
              "Medication awareness and side effects" = "medication_awareness",
              "Home blood pressure monitoring" = "home_monitoring",
              "Exercise encouragement" = "exercise_encouragement",
              "Dietary salt reduction" = "salt_reduction",
              "Stress management" = "stress_management",
              "Smoking cessation" = "smoking_cessation",
              "Alcohol intake moderation" = "alcohol_moderation",
              "Symptom monitoring for complications" = "symptom_monitoring",
              "Chest pain, dizziness, or fainting awareness" = "chest_pain_awareness",
              "Edema or swelling monitoring" = "edema_monitoring",
              "Healthcare follow-up adherence" = "follow_up_adherence"
            ),
            inline = FALSE
          ),
      ),
      column(6,
             h4("Home Medication")
             )
    )
  )
)


#--------------- Server --------------------

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
  observeEvent(input$hn_register, {
    updateTextInput(session, "hn_register", value = toupper(input$hn_register))  # Convert to uppercase
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
  observeEvent(input$check_hn_register, {
    file_path <- "patient_data.csv"
    if (file.exists(file_path)) {
      all_data <- read.csv(file_path)
      hn_to_search <- toupper(input$hn_register)  # Convert input HN to uppercase for search
      
      result <- all_data %>% filter(HN == hn_to_search)
      
      if (nrow(result) > 0) {
        updateSelectInput(session, "titles", selected = result$Titles[1])
        updateTextInput(session, "name", value = result$Name[1])
        updateTextInput(session, "email", value = result$Email[1])
        updateDateInput(session, "dob", value = result$DateOfBirth[1])
        updateTextInput(session, "phone", value = result$Phone[1])
        updateTextInput(session, "phone2", value = result$Phone2[1])
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
        updateSelectInput(session, "precriptionadjust", value = result$PrecriptionAdjust[1])
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
      hn_to_save <- toupper(input$hn_register)  # Convert HN to uppercase for saving
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
        Phone2 = input$phone2,
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
        HN = toupper(input$hn_register),  # Convert HN to uppercase for saving
        Titles = input$titles,
        Name = input$name,
        Email = input$email,
        DateOfBirth = input$dob,
        Phone = input$phone,
        Phone2 = input$phone2,
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

#-------------------  Doctor Form ----------------------------------
  observeEvent(input$check_hn_doc, {  # Use 'check_hn_doc' as the button ID
    # Path to the CSV file
    file_path <- "patient_data.csv"
    
    if (file.exists(file_path)) {
      # Read the data
      all_data <- read.csv(file_path, stringsAsFactors = FALSE)
      
      # Convert input HN to uppercase for consistent comparison
      hn_to_search <- toupper(input$hn_doc)
      
      # Search for the HN in the dataset
      result <- all_data[all_data$HN == hn_to_search, ]
      
      # Display the patient's name if found
      if (nrow(result) > 0) {
        output$patient_name <- renderText({
          paste(result$Name[1]) # Assuming the column storing the name is 'Name'
        })
      } else {
        output$patient_name <- renderText("Patient not found.")
      }
    } else {
      output$patient_name <- renderText("No data file exists.")
    }
  })

}


# Run the application 
shinyApp(ui = ui, server = server)
