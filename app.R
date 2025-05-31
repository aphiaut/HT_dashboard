
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
                         choices = c("ประถม",
                                     "ม.ต้น", 
                                     "ม.ปลาย",
                                     "ปวช.",
                                     "ปวส.",
                                     "ปริญญาตรี",
                                     "ปริญญาโท", 
                                     "ปริญญาเอก",
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
                                     "รัฐวิสหกิจ",
                                     "Other")),
             # Conditional input for "Other" Occupation
             conditionalPanel(
               condition = "input.occupation == 'Other'",
               textInput("other_occupation", "Please Specify Occupation:", "")
             )
             
      ),
      column(4,
             selectInput("comobid", "Comobid",
                         choices = c("DM", "HT", "Gout",
                                     "CKD", "Thyroid", "DLD",
                                     "DM HT", "DM DLD", "DM HT DLD",
                                     "HT Gout", "None", "Other")),
             conditionalPanel(
               condition = "input.comobid == 'Other'",
               textInput("other_comobid", "Please Specify Comobid:", "")
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
             selectInput("medfinancial", "Medical Financial",
                         choices = c("จ่ายตรง",
                                     "จ่ายเอง", 
                                     "บัตรทอง",
                                     "เบิกได้", 
                                     "ประกันสังคม",
                                     "ประกันชีวิต")),
             textInput("daystart", label = "First day", placeholder = "dd-mm-yyyy"),
             hr(),
             actionButton("save", "Save Data"), # Save button
             verbatimTextOutput("save_status")  # Save status
      )
    )
  ),
#--------------------------- Visit Form UI -------------------
  tabPanel(
    "Visit Form",
    fluidRow(
      column(4,
             textInput("hn_visit", "Patient Code (HN):", ""),  # User-provided HN
             actionButton("check_hn_visit", "Check HN"),   # Button to check HN
             tags$div(
               tags$h4("Patient Name:",
                       style = "display: inline-block; margin-right: 10px;"), # Inline label
               textOutput("patient_name", inline = TRUE)                     # Inline output
             ),
             tags$hr(),
            
             #textInput("num_visit", "Number of Visits:"), # User-provided Number of visits
             h4("Number of Visits:"),
             fluidRow(
               column(4, 
                      verbatimTextOutput("num_visit")),
               column(4,
                      textOutput("visit_position"))
             ),
             
             actionButton("prev_visit", "Previous Visit"),
             actionButton("next_visit", "Next Visit"),
             actionButton("add_visit", "Add Visit"),
             verbatimTextOutput("visit_data"),
             textInput("visit_date", label = "Date", placeholder = "dd-mm-yyyy"), # Provided date
             textAreaInput("patient_note", "Patient Notes:", "", rows = 10, 
                           placeholder = "Are there any specific questions or concerns you want to discuss with the doctor?"),
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
               inputId = "dypsnea",
               label = "6. Dypsnea",
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
             
            
      ),
      column(4,
             h3("Lab Results:"),
             column(6,
                    textInput("cr", "Cr:", ""),
                    textInput("na", "Na:", ""),
                    textInput("fbs", "FBS:", ""),
                    textInput("hba1c", "HBA1C:", ""),
                    textInput("cho", "CHO:", "")),
             column(6,
                    textInput("ldl", "LDL:", ""),
                    textInput("tg", "TG:", ""),
                    textInput("hdl", "HDL:", ""),
                    textInput("ast", "AST:", ""),
                    textInput("alt", "ALT:", "")),
      )
    ),
    hr(),
    fluidRow(
    column(4,
           #CC and PI Section
           radioButtons("cc", "CC:", 
                        choices = c("Follow-up Visit" = "follow_up", 
                                    "Early Visit" = "early_visit", 
                                    "Late Visit" = "late_visit"), 
                        inline = TRUE),
           conditionalPanel(
             condition = "input.cc == 'early_visit'",
             textAreaInput("cc_early_visit", "Please Specify Reason:", "", rows = 3)
           ),
           conditionalPanel(
             condition = "input.cc == 'late_visit'",
             textAreaInput("cc_late_visit", "Please Specify Reason:", "", rows = 3)
           ),
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
           checkboxGroupInput("medication_adherence", "Medication Adherence:", 
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
              column(6,textInput("waist", "Waist (cm):", "")),
                   ),
           fluidRow(
              column(6,textInput("height", "Height (cm):", "")),
              column(6,textInput("weight", "Weight (kg):", "")),
                   ),
           fluidRow(
             column(6,
              tags$label("Your BMI:"),
              textOutput("bmi_text"),
              h6("BMI Target: 18.5-23.0 kg/m2")
                   ))
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
           h4("Diagnosis:"),
           selectInput("diagnosis", "HT with",
                       list("DM", "Gout", "CKD", "Thyroid", "DLD",
                                   "None", "Other"), 
                       multiple = TRUE ),
           conditionalPanel(
             condition = "input.diagnosis.includes('Other')",  # Check if 'Other' is in the array of selected values
             textAreaInput("other_diagnosis", "Please Specify Diagnosis:", "", rows = 6)
           ),
           # Evaluate patient Activity
           fluidRow(
             column(10,
                    h4("Scores and BP Monitoring:")),
             column(5,
                    numericInput("bp_control_score", "BP Control Score:", value = 0, min = 0, max = 3),
                    numericInput("weight_control_score", "Weight Control Score:", value = 0, min = 0, max = 5)
                    ),
             column(7,
                    numericInput("self_care_score", "Self-Care Behavior Score:", value = 0, min = 0, max = 3),
                    numericInput("home_bp_score", "BP Measurement at Home Score:", value = 0, min = 0, max = 5)
                    ),
             column(6,
                    radioButtons(inputId = "hbpm_target", 
                                 label = "HBPM According to Target", 
                                 choices = c("Yes", "No", "Missed"), 
                                 inline = TRUE)
             )
           )
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
           
           # Date  Input
           textInput("follow_up_date", label = "Date:", placeholder = "dd-mm-yyyy"),
           
           #Lab Tests Section
           h4("Laboratory Tests:"),
           checkboxGroupInput(
             inputId = "lab_tests",
             label = NULL,
             choices = c(
               "UA (Urinalysis)" = "ua_test",
               "BUN (Blood Urea Nitrogen)" = "bun_test",
               "Cr (Creatinine)" = "cr_test",
               "HbA1C" = "hba1c_test",
               "FBS (Fasting Blood Sugar)" = "fbs_test",
               "Lipid Profiles" = "lipid_profiles_test",
               "CXR (Chest X-ray)" = "cxr_test",
               "ECG (Electrocardiogram)" = "ecg_test",
               "Other" = "other_test"
             ),
             inline = FALSE
           ),
           selectInput("complication", "Complication",
                       choices = c("Stroke", "Cardio", "Kidney",
                                   "Eye", "Other")),
           conditionalPanel(
             condition = "input.complication == 'Other'",
             textInput("other_complication", "Please Specify Complication:", "")
           ),
           )
           
    ),
    hr(),
    fluidRow(
      column(6,
             h3("Home Medication"),
             column(10, 
                    selectInput("precriptionadjust", "Prescription Adjusted",
                         choices = c("Off Medication", "Decrease", "Add",
                                     "Same", "Change : Complication"))),
             fluidRow(
               column(12,
               h4("Diuretics :"),
               column(6, uiOutput("medication_ui_diuretics")),
               column(6, actionButton("add_medication_diuretics", "Add Diuretics")),
               column(4, uiOutput("remove_ui_diuretics"))
             )),
             fluidRow(
               column(12,
               h4("ACEIs :"),
               column(6, uiOutput("medication_ui_aceis")),
               column(6, actionButton("add_medication_aceis", "Add ACEIs"))
             )),
             fluidRow(
               column(12,
               h4("ARBs :"),
               column(6, uiOutput("medication_ui_arbs")),
               column(6, actionButton("add_medication_arbs", "Add ARBs"))
             )),
             fluidRow(
               column(12,
               h4("CCBs :"),
               column(6, uiOutput("medication_ui_ccbs")),
               column(6, actionButton("add_medication_ccbs", "Add CCBs"))
             )
             )),
      column(6,
             h3(" "),
             fluidRow(
               h4("Beta blockers :"),
               column(6, uiOutput("medication_ui_beta_blockers")),
               column(6, actionButton("add_medication_beta_blockers", "Add Beta blockers"))
             ),
             fluidRow(
               h4("DM :"),
               column(6, uiOutput("medication_ui_oad")),
               column(6, actionButton("add_medication_oad", "Add DM"))
             ),
             fluidRow(
               h4("Statin :"),
               column(6, uiOutput("medication_ui_statin")),
               column(6, actionButton("add_medication_statin", "Add Statin"))
             ),
             fluidRow(
               h4("Other :"),
               column(6, uiOutput("medication_ui_other")),
               column(6, actionButton("add_medication_other", "Add Other"))
             )
      )
          
             
    ),
    fluidRow(
      actionButton("save_visit", "Save Visit")
    )
  ),

#------------------------Patient Dashboard UI -------------------------------
  tabPanel(
    "Patient Dashboard",
    fluidRow(
      column(4,
             textInput(inputId = "hn_dashboard", "Patient Code (HN):", ""),  # User-provided HN
             actionButton("check_hn_dashboard", "Check HN"),   # Button to check HN
             tags$div(
               tags$h4("Patient Name:",
                       style = "display: inline-block; margin-right: 10px;"), # Inline label
               textOutput("patient_name_dashboard", inline = TRUE)                     # Inline output
             )
      ),
      fluidRow(
        column(6,
               plotOutput("bpPlot")
        ),
        column(6,
               plotlyOutput("pulsePlot")
        )
      )
)
),

tabPanel("Clinic Dashboard",
         
)
)



#--------------- Server --------------------

server <- function(input, output, session) {
  
  
#----------- Patient Info Server --------------
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
      dob_input <- as.Date(input$dob, tryFormats = c("%d-%m-%Y", "%d/%m/%Y", "%Y-%m-%d"))
      
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
    
    # Format the DOB input
    formatted_dob <- if (!is.null(input$dob) && input$dob != "") {
      tryCatch({
        dob_input <- as.Date(input$dob, tryFormats = c("%d-%m-%Y", "%d/%m/%Y", "%Y-%m-%d"))
        if (!is.na(dob_input)) {
          format(dob_input, "%d/%m/%Y")  # Convert to dd/mm/yyyy format
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
      all_data <- read.csv(file_path, stringsAsFactors = FALSE)
      # Explicitly parse the DOB column to ensure it's in the correct date format
      all_data$dob <- as.Date(all_data$dob, format = "%d/%m/%Y")
      
      hn_to_search <- toupper(input$hn_register)  # Convert input HN to uppercase for search
      
      result <- all_data %>% filter(hn == hn_to_search)
      
      if (nrow(result) > 0) {
        updateSelectInput(session, "titles", selected = result$titles[1])
        updateTextInput(session, "name", value = result$name[1])
        updateDateInput(session, "dob", value = result$dob[1])
        updateTextInput(session, "phone", value = result$phone[1])
        updateTextInput(session, "phone2", value = result$phone2[1])
        updateSelectInput(session, "gender", selected = result$gender[1])
        updateTextInput(session, "address", value = result$address[1])
        updateSelectInput(session, "province", selected = result$province[1])
        updateSelectInput(session, "amphoe", selected = result$amphoe[1])
        updateSelectInput(session, "education", selected = result$education[1])
        updateSelectInput(session, "occupation", selected = result$occupation[1])
        updateTextInput(session, "comobid", value = result$comobid[1]) 
        updateTextInput(session, "ekg", value = result$ekg[1])                
        updateTextInput(session, "echo", value = result$echo[1])              
        updateTextInput(session, "eye", value = result$eye[1])     
        updateTextAreaInput(session, "drugallergy", value = result$drugallergy[1]) 
        updateSelectInput(session, "caregiver", selected = result$caregiver[1])
        updateSelectInput(session, "hbpm", selected = result$hbpm[1])
        updateSelectInput(session, "medfinancial", selected = result$medfinancial[1])
        updateTextInput(session, "daystart", value = result$daystart[1])
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
    
    # Format the DOB input
    formatted_dob <- if (!is.null(input$dob) && input$dob != "") {
      tryCatch({
        dob_input <- as.Date(input$dob, tryFormats = c("%d-%m-%Y", "%d/%m/%Y", "%Y-%m-%d"))
        if (!is.na(dob_input)) {
          format(dob_input, "%d/%m/%Y")  # Convert to dd/mm/yyyy format
        } else {
          NA  # Invalid date
        }
      }, error = function(e) {
        NA  # Handle errors in date parsing
      })
    } else {
      NA  # Empty DOB
    }
    
    # Calculate Age for saving
    calculated_age <- if (!is.null(input$dob) && input$dob != "") {
      tryCatch({
        dob_input <- as.Date(input$dob, tryFormats = c("%d-%m-%Y", "%d/%m/%Y", "%Y-%m-%d"))
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
      all_data$phone <- as.character(all_data$phone)
      all_data$phone2 <- as.character(all_data$phone2)
      
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
      comobid <- if (input$comobid == "Other") input$other_comobid else input$comobid
      caregiver <- if (input$caregiver == "Other") input$other_caregiver else input$caregiver
     
      
      
      user_data <- data.frame(
        no = nrow(all_data) + 1,  # Maintain sequential No.
        hn = hn_to_save,         # Save HN in uppercase
        titles = as.character(input$titles),
        name = as.character(input$name),
        dob = formatted_dob,
        phone = as.character(input$phone),
        phone2 = as.character(input$phone2),
        gender = as.character(input$gender),
        age = calculated_age,
        address = as.character(input$address),
        province = as.character(input$province),
        amphoe = as.character(input$amphoe),
        education = as.character(input$education),
        occupation = as.character(input$occupation),
        comobid = as.character(input$comobid),
        ekg = as.character(input$ekg),
        echo = as.character(input$echo),
        eye = as.character(input$eye),
        drugallergy = as.character(input$drugallergy),
        caregiver = as.character(input$caregiver),
        hbpm = as.character(input$hbpm),
        medfinancial = input$medfinancial,
        daystart = as.character(input$daystart),
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
        no = 1,
        hn = toupper(input$hn_register),  # Convert HN to uppercase for saving
        titles = as.character(input$titles),
        name = as.character(input$name),
        dob = formatted_dob,
        phone = as.character(input$phone),
        phone2 = as.character(input$phone2),
        gender = as.character(input$gender),
        age = calculated_age,
        address = as.character(input$address),
        province = as.character(input$province),
        amphoe = as.character(input$amphoe),
        education = as.character(input$education),
        occupation = as.character(input$occupation),
        comobid = as.character(input$comobid),
        ekg = as.character(input$ekg),
        echo = as.character(input$echo),
        eye = as.character(input$eye),
        drugallergy = as.character(input$drugallergy),
        caregiver = as.character(input$caregiver),
        hbpm = as.character(input$hbpm),
        medfinancial = input$medfinancial,
        daystart = as.character(input$daystart),
        stringsAsFactors = FALSE
      )
      write.csv(user_data, file_path, row.names = FALSE)
      output$save_status <- renderText("Data saved successfully!")
    }
  })

#-------------------  Visit Form Server  ----------------------------------
  
  # 1. Retrieve Patient Information by HN
  observeEvent(input$check_hn_visit, {  
    # File path to patient data
    file_path <- "patient_data.csv"
    
    # Check if the file exists
    if (!file.exists(file_path)) {
      output$patient_name <- renderText("No data file exists. Please upload the file.")
      return()
    }
    
    # Load patient data
    all_data <- read.csv(file_path, stringsAsFactors = FALSE)
    
    # Ensure required columns exist
    required_columns <- c("hn", "name")  # Adjusted to lowercase
    if (!all(required_columns %in% colnames(all_data))) {
      output$patient_name <- renderText("Invalid file format. Ensure columns 'hn' and 'name' are present.")
      return()
    }
    
    # Normalize HN input and dataset for case-insensitivity
    hn_to_search <- toupper(input$hn_visit)
    all_data$hn <- toupper(all_data$hn)  # Convert 'hn' column to uppercase
    
    # Search for the HN in the dataset
    result <- all_data[all_data$hn == hn_to_search, ]
    
    # Safely check if any rows are found
    if (nrow(result) > 0) {
      output$patient_name <- renderText(result$name[1])  # Display patient name
    } else {
      output$patient_name <- renderText("Patient not found.")
    }
  })
  
  output$visit_position <- renderText({
    visits <- filtered_visits()
    index <- current_visit_index()
    total <- nrow(visits)
    
    if (total > 0) {
      paste("Visit", index, "of", total)
    } else {
      "No visit records"
    }
  })
  
  # 2. Calculate BMI
  output$bmi_text <- renderText({
    if (input$height != "" && input$weight != "") {
      tryCatch({
        height_m <- as.numeric(input$height) / 100  # Convert height to meters
        weight_kg <- as.numeric(input$weight)
        bmi <- round(weight_kg / (height_m^2), 1)  # Calculate BMI
        paste(bmi, "kg/m²")
      }, error = function(e) {
        "Invalid input"
      })
    } else {
      ""
    }
  })
  
  observe({
    if (input$height != "" && input$weight != "") {
      tryCatch({
        height_m <- as.numeric(input$height) / 100  # Convert height to meters
        weight_kg <- as.numeric(input$weight)
        bmi <- weight_kg / (height_m^2)  # Calculate BMI
        
        # Determine Weight Control Score
        score <- ifelse(bmi < 24, 3,
                        ifelse(bmi < 25, 2,
                               ifelse(bmi < 26, 1, 0)))
        
        # Update the Weight Control Score input
        updateNumericInput(session, "weight_control_score", value = score)
        
      }, error = function(e) {
        # If there's an error, reset the score
        updateNumericInput(session, "weight_control_score", value = 0)
      })
    } else {
      # Reset the score if inputs are missing
      updateNumericInput(session, "weight_control_score", value = 0)
    }
  })
  
  
  
  # 3. Scores and BP Monitoring
  
  # Automatically calculate and update Self-Care Behavior Score
  observe({
    # Count the number of selected choices in Medication Adherence
    adherence_score <- length(input$medication_adherence)
    
    # Update the Self-Care Behavior Score numeric input automatically
    updateNumericInput(session, "self_care_score", value = adherence_score)
  })
  
  # Automatically calculate BP Control Score based on Sys BP
  observe({
    bp_sys <- as.numeric(input$bp_sys)
    
    # Determine the BP Control Score based on the rules
    if (is.na(bp_sys)) {
      score <- 0  # Default to 0 if input is invalid
    } else if (bp_sys <= 140) {
      score <- 3
    } else if (bp_sys >= 141 && bp_sys <= 169) {
      score <- 2
    } else if (bp_sys >= 170 && bp_sys <= 179) {
      score <- 1
    } else if (bp_sys >= 180) {
      score <- 0
    }
    
    # Update the BP Control Score in the numericInput
    updateNumericInput(session, "bp_control_score", value = score)
  })
  
  
  
 # 4. medicine
  
  getMedicationList <- function(category_list) {
    meds <- lapply(names(category_list$data), function(id) {
      med <- input[[id]]
      qty <- input[[category_list$data[[id]]]]
      if (!is.null(med) && !is.null(qty)) paste0(med, " (", qty, ")") else NULL
    })
    paste(unlist(meds), collapse = "; ")
  }
  
  # Reactive lists for dynamic medications
  medication_list_diuretics <- reactiveValues(data = list())
  medication_list_aceis <- reactiveValues(data = list())
  medication_list_arbs <- reactiveValues(data = list())
  medication_list_ccbs <- reactiveValues(data = list())
  medication_list_beta_blockers <- reactiveValues(data = list())
  medication_list_oad <- reactiveValues(data = list())
  medication_list_statin <- reactiveValues(data = list())
  medication_list_other <- reactiveValues(data = list())
  
  # Add medications dynamically for each category
  addMedication <- function(category_list, button_id, category_name) {
    # Track the number of items added (to ensure unique IDs)
    counter <- reactiveVal(0)
    
    observeEvent(input[[button_id]], {
      # Increment the counter for a new unique ID
      current_count <- counter() + 1
      counter(current_count)
      
      # Create new unique IDs for the medication and quantity
      new_id <- paste0(category_name, "_", current_count)
      new_qty_id <- paste0("quantity_", category_name, "_", current_count)
      
      # Add the new IDs to the reactive list
      category_list$data[[new_id]] <- new_qty_id
    })
  }
  
  # Apply Add functionality for each category
  addMedication(medication_list_diuretics, "add_medication_diuretics", "diuretics")
  addMedication(medication_list_aceis, "add_medication_aceis", "aceis")
  addMedication(medication_list_arbs, "add_medication_arbs", "arbs")
  addMedication(medication_list_ccbs, "add_medication_ccbs", "ccbs")
  addMedication(medication_list_beta_blockers, "add_medication_beta_blockers", "beta_blockers")
  addMedication(medication_list_oad, "add_medication_oad", "oad")
  addMedication(medication_list_statin, "add_medication_statin", "statin")
  addMedication(medication_list_other, "add_medication_other", "other")
  
  # Render UI for each medication category
  renderMedicationUI <- function(category_list, category_name) {
    renderUI({
      medication_ui <- lapply(names(category_list$data), function(id) {
        fluidRow(
          column(6, selectInput(
            inputId = id,
            label = paste("Select", category_name, ":"),
            choices = switch(category_name,
                             "Diuretics" = c("HCTZ (25)", "HCTZ (50)"),
                             "ACEIs" = c("Enalapril (5)", "Enalapril (20)"),
                             "ARBs" = c("Losartan (50)", "Losartan (100)"),
                             "CCBs" = c("Amlodipine (5)", "Amlodipine (10)", "Madiplot (20)", "Diltiazem (30)", "Diltiazem (60)"),
                             "Beta Blockers" = c("Atenolol (25)", "Atenolol (50)", "Atenolol (100)",
                                                 "Carvedilol (6.25)", "Carvedilol (12.5)", "Carvedilol (25)",
                                                 "Metoprolol (100)", "Propranolol (10)", "Propranolol (40)"),
                             "OAD" = c("Metformin (500)", "Metformin (850)", "Metformin (1000)",
                                       "Glipizide (5)"),
                             "Statin" = c("Atorvastatin (20)", "Atorvastatin (40)",
                                          "Simvastatin (10)", "Simvastatin (20)", "Simvastatin (40)"),
                             "Other" = c("Azilsartan (40)", "Hydralazine (25)", "Doxazosin (2)", "Methyldopa (250)")
            )
          )),
          column(4, selectInput(
            inputId = category_list$data[[id]],
            label = "Quantity:",
            choices = c("1*1", "1*2", "1*3")
          )),
          column(2, actionButton(
            inputId = paste0("remove_", id),
            label = "Remove"
          ))
        )
      })
      do.call(tagList, medication_ui)
    })
  }
  
  # Handle Remove functionality dynamically
  observeRemoveButtons <- function(category_list) {
    observe({
      req(names(category_list$data))  # Ensure there are medications to remove
      lapply(names(category_list$data), function(id) {
        observeEvent(input[[paste0("remove_", id)]], {
          # Remove the corresponding medication entry from the reactiveValues list
          category_list$data[[id]] <- NULL
        })
      })
    })
  }
  
  # Call Remove observer for each category
  observeRemoveButtons(medication_list_diuretics)
  observeRemoveButtons(medication_list_aceis)
  observeRemoveButtons(medication_list_arbs)
  observeRemoveButtons(medication_list_ccbs)
  observeRemoveButtons(medication_list_beta_blockers)
  observeRemoveButtons(medication_list_oad)
  observeRemoveButtons(medication_list_statin)
  observeRemoveButtons(medication_list_other)
  
  # Render the UI for each category
  output$medication_ui_diuretics <- renderMedicationUI(medication_list_diuretics, "Diuretics")
  output$medication_ui_aceis <- renderMedicationUI(medication_list_aceis, "ACEIs")
  output$medication_ui_arbs <- renderMedicationUI(medication_list_arbs, "ARBs")
  output$medication_ui_ccbs <- renderMedicationUI(medication_list_ccbs, "CCBs")
  output$medication_ui_beta_blockers <- renderMedicationUI(medication_list_beta_blockers, "Beta Blockers")
  output$medication_ui_oad <- renderMedicationUI(medication_list_oad, "OAD")
  output$medication_ui_statin <- renderMedicationUI(medication_list_statin, "Statin")
  output$medication_ui_other <- renderMedicationUI(medication_list_other, "Other")
 
  
  observe({
    visits <- filtered_visits()
    num_visits <- nrow(visits)
    output$num_visit <- renderText({
      paste0(num_visits)  # Render the number of visits as plain text
    })
  })
  
  display_visit_data <- function(visit) {
    output$visit_data <- renderText({
      paste0(
        "▶️ Visit Date: ", visit$visit_date, "\n",
        "📝 Patient Note: ", visit$patient_note, "\n",
        "📌 Status: ", visit$patient_status, "\n\n",
        
        "💊 Medications:\n",
        "- Diuretics: ", visit$diuretics, "\n",
        "- ACEIs: ", visit$aceis, "\n",
        "- ARBs: ", visit$arbs, "\n",
        "- CCBs: ", visit$ccbs, "\n",
        "- Beta Blockers: ", visit$beta_blockers, "\n",
        "- OAD: ", visit$oad, "\n",
        "- Statin: ", visit$statin, "\n",
        "- Other Medications: ", visit$other_medications, "\n\n",
        
        "🩺 Clinical Info:\n",
        "- BP: ", visit$bp_sys, "/", visit$bp_dia, " mmHg\n",
        "- Pulse: ", visit$pulse, " bpm\n",
        "- Waist: ", visit$waist, " cm\n",
        "- Height: ", visit$height, " cm\n",
        "- Weight: ", visit$weight, " kg\n",
        
        "📊 Scores:\n",
        "- BP Control Score: ", visit$bp_control_score, "\n",
        "- Weight Control Score: ", visit$weight_control_score, "\n",
        "- Self-Care Behavior Score: ", visit$self_care_score, "\n",
        "- Home BP Score: ", visit$home_bp_score, "\n\n",
        
        "🧪 Lab Results:\n",
        "- Cr: ", visit$cr, "\n",
        "- Na: ", visit$na, "\n",
        "- FBS: ", visit$fbs, "\n",
        "- HbA1C: ", visit$hba1c, "\n",
        "- CHO: ", visit$cho, "\n",
        "- LDL: ", visit$ldl, "\n",
        "- TG: ", visit$tg, "\n",
        "- HDL: ", visit$hdl, "\n",
        "- AST: ", visit$ast, "\n",
        "- ALT: ", visit$alt, "\n"
      )
    })
  }
  
  # Paths to data files
  patient_data_file <- "patient_data.csv"
  visit_data_file <- "visit_data.csv"
  
  # Reactive values to store filtered visits and the current visit index
  filtered_visits <- reactiveVal(data.frame())
  current_visit_index <- reactiveVal(1)
  
  # Load and initialize patient/visit data
  observeEvent(input$check_hn_visit, {
    if (!file.exists(patient_data_file) || !file.exists(visit_data_file)) {
      showNotification("Required data files are missing.", type = "error")
      return()
    }
    
    # Load patient data
    patient_data <- read.csv(patient_data_file, stringsAsFactors = FALSE)
    patient_data$hn <- toupper(patient_data$hn)
    hn_to_search <- toupper(input$hn_visit)
    
    # Fetch patient name
    patient_name <- patient_data$name[patient_data$hn == hn_to_search]
    if (length(patient_name) == 0) {
      output$patient_name <- renderText("Patient not found.")
      output$num_visit <- renderText("1")  # Default to 1 if no visits exist
      filtered_visits(data.frame())
      return()
    } else {
      output$patient_name <- renderText(patient_name[1])
    }
    
    # Load visit data
    all_visits <- read.csv(visit_data_file, stringsAsFactors = FALSE)
    all_visits$hn <- toupper(all_visits$hn)
    
    # Filter visits for this patient
    visits <- all_visits[all_visits$hn == hn_to_search, ]
    
    # Optional: sort by date descending to get the most recent visit first
    visits <- visits[order(as.Date(visits$visit_date, format = "%d-%m-%Y"), decreasing = TRUE), ]
    
    # Store filtered visits and initialize index
    filtered_visits(visits)
    
    if (nrow(visits) > 0) {
      current_visit_index(1)  # start with most recent visit
      output$num_visit <- renderText(nrow(visits))
      display_visit_data(visits[1, ])  # show first (most recent)
    } else {
      current_visit_index(1)
      output$num_visit <- renderText("1")
      output$visit_data <- renderText("No visit records found. Start filling visit details.")
    }
  })
  

  
  # Navigate to the previous visit
  observeEvent(input$prev_visit, {
    visits <- filtered_visits()
    index <- current_visit_index()
    
    if (index > 1) {
      current_visit_index(index - 1)
      if (index - 1 <= nrow(visits)) {
        display_visit_data(visits[index - 1, ])
      } else {
        output$visit_data <- renderText("No information for this visit. Start filling details.")
      }
    } else {
      showNotification("This is the first visit.", type = "warning")
    }
  })
  
  # Navigate to the next visit
  observeEvent(input$next_visit, {
    visits <- filtered_visits()
    index <- current_visit_index()
    
    if (index < nrow(visits) + 1) {
      current_visit_index(index + 1)
      if (index + 1 <= nrow(visits)) {
        display_visit_data(visits[index + 1, ])
      } else {
        output$visit_data <- renderText("No information for this visit. Start filling details.")
      }
    } else {
      showNotification("This is the last visit.", type = "warning")
    }
  })
  
  # Save the current visit data
  observeEvent(input$save_visit, {
    if (is.null(input$hn_visit) || input$hn_visit == "" || is.null(input$visit_date) || input$visit_date == "") {
      showNotification("Please provide both Patient Code (HN) and Date before saving.", type = "error")
      return()
    }
    
    # Load patient data
    patient_data <- read.csv(patient_data_file, stringsAsFactors = FALSE)
    patient_data$hn <- toupper(patient_data$hn)
    hn_to_search <- toupper(input$hn_visit)
    
    # Fetch patient name
    patient_name <- patient_data$name[patient_data$hn == hn_to_search]
    if (length(patient_name) == 0) {
      showNotification("Patient name not found. Ensure the HN exists in the patient data file.", type = "error")
      return()
    }
    
    # termine visit_number
    visit_data_file <- "visit_data.csv"
    
    if (file.exists(visit_data_file)) {
      existing_visits <- read.csv(visit_data_file, stringsAsFactors = FALSE)
      
      if ("hn" %in% colnames(existing_visits)) {
        existing_visits$hn <- toupper(existing_visits$hn)
        visit_number <- existing_visits %>%
          filter(hn == hn_to_search) %>%
          nrow() + 1
      } else {
        visit_number <- 1
      }
    } else {
      visit_number <- 1
    }
    
    # Create a new row with visit data
    new_data <- data.frame(
      visit_number = visit_number,
      hn = hn_to_search,
      name = patient_name[1],
      visit_date = input$visit_date,
      patient_note = input$patient_note,
      patient_status = input$pateintstatus,
      refer_hospital_details = ifelse(input$pateintstatus == "refer_hospital", input$refer_hospital_details, ""),
      consult_opd_details = ifelse(input$pateintstatus == "consult_opd", input$consult_opd_details, ""),
      
      #Patient Symptom Checklist:
      chest_tightness = input$chest_tightness,
      nervous_system = input$nervous_system,
      urinal_abnormal = input$urinal_abnormal,
      headache = input$headache,
      dizziness = input$dizziness,
      dypsnea = input$dypsnea,
      leg_swelling = input$leg_swelling,
      face_swelling = input$face_swelling,
      
      #Lab Results:
      cr = input$cr,
      na = input$na,
      fbs = input$fbs,
      hba1c = input$hba1c,
      cho = input$cho,
      ldl = input$ldl,
      tg = input$tg,
      hdl = input$hdl,
      ast = input$ast,
      alt = input$alt,
      
      #CC
      cc = input$cc,
      cc_early_visit = ifelse(input$cc == "early_visit", input$cc_early_visit, ""),
      cc_late_visit = ifelse(input$cc == "late_visit", input$cc_late_visit, ""),
      #PI
      pi = input$pi,
      pi_abnormal = ifelse(input$pi == "abnormal", input$pi_abnormal, ""),
      #Medical adherence
      medication_adherence = paste(input$medication_adherence, collapse = "; "),
      allergic_history = input$allergic_history,
      
      #Blood Pressure/BMI
      bp_sys = input$bp_sys,
      bp_dia = input$bp_dia,
      pulse = input$pulse,
      waist = input$waist,
      height = input$height,
      weight = input$weight,
      
      # Physical Examination:
      heent = input$heent,
      heent_abnormal = ifelse(input$heent == "abnormal", input$heent_abnormal, ""),
      heart = input$heart,
      heart_abnormal = ifelse(input$heart == "abnormal", input$heart_abnormal, ""),
      lungs = input$lungs,
      lungs_abnormal = ifelse(input$lungs == "abnormal", input$lungs_abnormal, ""),
      abd = input$abd,
      abd_abnormal = ifelse(input$abd == "abnormal", input$abd_abnormal, ""),
      ext = input$ext,
      ext_abnormal = ifelse(input$ext == "abnormal", input$ext_abnormal, ""),
      ns = input$ns,
      ns_abnormal = ifelse(input$ns == "abnormal", input$ns_abnormal, ""),
      
      #diaganosis
      diagnosis = paste(input$diagnosis, collapse = "; "),
      other_diagnosis = ifelse("Other" %in% input$diagnosis, input$other_diagnosis, ""),
      
      #Score
      bp_control_score = input$bp_control_score,
      weight_control_score = input$weight_control_score,
      self_care_score = input$self_care_score,
      home_bp_score = input$home_bp_score,
      hbpm_target = input$hbpm_target,
      
      #Follow-up
      follow_up = input$follow_up,
      follow_up_date = input$follow_up_date,
      
      #Lab test next time
      lab_tests = paste(input$lab_tests, collapse = "; "),
      complication = input$complication,
      other_complication = ifelse(input$complication == "Other", input$other_complication, ""),
      precriptionadjust = input$precriptionadjust,
      
      # add dynamic medication data
      diuretics = getMedicationList(medication_list_diuretics),
      aceis = getMedicationList(medication_list_aceis),
      arbs = getMedicationList(medication_list_arbs),
      ccbs = getMedicationList(medication_list_ccbs),
      beta_blockers = getMedicationList(medication_list_beta_blockers),
      oad = getMedicationList(medication_list_oad),
      statin = getMedicationList(medication_list_statin),
      other_medications = getMedicationList(medication_list_other),
      
      stringsAsFactors = FALSE
    )
    
    # Append or update the visit data
    if (file.exists(visit_data_file)) {
      current_data <- read.csv(visit_data_file, stringsAsFactors = FALSE)
      updated_data <- rbind(current_data, new_data)
    } else {
      updated_data <- new_data
    }
    
    # Write updated data to the file
    write.csv(updated_data, visit_data_file, row.names = FALSE)
    showNotification("Visit data saved successfully.", type = "message")
  })
  
  #------------------------Patient Dashboard Server -------------------------------
  
  # Reactive expression: only run when button is pressed
  filtered_visits_patient_dashboard <- eventReactive(input$check_hn_dashboard, {
    req(input$hn_dashboard)
    
    data <- read.csv("visit_data.csv", stringsAsFactors = FALSE)
    data$hn <- toupper(trimws(data$hn))
    data$visit_date <- parse_date_time(data$visit_date, orders = c("dmy", "ymd", "mdy"))
    
    data$bp_sys <- as.numeric(data$bp_sys)
    data$bp_dia <- as.numeric(data$bp_dia)
    data$pulse <- as.numeric(data$pulse)
    
    data <- data[data$hn == toupper(input$hn_dashboard), ]
    data <- data[!is.na(data$visit_date), ]
    data
  })
  
  # Show patient name
  output$patient_name_dashboard <- renderText({
    data <- filtered_visits_patient_dashboard()
    req(data)
    
    if (nrow(data) == 0 || is.null(data$name[1])) {
      return("Not found")
    } else {
      return(data$name[1])
    }
  })
  
  # Blood Pressure Plot (after button is pressed)
  output$bpPlot <- renderPlot({
    data <- filtered_visits_patient_dashboard()
    req(data)
    req(nrow(data) > 0)
    
    bp_long <- data %>%
      select(visit_date, bp_sys, bp_dia) %>%
      pivot_longer(cols = c(bp_sys, bp_dia),
                   names_to = "bp_type",
                   values_to = "bp_value") %>%
      filter(!is.na(bp_value))
    
    ggplot(bp_long, aes(x = visit_date, y = bp_value, color = bp_type)) +
      geom_line() +
      geom_point(size = 2) +
      labs(x = "Date", y = "Blood Pressure (mmHg)", color = "Type") +
      theme_minimal()
  })
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
