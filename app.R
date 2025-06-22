
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
library(shinydashboard)

thailand <- read_csv("data/thailand_province_amphoe.csv")

ui <- navbarPage(
  "HT Clinic",
  theme = shinytheme("flatly"),
  tags$head(
    includeCSS("styles.css")
  ),
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
             textInput("dob", label = "Date of Birth", placeholder = "dd-mm-yyyy (พ.ศ.)"),
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
             # selectInput("status", "Status",
             #             choices = c("Single", 
             #                         "Marriage", 
             #                         "Divorced",
             #                         "Separated",
             #                         "Widowed",
             #                         "Other")),
             # conditionalPanel(
             #   condition = "input.status == 'Other'",
             #   textInput("other_status", "Please Specify Status:", "")
             # ),
             # selectInput("ethnicity", "Ethnicity",
             #             choices = c("Thai", "American", "British", "Indian", "Chinese", "Other")),
             # # Conditional input for "Other" Ethnicity
             # conditionalPanel(
             #   condition = "input.ethnicity == 'Other'",
             #   textInput("other_ethnicity", "Please Specify Ethnicity:", "")
             # ),
             # selectInput("nationality", "Nationality",
             #             choices = c("Thai", "American", "British", "Indian", "Chinese", "Other")),
             # # Conditional input for "Other" Nationality
             # conditionalPanel(
             #   condition = "input.nationality == 'Other'",
             #   textInput("other_nationality", "Please Specify Nationality:", "")
             # ),
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
             textInput("ekg", label = "Latest EKG", placeholder = "dd-mm-yyyy (พ.ศ.)"),
             textInput("echo", label = "Latest Echo", placeholder = "dd-mm-yyyy (พ.ศ.)"),
             textInput("eye", label = "Latest eye examination", placeholder = "dd-mm-yyyy (พ.ศ.)"),
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
                                     "ประกันชีวิต"
                         )),
             textInput("daystart", label = "First day", placeholder = "dd-mm-yyyy (พ.ศ.)"),
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
               uiOutput("patient_name_html")                     # Inline output
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
             verbatimTextOutput("visit_summary"),
             textInput("visit_date", label = "Date", placeholder = "dd-mm-yyyy"), # Provided date
             selectInput(
               inputId = "doctor_name",
               label = "Doctor's Name",
               choices = c("รศ.พญ.แพรว โคตรุฉิน", 
                           "ผศ.นพ.ฐปนวงศ์ มิตรสูงเนิน", 
                           "ผศ.นพ.สิทธิชัย คำใสย์", 
                           "Other"),
               selected = NULL
             ),
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
             conditionalPanel(
               condition = "input.nervous_system == 'yes'",
               textInput("chest_tightness_note", "Please provide details about the chest tightness:")
             ),
             
             # Question 2
             radioButtons(
               inputId = "nervous_system",
               label = "2. Abnormal Nervous System (e.g., facial drooping, weakness in limbs, numbness on one side, or slurred speech)",
               choices = c("No" = "no", "Yes" = "yes"),
               inline = TRUE
             ),
             conditionalPanel(
               condition = "input.nervous_system == 'yes'",
               textInput("nervous_system_note", "Please provide details about the nervous system:")
             ),
             
             # Question 3
             radioButtons(
               inputId = "urinal_abnormal",
               label = "3. Abnormal Urination (e.g., frequent urination, pain during urination, urine retention, or inability to urinate)",
               choices = c("No" = "no", "Yes" = "yes"),
               inline = TRUE
             ),
             conditionalPanel(
               condition = "input.urinal_abnormal == 'yes'",
               textInput("urinal_abnormal_note", "Please provide details about the abnormal urination:")
             ),
             
             # Question 4
             radioButtons(
               inputId = "headache",
               label = "4. Headache",
               choices = c("No" = "no", "Occasionally" = "sometimes", "Often" = "often"),
               inline = TRUE
             ),
             conditionalPanel(
               condition = "input.headache == 'sometimes' || input.headache == 'often'",
               textInput("headache_note", "Please provide details about the headache:")
             ),
             
             # Question 5
             radioButtons(
               inputId = "dizziness",
               label = "5. Dizziness",
               choices = c("No" = "no", "Occasionally" = "sometimes", "Often" = "often"),
               inline = TRUE
             ),
             conditionalPanel(
               condition = "input.dizziness == 'sometimes' || input.dizziness == 'often'",
               textInput("dizziness_note", "Please provide details about the dizziness:")
             ),
             
             # Question 6
             radioButtons(
               inputId = "dypsnea",
               label = "6. Dypsnea",
               choices = c("No" = "no", "Occasionally" = "sometimes", "Often" = "often"),
               inline = TRUE
             ),
             conditionalPanel(
               condition = "input.dypsnea == 'sometimes' || input.dypsnea == 'often'",
               textInput("dypsnea_note", "Please provide details about the dypsnea:")
             ),
             
             # Question 7
             radioButtons(
               inputId = "leg_swelling",
               label = "7. Leg Swelling",
               choices = c("No" = "no", "Occasionally" = "sometimes", "Often" = "often"),
               inline = TRUE
             ),
             conditionalPanel(
               condition = "input.leg_swelling == 'sometimes' || input.leg_swelling == 'often'",
               textInput("leg_swelling_note", "Please provide details about the leg swelling:")
             ),
             
             
             # Question 8
             radioButtons(
               inputId = "face_swelling",
               label = "8. Face Swelling",
               choices = c("No" = "no", "Occasionally" = "sometimes", "Often" = "often"),
               inline = TRUE
             ),
             conditionalPanel(
               condition = "input.face_swelling == 'sometimes' || input.face_swelling == 'often'",
               textInput("face_swelling_note", "Please provide details about the face swelling:")
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
                                      "Late Visit" = "late_visit",
                                      "Other" = "other"), 
                          inline = TRUE),
             conditionalPanel(
               condition = "input.cc == 'early_visit'",
               textAreaInput("cc_early_visit", "Please Specify Reason:", "", rows = 3)
             ),
             conditionalPanel(
               condition = "input.cc == 'late_visit'",
               textAreaInput("cc_late_visit", "Please Specify Reason:", "", rows = 3)
             ),
             conditionalPanel(
               condition = "input.cc == 'other'",
               textAreaInput("cc_other", "Please Specify:", "", rows = 3)
             ),
             radioButtons(
               inputId = "pi",
               label = "PI:",
               choices = c(
                 "Normal" = "normal", 
                 "Abnormal" = "abnormal",
                 "Other" = "other"
               ),
               inline = TRUE
             ),
             conditionalPanel(
               condition = "input.pi == 'abnormal'",
               textAreaInput("pi_abnormal", "Please Specify Symptoms:", "", rows = 3)
             ),
             conditionalPanel(
               condition = "input.pi == 'other'",
               textAreaInput("pi_other", "Please Specify:", "", rows = 3)
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
                      h6("BMI Target: 18.5-24.0 kg/m2")
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
                 "ECG (Electrocardiogram)" = "ecg_test"
                 
               ),
               inline = FALSE
             ),
             textAreaInput("other_lab_tests", "Please Specify Other Lab Tests:", "", rows = 3),
             checkboxGroupInput("complication", "Complication:", 
                                choices = c("Stroke" = "complication_stroke", 
                                            "Cardio MI" = "complication_cardio_mi",
                                            "CHF" = "complication_chf",
                                            "Kidney" = "complication_kidney",
                                            "Eye" = "complication_eye"),
                                inline = TRUE),
             textAreaInput("other_complication", "Please Specify Complication:", "", rows = 3)
             
             
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
               h4("Anti-platelet :"),
               column(6, uiOutput("medication_ui_anti_platelet")),
               column(6, actionButton("add_medication_anti_platelet", "Add Anti-platelet"))
             ),
             fluidRow(
               h4("Other :"),
               column(6, uiOutput("medication_ui_other")),
               column(6, actionButton("add_medication_other", "Add Other"))
             ),
             fluidRow(
               column(12,
                      h4("Single-pill Combination:"),
                      column(6, uiOutput("medication_ui_spc")),
                      column(6, actionButton("add_medication_spc", "Add Single-pill Combination"))
               )
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
             
      ),
      column(8,
             tags$div(
               tags$h4("Patient Name:",
                       style = "display: inline-block; margin-right: 10px;"), # Inline label
               uiOutput("patient_name_dashboard")                     # Inline output
             )
      )
    ),
    tags$br(),
    fluidRow(
      column(6,
             div(class = "plot-card",
                 plotlyOutput("bpPlot")
             )
      ),
      column(6,
             div(class = "plot-card",
                 plotlyOutput("pulsePlot")
             )
      )
    ),
    tags$br(),
    fluidRow(
      column(6,
             div(class = "plot-card",
                 plotlyOutput("bmiPlot")
             )
      ),
      column(6,
             div(class = "plot-card",
                 plotlyOutput("waistPlot")
             )
      )
    ),
    tags$br(),
    fluidRow(
      column(4,
             div(class = "plot-card",
                 plotlyOutput("saltycontrolPlot")
             )
      ),
      column(4,
             div(class = "plot-card",
                 plotlyOutput("alwaytakemedicinePlot")
             )
      ),
      column(4,
             div(class = "plot-card",
                 plotlyOutput("exercisePlot")
             )
      )
    ),
  ),
  
  #------------------------Patient Dashboard UI -------------------------------
  
  tabPanel("Clinic Dashboard",
           fluidRow(
             shiny::HTML("<br><br><center> <h1>Hypertension Clinic</h1> </center><br>"),
           ),
           fluidRow(
             tabsetPanel(
               tabPanel("Overview",
                        fluidRow(
                          column(1,),
                          column(10,
                                 fluidRow(
                                   column(3,
                                          div(class = "custom-value-box box-total",
                                              h3(textOutput("total_count_text")),
                                              p("Total Patients"),
                                              icon("users", class = "custom-icon")
                                          )
                                   ),
                                   column(3,
                                          div(class = "custom-value-box box-male",
                                              h3(textOutput("male_count_text")),
                                              p("Male Patients"),
                                              icon("mars", class = "custom-icon")
                                          )
                                   ),
                                   column(3,
                                          div(class = "custom-value-box box-female",
                                              h3(textOutput("female_count_text")),
                                              p("Female Patients"),
                                              icon("venus", class = "custom-icon")
                                          )
                                   ),
                                   column(3,
                                          div(class = "custom-value-box box-other",
                                              h3(textOutput("other_count_text")),
                                              p("Other Gender"),
                                              icon("genderless", class = "custom-icon")
                                          )
                                   )
                                 )
                          ),
                          column(1,)
                        )
                        ,
                        fluidRow(
                          column(6,
                                 plotlyOutput("all_gender")
                          ),
                          column(6,
                                 plotlyOutput("all_age")
                          )
                        ),
                        
                        
               ),
               tabPanel("Insight",
                        sidebarPanel(
                          
                          h3("Time"),
                          
                          
                          dateRangeInput('dateRange',
                                         label = 'Date range input: dd/mm/yyyy',
                                         format = "dd/mm/yyyy",        
                                         start = Sys.Date() - 2, 
                                         end = Sys.Date() + 2),
                          
                          radioButtons("insightMode", "Count Mode",
                                       choices = c("Unique Patients" = "unique", "All Visits" = "visits"),
                                       selected = "unique",
                                       inline = TRUE),
                          
                          actionButton("actionDT", "Filter", class = "btn btn-warning")
                        ),
                        
                        fluidRow(
                          column(1,
                          ),
                          column(11,
                                 valueBoxOutput("total_count_box_insight", width = 3),
                                 valueBoxOutput("male_count_box_insight", width = 3),
                                 valueBoxOutput("female_count_box_insight", width = 3),
                                 valueBoxOutput("other_count_box_insight", width = 3)
                          )
                        ),
                        fluidRow(
                          column(1,),
                          column(10,
                                 fluidRow(
                                   column(3,
                                          div(class = "custom-value-box box-total",
                                              h3(textOutput("total_count_box_insight")),
                                              p("Total Patients"),
                                              icon("users", class = "custom-icon")
                                          )
                                   ),
                                   column(3,
                                          div(class = "custom-value-box box-male",
                                              h3(textOutput("male_count_box_insight")),
                                              p("Male Patients"),
                                              icon("mars", class = "custom-icon")
                                          )
                                   ),
                                   column(3,
                                          div(class = "custom-value-box box-female",
                                              h3(textOutput("female_count_box_insight")),
                                              p("Female Patients"),
                                              icon("venus", class = "custom-icon")
                                          )
                                   ),
                                   column(3,
                                          div(class = "custom-value-box box-other",
                                              h3(textOutput("other_count_box_insight")),
                                              p("Other Gender"),
                                              icon("genderless", class = "custom-icon")
                                          )
                                   )
                                 )
                          ),
                          column(1,)
                        ),
                        tags$br(),
                        fluidRow(
                          column(6,
                                 div(class = "plot-card",
                                     plotlyOutput("sex_insight")
                                 )
                                 
                          ),
                          column(6,
                                 div(class = "plot-card",
                                     plotlyOutput("age_insight")
                                 )
                          )
                        ),
                        
                        fluidRow(
                          column(6,
                                 div(class = "plot-card",
                                     plotlyOutput("hbpm_insight")
                                 )
                          ),
                          column(6,
                                 div(class = "plot-card",
                                     plotlyOutput("num_visit_insight")
                                 )
                          )
                        ),
                        fluidRow(
                          column(6,
                                 div(class = "plot-card",
                                     plotlyOutput("bp_control_score_insight")
                                 )
                          ),
                          column(6,
                                 div(class = "plot-card",
                                     plotlyOutput("home_bp_score_insight")
                                 )
                          )
                        ),
                        
                        fluidRow(
                          column(6,
                                 div(class = "plot-card",
                                     plotlyOutput("weight_control_score_insight")
                                 )
                          ),
                          column(6,
                                 div(class = "plot-card",
                                     plotlyOutput("self_care_score_insight")
                                 )
                          )
                        ),
                        fluidRow(
                          column(4,
                                 div(class = "plot-card",
                                     plotlyOutput("salty_control_insight")
                                 )
                          ),
                          column(4,
                                 div(class = "plot-card",
                                     plotlyOutput("alway_take_medicine_insight")
                                 )
                          ),
                          column(4,
                                 div(class = "plot-card",
                                     plotlyOutput("exercise_insight")
                                 )
                          )
                        ),
                        
               )
             )
           )
           
  )
)



#--------------- Server --------------------

server <- function(input, output, session) {
  
  # Ensure the "data" folder exists
  if (!dir.exists("data")) dir.create("data")
  
  # Centralized file paths
  patient_data_file <- "data/patient_data.csv"
  visit_data_file <- "data/visit_data.csv"
  
  
  #----------- Patient Info Server --------------
  # Auto-incrementing No.
  output$no <- renderText({
    file_path <- patient_data_file
    
    if (!file.exists(file_path)) {
      return("No.: 1")
    }
    
    all_data <- read.csv(file_path, stringsAsFactors = FALSE)
    hn_to_search <- toupper(input$hn_register)
    
    if (!is.null(hn_to_search) && hn_to_search %in% all_data$hn) {
      row_no <- which(all_data$hn == hn_to_search)[1]  # Use first match if duplicated
      paste("No.:", row_no)
    } else {
      paste("No.:", nrow(all_data) + 1)
    }
  })
  
  
  
  
  # Automatically calculate and display age when Date of Birth is selected
  output$age_text <- renderText({
    if (is.null(input$dob) || input$dob == "") {
      return("")
    }
    
    tryCatch({
      # Parse the input date (assuming Buddhist Era)
      dob_input <- as.Date(input$dob, tryFormats = c("%d-%m-%Y", "%d/%m/%Y", "%Y-%m-%d"))
      
      if (is.na(dob_input)) {
        return("Invalid date format. Please use dd-mm-yyyy or dd/mm/yyyy.")
      }
      
      # Convert Buddhist Era to Gregorian calendar (subtract 543 years)
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
    file_path <- patient_data_file
    
    if (file.exists(file_path)) {
      all_data <- read.csv(file_path, stringsAsFactors = FALSE)
      # Parse the DOB column (already in Gregorian format in the file)
      all_data$dob <- as.Date(all_data$dob, format = "%d/%m/%Y")
      
      hn_to_search <- toupper(input$hn_register)
      result <- all_data %>% filter(hn == hn_to_search)
      
      if (nrow(result) > 0) {
        # Convert Gregorian dates back to Buddhist Era for display
        convert_gregorian_to_be <- function(gregorian_date_str) {
          if (is.na(gregorian_date_str) || gregorian_date_str == "") return("")
          
          tryCatch({
            gregorian_date <- as.Date(gregorian_date_str, format = "%d/%m/%Y")
            if (!is.na(gregorian_date)) {
              # Add 543 years to convert back to Buddhist Era for display
              be_date <- gregorian_date + years(543)
              format(be_date, "%d/%m/%Y")
            } else {
              ""
            }
          }, error = function(e) {
            ""
          })
        }
        
        # Update form fields with Buddhist Era dates for display
        updateSelectInput(session, "titles", selected = result$titles[1])
        updateTextInput(session, "name", value = result$name[1])
        
        # Convert DOB back to Buddhist Era for display
        be_dob <- convert_gregorian_to_be(format(result$dob[1], "%d/%m/%Y"))
        updateTextInput(session, "dob", value = be_dob)
        
        updateTextInput(session, "phone", value = result$phone[1])
        updateTextInput(session, "phone2", value = result$phone2[1])
        updateSelectInput(session, "gender", selected = result$gender[1])
        updateTextInput(session, "address", value = result$address[1])
        updateSelectInput(session, "province", selected = result$province[1])
        updateSelectInput(session, "amphoe", selected = result$amphoe[1])
        updateSelectInput(session, "education", selected = result$education[1])
        updateSelectInput(session, "occupation", selected = result$occupation[1])
        updateTextInput(session, "comobid", value = result$comobid[1])
        
        # Convert other dates back to Buddhist Era for display
        updateTextInput(session, "ekg", value = convert_gregorian_to_be(result$ekg[1]))
        updateTextInput(session, "echo", value = convert_gregorian_to_be(result$echo[1]))
        updateTextInput(session, "eye", value = convert_gregorian_to_be(result$eye[1]))
        
        updateTextAreaInput(session, "drugallergy", value = result$drugallergy[1])
        updateSelectInput(session, "caregiver", selected = result$caregiver[1])
        updateSelectInput(session, "hbpm", selected = result$hbpm[1])
        updateSelectInput(session, "medfinancial", selected = result$medfinancial[1])
        
        # Convert daystart back to Buddhist Era for display
        updateTextInput(session, "daystart", value = convert_gregorian_to_be(result$daystart[1]))
        
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
    file_path <- patient_data_file
    
    # Validate required inputs
    if (is.null(input$hn_register) || input$hn_register == "") {
      output$save_status <- renderText("Error: HN is required!")
      return()
    }
    
    # Convert Buddhist Era to Gregorian and format DOB
    convert_be_to_gregorian <- function(date_string) {
      if (is.null(date_string) || date_string == "") return("")
      
      tryCatch({
        # Parse the date (assuming it's in Buddhist Era)
        be_date <- as.Date(date_string, tryFormats = c("%d-%m-%Y", "%d/%m/%Y", "%Y-%m-%d"))
        
        if (!is.na(be_date)) {
          # Convert Buddhist Era to Gregorian (subtract 543 years)
          gregorian_date <- be_date - years(543)
          format(gregorian_date, "%d/%m/%Y")  # Convert to dd/mm/yyyy format
        } else {
          ""  # Invalid date
        }
      }, error = function(e) {
        ""  # Handle errors in date parsing
      })
    }
    
    formatted_dob <- convert_be_to_gregorian(input$dob)
    
    # Calculate Age for saving (using Gregorian date)
    calculated_age <- if (!is.null(input$dob) && input$dob != "" && formatted_dob != "") {
      tryCatch({
        # The formatted_dob is already in Gregorian, so use it directly
        gregorian_dob <- as.Date(formatted_dob, format = "%d/%m/%Y")
        if (!is.na(gregorian_dob)) {
          today <- Sys.Date()
          as.numeric(floor(difftime(today, gregorian_dob, units = "days") / 365.25))
        } else {
          ""  # Invalid date
        }
      }, error = function(e) {
        ""  # Handle errors in date parsing
      })
    } else {
      ""  # Empty DOB
    }
    
    # Safely get input values with defaults
    get_input_safe <- function(input_name, default = "") {
      value <- input[[input_name]]
      if (is.null(value)) return(default)
      return(as.character(value))
    }
    
    # Handle conditional inputs
    titles_value <- if (!is.null(input$titles) && input$titles == "Other") {
      get_input_safe("other_titles")
    } else {
      get_input_safe("titles")
    }
    
    gender_value <- if (!is.null(input$gender) && input$gender == "Other") {
      get_input_safe("gender_other")
    } else {
      get_input_safe("gender")
    }
    
    education_value <- if (!is.null(input$education) && input$education == "Other") {
      get_input_safe("other_education")
    } else {
      get_input_safe("education")
    }
    
    occupation_value <- if (!is.null(input$occupation) && input$occupation == "Other") {
      get_input_safe("other_occupation")
    } else {
      get_input_safe("occupation")
    }
    
    comobid_value <- if (!is.null(input$comobid) && input$comobid == "Other") {
      get_input_safe("other_comobid")
    } else {
      get_input_safe("comobid")
    }
    
    caregiver_value <- if (!is.null(input$caregiver) && input$caregiver == "Other") {
      get_input_safe("other_caregiver")
    } else {
      get_input_safe("caregiver")
    }
    
    # Convert daystart from Buddhist Era to Gregorian
    formatted_daystart <- convert_be_to_gregorian(input$daystart)
    formatted_ekg <- convert_be_to_gregorian(input$ekg)
    formatted_echo <- convert_be_to_gregorian(input$echo)
    formatted_eye <- convert_be_to_gregorian(input$eye)
    
    if (file.exists(file_path)) {
      # Load existing data
      all_data <- read.csv(file_path, stringsAsFactors = FALSE)
      all_data$phone <- as.character(all_data$phone)
      all_data$phone2 <- as.character(all_data$phone2)
      
      # Check if HN exists
      hn_to_save <- toupper(input$hn_register)  # Convert HN to uppercase for saving
      if (hn_to_save %in% all_data$hn) {
        # Replace the existing data for this HN
        all_data <- all_data %>%
          filter(hn != hn_to_save)  # Fixed: use lowercase 'hn' not 'HN'
      }
      
      # Create new user data
      user_data <- data.frame(
        no = nrow(all_data) + 1,  # Maintain sequential No.
        hn = hn_to_save,         # Save HN in uppercase
        titles = titles_value,
        name = get_input_safe("name"),
        dob = formatted_dob,
        phone = get_input_safe("phone"),
        phone2 = get_input_safe("phone2"),
        gender = gender_value,
        gender_other = ifelse(!is.null(input$gender) && input$gender == "Other", get_input_safe("gender_other"), ""),
        age = calculated_age,
        address = get_input_safe("address"),
        province = get_input_safe("province"),
        amphoe = get_input_safe("amphoe"),
        education = education_value,
        education_other = ifelse(!is.null(input$education) && input$education == "Other", get_input_safe("education_other"), ""),
        occupation = occupation_value,
        occupation_other = ifelse(!is.null(input$occupation) && input$occupation == "Other", get_input_safe("occupation_other"), ""),
        comobid = comobid_value,
        comobid_other = ifelse(!is.null(input$comobid) && input$comobid == "Other", get_input_safe("comobid_other"), ""),
        ekg = formatted_ekg,
        echo = formatted_echo,
        eye = formatted_eye,
        drugallergy = get_input_safe("drugallergy"),
        caregiver = caregiver_value,
        caregiver_other = ifelse(!is.null(input$caregiver) && input$caregiver == "Other", get_input_safe("caregiver_other"), ""),
        hbpm = get_input_safe("hbpm"),
        medfinancial = get_input_safe("medfinancial"),
        daystart = formatted_daystart,
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
        titles = titles_value,
        name = get_input_safe("name"),
        dob = formatted_dob,
        phone = get_input_safe("phone"),
        phone2 = get_input_safe("phone2"),
        gender = gender_value,
        gender_other = ifelse(!is.null(input$gender) && input$gender == "Other", get_input_safe("gender_other"), ""),
        age = calculated_age,
        address = get_input_safe("address"),
        province = get_input_safe("province"),
        amphoe = get_input_safe("amphoe"),
        education = education_value,
        education_other = ifelse(!is.null(input$education) && input$education == "Other", get_input_safe("education_other"), ""),
        occupation = occupation_value,
        occupation_other = ifelse(!is.null(input$occupation) && input$occupation == "Other", get_input_safe("occupation_other"), ""),
        comobid = comobid_value,
        comobid_other = ifelse(!is.null(input$comobid) && input$comobid == "Other", get_input_safe("comobid_other"), ""),
        ekg = formatted_ekg,
        echo = formatted_echo,
        eye = formatted_eye,
        drugallergy = get_input_safe("drugallergy"),
        caregiver = caregiver_value,
        caregiver_other = ifelse(!is.null(input$caregiver) && input$caregiver == "Other", get_input_safe("caregiver_other"), ""),
        hbpm = get_input_safe("hbpm"),
        medfinancial = get_input_safe("medfinancial"),
        daystart = formatted_daystart,
        stringsAsFactors = FALSE
      )
      
      write.csv(user_data, file_path, row.names = FALSE)
      output$save_status <- renderText("Data saved successfully!")
    }
  })
  #-------------------  Visit Form Server  ----------------------------------
  
  # 1. Retrieve Patient Information by HN
  patient_info <- reactiveValues(name = "", hn = "", found = FALSE)
  
  observeEvent(input$check_hn_visit, {
    file_path <- patient_data_file
    
    if (!file.exists(file_path)) {
      patient_info$name <- "No data file exists"
      patient_info$found <- FALSE
      return()
    }
    
    all_data <- read.csv(file_path, stringsAsFactors = FALSE)
    
    if (!all(c("hn", "name") %in% colnames(all_data))) {
      patient_info$name <- "Invalid file format"
      patient_info$found <- FALSE
      return()
    }
    
    hn_to_search <- toupper(trimws(input$hn_visit))
    all_data$hn <- toupper(trimws(all_data$hn))
    
    matching_rows <- which(all_data$hn == hn_to_search)
    
    if (length(matching_rows) > 0) {
      patient_info$name <- all_data$name[matching_rows[1]]
      patient_info$hn <- hn_to_search
      patient_info$found <- TRUE
    } else {
      patient_info$name <- "Patient not found"
      patient_info$found <- FALSE
    }
  })
  
  # Display using reactive values
  output$patient_name <- renderText({
    if (patient_info$found) {
      paste("Patient:", patient_info$name)
    } else {
      patient_info$name
    }
  })
  
  
  # Option 2: Using HTML output for better formatting
  output$patient_name_html <- renderUI({
    if (patient_info$found) {
      tags$div(
        style = "color: green; font-weight: bold;",
        paste("👤 Patient Found:", patient_info$name)
      )
    } else {
      tags$div(
        style = "color: red;",
        paste("", patient_info$name)
      )
    }
  })
  # Fixed: Use correct syntax for reactiveVal
  output$visit_position <- renderText({
    visits <- filtered_visits_data()  # Use the correct reactive name
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
        score <- case_when(
          bmi < 18.5 ~ 0,
          bmi >= 18.5 & bmi <= 24 ~ 3,
          bmi > 24 & bmi <= 25 ~ 2,
          bmi > 25 & bmi < 26 ~ 1,
          bmi >= 26 ~ 0
        )
        
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
    } else if (bp_sys >= 141 && bp_sys <= 159) {
      score <- 2
    } else if (bp_sys >= 160 && bp_sys <= 179) {
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
  medication_list_anti_platelet <- reactiveValues(data = list())
  medication_list_other <- reactiveValues(data = list())
  medication_list_spc <- reactiveValues(data = list())
  
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
  addMedication(medication_list_anti_platelet, "add_medication_anti_platelet", "Anti-platelet")
  addMedication(medication_list_other, "add_medication_other", "other")
  addMedication(medication_list_spc, "add_medication_spc", "spc")
  
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
                                          "Simvastatin (10)", "Simvastatin (20)", "Simvastatin (40)",
                                          "Ezetimibe (10)"),
                             "Anti-platelet" = c("ASA (81)"),
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
  
  renderSPCMedicationUI <- function(category_list, category_name) {
    renderUI({
      medication_ui <- lapply(names(category_list$data), function(id) {
        fluidRow(
          column(6, textInput(
            inputId = id,
            label = paste("Pill name:")
          )),
          column(4, textInput(
            inputId = category_list$data[[id]],
            label = "Quantity:"
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
  observeRemoveButtons(medication_list_anti_platelet)
  observeRemoveButtons(medication_list_other)
  observeRemoveButtons(medication_list_spc)
  
  # Render the UI for each category
  output$medication_ui_diuretics <- renderMedicationUI(medication_list_diuretics, "Diuretics")
  output$medication_ui_aceis <- renderMedicationUI(medication_list_aceis, "ACEIs")
  output$medication_ui_arbs <- renderMedicationUI(medication_list_arbs, "ARBs")
  output$medication_ui_ccbs <- renderMedicationUI(medication_list_ccbs, "CCBs")
  output$medication_ui_beta_blockers <- renderMedicationUI(medication_list_beta_blockers, "Beta Blockers")
  output$medication_ui_oad <- renderMedicationUI(medication_list_oad, "OAD")
  output$medication_ui_statin <- renderMedicationUI(medication_list_statin, "Statin")
  output$medication_ui_anti_platelet <- renderMedicationUI(medication_list_anti_platelet, "Anti-platelet")
  output$medication_ui_other <- renderMedicationUI(medication_list_other, "Other")
  output$medication_ui_spc <- renderSPCMedicationUI(medication_list_spc, "Single-pill combination")
  
  
  
  
  display_visit_data <- function(visit) {
    output$visit_summary <- renderText({
      paste0(
        "▶️ Visit Date: ", visit$visit_date, "\n",
        "👨️ Doctor: ", visit$doctor_name, "\n",
        "📝 Patient Note: ", visit$patient_note, "\n",
        "📌 Status: ", visit$patient_status, "\n\n",
        
        "💊 Medications:\n",
        "- Diuretics: ", visit$diuretics, "\n",
        "- ACEIs: ", visit$aceis, "\n",
        "- ARBs: ", visit$arbs, "\n",
        "- CCBs: ", visit$ccbs, "\n",
        "- Beta Blockers: ", visit$beta_blockers, "\n",
        "- Anti Platelet: ", visit$anti_platelet, "\n",
        "- OAD: ", visit$oad, "\n",
        "- Statin: ", visit$statin, "\n",
        "- Single-pill Combination: ", visit$single_pill_combination, "\n",
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
  
  
  # FIXED: Use consistent naming for reactive values
  filtered_visits_data <- reactiveVal(data.frame())
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
      filtered_visits_data(data.frame())
      return()
    } else {
      output$patient_name <- renderText(patient_name[1])
    }
    
    # Load visit data
    all_visits <- read.csv(visit_data_file, stringsAsFactors = FALSE)
    all_visits$hn <- toupper(all_visits$hn)
    # Filter out visits with no meaningful data
    all_visits <- all_visits %>%
      filter(!(is.na(visit_date) & is.na(patient_note) & is.na(patient_status)))
    
    # Filter visits for this patient
    visits <- all_visits[all_visits$hn == hn_to_search, ]
    
    # Remove rows with missing or blank visit_date
    visits <- visits[!is.na(visits$visit_date) & visits$visit_date != "", ]
    
    # Optional: sort by date descending
    visits <- visits[order(as.Date(visits$visit_date, format = "%d-%m-%Y"), decreasing = TRUE), ]
    
    # Store filtered visits using the consistent reactive name
    filtered_visits_data(visits)
    current_visit_index(1)
    
    # Fix the num_visit display using correct reactive value
    output$num_visit <- renderText({
      paste0(nrow(filtered_visits_data()))
    })
    
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
    visits <- filtered_visits_data()  # Use consistent reactive name
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
    visits <- filtered_visits_data()  # Use consistent reactive name
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
    
    # Convert Buddhist Era to Gregorian and format DOB
    convert_be_to_gregorian <- function(date_string) {
      if (is.null(date_string) || date_string == "") return("")
      
      tryCatch({
        # Parse the date (assuming it's in Buddhist Era)
        be_date <- as.Date(date_string, tryFormats = c("%d-%m-%Y", "%d/%m/%Y", "%Y-%m-%d"))
        
        if (!is.na(be_date)) {
          # Convert Buddhist Era to Gregorian (subtract 543 years)
          gregorian_date <- be_date - years(543)
          format(gregorian_date, "%d/%m/%Y")  # Convert to dd/mm/yyyy format
        } else {
          ""  # Invalid date
        }
      }, error = function(e) {
        ""  # Handle errors in date parsing
      })
    }
    
    formatted_visit_date <- convert_be_to_gregorian(input$visit_date)
    formatted_follow_up_date <- convert_be_to_gregorian(input$follow_up_date)
    
    
    # Create a new row with visit data
    new_data <- data.frame(
      visit_number = visit_number,
      hn = hn_to_search,
      name = patient_name[1],
      visit_date = formatted_visit_date,
      doctor_name = input$doctor_name,
      patient_note = input$patient_note,
      patient_status = input$pateintstatus,
      refer_hospital_details = ifelse(input$pateintstatus == "refer_hospital", input$refer_hospital_details, ""),
      consult_opd_details = ifelse(input$pateintstatus == "consult_opd", input$consult_opd_details, ""),
      
      #Patient Symptom Checklist:
      chest_tightness = input$chest_tightness,
      chest_tightness_note = ifelse(input$chest_tightness == "yes", input$chest_tightness_note, ""),
      nervous_system = input$nervous_system,
      nervous_system_note = ifelse(input$nervous_system == "yes", input$nervous_system_note, ""),
      urinal_abnormal = input$urinal_abnormal,
      urinal_abnormal_note = ifelse(input$urinal_abnormal == "yes", input$urinal_abnormal_note, ""),
      headache = input$headache,
      headache_note = ifelse(input$headache %in% c("sometimes", "often"), input$headache_note, ""),
      dizziness = input$dizziness,
      dizziness_note = ifelse(input$dizziness %in% c("sometimes", "often"), input$dizziness_note, ""),
      dypsnea = input$dypsnea,
      dypsnea_note = ifelse(input$dypsnea %in% c("sometimes", "often"), input$dypsnea_note, ""),
      leg_swelling = input$leg_swelling,
      leg_swelling_note = ifelse(input$leg_swelling %in% c("sometimes", "often"), input$leg_swelling_note, ""),
      face_swelling = input$face_swelling,
      face_swelling_note = ifelse(input$face_swelling %in% c("sometimes", "often"), input$face_swelling_note, ""),
      
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
      cc_other = ifelse(input$cc == "other", input$cc_other, ""),
      #PI
      pi = input$pi,
      pi_abnormal = ifelse(input$pi == "abnormal", input$pi_abnormal, ""),
      pi_other = ifelse(input$pi == "other", input$pi_other, ""),
      #Medical adherence
      adherence_alway_take_medicine = ifelse("alway_take_medicine" %in% input$medication_adherence, "yes", "no"),
      adherence_salty_control = ifelse("salty_control" %in% input$medication_adherence, "yes", "no"),
      adherence_exercise = ifelse("excercise" %in% input$medication_adherence, "yes", "no"),
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
      follow_up_date = formatted_follow_up_date,
      
      #Lab test next time
      lab_tests = paste(input$lab_tests, collapse = "; "),
      other_lab_tests = input$other_lab_tests,
      complication_stroke = ifelse("complication_stroke" %in% input$complication, "yes", "no"),
      complication_cardio_mi = ifelse("complication_cardio_mi" %in% input$complication, "yes", "no"),
      complication_chf = ifelse("complication_chf" %in% input$complication, "yes", "no"),
      complication_kidney = ifelse("complication_kidney" %in% input$complication, "yes", "no"),
      complication_eye = ifelse("complication_eye" %in% input$complication, "yes", "no"),
      other_complication = input$other_complication,
      precriptionadjust = input$precriptionadjust,
      
      # add dynamic medication data
      diuretics = getMedicationList(medication_list_diuretics),
      aceis = getMedicationList(medication_list_aceis),
      arbs = getMedicationList(medication_list_arbs),
      ccbs = getMedicationList(medication_list_ccbs),
      beta_blockers = getMedicationList(medication_list_beta_blockers),
      oad = getMedicationList(medication_list_oad),
      statin = getMedicationList(medication_list_statin),
      anti_platelet = getMedicationList(medication_list_anti_platelet),
      other_medications = getMedicationList(medication_list_other),
      single_pill_combination = getMedicationList(medication_list_spc),
      
      stringsAsFactors = FALSE
    )
    
    # Prevent saving if visit_date and patient_note are both missing
    if (all(is.na(new_data$visit_date)) && all(is.na(new_data$patient_note))) {
      showNotification("Nothing to save. Visit data is empty or invalid.", type = "warning")
      return()
    }
    
    # Append or update the visit data
    if (file.exists(visit_data_file)) {
      existing_data <- read.csv(visit_data_file, stringsAsFactors = FALSE)
      
      # Make sure both have the same column names in the same order
      common_names <- intersect(names(existing_data), names(new_data))
      existing_data <- existing_data[common_names]
      new_data <- new_data[common_names]
      
      # Match column types by converting all to character to avoid bind errors
      for (col in names(new_data)) {
        if (class(existing_data[[col]]) != class(new_data[[col]])) {
          existing_data[[col]] <- as.character(existing_data[[col]])
          new_data[[col]] <- as.character(new_data[[col]])
        }
      }
      
      updated_data <- rbind(existing_data, new_data)
    } else {
      updated_data <- new_data
    }
    
    
    # Remove rows where ALL values are NA or empty strings
    updated_data <- updated_data %>%
      dplyr::filter(!if_all(everything(), ~ is.na(.) | . == ""))
    
    # Save cleaned data
    write.csv(updated_data, visit_data_file, row.names = FALSE)
    showNotification("Visit data saved successfully.", type = "message")
  })
  
  #------------------------Patient Dashboard Server -------------------------------
  
  # Reactive expression: only run when button is pressed
  filtered_visits_patient_dashboard <- eventReactive(input$check_hn_dashboard, {
    req(input$hn_dashboard)
    
    data <- read.csv(visit_data_file, stringsAsFactors = FALSE)
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
  patient_dashboard_info <- reactiveValues(name = "", found = FALSE)
  observeEvent(input$check_hn_dashboard, {
    file_path <- patient_data_file
    
    if (!file.exists(file_path)) {
      patient_dashboard_info$name <- "No data file exists"
      patient_dashboard_info$found <- FALSE
      return()
    }
    
    all_data <- read.csv(file_path, stringsAsFactors = FALSE)
    
    if (!all(c("hn", "name") %in% colnames(all_data))) {
      patient_dashboard_info$name <- "Invalid file format"
      patient_dashboard_info$found <- FALSE
      return()
    }
    
    hn_to_search <- toupper(trimws(input$hn_dashboard))
    all_data$hn <- toupper(trimws(all_data$hn))
    
    matching_rows <- which(all_data$hn == hn_to_search)
    
    if (length(matching_rows) > 0) {
      patient_dashboard_info$name <- all_data$name[matching_rows[1]]
      patient_dashboard_info$found <- TRUE
    } else {
      patient_dashboard_info$name <- "Patient not found"
      patient_dashboard_info$found <- FALSE
    }
  })
  output$patient_name_dashboard <- renderUI({
    if (patient_dashboard_info$found) {
      tags$div(
        style = "color: green; font-weight: bold;",
        paste("👤 Patient Found:", patient_dashboard_info$name)
      )
    } else {
      tags$div(
        style = "color: red; font-weight: bold;",
        paste("", patient_dashboard_info$name)
      )
    }
  })
  
  
  
  # Blood Pressure Plot 
  output$bpPlot <- renderPlotly({
    data <- filtered_visits_patient_dashboard()
    req(data)
    req(nrow(data) > 0)
    
    bp_long <- data %>%
      select(visit_date, bp_sys, bp_dia) %>%
      pivot_longer(cols = c(bp_sys, bp_dia),
                   names_to = "bp_type",
                   values_to = "bp_value") %>%
      mutate(bp_type = recode(bp_type,
                              "bp_sys" = "Systolic",
                              "bp_dia" = "Diastolic")) %>%
      filter(!is.na(bp_value)) 
    
    plot_ly(bp_long,
            x = ~visit_date,
            y = ~bp_value,
            color = ~bp_type,
            type = 'scatter',
            mode = 'lines+markers',
            text = ~paste("Date:", format(visit_date, "%d %b %Y"),
                          "<br>Type:", bp_type,
                          "<br>Value:", bp_value),
            hoverinfo = "text") %>%
      layout(
        title = "Blood Pressure Over Time",
        xaxis = list(title = "Visit Date"),
        yaxis = list(title = "Blood Pressure (mmHg)"),
        legend = list(
          orientation = "h",
          x = 0.5,
          y = -0.2,
          xanchor = "center"
        ),
        hovermode = "closest"
      )
  })
  
  # Blood Pressure Plot 
  output$pulsePlot <- renderPlotly({
    data <- filtered_visits_patient_dashboard()
    req(data)
    req(nrow(data) > 0)
    
    bp_long <- data %>%
      select(visit_date, pulse) %>%
      filter(!is.na(visit_date) & !is.na(pulse))
    
    plot_ly(data,
            x = ~visit_date,
            y = ~pulse,
            type = 'scatter',
            mode = 'lines+markers',
            text = ~paste("Date:", format(visit_date, "%d %b %Y"),
                          "<br>Pulse:", pulse),
            hoverinfo = "text",
            line = list(color = 'rgb(255,127,80)')) %>%
      layout(title = "Pulse Over Time",
             xaxis = list(title = "Visit Date"),
             yaxis = list(title = "Pulse (bpm)", range = c(50, 120)),
             hovermode = "closest")
  })
  
  output$bmiPlot <- renderPlotly({
    data <- filtered_visits_patient_dashboard()
    req(data)
    req(nrow(data) > 0)
    
    # Convert height to meters and compute BMI
    data <- data %>%
      mutate(
        height = as.numeric(height),
        weight = as.numeric(weight),
        visit_date = parse_date_time(visit_date, orders = c("dmy", "ymd", "mdy")),
        height_m = height / 100,
        bmi = round(weight / (height_m^2), 1)
      ) %>%
      filter(!is.na(visit_date) & !is.na(bmi) & is.finite(bmi))
    
    plot_ly(data,
            x = ~visit_date,
            y = ~bmi,
            type = 'scatter',
            mode = 'lines+markers',
            text = ~paste("Date:", format(visit_date, "%d %b %Y"),
                          "<br>BMI:", bmi),
            hoverinfo = "text",
            line = list(color = "darkgreen")) %>%
      layout(
        title = "BMI Over Time",
        xaxis = list(title = "Visit Date"),
        yaxis = list(title = "BMI"),
        hovermode = "closest"
      )
  })
  
  output$waistPlot <- renderPlotly({
    data <- filtered_visits_patient_dashboard()
    req(data)
    req(nrow(data) > 0)
    
    # Parse and clean data
    data <- data %>%
      mutate(
        visit_date = parse_date_time(visit_date, orders = c("dmy", "ymd", "mdy")),
        waist = as.numeric(waist)
      ) %>%
      filter(!is.na(visit_date) & !is.na(waist))
    
    plot_ly(data,
            x = ~visit_date,
            y = ~waist,
            type = 'scatter',
            mode = 'lines+markers',
            text = ~paste("Date:", format(visit_date, "%d %b %Y"),
                          "<br>Waist:", waist, "cm"),
            hoverinfo = "text",
            line = list(color = "darkred")) %>%
      layout(
        title = "Waist Circumference Over Time",
        xaxis = list(title = "Visit Date"),
        yaxis = list(title = "Waist (cm)"),
        hovermode = "closest"
      )
  })
  
  output$saltycontrolPlot <- renderPlotly({
    data <- filtered_visits_patient_dashboard()
    req(data)
    req(nrow(data) > 0)
    
    data <- data %>%
      mutate(
        visit_date = parse_date_time(visit_date, orders = c("dmy", "ymd", "mdy")),
        value = ifelse(adherence_salty_control == "yes", 0.6, 0.4), #mark value
        color = ifelse(adherence_salty_control == "yes", "darkgreen", "darkred")
      ) %>%
      filter(!is.na(visit_date) & !is.na(adherence_salty_control))
    
    plot_ly(data,
            x = ~visit_date,
            y = ~value,
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = "black"),  # keeps the line connected
            marker = list(size = 10,
                          color = ~color),  # color only markers
            text = ~paste("Date:", format(visit_date, "%d %b %Y"),
                          "<br>Response:", ifelse(value == 0.6, "Yes", "No")),
            hoverinfo = "text") %>%
      layout(
        title = "Salty Control Responses Over Time",
        xaxis = list(title = "Visit Date"),
        yaxis = list(title = "Response",
                     tickvals = c(0.4, 0.6), #show point
                     ticktext = c("No", "Yes"),
                     range = c(0.3, 0.7), #Zoom the plot
                     zeroline = FALSE),
        hovermode = "closest",
        showlegend = FALSE  
      )
  })
  
  
  output$alwaytakemedicinePlot <- renderPlotly({
    data <- filtered_visits_patient_dashboard()
    req(data)
    req(nrow(data) > 0)
    
    data <- data %>%
      mutate(
        visit_date = parse_date_time(visit_date, orders = c("dmy", "ymd", "mdy")),
        value = ifelse(adherence_alway_take_medicine == "yes", 0.6, 0.4),
        response = ifelse(adherence_alway_take_medicine == "yes", "Yes", "No"),
        color = ifelse(adherence_alway_take_medicine == "yes", "darkgreen", "darkred")
      ) %>%
      filter(!is.na(visit_date) & !is.na(adherence_alway_take_medicine))
    
    plot_ly(data,
            x = ~visit_date,
            y = ~value,
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = "black"),  # keeps the line connected
            marker = list(size = 10,
                          color = ~color),  # color only markers
            text = ~paste("Date:", format(visit_date, "%d %b %Y"),
                          "<br>Response:", ifelse(value == 0.6, "Yes", "No")),
            hoverinfo = "text") %>%
      layout(
        title = "Take Medicine Over Time",
        xaxis = list(title = "Visit Date"),
        yaxis = list(
          title = "Response",
          tickvals = c(0.4, 0.6),
          ticktext = c("No", "Yes"),
          range = c(0.3, 0.7),
          zeroline = FALSE
        ),
        hovermode = "closest",
        showlegend = FALSE
      )
  })
  
  output$exercisePlot <- renderPlotly({
    data <- filtered_visits_patient_dashboard()
    req(data)
    req(nrow(data) > 0)
    
    data <- data %>%
      mutate(
        visit_date = parse_date_time(visit_date, orders = c("dmy", "ymd", "mdy")),
        value = ifelse(adherence_exercise == "yes", 0.6, 0.4), #mark value
        color = ifelse(adherence_exercise == "yes", "darkgreen", "darkred")
      ) %>%
      filter(!is.na(visit_date) & !is.na(adherence_alway_take_medicine))
    
    plot_ly(data,
            x = ~visit_date,
            y = ~value,
            type = 'scatter',
            mode = 'lines+markers',
            line = list(color = "black"),  # keeps the line connected
            marker = list(size = 10,
                          color = ~color),  # color only markers
            text = ~paste("Date:", format(visit_date, "%d %b %Y"),
                          "<br>Response:", ifelse(value == 0.6, "Yes", "No")),
            hoverinfo = "text") %>%
      layout(
        title = "Exercise Responses Over Time",
        xaxis = list(title = "Visit Date"),
        yaxis = list(
          title = "Response",
          tickvals = c(0.4, 0.6),
          ticktext = c("No", "Yes"),
          range = c(0.3, 0.7),
          zeroline = FALSE
        ),
        hovermode = "closest",
        showlegend = FALSE
      )
  })
  
  
  #------------------------Clinic Dashboard Server -------------------------------  
  
  #----------------------------Overviwe Clinic--------------------------------
  
  output$total_count_text <- renderText({
    if (file.exists(patient_data_file)) {
      data <- read.csv(patient_data_file, stringsAsFactors = FALSE)
      total <- nrow(data)
      paste0(total, " (100%)")
    } else {
      "0 (0%)"
    }
  })
  
  output$male_count_text <- renderText({
    if (file.exists(patient_data_file)) {
      data <- read.csv(patient_data_file, stringsAsFactors = FALSE)
      total <- nrow(data)
      count <- sum(data$gender == "Male", na.rm = TRUE)
      perc <- if (total > 0) round((count / total) * 100, 1) else 0
      paste0(count, " (", perc, "%)")
    } else {
      "0 (0%)"
    }
  })
  
  output$female_count_text <- renderText({
    if (file.exists(patient_data_file)) {
      data <- read.csv(patient_data_file, stringsAsFactors = FALSE)
      total <- nrow(data)
      count <- sum(data$gender == "Female", na.rm = TRUE)
      perc <- if (total > 0) round((count / total) * 100, 1) else 0
      paste0(count, " (", perc, "%)")
    } else {
      "0 (0%)"
    }
  })
  
  output$other_count_text <- renderText({
    if (file.exists(patient_data_file)) {
      data <- read.csv(patient_data_file, stringsAsFactors = FALSE)
      total <- nrow(data)
      count <- sum(!(data$gender %in% c("Male", "Female")), na.rm = TRUE)
      perc <- if (total > 0) round((count / total) * 100, 1) else 0
      paste0(count, " (", perc, "%)")
    } else {
      "0 (0%)"
    }
  })
  
  
  
  #----------------------------Insight Clinic--------------------------------
  
  # Reactive to load and filter visit_data based on input date range
  filtered_visits <- eventReactive(input$actionDT, {
    if (file.exists(visit_data_file)) {
      visit_data <- read.csv(visit_data_file, stringsAsFactors = FALSE)
      visit_data$visit_date <- as.Date(visit_data$visit_date, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
      visit_data <- visit_data[visit_data$visit_date >= input$dateRange[1] & visit_data$visit_date <= input$dateRange[2], ]
      return(visit_data)
    } else {
      return(data.frame()) # Return empty data frame instead of NULL
    }
  })
  
  # Join patient info
  filtered_patient_info <- eventReactive(input$actionDT, {
    req(input$dateRange, input$insightMode)
    
    # Load visit data
    if (!file.exists(visit_data_file)) return(data.frame())
    visit_data_file <- read.csv(visit_data_file, stringsAsFactors = FALSE)
    visit_data_file$visit_date <- as.Date(visit_data_file$visit_date, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
    
    # Filter by date range
    visit_data_file <- visit_data_file[
      visit_data_file$visit_date >= input$dateRange[1] & visit_data_file$visit_date <= input$dateRange[2], ]
    
    if (nrow(visit_data_file) == 0) return(data.frame())
    
    # Load patient data
    patient_file <- patient_data_file
    if (!file.exists(patient_file)) return(data.frame())
    patient_data <- read.csv(patient_file, stringsAsFactors = FALSE)
    
    # Join with patient data
    joined_data <- merge(visit_data_file, patient_data, by = "hn", all.x = TRUE)
    
    # Apply mode logic
    if (input$insightMode == "unique") {
      joined_data <- joined_data %>%
        distinct(hn, .keep_all = TRUE)  # One row per patient
    }
    
    # Add age group
    joined_data <- joined_data %>%
      mutate(
        age_group = case_when(
          age >= 0 & age < 40 ~ "Less than 40",
          age >= 40 & age < 60 ~ "40-59",
          age >= 60 & age < 80 ~ "60-79",
          age >= 80 ~ "80+",
          TRUE ~ NA_character_
        )
      )
    
    return(joined_data)
  })
  
  
  output$total_count_box_insight <- renderText({
    df <- filtered_patient_info()
    total <- nrow(df)
    paste0(total, " (100%)")
  })
  
  output$male_count_box_insight <- renderText({
    df <- filtered_patient_info()
    count <- if (nrow(df) > 0) sum(df$gender == "Male", na.rm = TRUE) else 0
    total <- if (nrow(df) > 0) nrow(df) else 1
    paste0(count, " (", round(100 * count / total, 1), "%)")
  })
  
  output$female_count_box_insight <- renderText({
    df <- filtered_patient_info()
    count <- if (nrow(df) > 0) sum(df$gender == "Female", na.rm = TRUE) else 0
    total <- if (nrow(df) > 0) nrow(df) else 1
    paste0(count, " (", round(100 * count / total, 1), "%)")
  })
  
  output$other_count_box_insight <- renderText({
    df <- filtered_patient_info()
    count <- if (nrow(df) > 0) sum(!(df$gender %in% c("Male", "Female")), na.rm = TRUE) else 0
    total <- if (nrow(df) > 0) nrow(df) else 1
    paste0(count, " (", round(100 * count / total, 1), "%)")
  })
  
  #------- sex insight------
  
  output$sex_insight <- renderPlotly({
    df <- filtered_patient_info()
    
    # Check if we have data
    if (nrow(df) == 0 || sum(!is.na(df$gender)) == 0) {
      p <- ggplot() +
        geom_text(aes(x = 1, y = 1, label = "No data available"), size = 5) +
        theme_bw() +
        labs(title = "Gender Distribution")
      
      return(ggplotly(p))
    }
    
    # Filter and summarize
    gender_summary <- df %>%
      filter(!is.na(gender)) %>%
      mutate(gender = factor(gender, levels = c("Male", "Female", "Other"))) %>%
      count(gender, name = "count") %>%
      mutate(percentage = round(100 * count / sum(count), 1),
             label = paste0(count, " (", percentage, "%)"))
    
    # Custom color map
    gender_colors <- c("Male" = "#bae1ff", "Female" = "#ffd4e5", "Other" = "#e6e6ff")
    
    # Plot
    p <- ggplot(gender_summary, aes(x = gender, y = count, fill = gender, text = label)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = label, y = count + 5), size = 4) +
      scale_fill_manual(values = gender_colors) +
      theme_minimal() +
      labs(title = "Gender Distribution", x = "Gender", y = "Count") +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
            legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  
  
  #-------age distribution plot-------
  output$age_insight <- renderPlotly({
    df <- filtered_patient_info()
    
    # Check if we have data
    if (nrow(df) == 0 || sum(!is.na(df$age_group)) == 0) {
      # Create empty plot for no data
      p <- ggplot() +
        geom_text(aes(x = 1, y = 1, label = "No data available"), size = 5) +
        theme_minimal() +
        labs(title = "Age Distribution")
      
      return(ggplotly(p))
    }
    
    # Create the plot with data
    df_plot <- df %>%
      filter(!is.na(age_group)) %>%
      mutate(age_group = factor(age_group, levels = c("Less than 40", "40-59", "60-79", "80+")))
    
    p <- ggplot(df_plot, aes(x = age_group, fill = age_group)) +
      geom_bar() +
      scale_fill_brewer(palette = "Pastel1") +
      theme_minimal() +
      labs(title = "Age Distribution", x = "Age Group", y = "Number of Patients") +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p)
  })
  
  #------ hbpm ---------
  output$hbpm_insight <- renderPlotly({
    df <- filtered_patient_info()  # or read.csv(patient_data_file) if you want all patients
    
    # Ensure hbpm column exists and is not all NA
    if (!"hbpm" %in% names(df) || sum(!is.na(df$hbpm)) == 0) {
      p <- ggplot() +
        geom_text(aes(x = 1, y = 1, label = "No HBPM data available"), size = 5) +
        theme_minimal() +
        labs(title = "HBPM Usage")
      return(ggplotly(p))
    }
    
    # Clean and count values
    df$hbpm <- trimws(tolower(df$hbpm))  # handle Yes / No variations
    df$hbpm <- ifelse(df$hbpm %in% c("yes", "no"), tools::toTitleCase(df$hbpm), NA)
    
    summary_df <- df %>%
      filter(!is.na(hbpm)) %>%
      count(hbpm, name = "count") %>%
      mutate(hbpm = factor(hbpm, levels = c("Yes", "No")))
    
    # Bar plot
    p <- ggplot(summary_df, aes(x = hbpm, y = count, fill = hbpm)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      scale_fill_brewer(palette = "Pastel1") +
      labs(title = "HBPM Usage", x = "Used HBPM", y = "Number of Patients") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$num_visit_insight <- renderPlotly({
    visit_data <- filtered_visits()
    
    if (nrow(visit_data) == 0 || !"visit_number" %in% names(visit_data)) {
      p <- ggplot() +
        geom_text(aes(x = 1, y = 1, label = "No visit data available"), size = 5) +
        theme_minimal() +
        labs(title = "Visit Frequency per Patient")
      return(ggplotly(p))
    }
    
    # Clean visit_number: ensure numeric and remove NAs
    visit_data$visit_number <- suppressWarnings(as.numeric(visit_data$visit_number))
    visit_data <- visit_data[!is.na(visit_data$visit_number), ]
    
    # Now calculate the max visit number per patient
    visit_summary <- visit_data %>%
      group_by(hn) %>%
      summarise(num_visits = max(visit_number), .groups = "drop") %>%
      count(num_visits, name = "patients")
    
    # Bar plot
    p <- ggplot(visit_summary, aes(x = factor(num_visits), y = patients)) +
      geom_bar(stat = "identity", fill = "#d0ecc4") +
      theme_minimal() +
      labs(title = "Distribution of Number of Visits per Patient",
           x = "Number of Visits",
           y = "Number of Patients") +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
    
    ggplotly(p)
  })
  
  output$bp_control_score_insight <- renderPlotly({
    df <- filtered_visits()
    
    # Safeguard
    if (nrow(df) == 0 || !"bp_control_score" %in% names(df)) {
      p <- ggplot() +
        geom_text(aes(x = 1, y = 1, label = "No BP control score data"), size = 5) +
        theme_minimal() +
        labs(title = "BP Control Score")
      return(ggplotly(p))
    }
    
    # Proper filtering
    df <- df %>% filter(!is.na(bp_control_score), bp_control_score %in% 0:3)
    
    # Convert to factor with levels
    df$bp_control_score <- factor(df$bp_control_score, levels = c(0, 1, 2, 3))
    
    # Summarize cleanly
    summary_df <- df %>%
      group_by(bp_control_score) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(
        percentage = round(100 * count / sum(count), 1),
        label = paste0(count, " (", percentage, "%)")
      )
    
    # Custom colors
    custom_colors <- c("0" = "#ffb3ba",  # Red
                       "1" = "#ffdfba",  # Orange
                       "2" = "#ffffba",  # Yellow
                       "3" = "#baffc9")  # Green
    
    # Clean plot
    p <- ggplot(summary_df, aes(x = bp_control_score, y = count, fill = bp_control_score, text = label)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = label, y = count + 20), size = 3.5) +
      scale_fill_manual(values = custom_colors) +
      theme_minimal() +
      labs(title = "BP Control Score (0–3)", x = "Score", y = "Number of Visits") +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(b = 50, r = 120),
        annotations = list(
          x = 0.98,
          y = -0.15,
          text = paste0(
            "<b>Score Definitions:</b><br>",
            "0 = More than 180<br>",
            "1 = 160-179<br>",
            "2 = 141-159<br>",
            "3 = Less than 140"
          ),
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "bottom",
          align = "left",
          font = list(size = 12)
        )
      )
  })
  
  
  
  output$home_bp_score_insight <- renderPlotly({
    df <- filtered_visits()
    custom_colors <- c("0" = "#ffb3ba",  # Red
                       "1" = "#ffdfba",  # Orange
                       "2" = "#ffffba",  # Yellow
                       "3" = "#baffc9")  # Green
    
    if (nrow(df) == 0 || !"home_bp_score" %in% names(df)) {
      p <- ggplot() +
        geom_text(aes(x = 1, y = 1, label = "No Home BP score data"), size = 5) +
        theme_minimal() +
        labs(title = "Home BP Score")
      return(ggplotly(p))
    }
    
    df <- df %>% filter(!is.na(home_bp_score), home_bp_score %in% 0:3)
    df$home_bp_score <- factor(df$home_bp_score, levels = c(0, 1, 2, 3))
    
    summary_df <- df %>%
      count(home_bp_score, name = "count") %>%
      mutate(percentage = round(100 * count / sum(count), 1),
             label = paste0(count, " (", percentage, "%)"))
    
    p <- ggplot(summary_df, aes(x = home_bp_score, y = count, fill = home_bp_score, text = label)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = label, y = count + 10), size = 3.5) +
      scale_fill_manual(values = custom_colors) +
      theme_minimal() +
      labs(title = "Home BP Score (0–3)", x = "Score", y = "Number of Visits") +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(b = 50, r = 120),
        annotations = list(
          x = 0.98,
          y = -0.15,
          text = paste0(
            "<b>Score Definitions:</b><br>",
            "0 = More than 180<br>",
            "1 = 160-179<br>",
            "2 = 141-159<br>",
            "3 = Less than 140"
          ),
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "bottom",
          align = "left",
          font = list(size = 12)
        )
      )
  })
  
  
  output$weight_control_score_insight <- renderPlotly({
    df <- filtered_visits()
    custom_colors <- c("0" = "#ffb3ba",  # Red
                       "1" = "#ffdfba",  # Orange
                       "2" = "#ffffba",  # Yellow
                       "3" = "#baffc9")  # Green
    
    if (nrow(df) == 0 || !"weight_control_score" %in% names(df)) {
      p <- ggplot() +
        geom_text(aes(x = 1, y = 1, label = "No weight control score data"), size = 5) +
        theme_minimal() +
        labs(title = "Weight Control Score")
      return(ggplotly(p))
    }
    
    df <- df %>% filter(!is.na(weight_control_score), weight_control_score %in% 0:3)
    df$weight_control_score <- factor(df$weight_control_score, levels = c(0, 1, 2, 3))
    
    summary_df <- df %>%
      count(weight_control_score, name = "count") %>%
      mutate(percentage = round(100 * count / sum(count), 1),
             label = paste0(count, " (", percentage, "%)"))
    
    p <- ggplot(summary_df, aes(x = weight_control_score, y = count, fill = weight_control_score, text = label)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = label, y = count + 10), size = 3.5) +
      scale_fill_manual(values = custom_colors) +
      theme_minimal() +
      labs(title = "Weight Control Score (0–3)", x = "Score", y = "Number of Visits") +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(b = 50, r = 140),
        annotations = list(
          x = 0.94,
          y = -0.15,
          text = paste0(
            "<b>Score Definitions:</b><br>",
            "0 = BMI >= 26<br>",
            "1 = BMI >= 25.1 & < 26<br>",
            "2 = BMI >= 24.1 & < 25<br>",
            "3 = BMI >= 18.5 & < 24.1"
          ),
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "bottom",
          align = "left",
          font = list(size = 12)
        )
      )
  })
  
  
  
  
  output$self_care_score_insight <- renderPlotly({
    df <- filtered_visits()
    custom_colors <- c("0" = "#ffb3ba",  # Red
                       "1" = "#ffdfba",  # Orange
                       "2" = "#ffffba",  # Yellow
                       "3" = "#baffc9")  # Green
    
    if (nrow(df) == 0 || !"self_care_score" %in% names(df)) {
      p <- ggplot() +
        geom_text(aes(x = 1, y = 1, label = "No self-care score data"), size = 5) +
        theme_minimal() +
        labs(title = "Self-Care Score")
      return(ggplotly(p))
    }
    
    df <- df %>% filter(!is.na(self_care_score), self_care_score %in% 0:3)
    df$self_care_score <- factor(df$self_care_score, levels = c(0, 1, 2, 3))
    
    summary_df <- df %>%
      count(self_care_score, name = "count") %>%
      mutate(percentage = round(100 * count / sum(count), 1),
             label = paste0(count, " (", percentage, "%)"))
    
    p <- ggplot(summary_df, aes(x = self_care_score, y = count, fill = self_care_score, text = label)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = label, y = count + 10), size = 3.5) +
      scale_fill_manual(values = custom_colors) +
      theme_minimal() +
      labs(title = "Self-Care Score (0–3)", x = "Score", y = "Number of Visits") +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(b = 50, r = 140),
        annotations = list(
          x = 0.92,
          y = -0.15,
          text = paste0(
            "<b>Score Definitions:</b><br>",
            "0 = Does none of the behaviors<br>",
            "1 = Does one behavior<br>",
            "2 = Does two behaviors<br>",
            "3 = Does all three behaviors"
          ),
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "bottom",
          align = "left",
          font = list(size = 12)
        )
      )
  })
  
  
  #------ salty control --------------
  output$salty_control_insight <- renderPlotly({
    df <- filtered_visits()
    
    if (nrow(df) == 0 || !"adherence_salty_control" %in% names(df)) {
      p <- ggplot() +
        geom_text(aes(x = 1, y = 1, label = "No salty control data"), size = 5) +
        theme_minimal() +
        labs(title = "Salt Intake Control")
      return(ggplotly(p))
    }
    
    # Clean and standardize values
    df <- df %>%
      mutate(adherence_salty_control = trimws(tolower(adherence_salty_control))) %>%
      filter(adherence_salty_control %in% c("yes", "no")) %>%
      mutate(adherence_salty_control = factor(tools::toTitleCase(adherence_salty_control), levels = c("Yes", "No")))
    
    summary_df <- df %>%
      count(adherence_salty_control, name = "count") %>%
      mutate(percentage = round(100 * count / sum(count), 1),
             label = paste0(count, " (", percentage, "%)"))
    
    p <- ggplot(summary_df, aes(x = adherence_salty_control, y = count, fill = adherence_salty_control)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = label, y = count + 2), size = 3.5) +
      scale_fill_brewer(palette = "Pastel1") +
      theme_minimal() +
      labs(title = "Salt Intake Control", x = "Response", y = "Number of Visits") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  #-------- take medicine--------
  output$alway_take_medicine_insight <- renderPlotly({
    df <- filtered_visits()
    
    if (nrow(df) == 0 || !"adherence_alway_take_medicine" %in% names(df)) {
      p <- ggplot() +
        geom_text(aes(x = 1, y = 1, label = "No medication adherence data"), size = 5) +
        theme_minimal() +
        labs(title = "Always Take Medication")
      return(ggplotly(p))
    }
    
    df <- df %>%
      mutate(adherence_alway_take_medicine = trimws(tolower(adherence_alway_take_medicine))) %>%
      filter(adherence_alway_take_medicine %in% c("yes", "no")) %>%
      mutate(adherence_alway_take_medicine = factor(tools::toTitleCase(adherence_alway_take_medicine), levels = c("Yes", "No")))
    
    summary_df <- df %>%
      count(adherence_alway_take_medicine, name = "count") %>%
      mutate(percentage = round(100 * count / sum(count), 1),
             label = paste0(count, " (", percentage, "%)"))
    
    p <- ggplot(summary_df, aes(x = adherence_alway_take_medicine, y = count, fill = adherence_alway_take_medicine)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = label, y = count + 2), size = 3.5) +
      scale_fill_brewer(palette = "Pastel1") +
      theme_minimal() +
      labs(title = "Always Take Medication", x = "Response", y = "Number of Visits") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  
  #-------- exercise----------
  output$exercise_insight <- renderPlotly({
    df <- filtered_visits()
    
    if (nrow(df) == 0 || !"adherence_exercise" %in% names(df)) {
      p <- ggplot() +
        geom_text(aes(x = 1, y = 1, label = "No exercise data"), size = 5) +
        theme_minimal() +
        labs(title = "Exercise Adherence")
      return(ggplotly(p))
    }
    
    df <- df %>%
      mutate(adherence_exercise = trimws(tolower(adherence_exercise))) %>%
      filter(adherence_exercise %in% c("yes", "no")) %>%
      mutate(adherence_exercise = factor(tools::toTitleCase(adherence_exercise), levels = c("Yes", "No")))
    
    summary_df <- df %>%
      count(adherence_exercise, name = "count") %>%
      mutate(percentage = round(100 * count / sum(count), 1),
             label = paste0(count, " (", percentage, "%)"))
    
    p <- ggplot(summary_df, aes(x = adherence_exercise, y = count, fill = adherence_exercise)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = label, y = count + 2), size = 3.5) +
      scale_fill_brewer(palette = "Pastel1") +
      theme_minimal() +
      labs(title = "Exercise Adherence", x = "Response", y = "Number of Visits") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
