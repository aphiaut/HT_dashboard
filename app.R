
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
library(stringr)
library(later)

thailand <- read_csv("data/thailand_province_amphoe.csv")

ui <- navbarPage(
  "HT Clinic",
  theme = shinytheme("flatly"),
  tags$head(
    includeCSS("styles.css")
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
             dateInput("visit_date", label = "Visit Date", format = "dd-mm-yyyy", autoclose = TRUE),
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
               condition = "input.chest_tightness == 'yes'",
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
               column(6, 
                      h4("Blood Pressure")),
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
             ),
             fluidRow( 
               column(10,
                    selectInput("precriptionadjust", "Prescription Adjusted",
                                choices = c("Off Medication", "Decrease", "Add",
                                            "Same", "Change : Complication"))))
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
             dateInput("follow_up_date", label = "Date:", format = "dd-mm-yyyy", autoclose = TRUE),
             
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
             
             
      ),
      ),
    fluidRow(
      column(width = 4, offset = 3, align = "center",
             actionButton("save_visit_only", "Save Visit Only", class = "btn-info")
      )
    ),
    hr(),
    fluidRow(
      column(6,
             h3("Prescription"),
             fluidRow(
               column(12,
                      h4("Diuretics :"),
                      column(6, div(id = "diuretics_container")),
                      column(6, actionButton("add_medication_diuretics", "➕ Add Diuretic")),
                      column(3,actionButton(
                        "clear_diuretics",
                        tagList(icon("trash", style = "color:black;"), "Clear"),
                        class = "btn btn-danger btn-sm",
                        style = "color:black;"
                      ))
               )),
             fluidRow(
               column(12,
                      h4("ACEIs :"),
                      column(6, div(id = "aceis_container")),
                      column(6, actionButton("add_medication_aceis", "➕ Add ACEIs")),
                      column(3,actionButton(
                        "clear_aceis",
                        tagList(icon("trash", style = "color:black;"), "Clear"),
                        class = "btn btn-danger btn-sm",
                        style = "color:black;"
                      ))
               )),
             fluidRow(
               column(12,
                      h4("ARBs :"),
                      column(6, div(id = "arbs_container")),
                      column(6, actionButton("add_medication_arbs", "➕ Add ARBs")),
                      column(3,actionButton(
                        "clear_arbs",
                        tagList(icon("trash", style = "color:black;"), "Clear"),
                        class = "btn btn-danger btn-sm",
                        style = "color:black;"
                      ))
               )),
             fluidRow(
               column(12,
                      h4("CCBs :"),
                      column(6, div(id = "ccbs_container")),
                      column(6, actionButton("add_medication_ccbs", "➕ Add CCBs")),
                      column(3,actionButton(
                        "clear_ccbs",
                        tagList(icon("trash", style = "color:black;"), "Clear"),
                        class = "btn btn-danger btn-sm",
                        style = "color:black;"
                      ))
               )
             ),
             fluidRow(
               column(12,
               h4("Beta blockers :"),
               column(6, div(id = "beta_blockers_container")),
               column(6, actionButton("add_medication_beta_blockers", "➕ Add Beta blockers")),
               column(3,actionButton(
                 "clear_beta_blockers",
                 tagList(icon("trash", style = "color:black;"), "Clear"),
                 class = "btn btn-danger btn-sm",
                 style = "color:black;"
               )))
             ),
             ),
      column(6,
             h3(" "),
             fluidRow(
               h4("DM :"),
               column(6, div(id = "oad_container")),
               column(6, actionButton("add_medication_oad", "➕ Add DM")),
               column(3,actionButton(
                 "clear_oad",
                 tagList(icon("trash", style = "color:black;"), "Clear"),
                 class = "btn btn-danger btn-sm",
                 style = "color:black;"
               ))
             ),
             fluidRow(
               h4("Statin :"),
               column(6, div(id = "statin_container")),
               column(6, actionButton("add_medication_statin", "➕ Add Statin")),
               column(3,actionButton(
                 "clear_statin",
                 tagList(icon("trash", style = "color:black;"), "Clear"),
                 class = "btn btn-danger btn-sm",
                 style = "color:black;"
               ))
             ),
             fluidRow(
               h4("Anti-platelet :"),
               column(6, div(id = "anti_platelet_container")),
               column(6, actionButton("add_medication_anti_platelet", "➕ Add Anti-platelet")),
               column(3,actionButton(
                 "clear_anti_platelet",
                 tagList(icon("trash", style = "color:black;"), "Clear"),
                 class = "btn btn-danger btn-sm",
                 style = "color:black;"
               ))
             ),
             fluidRow(
               h4("Other :"),
               column(6, div(id = "other_container")),
               column(6, actionButton("add_medication_other", "➕ Add Other")),
               column(3,actionButton(
                 "clear_other",
                 tagList(icon("trash", style = "color:black;"), "Clear"),
                 class = "btn btn-danger btn-sm",
                 style = "color:black;"
               ))
             ),
             fluidRow(
               column(12,
                      h4("Single-pill Combination:"),
                      column(6, div(id = "single-pill")),
                      column(6, actionButton("add_medication_spc", "➕ Add Single-pill Combination")),
                      column(3,actionButton(
                        "clear_spc",
                        tagList(icon("trash", style = "color:black;"), "Clear"),
                        class = "btn btn-danger btn-sm",
                        style = "color:black;"
                      ))
               )
             )
      )
      
      
    ),
    
    fluidRow(
      column(width = 4, offset = 3, align = "center",
             actionButton("save_meds_only", "Save Medications Only", class = "btn-warning")
      )
    ),
    hr()
  ),
  
  
  
  #------------- Patient Info ---------
  
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
                        fluidRow(
                          column(4,
                                 div(class = "plot-card",
                                     plotlyOutput("salty_control_trend")
                                 )
                          ),
                          column(4,
                                 div(class = "plot-card",
                                     plotlyOutput("alway_take_medicine_trend")
                                 )
                          ),
                          column(4,
                                 div(class = "plot-card",
                                     plotlyOutput("exercise_trend")
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
  visit_data_file <- "data/visit.csv"
  
  # Helper function to convert BE to Gregorian (if needed)
  convert_be_to_gregorian <- function(date_input) {
    if (is.null(date_input) || is.na(date_input)) return(NA)
    return(date_input)  # Modify this function based on your BE conversion logic
  }
  
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
  # ---- ensure files exist ----
  ensure_visit_file <- function() {
    if (!dir.exists("data")) dir.create("data", recursive = TRUE)
    
    if (!file.exists(visit_data_file)) {
      empty_visit <- data.frame(
        visit_id = character(),
        visit_number = numeric(),
        hn = character(),
        name = character(),
        visit_date = character(),
        doctor_name = character(),
        patient_note = character(),
        patient_status = character(),
        refer_hospital_details = character(),
        consult_opd_details = character(),
        stringsAsFactors = FALSE
      )
      
      write.csv(empty_visit, visit_data_file, row.names = FALSE)
    }
  }
  
  ensure_medication_file <- function() {
    if (!dir.exists("data")) dir.create("data", recursive = TRUE)
    
    required_med_cols <- c("visit_id", "hn", "visit_date", "med_group", "drug_name", "quantity")
    
    if (!file.exists(medication_data_file)) {
      empty_med <- data.frame(
        visit_id = character(),
        hn = character(),
        visit_date = character(),
        med_group = character(),
        drug_name = character(),
        quantity = character(),
        stringsAsFactors = FALSE
      )
      
      write.csv(empty_med, medication_data_file, row.names = FALSE)
    } else {
      tmp <- tryCatch(read.csv(medication_data_file, stringsAsFactors = FALSE), error = function(e) NULL)
      
      if (is.null(tmp) || !all(required_med_cols %in% names(tmp))) {
        empty_med <- data.frame(
          visit_id = character(),
          hn = character(),
          visit_date = character(),
          med_group = character(),
          drug_name = character(),
          quantity = character(),
          stringsAsFactors = FALSE
        )
        
        write.csv(empty_med, medication_data_file, row.names = FALSE)
      }
    }
  }
  
  safe_string <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x) || (is.character(x) && all(x == ""))) "" else as.character(x)[1]
  }
  
  safe_vec <- function(x) {
    if (is.null(x) || length(x) == 0 || all(x == "")) "" else paste(x, collapse = "; ")
  }
  
  safe_num <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x) || x == "") NA else suppressWarnings(as.numeric(x)[1])
  }
  
  `%||%` <- function(a, b) if (is.null(a)) b else a
  
  # ---- file paths ----
  visit_data_file <- "data/visits.csv"
  medication_data_file <- "data/medications.csv"
  
  if (!dir.exists("data")) dir.create("data", recursive = TRUE)
  
  # ---- reactive values ----
  patient_visits <- reactiveVal(data.frame())
  current_visit_index <- reactiveVal(1)
  current_visit_row <- reactiveVal(NULL)
  current_visit_id <- reactiveVal(NULL)
  
  patient_info <- reactiveValues(name = "", hn = "", found = FALSE)
  
  active_med_rows <- reactiveValues(
    diuretics = character(),
    aceis = character(),
    arbs = character(),
    ccbs = character(),
    beta_blockers = character(),
    oad = character(),
    statin = character(),
    anti_platelet = character(),
    other = character(),
    spc = character()
  )
  
  # ---- medication master list ----
  medication_choices <- list(
    "Diuretics" = c("HCTZ (25)", "HCTZ (50)", "Other"),
    "ACEIs" = c("Enalapril (5)", "Enalapril (20)", "Other"),
    "ARBs" = c("Losartan (50)", "Losartan (100)", "Other"),
    "CCBs" = c("Amlodipine (5)", "Amlodipine (10)", "Madiplot (20)", "Diltiazem (30)", "Diltiazem (60)", "Other"),
    "Beta Blockers" = c("Atenolol (25)", "Atenolol (50)", "Atenolol (100)",
                        "Carvedilol (6.25)", "Carvedilol (12.5)", "Carvedilol (25)",
                        "Metoprolol (100)", "Propranolol (10)", "Propranolol (40)", "Other"),
    "OAD" = c("Metformin (500)", "Metformin (850)", "Metformin (1000)",
              "Glipizide (5)", "Other"),
    "Statin" = c("Atorvastatin (20)", "Atorvastatin (40)",
                 "Simvastatin (10)", "Simvastatin (20)", "Simvastatin (40)",
                 "Ezetimibe (10)", "Other"),
    "Anti-platelet" = c("ASA (81)", "Other"),
    "Other" = c("Azilsartan (40)", "Hydralazine (25)", "Doxazosin (2)", "Methyldopa (250)", "Other")
  )
  
  med_groups <- data.frame(
    name         = c("Diuretics", "ACEIs", "ARBs", "CCBs", "Beta Blockers", "OAD", "Statin", "Anti-platelet", "Other"),
    add_btn      = c("add_medication_diuretics", "add_medication_aceis", "add_medication_arbs", "add_medication_ccbs", "add_medication_beta_blockers", "add_medication_oad", "add_medication_statin", "add_medication_anti_platelet", "add_medication_other"),
    container_id = c("diuretics_container", "aceis_container", "arbs_container", "ccbs_container", "beta_blockers_container", "oad_container", "statin_container", "anti_platelet_container", "other_container"),
    prefix       = c("diuretics_", "aceis_", "arbs_", "ccbs_", "beta_blockers_", "oad_", "statin_", "anti_platelet_", "other_"),
    stringsAsFactors = FALSE
  )
  
  prefix_to_tracker <- function(prefix) {
    switch(prefix,
           "diuretics_" = "diuretics",
           "aceis_" = "aceis",
           "arbs_" = "arbs",
           "ccbs_" = "ccbs",
           "beta_blockers_" = "beta_blockers",
           "oad_" = "oad",
           "statin_" = "statin",
           "anti_platelet_" = "anti_platelet",
           "other_" = "other",
           "spc_" = "spc",
           NULL)
  }
  
  #--- remove all the medicine in each group----
  
  observeEvent(input$clear_diuretics, {
    removeUI(selector = "#diuretics_container > *", multiple = TRUE, immediate = TRUE)
    active_med_rows$diuretics <- character()
  })
  
  observeEvent(input$clear_aceis, {
    removeUI(selector = "#aceis_container > *", multiple = TRUE, immediate = TRUE)
    active_med_rows$aceis <- character()
  })
  
  observeEvent(input$clear_arbs, {
    removeUI(selector = "#arbs_container > *", multiple = TRUE, immediate = TRUE)
    active_med_rows$arbs <- character()
  })
  
  observeEvent(input$clear_ccbs, {
    removeUI(selector = "#ccbs_container > *", multiple = TRUE, immediate = TRUE)
    active_med_rows$ccbs <- character()
  })
  
  observeEvent(input$clear_beta_blockers, {
    removeUI(selector = "#beta_blockers_container > *", multiple = TRUE, immediate = TRUE)
    active_med_rows$beta_blockers <- character()
  })
  
  observeEvent(input$clear_oad, {
    removeUI(selector = "#oad_container > *", multiple = TRUE, immediate = TRUE)
    active_med_rows$oad <- character()
  })
  
  observeEvent(input$clear_statin, {
    removeUI(selector = "#statin_container > *", multiple = TRUE, immediate = TRUE)
    active_med_rows$statin <- character()
  })
  
  observeEvent(input$clear_statin, {
    removeUI(selector = "#statin_container > *", multiple = TRUE, immediate = TRUE)
    active_med_rows$statin <- character()
  })
  
  observeEvent(input$clear_anti_platelet, {
    removeUI(selector = "#anti_platelet_container > *", multiple = TRUE, immediate = TRUE)
    active_med_rows$anti_platelet <- character()
  })
  
  observeEvent(input$clear_spc, {
    removeUI(selector = "#single-pill > *", multiple = TRUE, immediate = TRUE)
    active_med_rows$spc <- character()
  })
  
  
  # ---- add dynamic medication UI ----
  for (i in seq_len(nrow(med_groups))) {
    local({
      group_name    <- med_groups$name[i]
      add_btn       <- med_groups$add_btn[i]
      container_id  <- med_groups$container_id[i]
      prefix        <- med_groups$prefix[i]
      
      observeEvent(input[[add_btn]], {
        uid <- paste0(as.integer(Sys.time()), sample(1000:9999, 1))
        tracker_name <- prefix_to_tracker(prefix)
        active_med_rows[[tracker_name]] <- c(active_med_rows[[tracker_name]], uid)
        
        select_id <- paste0(prefix, uid)
        other_id  <- paste0(prefix, "other_", uid)
        qty_id    <- paste0("qty_", prefix, uid)
        remove_id <- paste0("remove_", prefix, uid)
        
        insertUI(
          selector = paste0("#", container_id),
          where = "beforeEnd",
          ui = tags$div(
            id = paste0(container_id, "_row_", uid),
            fluidRow(
              column(
                4,
                conditionalPanel(
                  condition = paste0("input.", select_id, " != 'Other' || input.", select_id, " == null"),
                  selectInput(select_id, label = NULL, choices = medication_choices[[group_name]], width = "100%")
                ),
                conditionalPanel(
                  condition = paste0("input.", select_id, " == 'Other'"),
                  textInput(other_id, label = NULL, placeholder = "Enter medication name")
                )
              ),
              column(4, textInput(qty_id, label = NULL, placeholder = "Quantity")),
              column(2, actionButton(remove_id, "❌", class = "btn btn-danger"))
            )
          )
        )
        
        observeEvent(input[[remove_id]], {
          removeUI(selector = paste0("#", container_id, "_row_", uid), immediate = TRUE)
          active_med_rows[[tracker_name]] <- setdiff(active_med_rows[[tracker_name]], uid)
          
          if (length(active_med_rows[[tracker_name]]) == 0) {
            removeUI(selector = paste0("#", container_id, " > *"), multiple = TRUE, immediate = TRUE)
          }
        }, ignoreInit = TRUE)
      })
    })
  }
  
  # ---- add SPC UI ----
  observeEvent(input$add_medication_spc, {
    uid <- paste0(as.integer(Sys.time()), sample(1000:9999, 1))
    active_med_rows$spc <- c(active_med_rows$spc, uid)
    
    med_id <- paste0("spc_med_", uid)
    qty_id <- paste0("spc_qty_", uid)
    remove_id <- paste0("remove_spc_", uid)
    
    insertUI(
      selector = "#single-pill",
      where = "beforeEnd",
      ui = tags$div(
        id = paste0("single_pill_row_", uid),
        fluidRow(
          column(6, textInput(med_id, label = NULL, placeholder = "Enter pill name")),
          column(4, textInput(qty_id, label = NULL, placeholder = "Quantity")),
          column(2, actionButton(remove_id, "❌", class = "btn btn-danger"))
        )
      )
    )
    
    observeEvent(input[[remove_id]], {
      removeUI(selector = paste0("#single_pill_row_", uid), immediate = TRUE)
      active_med_rows$spc <- setdiff(active_med_rows$spc, uid)
      
      if (length(active_med_rows$spc) == 0) {
        removeUI(selector = "#single-pill > *", multiple = TRUE, immediate = TRUE)
      }
    }, ignoreInit = TRUE)
  })
  
  # ---- clear dynamic medication UI ----
  clear_dynamic_medications <- function() {
    removeUI(selector = "#diuretics_container > div", multiple = TRUE, immediate = TRUE)
    removeUI(selector = "#aceis_container > div", multiple = TRUE, immediate = TRUE)
    removeUI(selector = "#arbs_container > div", multiple = TRUE, immediate = TRUE)
    removeUI(selector = "#ccbs_container > div", multiple = TRUE, immediate = TRUE)
    removeUI(selector = "#beta_blockers_container > div", multiple = TRUE, immediate = TRUE)
    removeUI(selector = "#oad_container > div", multiple = TRUE, immediate = TRUE)
    removeUI(selector = "#statin_container > div", multiple = TRUE, immediate = TRUE)
    removeUI(selector = "#anti_platelet_container > div", multiple = TRUE, immediate = TRUE)
    removeUI(selector = "#other_container > div", multiple = TRUE, immediate = TRUE)
    removeUI(selector = "#single-pill > div", multiple = TRUE, immediate = TRUE)
    
    active_med_rows$diuretics <- character()
    active_med_rows$aceis <- character()
    active_med_rows$arbs <- character()
    active_med_rows$ccbs <- character()
    active_med_rows$beta_blockers <- character()
    active_med_rows$oad <- character()
    active_med_rows$statin <- character()
    active_med_rows$anti_platelet <- character()
    active_med_rows$other <- character()
    active_med_rows$spc <- character()
  }
  
  # ---- clear full form ----
  clear_form <- function() {
    updateDateInput(session, "visit_date", value = Sys.Date())
    updateSelectInput(session, "doctor_name", selected = character(0))
    updateTextAreaInput(session, "patient_note", value = "")
    updateSelectInput(session, "pateintstatus", selected = "continuing_treatment")
    updateTextInput(session, "refer_hospital_details", value = "")
    updateTextInput(session, "consult_opd_details", value = "")
    
    updateRadioButtons(session, "chest_tightness", selected = character(0))
    updateRadioButtons(session, "nervous_system", selected = character(0))
    updateRadioButtons(session, "urinal_abnormal", selected = character(0))
    updateRadioButtons(session, "headache", selected = character(0))
    updateRadioButtons(session, "dizziness", selected = character(0))
    updateRadioButtons(session, "dypsnea", selected = character(0))
    updateRadioButtons(session, "leg_swelling", selected = character(0))
    updateRadioButtons(session, "face_swelling", selected = character(0))
    
    updateTextInput(session, "chest_tightness_note", value = "")
    updateTextInput(session, "nervous_system_note", value = "")
    updateTextInput(session, "urinal_abnormal_note", value = "")
    updateTextInput(session, "headache_note", value = "")
    updateTextInput(session, "dizziness_note", value = "")
    updateTextInput(session, "dypsnea_note", value = "")
    updateTextInput(session, "leg_swelling_note", value = "")
    updateTextInput(session, "face_swelling_note", value = "")
    
    updateTextInput(session, "cr", value = "")
    updateTextInput(session, "na", value = "")
    updateTextInput(session, "fbs", value = "")
    updateTextInput(session, "hba1c", value = "")
    updateTextInput(session, "cho", value = "")
    updateTextInput(session, "ldl", value = "")
    updateTextInput(session, "tg", value = "")
    updateTextInput(session, "hdl", value = "")
    updateTextInput(session, "ast", value = "")
    updateTextInput(session, "alt", value = "")
    
    updateRadioButtons(session, "cc", selected = character(0))
    updateTextAreaInput(session, "cc_early_visit", value = "")
    updateTextAreaInput(session, "cc_late_visit", value = "")
    updateTextAreaInput(session, "cc_other", value = "")
    updateRadioButtons(session, "pi", selected = character(0))
    updateTextAreaInput(session, "pi_abnormal", value = "")
    updateTextAreaInput(session, "pi_other", value = "")
    
    updateCheckboxGroupInput(session, "medication_adherence", selected = character(0))
    updateTextAreaInput(session, "allergic_history", value = "")
    
    updateTextInput(session, "bp_sys", value = "")
    updateTextInput(session, "bp_dia", value = "")
    updateTextInput(session, "pulse", value = "")
    updateTextInput(session, "waist", value = "")
    updateTextInput(session, "height", value = "")
    updateTextInput(session, "weight", value = "")
    
    updateRadioButtons(session, "heent", selected = character(0))
    updateTextAreaInput(session, "heent_abnormal", value = "")
    updateRadioButtons(session, "heart", selected = character(0))
    updateTextAreaInput(session, "heart_abnormal", value = "")
    updateRadioButtons(session, "lungs", selected = character(0))
    updateTextAreaInput(session, "lungs_abnormal", value = "")
    updateRadioButtons(session, "abd", selected = character(0))
    updateTextAreaInput(session, "abd_abnormal", value = "")
    updateRadioButtons(session, "ext", selected = character(0))
    updateTextAreaInput(session, "ext_abnormal", value = "")
    updateRadioButtons(session, "ns", selected = character(0))
    updateTextAreaInput(session, "ns_abnormal", value = "")
    
    updateSelectInput(session, "diagnosis", selected = character(0))
    updateTextAreaInput(session, "other_diagnosis", value = "")
    
    updateNumericInput(session, "bp_control_score", value = 0)
    updateNumericInput(session, "weight_control_score", value = 0)
    updateNumericInput(session, "self_care_score", value = 0)
    updateNumericInput(session, "home_bp_score", value = 0)
    updateRadioButtons(session, "hbpm_target", selected = character(0))
    
    updateRadioButtons(session, "follow_up", selected = character(0))
    updateDateInput(session, "follow_up_date", value = Sys.Date())
    updateCheckboxGroupInput(session, "lab_tests", selected = character(0))
    updateTextAreaInput(session, "other_lab_tests", value = "")
    updateCheckboxGroupInput(session, "complication", selected = character(0))
    updateTextAreaInput(session, "other_complication", value = "")
    updateSelectInput(session, "precriptionadjust", selected = "Same")
    
    clear_dynamic_medications()
  }
  
  # ---- medication extraction as rows ----
  getMedicationRows <- function(prefix, group_name, visit_id, hn, visit_date) {
    tracker_name <- prefix_to_tracker(prefix)
    ids <- active_med_rows[[tracker_name]]
    
    rows <- list()
    
    for (uid in ids) {
      select_id <- paste0(prefix, uid)
      other_id  <- paste0(prefix, "other_", uid)
      qty_id    <- paste0("qty_", prefix, uid)
      
      drug <- NULL
      qty  <- input[[qty_id]]
      
      if (!is.null(input[[select_id]])) {
        drug <- input[[select_id]]
        
        if (!is.null(drug) && drug == "Other") {
          typed_other <- input[[other_id]]
          if (!is.null(typed_other) && typed_other != "") {
            drug <- typed_other
          }
        }
      } else {
        typed_other <- input[[other_id]]
        if (!is.null(typed_other) && typed_other != "") {
          drug <- typed_other
        }
      }
      
      if (!is.null(drug) && drug != "" && !is.null(qty) && qty != "") {
        rows[[length(rows) + 1]] <- data.frame(
          visit_id   = as.character(visit_id),
          hn         = as.character(hn),
          visit_date = as.character(visit_date),
          med_group  = as.character(group_name),
          drug_name  = as.character(drug),
          quantity   = as.character(qty),
          stringsAsFactors = FALSE
        )
      }
    }
    
    if (length(rows) == 0) {
      return(data.frame(
        visit_id = character(),
        hn = character(),
        visit_date = character(),
        med_group = character(),
        drug_name = character(),
        quantity = character(),
        stringsAsFactors = FALSE
      ))
    }
    
    do.call(rbind, rows)
  }
  
  getSPCRows <- function(visit_id, hn, visit_date) {
    rows <- list()
    
    for (uid in active_med_rows$spc) {
      med <- input[[paste0("spc_med_", uid)]]
      qty <- input[[paste0("spc_qty_", uid)]]
      
      if (!is.null(med) && med != "" && !is.null(qty) && qty != "") {
        rows[[length(rows) + 1]] <- data.frame(
          visit_id   = as.character(visit_id),
          hn         = as.character(hn),
          visit_date = as.character(visit_date),
          med_group  = "Single-pill Combination",
          drug_name  = as.character(med),
          quantity   = as.character(qty),
          stringsAsFactors = FALSE
        )
      }
    }
    
    if (length(rows) == 0) {
      return(data.frame(
        visit_id = character(),
        hn = character(),
        visit_date = character(),
        med_group = character(),
        drug_name = character(),
        quantity = character(),
        stringsAsFactors = FALSE
      ))
    }
    
    do.call(rbind, rows)
  }
  
  # ---- medication restore from separate medication file ----
  restoreMedicationUIFromRows <- function(meds_df, group_name, prefix, container_id, choices = NULL) {
    if (is.null(meds_df) || nrow(meds_df) == 0) return()
    
    group_rows <- meds_df[meds_df$med_group == group_name, , drop = FALSE]
    if (nrow(group_rows) == 0) return()
    
    for (i in seq_len(nrow(group_rows))) {
      drug <- as.character(group_rows$drug_name[i])
      qty  <- as.character(group_rows$quantity[i])
      
      uid <- paste0(as.integer(Sys.time()), sample(1000:9999, 1), i)
      tracker_name <- prefix_to_tracker(prefix)
      active_med_rows[[tracker_name]] <- c(active_med_rows[[tracker_name]], uid)
      
      select_id <- paste0(prefix, uid)
      other_id  <- paste0(prefix, "other_", uid)
      qty_id    <- paste0("qty_", prefix, uid)
      remove_id <- paste0("remove_", prefix, uid)
      
      insertUI(
        selector = paste0("#", container_id),
        where = "beforeEnd",
        ui = tags$div(
          id = paste0(container_id, "_row_", uid),
          fluidRow(
            column(
              4,
              if (is.null(choices) || !(drug %in% choices)) {
                textInput(other_id, label = NULL, value = drug, placeholder = "Enter medication name")
              } else {
                selectInput(select_id, label = NULL, choices = choices, selected = drug, width = "100%")
              }
            ),
            column(4, textInput(qty_id, label = NULL, value = qty, placeholder = "Quantity")),
            column(2, actionButton(remove_id, "❌", class = "btn btn-danger"))
          )
        )
      )
      
      observeEvent(input[[remove_id]], {
        removeUI(selector = paste0("#", container_id, "_row_", uid), immediate = TRUE)
        active_med_rows[[tracker_name]] <- setdiff(active_med_rows[[tracker_name]], uid)
        
        if (length(active_med_rows[[tracker_name]]) == 0) {
          removeUI(selector = paste0("#", container_id, " > *"), multiple = TRUE, immediate = TRUE)
        }
      }, ignoreInit = TRUE)
    }
  }
  
  load_visit_medications <- function(visit_id) {
    ensure_medication_file()
    if (!file.exists(medication_data_file)) {
      return(data.frame())
    }
    
    meds <- read.csv(medication_data_file, stringsAsFactors = FALSE)
    if (!"visit_id" %in% names(meds)) return(data.frame())
    
    meds[meds$visit_id == visit_id, , drop = FALSE]
  }
  
  # ---- populate form ----
  populate_visit_form <- function(visit) {
    if (is.null(visit) || nrow(visit) == 0) return()
    
    if ("visit_id" %in% names(visit)) {
      current_visit_id(as.character(visit$visit_id[1]))
    } else {
      current_visit_id(NULL)
    }
    
    visit_date <- as.Date(visit$visit_date, format = "%d/%m/%Y")
    if (!is.na(visit_date)) updateDateInput(session, "visit_date", value = visit_date)
    
    updateSelectInput(session, "doctor_name", selected = visit$doctor_name)
    updateTextAreaInput(session, "patient_note", value = visit$patient_note)
    updateSelectInput(session, "pateintstatus", selected = visit$patient_status)
    updateTextInput(session, "refer_hospital_details", value = visit$refer_hospital_details)
    updateTextInput(session, "consult_opd_details", value = visit$consult_opd_details)
    
    updateRadioButtons(session, "chest_tightness", selected = visit$chest_tightness)
    updateTextInput(session, "chest_tightness_note", value = visit$chest_tightness_note)
    updateRadioButtons(session, "nervous_system", selected = visit$nervous_system)
    updateTextInput(session, "nervous_system_note", value = visit$nervous_system_note)
    updateRadioButtons(session, "urinal_abnormal", selected = visit$urinal_abnormal)
    updateTextInput(session, "urinal_abnormal_note", value = visit$urinal_abnormal_note)
    updateRadioButtons(session, "headache", selected = visit$headache)
    updateTextInput(session, "headache_note", value = visit$headache_note)
    updateRadioButtons(session, "dizziness", selected = visit$dizziness)
    updateTextInput(session, "dizziness_note", value = visit$dizziness_note)
    updateRadioButtons(session, "dypsnea", selected = visit$dypsnea)
    updateTextInput(session, "dypsnea_note", value = visit$dypsnea_note)
    updateRadioButtons(session, "leg_swelling", selected = visit$leg_swelling)
    updateTextInput(session, "leg_swelling_note", value = visit$leg_swelling_note)
    updateRadioButtons(session, "face_swelling", selected = visit$face_swelling)
    updateTextInput(session, "face_swelling_note", value = visit$face_swelling_note)
    
    updateTextInput(session, "cr", value = visit$cr)
    updateTextInput(session, "na", value = visit$na)
    updateTextInput(session, "fbs", value = visit$fbs)
    updateTextInput(session, "hba1c", value = visit$hba1c)
    updateTextInput(session, "cho", value = visit$cho)
    updateTextInput(session, "ldl", value = visit$ldl)
    updateTextInput(session, "tg", value = visit$tg)
    updateTextInput(session, "hdl", value = visit$hdl)
    updateTextInput(session, "ast", value = visit$ast)
    updateTextInput(session, "alt", value = visit$alt)
    
    updateRadioButtons(session, "cc", selected = visit$cc)
    updateTextAreaInput(session, "cc_early_visit", value = visit$cc_early_visit)
    updateTextAreaInput(session, "cc_late_visit", value = visit$cc_late_visit)
    updateTextAreaInput(session, "cc_other", value = visit$cc_other)
    updateRadioButtons(session, "pi", selected = visit$pi)
    updateTextAreaInput(session, "pi_abnormal", value = visit$pi_abnormal)
    updateTextAreaInput(session, "pi_other", value = visit$pi_other)
    
    updateCheckboxGroupInput(session, "medication_adherence",
                             selected = c(
                               if (isTRUE(visit$adherence_alway_take_medicine == "yes")) "alway_take_medicine",
                               if (isTRUE(visit$adherence_salty_control == "yes")) "salty_control",
                               if (isTRUE(visit$adherence_exercise == "yes")) "excercise"
                             ))
    
    updateTextAreaInput(session, "allergic_history", value = visit$allergic_history)
    
    updateTextInput(session, "bp_sys", value = visit$bp_sys)
    updateTextInput(session, "bp_dia", value = visit$bp_dia)
    updateTextInput(session, "pulse", value = visit$pulse)
    updateTextInput(session, "waist", value = visit$waist)
    updateTextInput(session, "height", value = visit$height)
    updateTextInput(session, "weight", value = visit$weight)
    
    updateRadioButtons(session, "heent", selected = visit$heent)
    updateTextAreaInput(session, "heent_abnormal", value = visit$heent_abnormal)
    updateRadioButtons(session, "heart", selected = visit$heart)
    updateTextAreaInput(session, "heart_abnormal", value = visit$heart_abnormal)
    updateRadioButtons(session, "lungs", selected = visit$lungs)
    updateTextAreaInput(session, "lungs_abnormal", value = visit$lungs_abnormal)
    updateRadioButtons(session, "abd", selected = visit$abd)
    updateTextAreaInput(session, "abd_abnormal", value = visit$abd_abnormal)
    updateRadioButtons(session, "ext", selected = visit$ext)
    updateTextAreaInput(session, "ext_abnormal", value = visit$ext_abnormal)
    updateRadioButtons(session, "ns", selected = visit$ns)
    updateTextAreaInput(session, "ns_abnormal", value = visit$ns_abnormal)
    
    updateSelectInput(session, "diagnosis",
                      selected = if (isTruthy(visit$diagnosis)) strsplit(as.character(visit$diagnosis), ";\\s*")[[1]] else character(0))
    updateTextAreaInput(session, "other_diagnosis", value = visit$other_diagnosis)
    
    updateNumericInput(session, "bp_control_score", value = safe_num(visit$bp_control_score) %||% 0)
    updateNumericInput(session, "weight_control_score", value = safe_num(visit$weight_control_score) %||% 0)
    updateNumericInput(session, "self_care_score", value = safe_num(visit$self_care_score) %||% 0)
    updateNumericInput(session, "home_bp_score", value = safe_num(visit$home_bp_score) %||% 0)
    
    updateRadioButtons(session, "hbpm_target", selected = visit$hbpm_target)
    updateRadioButtons(session, "follow_up", selected = visit$follow_up)
    
    follow_up_date <- as.Date(visit$follow_up_date, format = "%d/%m/%Y")
    if (!is.na(follow_up_date)) updateDateInput(session, "follow_up_date", value = follow_up_date)
    
    updateCheckboxGroupInput(session, "lab_tests",
                             selected = if (isTruthy(visit$lab_tests)) strsplit(as.character(visit$lab_tests), ";\\s*")[[1]] else character(0))
    updateTextAreaInput(session, "other_lab_tests", value = visit$other_lab_tests)
    
    updateCheckboxGroupInput(session, "complication",
                             selected = {
                               sel <- c()
                               if (isTRUE(visit$complication_stroke == "yes")) sel <- c(sel, "complication_stroke")
                               if (isTRUE(visit$complication_cardio_mi == "yes")) sel <- c(sel, "complication_cardio_mi")
                               if (isTRUE(visit$complication_chf == "yes")) sel <- c(sel, "complication_chf")
                               if (isTRUE(visit$complication_kidney == "yes")) sel <- c(sel, "complication_kidney")
                               if (isTRUE(visit$complication_eye == "yes")) sel <- c(sel, "complication_eye")
                               sel
                             })
    updateTextAreaInput(session, "other_complication", value = visit$other_complication)
    
    updateSelectInput(session, "precriptionadjust", selected = visit$precriptionadjust)
    
    clear_dynamic_medications()
    
    if ("visit_id" %in% names(visit) && !is.na(visit$visit_id) && visit$visit_id != "") {
      meds <- load_visit_medications(visit$visit_id)
      
      restoreMedicationUIFromRows(meds, "Diuretics", "diuretics_", "diuretics_container", medication_choices$Diuretics)
      restoreMedicationUIFromRows(meds, "ACEIs", "aceis_", "aceis_container", medication_choices$ACEIs)
      restoreMedicationUIFromRows(meds, "ARBs", "arbs_", "arbs_container", medication_choices$ARBs)
      restoreMedicationUIFromRows(meds, "CCBs", "ccbs_", "ccbs_container", medication_choices$CCBs)
      restoreMedicationUIFromRows(meds, "Beta Blockers", "beta_blockers_", "beta_blockers_container", medication_choices$`Beta Blockers`)
      restoreMedicationUIFromRows(meds, "OAD", "oad_", "oad_container", medication_choices$OAD)
      restoreMedicationUIFromRows(meds, "Statin", "statin_", "statin_container", medication_choices$Statin)
      restoreMedicationUIFromRows(meds, "Anti-platelet", "anti_platelet_", "anti_platelet_container", medication_choices$`Anti-platelet`)
      restoreMedicationUIFromRows(meds, "Other", "other_", "other_container", medication_choices$Other)
      restoreMedicationUIFromRows(meds, "Single-pill Combination", "spc_", "single-pill", NULL)
    }
  }
  
  # ---- display patient name ----
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
  
  # ---- load visits for patient ----
  load_patient_visits <- function(hn) {
    ensure_visit_file()
    if (!file.exists(visit_data_file)) {
      patient_visits(data.frame())
      current_visit_index(1)
      current_visit_row(NULL)
      clear_form()
      return()
    }
    
    all_visits <- read.csv(visit_data_file, stringsAsFactors = FALSE)
    
    if (!"hn" %in% names(all_visits)) {
      patient_visits(data.frame())
      current_visit_index(1)
      current_visit_row(NULL)
      clear_form()
      return()
    }
    
    all_visits$hn <- toupper(trimws(all_visits$hn))
    visits <- all_visits[all_visits$hn == hn, , drop = FALSE]
    visits <- visits[!is.na(visits$visit_date) & visits$visit_date != "", , drop = FALSE]
    
    if (nrow(visits) > 0) {
      visits$visit_date_parsed <- as.Date(visits$visit_date, format = "%d/%m/%Y")
      
      if ("visit_number" %in% names(visits)) {
        visits$visit_number_num <- suppressWarnings(as.numeric(visits$visit_number))
        visits <- visits[order(visits$visit_date_parsed, visits$visit_number_num, decreasing = FALSE), , drop = FALSE]
        visits$visit_number_num <- NULL
      } else {
        visits <- visits[order(visits$visit_date_parsed, decreasing = FALSE), , drop = FALSE]
      }
      
      visits$visit_date_parsed <- NULL
      
      patient_visits(visits)
      current_visit_index(nrow(visits))
      populate_visit_form(visits[nrow(visits), , drop = FALSE])
      
      visit_to_find <- visits[nrow(visits), , drop = FALSE]
      all_visits$visit_date_parsed <- as.Date(all_visits$visit_date, format = "%d/%m/%Y")
      
      row_index <- which(all_visits$visit_id == visit_to_find$visit_id)[1]
      
      if (!is.na(row_index)) current_visit_row(row_index)
    } else {
      patient_visits(data.frame())
      current_visit_index(1)
      current_visit_row(NULL)
      clear_form()
    }
  }
  
  # ---- check HN ----
  observeEvent(input$check_hn_visit, {
    hn_to_search <- toupper(trimws(input$hn_visit))
    
    if (hn_to_search == "") {
      showNotification("Please enter a Patient Code (HN).", type = "warning")
      return()
    }
    
    if (!file.exists(patient_data_file)) {
      patient_info$name <- "No patient data file exists"
      patient_info$found <- FALSE
      return()
    }
    
    patient_data <- read.csv(patient_data_file, stringsAsFactors = FALSE)
    
    if (!all(c("hn", "name") %in% colnames(patient_data))) {
      patient_info$name <- "Invalid patient data file format"
      patient_info$found <- FALSE
      return()
    }
    
    patient_data$hn <- toupper(trimws(patient_data$hn))
    matching_rows <- which(patient_data$hn == hn_to_search)
    
    if (length(matching_rows) > 0) {
      patient_info$name <- patient_data$name[matching_rows[1]]
      patient_info$hn <- hn_to_search
      patient_info$found <- TRUE
      load_patient_visits(hn_to_search)
    } else {
      patient_info$name <- "Patient not found"
      patient_info$found <- FALSE
      patient_visits(data.frame())
      current_visit_row(NULL)
      clear_form()
    }
  })
  
  # ---- visit count ----
  output$num_visit <- renderText({
    visits <- patient_visits()
    if (nrow(visits) > 0) as.character(nrow(visits)) else "0"
  })
  
  # ---- BMI ----
  output$bmi_text <- renderText({
    if (input$height != "" && input$weight != "") {
      tryCatch({
        height_m <- as.numeric(input$height) / 100
        weight_kg <- as.numeric(input$weight)
        bmi <- round(weight_kg / (height_m^2), 1)
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
        height_m <- as.numeric(input$height) / 100
        weight_kg <- as.numeric(input$weight)
        bmi <- weight_kg / (height_m^2)
        
        score <- dplyr::case_when(
          bmi < 18.5 ~ 0,
          bmi >= 18.5 & bmi <= 24 ~ 3,
          bmi > 24 & bmi <= 25 ~ 2,
          bmi > 25 & bmi < 26 ~ 1,
          bmi >= 26 ~ 0
        )
        
        updateNumericInput(session, "weight_control_score", value = score)
      }, error = function(e) {
        updateNumericInput(session, "weight_control_score", value = 0)
      })
    } else {
      updateNumericInput(session, "weight_control_score", value = 0)
    }
  })
  
  # ---- self care score ----
  observe({
    adherence_score <- length(input$medication_adherence)
    updateNumericInput(session, "self_care_score", value = adherence_score)
  })
  
  # ---- BP control score ----
  observe({
    bp_sys <- suppressWarnings(as.numeric(input$bp_sys))
    
    score <- if (is.na(bp_sys)) {
      0
    } else if (bp_sys <= 140) {
      3
    } else if (bp_sys >= 141 && bp_sys <= 159) {
      2
    } else if (bp_sys >= 160 && bp_sys <= 179) {
      1
    } else {
      0
    }
    
    updateNumericInput(session, "bp_control_score", value = score)
  })
  
  # ---- visit position ----
  output$visit_position <- renderText({
    visits <- patient_visits()
    index <- current_visit_index()
    total <- nrow(visits)
    
    if (total > 0) paste("Visit", index, "of", total) else "No visits"
  })
  
  # ---- visit summary ----
  display_visit_data <- function(visit) {
    if (is.null(visit) || nrow(visit) == 0) {
      output$visit_summary <- renderText("No visit data available")
      return()
    }
    
    safe <- function(x) {
      if (is.null(x) || is.na(x)) "N/A" else as.character(x)
    }
    
    output$visit_summary <- renderText({
      paste0(
        "▶️ Visit Date: ", safe(visit$visit_date), "\n",
        "👨‍⚕️ Doctor: ", safe(visit$doctor_name), "\n",
        "📝 Patient Note: ", substr(safe(visit$patient_note), 1, 100),
        if (nchar(safe(visit$patient_note)) > 100) "..." else "", "\n",
        "📌 Status: ", safe(visit$patient_status), "\n",
        "🩺 BP: ", safe(visit$bp_sys), "/", safe(visit$bp_dia), " mmHg\n",
        "💊 Medications loaded in form"
      )
    })
  }
  
  # ---- previous visit ----
  observeEvent(input$prev_visit, {
    visits <- patient_visits()
    index <- current_visit_index()
    
    if (index > 1) {
      new_index <- index - 1
      current_visit_index(new_index)
      selected_visit <- visits[new_index, , drop = FALSE]
      populate_visit_form(selected_visit)
      display_visit_data(selected_visit)
      
      if (file.exists(visit_data_file)) {
        all_visits <- read.csv(visit_data_file, stringsAsFactors = FALSE)
        all_visits$visit_id <- as.character(all_visits$visit_id)
        
        row_index <- which(all_visits$visit_id == as.character(selected_visit$visit_id))[1]
        
        if (!is.na(row_index)) current_visit_row(row_index)
      }
    } else {
      showNotification("This is the first visit.", type = "warning")
    }
  })
  
  # ---- next visit ----
  observeEvent(input$next_visit, {
    visits <- patient_visits()
    index <- current_visit_index()
    
    if (index < nrow(visits)) {
      new_index <- index + 1
      current_visit_index(new_index)
      selected_visit <- visits[new_index, , drop = FALSE]
      populate_visit_form(selected_visit)
      display_visit_data(selected_visit)
      
      if (file.exists(visit_data_file)) {
        all_visits <- read.csv(visit_data_file, stringsAsFactors = FALSE)
        all_visits$visit_id <- as.character(all_visits$visit_id)
        
        row_index <- which(all_visits$visit_id == as.character(selected_visit$visit_id))[1]
        
        if (!is.na(row_index)) current_visit_row(row_index)
      }
    } else {
      showNotification("This is the last visit.", type = "warning")
    }
  })
  
  # ---- add new visit ----
  observeEvent(input$add_visit, {
    clear_form()
    current_visit_row(NULL)
    current_visit_id(NULL)
    updateDateInput(session, "visit_date", value = Sys.Date())
    showNotification("Fill the form and press 'Save Visit' to create a new visit.", type = "message")
  })
  
  # ---- save visit only ----
  observeEvent(input$save_visit_only, {
    cat("SAVE VISIT ONLY CLICKED\n")
    
    ensure_visit_file()
    
    tryCatch({
      if (!isTruthy(input$hn_visit) || !isTruthy(input$visit_date)) {
        showNotification("Please provide both Patient Code (HN) and Date before saving.", type = "error")
        return()
      }
      
      if (!file.exists(patient_data_file)) {
        showNotification("Patient data file not found.", type = "error")
        return()
      }
      
      patient_data <- read.csv(patient_data_file, stringsAsFactors = FALSE)
      patient_data$hn <- toupper(trimws(patient_data$hn))
      hn_to_search <- toupper(trimws(input$hn_visit))
      
      patient_name <- patient_data$name[patient_data$hn == hn_to_search]
      if (length(patient_name) == 0) {
        showNotification("Patient name not found. Ensure the HN exists in patient data file.", type = "error")
        return()
      }
      
      existing_visits <- read.csv(visit_data_file, stringsAsFactors = FALSE)
      if ("hn" %in% names(existing_visits)) {
        existing_visits$hn <- toupper(trimws(existing_visits$hn))
      }
      
      is_editing <- !is.null(current_visit_row())
      
      if (is_editing && nrow(existing_visits) >= current_visit_row()) {
        old_row <- existing_visits[current_visit_row(), , drop = FALSE]
        visit_number <- old_row$visit_number
        visit_id <- old_row$visit_id
      } else {
        if (nrow(existing_visits) > 0 && "hn" %in% names(existing_visits)) {
          visit_number <- nrow(existing_visits[existing_visits$hn == hn_to_search, , drop = FALSE]) + 1
        } else {
          visit_number <- 1
        }
        visit_id <- paste0("V", format(Sys.time(), "%Y%m%d%H%M%S"), "_", hn_to_search)
      }
      
      formatted_visit_date <- format(as.Date(input$visit_date), "%d/%m/%Y")
      formatted_follow_up_date <- if (!is.null(input$follow_up_date) && !is.na(input$follow_up_date)) {
        format(as.Date(input$follow_up_date), "%d/%m/%Y")
      } else {
        ""
      }
      
      new_visit_data <- data.frame(
        visit_id = safe_string(visit_id),
        visit_number = safe_num(visit_number),
        hn = safe_string(hn_to_search),
        name = safe_string(patient_name[1]),
        visit_date = safe_string(formatted_visit_date),
        doctor_name = safe_string(input$doctor_name),
        patient_note = safe_string(input$patient_note),
        patient_status = safe_string(input$pateintstatus),
        refer_hospital_details = if (safe_string(input$pateintstatus) == "refer_hospital") safe_string(input$refer_hospital_details) else "",
        consult_opd_details = if (safe_string(input$pateintstatus) == "consult_opd") safe_string(input$consult_opd_details) else "",
        
        chest_tightness = safe_string(input$chest_tightness),
        chest_tightness_note = if (safe_string(input$chest_tightness) == "yes") safe_string(input$chest_tightness_note) else "",
        nervous_system = safe_string(input$nervous_system),
        nervous_system_note = if (safe_string(input$nervous_system) == "yes") safe_string(input$nervous_system_note) else "",
        urinal_abnormal = safe_string(input$urinal_abnormal),
        urinal_abnormal_note = if (safe_string(input$urinal_abnormal) == "yes") safe_string(input$urinal_abnormal_note) else "",
        headache = safe_string(input$headache),
        headache_note = if (safe_string(input$headache) %in% c("sometimes", "often")) safe_string(input$headache_note) else "",
        dizziness = safe_string(input$dizziness),
        dizziness_note = if (safe_string(input$dizziness) %in% c("sometimes", "often")) safe_string(input$dizziness_note) else "",
        dypsnea = safe_string(input$dypsnea),
        dypsnea_note = if (safe_string(input$dypsnea) %in% c("sometimes", "often")) safe_string(input$dypsnea_note) else "",
        leg_swelling = safe_string(input$leg_swelling),
        leg_swelling_note = if (safe_string(input$leg_swelling) %in% c("sometimes", "often")) safe_string(input$leg_swelling_note) else "",
        face_swelling = safe_string(input$face_swelling),
        face_swelling_note = if (safe_string(input$face_swelling) %in% c("sometimes", "often")) safe_string(input$face_swelling_note) else "",
        
        cr = safe_string(input$cr),
        na = safe_string(input$na),
        fbs = safe_string(input$fbs),
        hba1c = safe_string(input$hba1c),
        cho = safe_string(input$cho),
        ldl = safe_string(input$ldl),
        tg = safe_string(input$tg),
        hdl = safe_string(input$hdl),
        ast = safe_string(input$ast),
        alt = safe_string(input$alt),
        
        cc = safe_string(input$cc),
        cc_early_visit = if (safe_string(input$cc) == "early_visit") safe_string(input$cc_early_visit) else "",
        cc_late_visit = if (safe_string(input$cc) == "late_visit") safe_string(input$cc_late_visit) else "",
        cc_other = if (safe_string(input$cc) == "other") safe_string(input$cc_other) else "",
        pi = safe_string(input$pi),
        pi_abnormal = if (safe_string(input$pi) == "abnormal") safe_string(input$pi_abnormal) else "",
        pi_other = if (safe_string(input$pi) == "other") safe_string(input$pi_other) else "",
        
        adherence_alway_take_medicine = if ("alway_take_medicine" %in% (input$medication_adherence %||% character(0))) "yes" else "no",
        adherence_salty_control = if ("salty_control" %in% (input$medication_adherence %||% character(0))) "yes" else "no",
        adherence_exercise = if ("excercise" %in% (input$medication_adherence %||% character(0))) "yes" else "no",
        allergic_history = safe_string(input$allergic_history),
        
        bp_sys = safe_string(input$bp_sys),
        bp_dia = safe_string(input$bp_dia),
        pulse = safe_string(input$pulse),
        waist = safe_string(input$waist),
        height = safe_string(input$height),
        weight = safe_string(input$weight),
        
        heent = safe_string(input$heent),
        heent_abnormal = if (safe_string(input$heent) == "abnormal") safe_string(input$heent_abnormal) else "",
        heart = safe_string(input$heart),
        heart_abnormal = if (safe_string(input$heart) == "abnormal") safe_string(input$heart_abnormal) else "",
        lungs = safe_string(input$lungs),
        lungs_abnormal = if (safe_string(input$lungs) == "abnormal") safe_string(input$lungs_abnormal) else "",
        abd = safe_string(input$abd),
        abd_abnormal = if (safe_string(input$abd) == "abnormal") safe_string(input$abd_abnormal) else "",
        ext = safe_string(input$ext),
        ext_abnormal = if (safe_string(input$ext) == "abnormal") safe_string(input$ext_abnormal) else "",
        ns = safe_string(input$ns),
        ns_abnormal = if (safe_string(input$ns) == "abnormal") safe_string(input$ns_abnormal) else "",
        
        diagnosis = safe_vec(input$diagnosis),
        other_diagnosis = if ("Other" %in% (input$diagnosis %||% character(0))) safe_string(input$other_diagnosis) else "",
        bp_control_score = safe_num(input$bp_control_score),
        weight_control_score = safe_num(input$weight_control_score),
        self_care_score = safe_num(input$self_care_score),
        home_bp_score = safe_num(input$home_bp_score),
        hbpm_target = safe_string(input$hbpm_target),
        follow_up = safe_string(input$follow_up),
        follow_up_date = safe_string(formatted_follow_up_date),
        lab_tests = safe_vec(input$lab_tests),
        other_lab_tests = safe_string(input$other_lab_tests),
        
        complication_stroke = if ("complication_stroke" %in% (input$complication %||% character(0))) "yes" else "no",
        complication_cardio_mi = if ("complication_cardio_mi" %in% (input$complication %||% character(0))) "yes" else "no",
        complication_chf = if ("complication_chf" %in% (input$complication %||% character(0))) "yes" else "no",
        complication_kidney = if ("complication_kidney" %in% (input$complication %||% character(0))) "yes" else "no",
        complication_eye = if ("complication_eye" %in% (input$complication %||% character(0))) "yes" else "no",
        other_complication = safe_string(input$other_complication),
        
        precriptionadjust = safe_string(input$precriptionadjust),
        stringsAsFactors = FALSE
      )
      
      if (nrow(existing_visits) == 0) {
        to_save_visits <- new_visit_data
      } else if (is_editing && nrow(existing_visits) >= current_visit_row()) {
        for (col in setdiff(names(new_visit_data), names(existing_visits))) {
          existing_visits[[col]] <- ""
        }
        for (col in setdiff(names(existing_visits), names(new_visit_data))) {
          new_visit_data[[col]] <- ""
        }
        
        existing_visits <- existing_visits[, names(new_visit_data), drop = FALSE]
        existing_visits[current_visit_row(), ] <- new_visit_data[1, ]
        to_save_visits <- existing_visits
      } else {
        for (col in setdiff(names(new_visit_data), names(existing_visits))) {
          existing_visits[[col]] <- ""
        }
        for (col in setdiff(names(existing_visits), names(new_visit_data))) {
          new_visit_data[[col]] <- ""
        }
        
        existing_visits <- existing_visits[, names(new_visit_data), drop = FALSE]
        new_visit_data <- new_visit_data[, names(existing_visits), drop = FALSE]
        to_save_visits <- rbind(existing_visits, new_visit_data)
      }
      
      write.csv(to_save_visits, visit_data_file, row.names = FALSE)
      current_visit_id(visit_id)
      
      # reload visits
      load_patient_visits(hn_to_search)
      
      # force current form back to the exact saved visit_id
      all_visits_after_save <- read.csv(visit_data_file, stringsAsFactors = FALSE)
      all_visits_after_save$visit_id <- as.character(all_visits_after_save$visit_id)
      
      saved_row <- all_visits_after_save[all_visits_after_save$visit_id == as.character(visit_id), , drop = FALSE]
      
      if (nrow(saved_row) > 0) {
        current_visit_id(as.character(visit_id))
        populate_visit_form(saved_row[1, , drop = FALSE])
        
        patient_rows <- patient_visits()
        if (nrow(patient_rows) > 0 && "visit_id" %in% names(patient_rows)) {
          idx <- which(as.character(patient_rows$visit_id) == as.character(visit_id))[1]
          if (!is.na(idx)) current_visit_index(idx)
        }
        
        row_idx <- which(all_visits_after_save$visit_id == as.character(visit_id))[1]
        if (!is.na(row_idx)) current_visit_row(row_idx)
      }
      
      showNotification("✅ Visit saved successfully.", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Visit Save Error:", e$message), type = "error")
      print(e)
    })
  })
  
  # ---Save Medications Only ------
  observeEvent(input$save_meds_only, {
    cat("SAVE MEDS ONLY CLICKED\n")
    
    ensure_visit_file()
    ensure_medication_file()
    
    tryCatch({
      if (!isTruthy(input$hn_visit)) {
        showNotification("Please enter Patient Code (HN).", type = "error")
        return()
      }
      
      hn_to_search <- toupper(trimws(input$hn_visit))
      
      if (!file.exists(visit_data_file)) {
        showNotification("Visit file not found.", type = "error")
        return()
      }
      
      existing_visits <- read.csv(visit_data_file, stringsAsFactors = FALSE)
      if (!"hn" %in% names(existing_visits)) {
        showNotification("Visit file format is invalid.", type = "error")
        return()
      }
      
      existing_visits$hn <- toupper(trimws(existing_visits$hn))
      
      visit_id <- current_visit_id()
      
      if (is.null(visit_id) || is.na(visit_id) || visit_id == "") {
        showNotification("Visit ID not found. Please save or reload the visit first.", type = "error")
        return()
      }
      
      if (!"visit_id" %in% names(existing_visits)) {
        showNotification("Visit file does not contain visit_id.", type = "error")
        return()
      }
      
      existing_visits$visit_id <- as.character(existing_visits$visit_id)
      
      visit_row <- existing_visits[existing_visits$visit_id == visit_id, , drop = FALSE]
      
      if (nrow(visit_row) == 0) {
        showNotification("No matching visit found for this medication save.", type = "error")
        return()
      }
      
      formatted_visit_date <- as.character(visit_row$visit_date[1])
      
      visit_id <- as.character(visit_row$visit_id[1])
      formatted_visit_date <- as.character(visit_row$visit_date[1])
      
      if (is.na(visit_id) || visit_id == "") {
        showNotification("Visit ID not found. Please save visit information first.", type = "error")
        return()
      }
      
      medication_rows <- dplyr::bind_rows(
        getMedicationRows("diuretics_", "Diuretics", visit_id, hn_to_search, formatted_visit_date),
        getMedicationRows("aceis_", "ACEIs", visit_id, hn_to_search, formatted_visit_date),
        getMedicationRows("arbs_", "ARBs", visit_id, hn_to_search, formatted_visit_date),
        getMedicationRows("ccbs_", "CCBs", visit_id, hn_to_search, formatted_visit_date),
        getMedicationRows("beta_blockers_", "Beta Blockers", visit_id, hn_to_search, formatted_visit_date),
        getMedicationRows("oad_", "OAD", visit_id, hn_to_search, formatted_visit_date),
        getMedicationRows("statin_", "Statin", visit_id, hn_to_search, formatted_visit_date),
        getMedicationRows("anti_platelet_", "Anti-platelet", visit_id, hn_to_search, formatted_visit_date),
        getMedicationRows("other_", "Other", visit_id, hn_to_search, formatted_visit_date),
        getSPCRows(visit_id, hn_to_search, formatted_visit_date)
      )
      
      if (nrow(medication_rows) == 0) {
        
        existing_meds <- read.csv(medication_data_file, stringsAsFactors = FALSE)
        
        required_med_cols <- c("visit_id","hn","visit_date","med_group","drug_name","quantity")
        
        for (col in required_med_cols) {
          if (!col %in% names(existing_meds)) {
            existing_meds[[col]] <- character()
          }
        }
        
        existing_meds <- existing_meds[, required_med_cols, drop = FALSE]
        existing_meds$visit_id <- as.character(existing_meds$visit_id)
        
        existing_meds <- existing_meds[existing_meds$visit_id != visit_id, , drop = FALSE]
        
        write.csv(existing_meds, medication_data_file, row.names = FALSE)
        
        showNotification("✅ Medications cleared for this visit.", type = "message")
        load_patient_visits(hn_to_search)
        
        return()
      }
      
      existing_meds <- read.csv(medication_data_file, stringsAsFactors = FALSE)
      # force correct column types for empty or old files
      required_med_cols <- c("visit_id", "hn", "visit_date", "med_group", "drug_name", "quantity")
      
      for (col in required_med_cols) {
        if (!col %in% names(existing_meds)) {
          existing_meds[[col]] <- character()
        }
      }
      
      existing_meds <- existing_meds[, required_med_cols, drop = FALSE]
      
      existing_meds$visit_id   <- as.character(existing_meds$visit_id)
      existing_meds$hn         <- as.character(existing_meds$hn)
      existing_meds$visit_date <- as.character(existing_meds$visit_date)
      existing_meds$med_group  <- as.character(existing_meds$med_group)
      existing_meds$drug_name  <- as.character(existing_meds$drug_name)
      existing_meds$quantity   <- as.character(existing_meds$quantity)
      
      medication_rows$visit_id   <- as.character(medication_rows$visit_id)
      medication_rows$hn         <- as.character(medication_rows$hn)
      medication_rows$visit_date <- as.character(medication_rows$visit_date)
      medication_rows$med_group  <- as.character(medication_rows$med_group)
      medication_rows$drug_name  <- as.character(medication_rows$drug_name)
      medication_rows$quantity   <- as.character(medication_rows$quantity)
      
      if (nrow(existing_meds) > 0 && "visit_id" %in% names(existing_meds)) {
        existing_meds <- existing_meds[existing_meds$visit_id != visit_id, , drop = FALSE]
      }
      
      to_save_meds <- dplyr::bind_rows(existing_meds, medication_rows)
      write.csv(to_save_meds, medication_data_file, row.names = FALSE)
      
      showNotification("✅ Medications saved successfully.", type = "message")
      load_patient_visits(hn_to_search)
      
    }, error = function(e) {
      showNotification(paste("Medication Save Error:", e$message), type = "error")
      print(e)
    })
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
  
  #------ salty control monthly trend --------------
  output$salty_control_trend <- renderPlotly({
    df <- filtered_visits()
    
    if (nrow(df) == 0 || !all(c("adherence_salty_control", "visit_date") %in% names(df))) {
      p <- ggplot() +
        geom_text(aes(x = 1, y = 1, label = "No salty control data"), size = 5) +
        theme_minimal() +
        labs(title = "Salt Intake Control – Monthly Trend")
      return(ggplotly(p))
    }
    
    df <- df %>%
      mutate(
        # visit_date already Date from filtered_visits()
        adherence_salty_control = trimws(tolower(adherence_salty_control))
      ) %>%
      filter(!is.na(visit_date), adherence_salty_control %in% c("yes", "no")) %>%
      mutate(
        year = lubridate::year(visit_date),
        month = lubridate::month(visit_date, label = TRUE, abbr = TRUE)
      )
    
    monthly <- df %>%
      group_by(year, month) %>%
      summarise(
        yes = sum(adherence_salty_control == "yes"),
        no  = sum(adherence_salty_control == "no"),
        total = n(),
        pct_yes = round(100 * yes / total, 1),
        pct_no  = round(100 * no / total, 1),
        .groups = "drop"
      ) %>%
      arrange(year, month)
    
    if (nrow(monthly) == 0) {
      p <- ggplot() +
        geom_text(aes(x = 1, y = 1, label = "No monthly data to plot"), size = 5) +
        theme_minimal() +
        labs(title = "Salt Intake Control – Monthly Trend")
      return(ggplotly(p))
    }
    
    # build a proper year-month date
    monthly <- monthly %>%
      mutate(year_month = as.Date(paste(year, month, "1"), format = "%Y %b %d"))
    
    # reshape for Yes/No lines
    monthly_long <- monthly %>%
      select(year_month, yes, no, pct_yes, pct_no) %>%
      pivot_longer(cols = c(pct_yes, pct_no),
                   names_to = "adherence",
                   values_to = "percentage") %>%
      mutate(
        adherence = ifelse(adherence == "pct_yes", "Yes", "No"),
        adherence = factor(adherence, levels = c("Yes", "No")),
        count = ifelse(adherence == "Yes", yes, no),
        label = paste0(count, " (", percentage, "%)")   # number + percentage
      )
    
    p <- ggplot(monthly_long, aes(x = year_month, y = percentage, color = adherence, group = adherence)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      geom_text(aes(label = paste0(percentage, "%")), vjust = -0.6, size = 3 , color = "black") +
      # geom_text(aes(label = label), vjust = -0.6, size = 3, color = "black") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", expand = c(0.01, 0.01)) +
      coord_cartesian(ylim = c(0, 100)) +
      scale_color_brewer(palette = "Pastel1")
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = "Salt Intake Control – Monthly Trend",
        x = "Month",
        y = "Percentage (%)",
        color = "Adherence"
      )
    
    ggplotly(p, tooltip = c("x", "y", "adherence"))
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
  
  #------ always take medicine monthly trend --------------
  output$alway_take_medicine_trend <- renderPlotly({
    df <- filtered_visits()
    
    if (nrow(df) == 0 || !all(c("adherence_alway_take_medicine", "visit_date") %in% names(df))) {
      p <- ggplot() +
        geom_text(aes(x = 1, y = 1, label = "No medication adherence data"), size = 5) +
        theme_minimal() +
        labs(title = "Always Take Medication – Monthly Trend")
      return(ggplotly(p))
    }
    
    df <- df %>%
      mutate(
        adherence_alway_take_medicine = trimws(tolower(adherence_alway_take_medicine))
      ) %>%
      filter(!is.na(visit_date), adherence_alway_take_medicine %in% c("yes", "no")) %>%
      mutate(
        year = lubridate::year(visit_date),
        month = lubridate::month(visit_date, label = TRUE, abbr = TRUE)
      )
    
    monthly <- df %>%
      group_by(year, month) %>%
      summarise(
        yes = sum(adherence_alway_take_medicine == "yes"),
        no  = sum(adherence_alway_take_medicine == "no"),
        total = n(),
        pct_yes = round(100 * yes / total, 1),
        pct_no  = round(100 * no / total, 1),
        .groups = "drop"
      ) %>%
      arrange(year, month) 
    
    if (nrow(monthly) == 0) {
      p <- ggplot() +
        geom_text(aes(x = 1, y = 1, label = "No monthly data to plot"), size = 5) +
        theme_minimal() +
        labs(title = "Always Take Medication – Monthly Trend")
      return(ggplotly(p))
    }
    
    # build a proper year-month date
    monthly <- monthly %>%
      mutate(year_month = as.Date(paste(year, month, "1"), format = "%Y %b %d"))
    
    # reshape for Yes/No lines
    monthly_long <- monthly %>%
      select(year_month, yes, no, pct_yes, pct_no) %>%
      pivot_longer(cols = c(pct_yes, pct_no),
                   names_to = "adherence",
                   values_to = "percentage") %>%
      mutate(
        adherence = ifelse(adherence == "pct_yes", "Yes", "No"),
        adherence = factor(adherence, levels = c("Yes", "No")),
        count = ifelse(adherence == "Yes", yes, no),
        label = paste0(count, " (", percentage, "%)")   # number + percentage
      )
    
    p <- ggplot(monthly_long, aes(x = year_month, y = percentage, color = adherence, group = adherence)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      geom_text(aes(label = paste0(percentage, "%")), vjust = -0.6, size = 3 , color = "black") +
      # geom_text(aes(label = label), vjust = -0.6, size = 3, color = "black") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", expand = c(0.01, 0.01)) +
      coord_cartesian(ylim = c(0, 100)) +
      scale_color_brewer(palette = "Pastel1")
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Always Take Medication – Monthly Trend", 
           x = "Month", 
           y = "Percentage (%)", 
           color = "Adherence")
    
    ggplotly(p, tooltip = c("x", "y", "adherence"))
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
  
  #------ exercise monthly trend --------------
  output$exercise_trend <- renderPlotly({
    df <- filtered_visits()
    
    if (nrow(df) == 0 || !all(c("adherence_exercise", "visit_date") %in% names(df))) {
      p <- ggplot() +
        geom_text(aes(x = 1, y = 1, label = "No exercise data"), size = 5) +
        theme_minimal() +
        labs(title = "Exercise Adherence – Monthly Trend")
      return(ggplotly(p))
    }
    
    df <- df %>%
      mutate(
        adherence_exercise = trimws(tolower(adherence_exercise))
      ) %>%
      filter(!is.na(visit_date), adherence_exercise %in% c("yes", "no")) %>%
      mutate(
        year = lubridate::year(visit_date),
        month = lubridate::month(visit_date, label = TRUE, abbr = TRUE)
      )
    
    monthly <- df %>%
      group_by(year, month) %>%
      summarise(
        yes = sum(adherence_exercise == "yes"),
        no  = sum(adherence_exercise == "no"),
        total = n(),
        pct_yes = round(100 * yes / total, 1),
        pct_no  = round(100 * no / total, 1),
        .groups = "drop"
      ) %>%
      arrange(year, month) 
    
    if (nrow(monthly) == 0) {
      p <- ggplot() +
        geom_text(aes(x = 1, y = 1, label = "No monthly data to plot"), size = 5) +
        theme_minimal() +
        labs(title = "Exercise – Monthly Trend")
      return(ggplotly(p))
    }
    
    # build a proper year-month date
    monthly <- monthly %>%
      mutate(year_month = as.Date(paste(year, month, "1"), format = "%Y %b %d"))
    
    # reshape for Yes/No lines
    monthly_long <- monthly %>%
      select(year_month, yes, no, pct_yes, pct_no) %>%
      pivot_longer(cols = c(pct_yes, pct_no),
                   names_to = "adherence",
                   values_to = "percentage") %>%
      mutate(
        adherence = ifelse(adherence == "pct_yes", "Yes", "No"),
        adherence = factor(adherence, levels = c("Yes", "No")),
        count = ifelse(adherence == "Yes", yes, no),
        label = paste0(count, " (", percentage, "%)")   # number + percentage
      )
    
    p <- ggplot(monthly_long, aes(x = year_month, y = percentage, color = adherence, group = adherence)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      geom_text(aes(label = paste0(percentage, "%")), vjust = -0.6, size = 3 , color = "black") +
      # geom_text(aes(label = label), vjust = -0.6, size = 3, color = "black") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", expand = c(0.01, 0.01)) +
      coord_cartesian(ylim = c(0, 100)) +
      scale_color_brewer(palette = "Pastel1")
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Exercise Adherence – Monthly Trend", x = "Month", y = "Percentage (%)", color = "Adherence")
    
    ggplotly(p, tooltip = c("x", "y", "adherence"))
  })
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
