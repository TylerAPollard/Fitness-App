#### Fitness App for tracking progress
## Tyler Pollard
## 18 March 2023

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(shinyBS)
library(shinyjs)
library(colourpicker)
library(bs4Dash)
library(tidyverse)
library(plyr)
library(dplyr)
library(readxl)
library(readr)
library(haven)
library(ggplot2)
library(data.table)
library(plotly)
library(shinycssloaders)
library(RColorBrewer)
library(sp)
library(htmltools)
library(fresh)
library(likert)
library(extrafont)
library(stringr)
library(reshape2)
library(png)
library(ggpubr)
library(htmlTable)
library(tibble)
library(EnvStats)
library(xtable)
library(grid)
library(DT)
library(rhandsontable)

# Set theme
my_theme <- create_theme(
    theme = "paper",
    bs4dash_sidebar_dark(
        bg = "#2d3b4d"
    ),
    bs4dash_status(
        primary = "#3b4d63", info = "#507B80"
    )
)
tags$style(".buttoncolor.bttn-primary{background-color: #6399b8")

# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(dark = NULL, footer = dashboardFooter(left = br()), freshTheme = my_theme,
                  # ========= Dahsboard Header ===============
                  header = dashboardHeader(
                      title = dashboardBrand(
                          title = div(style = "font-size:14pt",
                                      align = "center", 
                                      "Fitness App",
                                      dashboardBadge(
                                          color = "warning",
                                          rounded = TRUE,
                                          position = "right",
                                          "v1.0")
                          ),
                          color = "primary"
                      ),
                      compact = FALSE,
                      rightUi = tags$li(
                          class = "dropdown",
                          dropdownMenu(
                              badgeStatus = NULL,
                              type = "notifications",
                              headerText = "Fitness App",
                              icon = icon("info-circle"),
                              notificationItem(
                                  inputId = "info1",
                                  text = "Developer: Tyler Pollard",
                                  icon = icon("users-cog"),
                                  status = "info"
                              ),
                              notificationItem(
                                  inputId = "info2",
                                  text = "Release Date: 18 March 2023",
                                  icon = icon("calendar"),
                                  status = "info"
                              ),
                              notificationItem(
                                  inputId = "info3",
                                  text = "Version: 1.0",
                                  icon = icon("code"),
                                  status = "info"
                              )
                          )
                      )
                  ), # close header
                  scrollToTop = TRUE,
                  # ============ Dashboard Sidebar =============
                  sidebar = dashboardSidebar(
                      skin = "dark",
                      elevation = 5,
                      fixed = FALSE,
                      minified = FALSE,
                      status = "primary",
                      compact = TRUE,
                      # ------------ Sidebar Menu ---------------
                      sidebarMenu(
                          id = "menu_items",
                          menuItem(text = "Home", tabName = "home", icon = icon("home")),
                          menuItem(text = "Workouts", tabName = "workouts",
                                   menuSubItem(text = "Workout Options", tabName = "workout_options"),
                                   menuSubItem(text = "Workout Input", tabName = "workout_input_tab")
                          ),
                          menuItem(text = "Nutrition", tabName = "nutrition")
                      ) # close sidebar menu
                  ), # close dashboard sidebar
                  # ============= Dashboard Body ================
                  body = dashboardBody(
                      tabItems(
                          # ============ Home Tab ===============
                          tabItem(
                              tabName = "home",
                              jumbotron(
                                  width = 12,
                                  closable = FALSE,
                                  collapsible = FALSE,
                                  headerBorder = FALSE,
                                  background = "info",
                                  title = "Welcome to the Fitness App",
                                  style = "font-size: 36pt",
                                  lead = "Me and my babeh created this app to track our fitness and nutrtion.",
                                  fluidRow(
                                      column(6,
                                             p("This dashboard was developed by Tyler Pollard and Gabrielle Julbert."),
                                             div("Questions, comments, or suggestions can be directed to: ",
                                                 em("tpollar@g.clemson.edu, gabbyjulbertphotos@gmail.com")),
                                             br(),
                                             br()
                                      ),
                                      column(6,
                                             div(img(src = "coverphoto.png", height = 400, width = 400))
                                      )
                                  ), # close fluidRow
                                  btnName = "Contact Us",
                                  href = "mailto:tpollar@g.clemson.edu; gabbyjulbertphotos@gmail.com"
                              ) # close jumbotron
                          ), # end home tab
                          # ============= Workout Tab ===================
                          tabItem(
                              tabName = "workouts",
                              fluidPage(
                                  h1("Workouts"),
                                  hr()
                              ) # close fluid page
                          ), # close workout tab
                          # ============= Workout Options ===============
                          tabItem(
                              tabName = "workout_options",
                              fluidPage(
                                  h1("Workouts Options"),
                                  hr()
                              ) # close fluid page
                          ), # close workout options tab
                          # ============= Workout Input Tab ===============
                          tabItem(
                              tabName = "workout_input_tab",
                              fluidPage(
                                  h1("Workouts Inputs"),
                                  hr(),
                                  tabsetPanel(
                                      id = "workout_input_tabs",
                                      type = "pills",
                                      tabPanel(
                                          id = "workout_input",
                                          title = "Workout Input",
                                          fluidPage(
                                              br(),
                                              fluidRow(
                                                  column(3,
                                                         box(id = "workout_details",
                                                             width = 12,
                                                             title = h4("Workout Details"),
                                                             pickerInput(
                                                                 inputId = "user_input",
                                                                 label = "Select User",
                                                                 choices = c(
                                                                     "Tyler Pollard" = "Tyler Pollard",
                                                                     "Gabrielle Julbert" = "Gabrielle Julbert"
                                                                 ),
                                                                 selected = NULL
                                                             ), # end picker input
                                                             dateInput(
                                                                 inputId = "date_input", label = "Select Date", value = NULL
                                                             ), # end date input
                                                             pickerInput(
                                                                 inputId = "workout_type",
                                                                 label = "Select Workout Type",
                                                                 choices = c(
                                                                     "Cardio" = "cardio",
                                                                     "Weight Lifting" = "lifting",
                                                                     "Abs" = "abs"
                                                                 ),
                                                                 selected = NULL
                                                             ) # end picker input
                                                         ) # end box
                                                  ), # end input column
                                                  column(9,
                                                         box(id = "exercise_input",
                                                             width = 12,
                                                             title = h4("Workout Table Input"),
                                                             fluidRow(
                                                                 column(3,
                                                                        actionBttn(
                                                                            inputId = "add_row",
                                                                            label = "Add Data Row"
                                                                        )
                                                                 ),
                                                                 column(3,
                                                                        actionBttn(
                                                                            inputId = "delete_row",
                                                                            label = "Delete Data Row"
                                                                        )
                                                                 )
                                                             ),
                                                             br(),
                                                             fluidRow(
                                                                 rHandsontableOutput(
                                                                     outputId = "lifting_input_table"
                                                                 )
                                                             ), # end fluidRow
                                                             br(),
                                                             fluidRow(
                                                                 actionBttn(
                                                                     inputId = "update_workout_log",
                                                                     label = "Update Workout Log"
                                                                 )
                                                             ) # End fluid Row
                                                         ) # end box
                                                  ) # end edit table column
                                              ) # end fluid Row
                                          ) # close fluid page
                                      ), # end tabPanel
                                      tabPanel(
                                          id = "workout_log",
                                          title = "Workout Log"
                                      ) # end tabPanel
                                  ) # end tabsetPanel
                              ) # end workout input fluid page
                          ), # close workout options tab
                          # ============= Nutrition Tab =================
                          tabItem(
                              tabName = "nutrition",
                              fluidPage(
                                  h1("Nutrition"),
                                  hr()
                              ) # close fluid page
                          ) # close nutrition tab
                      ) # end Tab items
                  ) # end dashboard body
    ) # end dashboard Page
) # end ShinyUI
