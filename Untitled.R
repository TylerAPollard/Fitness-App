## Tyler's workouts

ui <- fluidPage(
  hr(),
  mainPanel(rHandsontableOutput("lifting_input_table"))
)

server <- function(input, output, session) {
  
  ## Exercise options
  Chest_df <- data.frame(
    `Muscle Group` = "Chest",
    Exercise = c(
      "Barbell Bench",
      "Dumbbell Bench",
      "Incline Dumbbell Bench",
      "Dumbbell Flys"
    ),
    check.names = FALSE
  )
  
  Back_df <- data.frame(
    `Muscle Group` = "Back",
    Exercise = c(
      "Lat Pulldown",
      "Dumbbell Row",
      "Cable Rows"
    ),
    check.names = FALSE
  )
  
  Legs_df <- data.frame(
    `Muscle Group` = "Legs",
    Exercise = c(
      "Squat"
    ),
    check.names = FALSE
  )
  
  Shoulders_df <- data.frame(
    `Muscle Group` = "Shoulders",
    Exercise = c(
      "Dumbbell Shoulder Press"
    ),
    check.names = FALSE
  )
  
  Biceps_df <- data.frame(
    `Muscle Group` = "Biceps",
    Exercise = c(
      "Dumbell Hammer Curl"
    ),
    check.names = FALSE
  )
  
  Triceps_df <- data.frame(
    `Muscle Group` = "Triceps",
    Exercise = c(
      "Cross Cable Tricep Extension"
    ),
    check.names = FALSE
  )
  
  Abs_df <- data.frame(
    `Muscle Group` = "Abs",
    Exercise = c(
      "SD Machine"
    ),
    check.names = FALSE
  )
  
  Exercise_df <- rbind(
    Chest_df,
    Back_df,
    Legs_df,
    Shoulders_df,
    Biceps_df,
    Triceps_df,
    Abs_df
  )
  Exercise_df <- data.table(Exercise_df)
  
  lifting_shell_df <- data.table(
    `Muscle Group` = NA,
    Exercise = NA,
    Set = NA,
    Reps = NA,
    #Tempo = NA,
    Weight = NA,
    Rest = NA,
    check.names = FALSE
  )
  
  displayRV <- reactiveVal(lifting_shell_df)
  selectedRowRV <- reactiveVal(NULL)
  
  observeEvent(input$lifting_input_table, {
    displayRV(hot_to_r(input$lifting_input_table))
  })
  
  output$lifting_input_table <- renderRHandsontable({
    rhandsontableObj <- rhandsontable(displayRV(), 
                                      rowHeaders = NULL, 
                                      stretchH = "all",
                                      selectCallback = TRUE,
                                      widisplayRVh = 300, 
                                      height = 300, 
                                      digits = 6, 
                                      overflow = "visible")
    if(is.null(selectedRowRV())){
      for(col in names(displayRV())[1:2]){
        rhandsontableObj <- hot_col(rhandsontableObj, col, allowInvalid = FALSE, type = "dropdown", source = c(NA_character_, sort(unique(Exercise_df[[col]]))), readOnly = FALSE)
      }
    } else {
      rowOptionsDT <- Exercise_df[selectedRowRV(), on = names(selectedRowRV())]
      for(col in names(displayRV())[1:2]){
        rhandsontableObj <- hot_col(rhandsontableObj, col, allowInvalid = FALSE, type = "dropdown", source = c(NA_character_, sort(unique(rowOptionsDT[[col]]))), readOnly = TRUE) %>%
          hot_cell(row = input$lifting_input_table_select$select$r, col = col, readOnly = FALSE)
      }
    }
    rhandsontableObj %>% 
      hot_col(col = "Set", type = "numeric") %>%
      hot_col(col = "Reps", type = "numeric") %>%
      # hot_col(col = "Tempo", type = "dropdown", source = c(
      #   "1-0-1-0",
      #   "2-0-1-0",
      #   "3-0-1-0",
      #   "1-0-1-1",
      # )) %>%
      hot_col(col = "Weight", type = "numeric") %>%
      hot_col(col = "Rest", type = "numeric")
  })
  
  observeEvent(input$lifting_input_table, {
    selectedRowDT <- hot_to_r(input$lifting_input_table)[input$lifting_input_table_select$select$r,]
    if(nrow(selectedRowDT) > 0){
      selectedRowDT <- selectedRowDT[, which(unlist(lapply(selectedRowDT, function(x)!all(is.na(x))))), with = FALSE] # drop NA columns
      if(nrow(selectedRowDT) > 0){
        selectedRowRV(selectedRowDT)
      } else {
        selectedRowRV(NULL)
      }
    } else {
      selectedRowRV(NULL)
    }
  })
}

# library(shiny)
# library(rhandsontable)
# library(data.table)
# library(usa)
# 
# Exercise_df <- merge(merge(data.table("state_region" = as.character(state.region), "state" = state.abb), counties), zipcodes,  allow.cartesian = TRUE)
# Exercise_df <- Exercise_df[sample(nrow(Exercise_df), 1000), ] # reduce data
# 
# # lifting_shell_df <- data.table(state_region = rep(NA_character_, 10), state = rep(NA_character_, 10), fips = rep(NA_character_, 10), 
# #                       name = rep(NA_character_, 10), zip = rep(NA_character_, 10), city = rep(NA_character_, 10), lat = NA_real_, 
# #                       long = NA_real_)
# lifting_shell_df <- data.table(state_region = rep(NA, 10), state = rep(NA, 10), fips = rep(NA, 10), 
#                                name = rep(NA, 10), zip = rep(NA, 10), city = rep(NA, 10), lat = NA, 
#                                long = NA)
# 
# ui <- fluidPage(
#   hr(),
#   mainPanel(rHandsontableOutput("lifting_input_table"))
# )
# 
# server <- function(input, output, session) {
#   
#   displayRV <- reactiveVal(lifting_shell_df)
#   selectedRowRV <- reactiveVal(NULL)
#   
#   observeEvent(input$lifting_input_table, {
#     displayRV(hot_to_r(input$lifting_input_table))
#   })
#   
#   output$lifting_input_table <- renderRHandsontable({
#     rhandsontableObj <- rhandsontable(displayRV(), rowHeaders = NULL, stretchH = "all", selectCallback = TRUE, widisplayRVh = 300, height = 300, digits = 6)
#     if(is.null(selectedRowRV())){
#       for(col in names(displayRV())){
#         rhandsontableObj <- hot_col(rhandsontableObj, col, allowInvalid = FALSE, type = "dropdown", source = c(NA_character_, sort(unique(Exercise_df[[col]]))), readOnly = FALSE)
#       }
#     } else {
#       rowOptionsDT <- Exercise_df[selectedRowRV(), on = names(selectedRowRV())]
#       for(col in names(displayRV())){
#         rhandsontableObj <- hot_col(rhandsontableObj, col, allowInvalid = FALSE, type = "dropdown", source = c(NA_character_, sort(unique(rowOptionsDT[[col]]))), readOnly = TRUE) %>% hot_cell(row = input$lifting_input_table_select$select$r, col = col, readOnly = FALSE)
#       }
#     }
#     rhandsontableObj
#   })
#   
#   observeEvent(input$lifting_input_table, {
#     selectedRowDT <- hot_to_r(input$lifting_input_table)[input$lifting_input_table_select$select$r,]
#     if(nrow(selectedRowDT) > 0){
#       selectedRowDT <- selectedRowDT[, which(unlist(lapply(selectedRowDT, function(x)!all(is.na(x))))), with = FALSE] # drop NA columns
#       if(nrow(selectedRowDT) > 0){
#         selectedRowRV(selectedRowDT)
#       } else {
#         selectedRowRV(NULL)
#       }
#     } else {
#       selectedRowRV(NULL)
#     }
#   })
#   
# }

shinyApp(ui = ui, server = server)
