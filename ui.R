
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny); library(rCharts)
#options(RCHART_LIB = 'polycharts')

shinyUI(fluidPage(
  tags$head(tags$style(HTML("
            .sidebyside label {width: auto; font-size:12px;}
            .sidebyside input {display: inline-block; width: 70px; float: right;}
            .sidebyside .col-sm-4 {min-width: 300px;}
            .lowerunits {margin-top:25px;}
            .floaterPanel {position: fixed; top: 75px; z-index:999;
                            width: 910px; margin: 0 auto;}"))),    

  # Application title
  titlePanel("Accurate Estimates of Calorie Needs"),
  p("Introduction and instructions"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons("sex", "Sex", list("Male"="m", "Female"="f")),
      fluidRow(column(6, numericInput("age", "Age", value = 30, min = 18)),
               column(6, p("Years", class="lowerunits"))),
      fluidRow(column(6, conditionalPanel("input.ht_units != 'cm'",
                                          textInput("height_ftin", "Height", value="5'9.7\"")),
                      conditionalPanel("input.ht_units == 'cm'",
                                       numericInput("height_cm", "Height", value = 177))),
               column(6,div(radioButtons("ht_units",NULL, 
                                         list(`ft\' in\"` = 'ftin', cm = 'cm'),
                                         inline = TRUE),class="lowerunits"))),
      fluidRow(column(6, conditionalPanel("input.wt_units!='kg'", 
                                          numericInput("weight_lb", "Weight", value = 154)),
                      conditionalPanel("input.wt_units=='kg'", 
                                       numericInput("weight_kg", "Weight", value = 70))),
               column(6,div(radioButtons("wt_units", NULL, list("lb", "kg"), inline = TRUE),
                            class="lowerunits"))),
      span("Physical Activity Level", style="font-weight:700;"),
      actionLink("PAL_help", "(Help)"),
      radioButtons("PALcat", NULL, 
                   list("Sedentary"="Sedentary", "Low Active"="Low_Active", 
                        "Active"="Active", "Very Active"="Very_Active")),
     
      conditionalPanel("false", 
                        numericInput("PAL_help_visible", "is the PAL screen showing?", 
                                     value=0)),
      actionButton("goButton", "Calculate Energy Needs"),
      conditionalPanel("input.PAL_help_visible>0",
                       div(
                           div(class="panel-heading",
                           h3(class="panel-title", "Determine your physical activity level")),
                           div(class="panel-body", p("The physical activity levels are defined as..."),
                           p("To calculate your physical activity level, enter the total 
                              number of hours your spend", strong("per week"), 
                             "doing the following activities:"),
                           
                           uiOutput("activityList"),
                           #textOutput("PAL_calc"),
                           uiOutput("PAL_calc"),
                           actionButton("PAL_calculate", "Save and Continue")),
                           class="panel panel-primary floaterPanel"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      h3("Estimated Energy Needs"),
      textOutput("EER"),
      showOutput("chart", "polycharts")
    )
  )
))
