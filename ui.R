
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny); library(rCharts)
#options(RCHART_LIB = 'polycharts')

## lengthy text objects are defined here rather than in the nested
## document structure

PAL_help_text <- div(p("A Physical Activity Level above sedentary is 
determined from the amount of time spent engaged in exercise or activities 
beyond the basic tasks associated with normal living. Based on a moderate 
intensity activity (walking at 4 mph), 30 years of age, and average height 
and weight, at least 30 minutes of weekly activity would be required to reach 
the Low Active category, 8 hours per week for Active, and 19 hours per week 
for Very Active."), p("You can calculate your physical activity level based on 
your age, height, weight, and activities below."), p("Enter the total number 
of ", strong("hours per week"), " you spend doing the following activities:"))

intro_text <- div(p("This application provides estimates of daily calorie
requirements for individuals. Because of differences between people, the 
result is not an exact value but a range of possible energy levels. The 
range is determined statistically so that, for 95% of people, their true
individual energy requirement will be between the lower and upper values.
"),p("To estimate your individual energy needs, enter your sex,
age, height, weight, and physical activity level below, then click the 
\"Calculate Energy Needs\" button. If you are unsure of your physical 
activity level, click the \"Help\" link in that section to use the physical activity level
calculator"))

shinyUI(fluidPage(
  tags$head(tags$style(HTML("
            .sidebyside label {width: auto; font-size:12px;}
            .sidebyside input {display: inline-block; width: 70px; float: right;}
            .sidebyside .col-sm-4 {min-width: 300px;}
            .lowerunits {margin-top:28%;}
            .floaterPanel {position: absolute; top: 40px; z-index:999;
                            width: 910px; margin: 0 auto;}
            .emphasizeText { font-weight: bold; font-size: larger;}
            body {max-width: 1100px; margin: 0 auto;}"))),    

  # Application title and introduction
  titlePanel("Accurate Estimation of Individual Calorie Needs"),
  intro_text,
  conditionalPanel("input.PAL_help_visible>0",
                   div(
                       div(class="panel-heading",
                           h3(class="panel-title", "Determine your physical activity level")),
                       div(class="panel-body", 
                           PAL_help_text,
                           uiOutput("activityList"),
                           uiOutput("PAL_calc"),
                           actionButton("PAL_calculate", "Accept and Continue")),
                       class="panel panel-primary floaterPanel")),
  # Sidebar with inputs for calculation
  sidebarLayout(
    sidebarPanel(
      radioButtons("sex", "Sex", list("Male"="m", "Female"="f")),
      fluidRow(column(6, numericInput("age", "Age", value = 40, min = 18)),
               column(6, p("Years", class="lowerunits"))),
      fluidRow(column(6, conditionalPanel("input.ht_units != 'cm'",
                                          textInput("height_ftin", "Height", value="5'9\"")),
                      conditionalPanel("input.ht_units == 'cm'",
                                       numericInput("height_cm", "Height", value = 175))),
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
      actionButton("goButton", "Calculate Energy Needs")
      
    ),

    # Results section
    mainPanel(
        
        h3("Your Estimated Energy Needs"),
        uiOutput("EER"),
        showOutput("chart", "polycharts"),
        textOutput("chart_caption")
    )
  )
))
