
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny); library(rCharts)
library(propagate); library(dplyr);
library(Hmisc)
source("setup.R")
options(RCHART_WIDTH = 600)

activities <- read.csv("mets.csv")

shinyServer(function(input, output, clientData, session) {

    ### reactives
    
    # body mass index calculated from height and weight
    bmi <- reactive({
        round(input$weight_kg / (input$height_cm/100)^2, digits=1)
    })
    
    # calculate the estimated energy needs from the reconstructed non-linear least
    # squares model (see setup.R) using error propagation  and monte carlo simulation
    # (propagate package)
    EER_range <- reactive({
        input$goButton
        # extract the model for the sex and bmi category
        modelName <- paste(isolate(input$sex), 
                           ifelse(isolate(bmi()) > 25, "overweight","normal"), 
                           sep=".")
        m <- models[[modelName]]
        #calculate prediction interval
        isolate({
            nd <- data.frame(age = input$age, 
                             PALcatLow_Active = as.integer(input$PALcat == "Low_Active"), 
                             PALcatActive = as.integer(input$PALcat == "Active"),
                             PALcatVery_Active = as.integer(input$PALcat == "Very_Active"),
                             ht = input$height_cm/100, wt = input$weight_kg)
            p <- predictNLS(m, nd, interval="prediction")
            list(propLower=p$summary$`Prop.2.5%`, 
                 propUpper=p$summary$`Prop.97.5%`)
            
        })
    })
    
    ### observers
    
    # keep the height input with the non-selected units up to date with the 
    # current input
    observe({
        if(input$ht_units=="ftin") {
            matches <- strsplit(input$height_ftin, "[^[:digit:]\\.]")[[1]]
            matches <- suppressWarnings(as.numeric(matches))
            matches <- matches[which(!is.na(matches))]
            if(length(matches) > 1) inc <- matches[1]*12 + matches[2] else
                if(length(matches) == 1 && grepl("'", input$height_ftin)) 
                    inc <- matches * 12 else
                        inc <- matches
            if(length(inc))updateNumericInput(session, "height_cm", 
                                              value = round(inc * 2.54, digits=0))
        }
        
        if(input$ht_units=="cm") {
            inc <- round(input$height_cm / 2.54, digits=0)
            ft <- floor(inc / 12)
            inc <- inc %% 12
            updateTextInput(session, "height_ftin", 
                            value = paste0(ft, "'", inc, "\""))
        }
    })
    
    # keep the input for the non-selected units (hidden) in sync with the current input
    observe({
        if(input$wt_units == "lb") 
            updateNumericInput(session, "weight_kg", 
                               value=round(input$weight_lb / 2.2, digits = 1))
        if(input$wt_units == "kg")
            updateNumericInput(session, "weight_lb", 
                               value=round(input$weight_kg * 2.2, digits = 1))
    })
    
    # show and hide PAL help screen using hidden numeric input
    observe({
        input$PAL_help
        updateNumericInput(session, "PAL_help_visible", value = 1)
    })
    observe({
        input$PAL_calculate
        updateNumericInput(session, "PAL_help_visible", value = 0)
    })
    
    ### renderers
    
    #text version of estimated energy range
    output$EER <- renderText({
        validate(need(input$goButton > 0, "Select your age, height, weight, and physical activity level to the left, then click \"Calculate Energy Needs\""),
        need(isolate(input$age) >= 18, "At present, energy needs are available only  for adults 18 years or older"))

       paste("Estimated Energy Requirement:", 
             round(EER_range()$propLower, digits = 0),
             " - ", round(EER_range()$propUpper, digits = 0))
    })
 
    # renders rCharts polychart of the estimated EER range compared to nearby
    # observed values from the IOM data set
    output$chart <- renderChart2({
        validate(need(input$goButton > 0, ""),
                 need(isolate(input$age) >= 18, ""))
        pData <- isolate({
            adults %>% filter(sex == input$sex, PALcat == input$PALcat) %>%
                mutate(ageDiff = age - input$age, bmiDiff = BMI - bmi()) %>% zoomPlot 
        })
        p <- rPlot(TEE ~ BMI, data=pData, type="point", color=list(var="ageDiff"),
                   tooltip = "#! function(x) { 
                                    return('Age: ' + Math.round(x.age) +
                                          ' BMI: ' + Math.round(x.BMI) + 
                                          ' Energy needs: ' + Math.round(x.TEE) +
                                          ' calories'); 
                                } !#" )
        p$layer(x="BMI", y="TEE", type="line", size=list(const="20"),
                opacity=list(const=".5"), color=list(const="green"), 
                data=data.frame(BMI = c(isolate(bmi()), isolate(bmi())),
                                TEE = c(EER_range()$propLower,
                                        EER_range()$propUpper))
                 )
        p$guides(color = list(title = "Age Difference"),
                 x = list(title = "Body Mass Index (kg/m²)"),
                 y = list(title = "Energy Needs (Calories)"))
        p
    }) 
    
    # dynamically creates a numeric input for each activity in the activities table
    # for the PAL helper panel
    output$activityList <- renderUI({
        act <- split(as.character(activities$Activity), cut2(1:nrow(activities), g=3))
        div(class = "sidebyside", 
            fluidRow(column(4,lapply(as.character(act[[1]]), 
                           function(x) {numericInput(x,x, value = 0, step=0.5, min = 0)})),
                     column(4,lapply(as.character(act[[2]]), 
                           function(x) {numericInput(x,x, value = 0, step=0.5, min = 0)})),
                     column(4,lapply(as.character(act[[3]]), 
                           function(x) {numericInput(x,x, value = 0, step=0.5, min = 0)}))
                     )
        )
            
    })
    
    # Writes description of physical activity level calculation to the bottom of the
    # PAL help panel, also updates the PAL_cat radio buttons based on the results
    output$PAL_calc <- renderUI({
        
        # MET inflation factor formula from IOM energy manual
        # Accounts for indirect increases in energy expenditure from exercise
        # MET_factor =  1.15 (EPOC) / .9 (TEF) / BEE (estimated) / 1 MET-day
        # MET-day = 1440 minutes * 0.0175 kcal/kg/min * weight
        
        # estimate basal energy requirement via Harris-Benedict
        if(input$sex=="m") BEE <- 
                input$weight_kg * 13.397 + input$height_cm * 4.799 - input$age * 5.677 + 88.632
        else BEE <-
                input$weight_kg * 9.247 + input$height_cm * 3.098 - input$age * 4.33 + 447.593
        
        MET_factor <- 1.15 / .9 / (BEE / (1440 * 0.0175 * input$weight_kg))
        
        
        # Total of metabolic equivalents from all activities entered
        
        # extract the activity related inputs
        acts <- names(input)[which(names(input) %in% activities$Activity)]

        # Abort if this function gets called before the activity inputs have been initialized
        if(length(acts)<1) return("")

        input_activity_values <- sapply(acts, function(x){input[[x]]})
        
        # to get relative increase in metabolism per activity we take the metabolic
        # equivalents value (METs from the activities table) less one multiplied by
        # the proportion of the day spent on the activity 
        #   (hours per week of activity ) / (total hours per week)
        # and then multiply by inflation factor
        row.names(activities) <- activities$Activity
        activity_METs <- 
            sum((activities[acts, "METs"] - 1) * input_activity_values, na.rm = TRUE)
        delta_PAL <- activity_METs / 168 * MET_factor
        
        # Convert calculated PAL into factor using switch and cut2 for convenience
        # (but cut2 only works right if at least one value for each category is present
        #  so some filler data is added and the dropped)
        PAL_category <- switch(as.numeric(first(cut2(c(delta_PAL + 1.39, 1, 1.5, 1.7, 2), 
                                                     cuts=c(1.4, 1.6, 1.9)))),
                               "Sedentary", "Low_Active", "Active", "Very_Active")
        
        updateRadioButtons(session, "PALcat", selected=PAL_category)                
        
        div(p(paste("Physical activity level:", round(activity_METs, digits=1), 
                    "activity MET-hours per week /", 
                    "168 hours per week ×", round(MET_factor, digits = 2), 
                    "(personalized energy expenditure inflation factor)",
                    " + 1.39 for basic activities of daily living =", 
                    round(delta_PAL + 1.39, digits = 2))), 
            p("Your physical activity category is: ", 
              strong(gsub("_", " ", PAL_category))))
        
    })
    

})

# misc helper funcs

# Function that filters data to show just points similar to the user,
# but takes into account sparsity of data to avoid overzooming 
zoomPlot <- function(data) {
    ageThresh <- max(abs(data$ageDiff))
    ageStep <- ageThresh / 20
    bmiThresh <- max(abs(data$bmiDiff))
    bmiStep <- bmiThresh / 20
    odd <- FALSE
    while(nrow(data) > 20) {
        if(odd) {
            ageThresh <- ageThresh - ageStep
            odd=FALSE
        } else {
            bmiThresh <- bmiThresh - bmiStep
            odd=TRUE
        }
        data <- filter(data, abs(ageDiff) < ageThresh, abs(bmiDiff) < bmiThresh)
    }
    data
} 
