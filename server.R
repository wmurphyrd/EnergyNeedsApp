
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
    # squares model (see setup.R) using error propagation and monte carlo simulation
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
    
    activity_METs <- reactive({
        # Total of metabolic equivalents from all activities entered
        
        # extract the activity related inputs
        acts <- names(input)[which(names(input) %in% activities$Activity)]
        
        # Abort if this function gets called before the activity inputs have been initialized
        if(length(acts)<1) return(NA)
        
        input_activity_values <- sapply(acts, function(x){input[[x]]})
        
        # (activity MET - 1) * hours = MET-hours above basal metabolic rate
        row.names(activities) <- activities$Activity
        sum((activities[acts, "METs"] - 1) * input_activity_values, na.rm = TRUE)       
    })
    
    MET_factor <- reactive({
        # MET inflation factor formula from IOM energy manual
        # Accounts for indirect increases in energy expenditure from exercise
        # MET_factor =  1.15 (EPOC) / .9 (TEF) / BEE (estimated) / 1 MET-day
        # MET-day = 1440 minutes * 0.0175 kcal/kg/min * weight
        
        # estimate basal energy requirement via Harris-Benedict
        if(input$sex=="m") BEE <- 
                input$weight_kg * 13.397 + input$height_cm * 4.799 - 
                input$age * 5.677 + 88.632
        else BEE <-
                input$weight_kg * 9.247 + input$height_cm * 3.098 - 
                input$age * 4.33 + 447.593
        
        1.15 / .9 / (BEE / (1440 * 0.0175 * input$weight_kg))        
    })
    
    PAL_value <- reactive({
        # to get PAL we take the metabolic
        # equivalents value  multiplied by
        # the proportion of the day spent on the activity 
        #   (hours per week of activity ) / (total hours per week)
        # and then multiply by inflation factor and add 1.39 for basic
        # activities of daily living
        activity_METs() / 168 * MET_factor() + 1.39
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
    output$EER <- renderUI({
        validate(need(input$goButton > 0, "Your estimated energy needs will appear after clicking the \"Calculate Energy Needs\" button to the left."),
                 need(isolate(input$age) >= 18, "At present, energy needs are available only  for adults 18 years or older"))
        
        div(p("Your energy needs are estimated to be between ", 
              span(paste(prettyNum(EER_range()$propLower, big.mark=",", digits = 0),
              "and", prettyNum(EER_range()$propUpper, big.mark=",", digits = 0)), 
              class="emphasizeText"),
              " Calories per day"),p("The estimated range will include the actual energy
        needs of 95% of people, but 5% of people will have individual energy needs higher
        or lower than the estimate."), p("You can compare your estimate with the 
        laboratory-measured energy needs of people similar to you using the chart below."),
            h4("Energy Needs Comparison Chart"))
    })
    
    # this caption for the chart is setup to be rendered so it doesn't appear 
    # until the chart does
    output$chart_caption <- renderText({
        validate(need(input$goButton > 0, ""))
        "Your estimated energy needs range is highlighted 
        in green, and the points on the plot represent measured energy needs of 
        people of the same sex with similar physical activity level, age, and 
        body mass index. Mouse over a point on the plot to view details."
    })
 
    # renders rCharts polychart of the estimated EER range compared to nearby
    # observed values from the IOM data set
    output$chart <- renderChart2({
        validate(need(input$goButton > 0, ""),
                 need(isolate(input$age) >= 18, ""))
        pal_current <- isolate(PAL_value())
        # if the user didn't use the PAL help panel, set the pal value to equal
        # the average for the selected PAL category (averages from IOM report)
        if(pal_current == 1.39) pal_current <- 
            switch(isolate(input$PALcat), Sedentary = 1.39,
                   Low_Active = 1.5, Active = 1.75, Very_Active = 2.2)
        
        # filter out values with large differences in bmi, age, and PAL
        pData <- isolate({
            adults %>% filter(sex == input$sex) %>%
                mutate(ageDiff = age - input$age, bmiDiff = BMI - bmi(),
                       palDiff = PAL - pal_current) %>% zoomPlot 
        })
        
        # plot the prediction interval for the user with a wide vertica line
#         p <- rPlot(TEE ~ BMI, type="line", size=list(const="20"),
#                    color=list(const="#00CC66"), 
#                    data=data.frame(BMI = c(isolate(bmi()), isolate(bmi())),
#                                    TEE = c(EER_range()$propLower,
#                                            EER_range()$propUpper)))
        # ploting the prediction interval
        # used a wide line at first, but polycharts cannot do tooltips for line charts
        # instead, we use a sequence of overlapping points 
        EER_range_df <- data.frame(BMI = isolate(bmi()), 
                                   TEE = seq(EER_range()$propLower,
                                             EER_range()$propUpper,
                                             length.out = 50))
        # because we use oversized circles to make the shape, the ends extend
        # past the actual limits of the range by an amount equal to the radius
        # we compensate by censoring the extreme points so that the outter edge
        # of the circle from an inner point will approximate the true border
        # of the range
        EER_range_df <- EER_range_df[2:(nrow(EER_range_df)-2), ]
        # With this points setup, the auto tooltip would display the erroneous values
        # for each point. We override that with a tooltip function the returns the
        # range limits no matter which point is highlighted
        tt <- paste0("#! function(x) {return('You. Age: ", isolate(input$age),
                     " BMI: ", isolate(bmi()), " PAL: ", round(pal_current, digits=2),
                     " Energy needs: ", round(EER_range()$propLower, digits = 0),
                     " - ", round(EER_range()$propUpper, digits = 0)," Calories')} !#")
        p <- rPlot(TEE ~ BMI, data = EER_range_df, type="point", size=list(const=15),
                   color=list(const="#00CC66"), tooltip = tt, opacity=list(const=100))
        # add hidden points above and below the estimated range to keep it from
        # touching the edge of the chart
        p$layer(TEE ~ BMI, data=data.frame(BMI=rep(isolate(bmi()),2),
                                                   TEE= c(EER_range()$propLower - 100,
                                                          EER_range()$propUpper + 100)),
                type="point", opacity=list(const=0.00001), color=list(cost="white"), 
                tooltip= "#! function(x) {return(NULL)} !#")
        # plot the source data as points
        p$layer(TEE ~ BMI, data=pData, type="point", size=list(var="PAL"),
                   color=list(var="age"),
                   tooltip = "#! function(x) { 
                                    return('Age: ' + Math.round(x.age) +
                                          ' BMI: ' + Math.round(x.BMI) + 
                                          ' PAL: ' + Math.round(x.PAL * 100) / 100 +
                                          ' Energy needs: ' + Math.round(x.TEE) +
                                          ' Calories'); 
                                } !#" )

        p$guides(color = list(title = "Age"),
                 x = list(title = "Body Mass Index (kg/m²)"),
                 y = list(title = "Energy Needs (Calories)"),
                 size = list(title = "Physical Activity Level"))
        p
    }) 

    # dynamically creates a numeric input for each activity in the activities table
    # for the PAL helper panel
    output$activityList <- renderUI({
        act <- split(as.character(activities[order(activities$Sort, activities$Activity), 
                                             "Activity"]), 
                     cut2(1:nrow(activities), g=3))
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
        validate(need(!is.na(activity_METs()), ""))
        # Convert calculated PAL into factor using switch and cut2 for 
        # convenience (but cut2 only works right if at least one value 
        # for each category is present so some filler data is added and 
        # then dropped)
        PAL_category <- switch(as.numeric(first(cut2(c(PAL_value(), 1, 1.5, 1.7, 2), 
                                                     cuts=c(1.4, 1.6, 1.9)))),
                               "Sedentary", "Low_Active", "Active", "Very_Active")
        
        # update the PAL input with the resutls of the calculation
        updateRadioButtons(session, "PALcat", selected=PAL_category)                
        
        # describe the calculation
        div(p(paste("Physical activity:", round(activity_METs(), digits=1), 
                    "activity MET-hours per week /", 
                    "168 hours per week ×", round(MET_factor(), digits = 2), 
                    "(personalized expenditure factor)",
                    " + 1.39 for basic activities of daily living =", 
                    round(PAL_value(), digits = 2))), 
            p("Your Physical Activity Level is: ", 
              span(gsub("_", " ", PAL_category),class="emphasizeText")))
        
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
    palThresh <- max(abs(data$palDiff))
    palStep <- palThresh / 20
    cycle <- 2
    while(nrow(data) > 25) {
        switch(cycle,
               {ageThresh <- ageThresh - ageStep; cycle <- 2;},
               {bmiThresh <- bmiThresh - bmiStep; cycle <- 3;},
               {palThresh <- palThresh - palStep; cycle <- 1})

        data <- filter(data, abs(ageDiff) < ageThresh, abs(bmiDiff) < bmiThresh,
                       abs(palDiff) < palThresh)
    }
    data
} 
