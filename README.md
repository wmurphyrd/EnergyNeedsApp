# EnergyNeedsApp
This is a Shiny applicaiton that uses the R statistical computing package to estimate the daily Calorie needs for individuals.
The estimation works by recreated the non-linear least squares regression model used by the Institute of Medicine in the 2005
Dietary Reference Intakes for energy using their original data. With the rebuilt model, the propagate R package is used to 
create a 95% prediction interval for estimates using error propagation and Monte Carlo simulation. 

Users enter their gender, age, height, weight, and physical activity level to get their estimated needs. A physical activity
level calculator is included to help choose the correct category based on time spent doing various exercises. Users 
are presented with their energy needs prediction interval and a plot of their interval compared to the measured energy needs
of similar individuals from the source data. 

A live version of the app can be viewed at https://wmurphyrd.shinyapps.io/EnergyNeeds 
