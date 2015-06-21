Estimating Individual Energy Needs
========================================================
author: William Murphy, MS RDN
date: June 21st, 2015
font-family: serif

Improving the accuracy of daily Calorie requirement estimations with
statistical computing

Background
========================================================

* Accurate estimates of dietary energy needs are important to medical
professionals and the general population
* Estimating equations like the example below from the 2005 Dietary Refernce Intakes are often used

$$ EER = 662 – (9.53 \times age) + PA \times (15.91 \times weight
+ 539.6 \times height) $$

* The results are frequently misinterpreted as an estimate of an individual's energy needs
* The result is actually an estimate the **mean** energy needs for individuals of
    the same age, height, weight, and physical activity level
* For an individual, uncertainty in the estimate should be expressed 
    with a prediction interval


Model Recreation
========================================================

* Accurate calculation of prediction intervals requires a recreation of the original model




```r
library(knitr)
mod <- nls(TEE ~ Bint + Bage * age + (1 + BPALla * PALcatLow_Active + BPALa * PALcatActive + BPALva * PALcatVery_Active) * (Bht * ht + Bwt * wt), data = IOM_data, start = original_coef)
approx_r_squared <- 1 - mod$m$deviance() / sum((IOM_data$TEE - mean(IOM_data$TEE))^2)
```


|          |Bage  |Bwt   |Bht    |BPALla |BPALa |BPALva |Bint   | r.squared|
|:---------|:-----|:-----|:------|:------|:-----|:------|:------|---------:|
|Original  |-9.53 |15.91 |539.6  |0.11   |0.25  |0.48   |662    |      0.75|
|Recreated |-9.71 |13.4  |693.02 |0.1    |0.25  |0.47   |577.89 |      0.76|

<small>Data retrieval and processing code can be viewed in the source for this slide</small>

NLS Prediction Intervals
========================================================

* The [propagate R package](http://cran.r-project.org/web/packages/propagate/index.html) implements a first and second order Taylor expansion method for error propagation into NLS prediction intervals



```r
library(propagate)
nd <- data.frame(age = 32, PALcatLow_Active = 0, PALcatActive = 1, PALcatVery_Active = 0, ht = 1.8, wt = 81.8)
p <- predictNLS(mod, nd, interval="prediction")
```


| Prop.Mean.2| Prop.sd.2| Prop.2.5%| Prop.97.5%|
|-----------:|---------:|---------:|----------:|
|    3137.227|  102.2527|  2548.553|   3725.901|

The Energy Needs Shiny App
========================================================

* The [Energy Needs Shiny App](https://wmurphyrd.shinyapps.io/EnergyNeeds) provides a web
user interface to the model and prediction function
* The app includes recreated prediction models for adult men and women of normal or 
overweight status from the 2005 Dietary Reference Intakes
* Indviduals and medical professionals can use the application to obtain accurate estimates 
of energy requirements
* The app provides a calculator to help users select a physical activity level based on the reference activities and formulas published in the 2005 Dietary Reference Intakes
* The app helps users evaluate their prediction interval by plotting it against the measured energy needs of individuals with similar characteristics in the source data


