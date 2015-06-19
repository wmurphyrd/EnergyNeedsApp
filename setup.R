
# Data acquisition and munging 

datafile <- "adults_clean.RDS"
if(!file.exists(datafile) || try(rerun_data==TRUE, silent=TRUE)==TRUE) {
    library(xlsx); library(Hmisc);  library(magrittr); library(dplyr)
    furl <- "http://iom.edu/~/media/Files/Activity%20Files/Nutrition/DRIs/New%20Material/10_DLW_Database.xls"
    fname <- "dlw_data.xls"
    if(!file.exists("dlw_data.xls")) download.file(furl, destfile = fname,
                                                   mode= "wb")
    
    cindex <- c(5, 6, 8, 9, 11, 12, 14, 19,24)
    cnames <- c("rep", "metabolism",  "sex", "age", "ht", "wt", "BMR", "TEE", "PAL")
    cclasses <- c(rep("numeric",2), "character", rep("numeric", 6))
    adultNW <- read.xlsx(fname, sheetName = "adults", 
                         colIndex = cindex, colClasses = cclasses)
    names(adultNW) <- cnames
    adultNW$BMIcat <- "normal"
    adultOB <- read.xlsx(fname, sheetName = "obese adults", 
                         colIndex = cindex, colClasses = cclasses)
    names(adultOB) <- cnames
    adultOB$BMIcat <- "overweight"
    adults <- rbind(adultNW, adultOB)
    rm(adultNW, adultOB)
    
    adults %<>% mutate(BMIcat = as.factor(BMIcat), BMI = wt / ht^2) %>% 
        #exclude measuremens that were repeats on a single individual
        #exclude cases with metabolic stress / clinical condition
        filter(is.na(rep) | rep < 2, is.na(metabolism) | metabolism < 4) %>% 
        dplyr::select(-rep, -metabolism) %>%
        #exclude 2 discovered outlier cases that were also excluded by IOM
        filter(!(sex == "m" & ht== 1.79 & age == 54))
        
        
    levels(adults$sex) <- list(f = c("F", "f"), m = c("M", "m"))
    
    # apply physical activity categories using IOM groupings
    # and exclude PAL < 1.0 or > 2.5 as per IOM
    adults$PALcat <- cut2(adults$PAL, c(1, 1.4, 1.6, 1.9, 2.5))
    levels(adults$PALcat) <- c(NA, "Sedentary", "Low_Active", "Active", "Very_Active", NA)
    
    adults %<>% filter(complete.cases(.))
    
    # create dummy variables for PAL categories
    adults <- cbind(adults, model.matrix(~PALcat-1, data=adults))
    
    save(adults, file=datafile)
} else load(datafile)

# modeling 
#IOM's starting parameters are unkown, so their final parameters are used
parstart = list(f.normal = list(Bage = -6.91, Bwt = 9.36, Bht = 726,
                    BPALla = 0.12, BPALa = 0.27, BPALva = 0.45, Bint=354),
                m.normal = list(Bage = -9.53, Bwt = 15.91, Bht = 539.6,
                    BPALla = 0.11, BPALa = 0.25, BPALva = 0.48, Bint=662),
                f.overweight = list(Bage = -7.95, Bwt = 11.4, Bht = 619,
                        BPALla = 0.16, BPALa = 0.27, BPALva = 0.44, Bint=448),
                m.overweight = list(Bage = -10.1, Bwt = 13.7, Bht = 416,
                        BPALla = 0.12, BPALa = 0.29, BPALva = 0.59, Bint=1086))

modelsfile <- "adults_models.Rdata"
if(!file.exists(modelsfile) || try(rerun_models==TRUE, silent=TRUE)==TRUE) {
library(minpack.lm)
datasets <- split(adults, list(adults$sex, adults$BMIcat))

# Model each group using non-linear least squares and the IOM formula:
# TEE = A + B × age + PA × (D × weight + E × height)
# A = intercept (Bint), B = age coefficient (Bage), 
# D = weight coefficient (Bwt), E = height coeffcieint (Bht)
# for PA (physical activity), manually created dummy variables are used, and
# the default PA category (sendentary) is replaced with the constant 1
# in order to avoid a fully paramaterized model (which won't run) while
# still producing 
models <- mapply(nlsLM, data=datasets, start=parstart, 
       MoreArgs = list(formula = TEE ~ Bint + Bage*age + 
                            (1 + BPALla*PALcatLow_Active + BPALa*PALcatActive + 
                            BPALva*PALcatVery_Active) * (Bht*ht + Bwt*wt)),
       SIMPLIFY = FALSE)

# n <- nlsLM(TEE ~ Bint + Bage*age + 
#                 (1 + BPALla*PALcatLow_Active + BPALa*PALcatActive + 
#                      BPALva*PALcatVery_Active) * (Bht*ht + Bwt*wt), 
#            data=filter(adults, sex=="m", BMIcat=="normal"),
#            start=parstart)

# library(nlstools)
# j <- nlsJack(n)
save(models, file=modelsfile)
} else load(modelsfile)

predict.iom <- function(parameter_set, newdata) {
    if(!all(c("age", "PALcatLow_Active", "PALcatActive", "PALcatVery_Active", 
      "ht", "wt") %in% names(newdata))) stop("Missing parameters in newdata")
    parameter_set$Bint + parameter_set$Bage * newdata$age + 
        (1 + parameter_set$BPALla * newdata$PALcatLow_Active + 
             parameter_set$BPALa  * newdata$PALcatActive + 
             parameter_set$BPALva * newdata$PALcatVery_Active) *
        (parameter_set$Bht * newdata$ht + parameter_set$Bwt * newdata$wt)
}


# #library(propagate)
# nd <- data.frame(age = 32, PALcatLow_Active = 0, PALcatActive = 1, PALcatVery_Active = 0, ht = 1.8, wt = 81.8)
# predictNLS(n, nd, interval="prediction")$summary
# 
# ssratio <- function(nlsmodel) {
#     y <- eval(quote(TEE), envir=nlsmodel$m$getEnv())
#     1 - nlsmodel$m$deviance()/sum((y - mean(y))^2)
# }
# 
# ssratio(n)
           