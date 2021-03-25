## ----setup, include=FALSE---------------------------------------------------------------------------------
#Set knitr global options. Turn off echo,warnings and message to prevent these items showing up in final report. 
knitr::opts_chunk$set(echo = FALSE, cache=FALSE, warning=FALSE, message=FALSE)
options(knitr.table.format = "latex")


## ----run_time_start, cache=FALSE--------------------------------------------------------------------------
#Save the time this code started running. This is used at the end to calculate total run time for this model.
start_time <- Sys.time()


## ----library_packages, echo=FALSE, warning=FALSE, message=FALSE-------------------------------------------
#####
#Code to install all required libraries
#####

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(dplyr)
library(data.table)
library(lubridate)
library(knitr)
library(kableExtra)
library(gam)



## ----show_package, echo=FALSE, comment=NA-----------------------------------------------------------------

########
#Code generates a list of used packages/versions to show in report
########

    # Get all non-base packages in sessions
    packages <- names(sessionInfo()$otherPkgs)

    # Sort list alphabetically
    packages <- sort(packages)
    
    #get associated version for each package, into a list
    packages <- sapply(packages, packageVersion, simplify=FALSE)
    
    #turn into a character
    packages <- sapply(packages, as.character)
    
    #turn into a dataframe to make it nice to print to screen
    packages <- data.frame(packages)

    #Move rowname into first column of dataframe, assign header, and then move version into second  column
    packages <- (data.frame(package=rownames(packages), version=packages[1], row.names=NULL))
    
    #Print to PDF
    kbl(packages, row.names = FALSE, caption="Packages")  %>% kable_styling(latex_options="hold_position", bootstrap_options = c("striped", "condensed"), full_width = FALSE, position = "left", fixed_thead = FALSE, )



## ----initial_data_download, echo=FALSE, warning=FALSE, message=FALSE, results = FALSE---------------------

#Increase timeout to 600s because my internet is really slow
options(timeout=600)

#List of files that we need to download
last.message <- NULL
zipFileList <- c("https://github.com/TuckerLab-DS/ds_Pred_Maint/raw/main/Data.zip", 
"https://github.com/TuckerLab-DS/ds_Pred_Maint/raw/main/PS1.txt.zip",
"https://github.com/TuckerLab-DS/ds_Pred_Maint/raw/main/PS2.txt.zip",
"https://github.com/TuckerLab-DS/ds_Pred_Maint/raw/main/PS3%20and%204.zip",
"https://github.com/TuckerLab-DS/ds_Pred_Maint/raw/main/PS5.txt.zip",
"https://github.com/TuckerLab-DS/ds_Pred_Maint/raw/main/PS6.txt.zip")

#download each file, and store in a temporary file name
for (f in 1:length(zipFileList)){
  dl <- tempfile()
  tryCatch( { download.file(zipFileList[f], dl); 
            print(paste("Download Complete:", zipFileList[f])) }
          , message = function(m) { print("Error") })

#Unzip (decompress) each file, and save under the Data/pm folder
unzip(zipfile=dl, exdir="Data/pm_data")
}

#clean up unused variables
rm(dl, f, zipFileList)


## ----file_list_, comment=""-------------------------------------------------------------------------------
#get a lit of files that we downloaded/decompressed so we can load them into a dataset 
allFileList = list.files(path = "Data/pm_data", pattern = "*.txt", full.names=TRUE)
allFileName <- list.files(path = "Data/pm_data", pattern = "*.txt", full.names=FALSE)

#we want to combine all the sensor files. File list contains only sensor and profile files. This means we need to exclude the profile file from the list.
intRemove <- which(allFileName=="profile.txt")
if(!is.null(intRemove) & length(intRemove)!=0){
  sensorFileList <- allFileList[-intRemove]
  sensorFileName <- allFileName[-intRemove]
}

#this is a list of all the profile files. There is only 1 of these, but coded like the below in case their are multiple in the future. 
profileFileList <- allFileList[intRemove]
profileFileName <- allFileName[intRemove]

#Print a list of files to the screen for use in the final report.
kbl(allFileName, col.names = c("Files"), caption="Files") %>% kable_styling(latex_options="HOLD_position", bootstrap_options = c("striped", "condensed"), full_width = FALSE, position = "left", fixed_thead = FALSE, )

#clean up the unused variables
rm(intRemove)


## ----sensor_data_file_structure_stats---------------------------------------------------------------------

#a tibble used for showing the number of rows/columns per sensor files. 
df_sensor_file_str <- tibble("File" = character(), "Rows" = numeric(), "Columns"= numeric())

#variables where we store the std. dev and mean for each row in each sensor file. We summarize this data at the end
temp_sensor_sd <- list() 
temp_sensor_mean <- list() 

#loop through each sensor text file
for (i in 1:length(sensorFileList)){
  
  #read the file (comma delimited file)
  readFile <- fread(file=sensorFileList[i], header=FALSE, stringsAsFactors = FALSE, sep="\t")
 
  #save the number of rows/columns per file for displaying at the end.
  df_sensor_file_str <- add_row(df_sensor_file_str,  tibble_row(File = sensorFileName[i], Rows = dim(readFile)[1], Columns = dim(readFile)[2]))

  #calculate the std. dev for each line in the file.
  r_sd <- apply(readFile,1,sd)
  temp_sensor_sd <- c(temp_sensor_sd,list(r_sd))
  
  #calculate the mean for each line in the file.
  r_mean <-  rowMeans(readFile)
  temp_sensor_mean <- c(temp_sensor_mean,list(r_mean))
  
} 

#Convert the SD data into a data-frame, and name the columns based on the text file. 
df_sd <- as.data.frame(temp_sensor_sd,check.rows=FALSE)
colnames(df_sd) <-  tolower(paste("sensor", sub('\\.txt$', '', sensorFileName), sep="_"))

#Convert the Mean data into a data-frame, and name the columns based on the text file. 
df_mean <- as.data.frame(temp_sensor_mean,check.rows=FALSE)
colnames(df_mean) <-  tolower(paste("sensor", sub('\\.txt$', '', sensorFileName), sep="_"))

#Calculate the RSD and store in a dataframe
df_rsd <- df_sd/df_mean
df_rsd[is.na(df_rsd)] <- 0

#For each SD Row determine the Range (highest and lowest value), then store this data for displaying in the report.
rsdColRange <- data.frame(apply(df_rsd, 2, range))
rownames(rsdColRange) <- c("RSD Min", "RSD Max")

#For each Mean Row determine the Range (highest and lowest value), then store this data for displaying in the report.
meanColRange <- data.frame(apply(df_mean, 2, range))
rownames(meanColRange) <- c("Mean Min", "Mean Max")

#Print the Structure (Rows/Columns) for each sensor file
kbl(df_sensor_file_str, caption = "File Sensor Row/Columns") %>% kable_styling(latex_options="HOLD_position", bootstrap_options = c("striped", "condensed"), full_width = FALSE, position = "left", fixed_thead = FALSE)

#Show the Structure for a typical sensor file. All files have a similar format, so this just shows the last loaded sensor file. Limit to the structure to 10 data points. Instead of use str function, this function makes the print out a little nicer, and prevents issues with overflow off the PDF margin.
kbl(head(readFile[,1:10]), caption="Typical Sensor File Data (showing 10 columns only)") %>% kable_styling(latex_options=c("scale_down", "HOLD_position")) %>% kable_styling(bootstrap_options = c("striped", "condensed"), full_width = TRUE, position = "left", fixed_thead = FALSE) 

#Show the RSD range for each sensor file
kbl(rsdColRange, digits=3, caption="RSD Range for each Sensor") %>% kable_styling(latex_options=c("scale_down", "HOLD_position")) %>% kable_styling(bootstrap_options = c("striped", "condensed"), full_width = TRUE, position = "left", fixed_thead = FALSE, font_size = 6) %>% row_spec(0, angle = -45)

#Show the Mean range for each sensor file
kbl(meanColRange, digits=2, caption="Mean Range for each Sensor") %>%  kable_styling(latex_options=c("scale_down", "HOLD_position")) %>% kable_styling(bootstrap_options = c("striped", "condensed"), full_width = TRUE, position = "left", fixed_thead = FALSE, font_size = 6) %>% row_spec(0, angle = -45)

#Clean up some variables
rm(temp_sensor_sd, temp_sensor_mean, r_sd, r_mean, readFile)


## ----profile_data_file_str--------------------------------------------------------------------------------

#stores the row/column data for each profile file
df_profile_file_str <- tibble("File" = character(), "Rows" = numeric(), "Columns"= numeric())

#there is only 1 profile file, but this has been written in case there are more in the future.  This reads the file, stores it in a dataframe, and also stores the row/column count for displaying in the report. 
for (i in 1:length(profileFileList)){
  readFile <- fread(file=profileFileList[i], header=FALSE, stringsAsFactors = FALSE, sep="\t")
 
  df_profile <- readFile
  df_profile_file_str <- add_row(df_profile_file_str,  tibble_row(File = profileFileName[i], Rows = dim(readFile)[1], Columns = dim(readFile)[2]))

} 

#The profile file doesnt contain any headers, so we manually set them here. 
colnames(df_profile) <-  c("profile_cooler","profile_valve_condition","profile_internal_pump_leakage","profile_hydraulic_accumulator","profile_stable")

#print the file structure (rows/columns)
kbl(df_profile_file_str, caption="Profile File Structure") %>% kable_styling(latex_options="hold_position", bootstrap_options = c("striped", "condensed"), full_width = FALSE, position = "left", fixed_thead = FALSE, )

#we need to rename the headers in the profile df because they took up too much room in the report table header. Remove the "Profile" and replace underscores with spaces. 
  df_profile_disp <- df_profile
  names(df_profile_disp) <- gsub(names(df_profile_disp), pattern = "_", replacement = " ")  
  names(df_profile_disp) <- str_replace(names(df_profile_disp), "profile", "")

#Print the unique values for each profile column (ie, component). There are discrete values, so this is useful information to display. 
kbl(summarise_all(df_profile_disp, ~(paste0(sort(unique(.)), collapse = ','))), caption="Profile Component Unique Values") %>% kable_styling(latex_options=c("scale_down", "hold_position")) %>% kable_styling(bootstrap_options = c("striped", "condensed"), full_width = TRUE, position = "left", fixed_thead = FALSE, font_size = 8) %>% row_spec(0, angle = 0)



## ----df_combine_data--------------------------------------------------------------------------------------

#combine the average sensor data with the profile data, this is the master data-frame containing all the data we need to analyse. This gets split up into different working data sets soon.
df <- cbind(df_mean, df_profile)



## ----data_split-------------------------------------------------------------------------------------------

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

# Validation set will be 10% of master dataframe
test_index <- createDataPartition(y = df$sensor_ce, times = 1, p = 0.1, list = FALSE)
df_set <- df[-test_index,]
validation_set <- df[test_index,]

# Create test_set data using 10% of df_set data (which is 90% of master dataframe. This is the data that will be used to verify models, because we cant use the validation data for this as set by project scope
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

test_index <- createDataPartition(y = df_set$sensor_ce, times = 1, p = 0.1, list = FALSE)
train_set <- df_set[-test_index,]
test_set <- df_set[test_index,]

#clean up variables
rm(df_set, test_index)


## ----data_structure, comment=""---------------------------------------------------------------------------

#This performs the same function as STR, but looks a lot nicer when printed to the screen. Show the structure of the master dataframe.
data.frame(Variable = names(df),
           Class = sapply(df, typeof),
           "First Values" = sapply(df, function(x) paste0(head(x, 3),  collapse = ", ")),
           row.names = NULL) %>% 
  kable() %>% kable_styling(latex_options="hold_position",bootstrap_options = c("striped", "condensed"), full_width = FALSE, position = "left", fixed_thead = FALSE, )



## ----data_rows--------------------------------------------------------------------------------------------
#get and print number of rows/columns in master data set
d <- data.frame(dim(df))
row.names(d) <- c("rows", "columns")

kbl(d, "simple", row.names=TRUE, col.names="value", caption="Data Set Row/Column Count") %>% kable_styling(latex_options="hold_position",bootstrap_options = c("striped", "condensed"), full_width = FALSE, position = "left", fixed_thead = FALSE)


## ----set_x_y----------------------------------------------------------------------------------------------
#set up our training/test/validation x (predictor) and y (outcome) variables
train_set_x <- train_set %>% select(starts_with("sensor_"))
train_set_y <- train_set %>% select(starts_with("profile_"))
test_set_y <- test_set %>% select(starts_with("profile_"))

validation_set_x <- validation_set %>% select(starts_with("sensor_"))
validation_set_y <- validation_set %>% select(starts_with("profile_"))


## ----near_zero_predictors---------------------------------------------------------------------------------
#Calculate the near zero variability for each x component.
nzv <- nearZeroVar(train_set_x, saveMetrics= TRUE)

#set up some column names
colnames(nzv) <-  c("Freq. Ratio", "Percent Unique", "Zero Variation", "Near Zero Variation")

#print to screen.
kbl(nzv, caption="Predictor Near Zero Variation Analysis") %>% kable_styling(latex_options="HOLD_position", bootstrap_options = c("striped", "condensed"), full_width = FALSE, position = "left", fixed_thead = FALSE, )



## ----predictors_correlation-------------------------------------------------------------------------------

#calculate correlation for each predictor.
descrCor <-  cor(train_set_x)

#get any values with a correlation > 0.999
highCorr <- sum(abs(descrCor[upper.tri(train_set_x)]) > .999)

#print to screen
kbl(descrCor, caption="Predictor Correlation", digits=3) %>% kable_styling(latex_options=c("scale_down", "HOLD_position"), bootstrap_options = c("striped", "condensed"), full_width = FALSE, position = "left", fixed_thead = FALSE)

#Count number of predictors with a correlation > 0.999. 
highCorr <- sum(!is.na(highCorr))
kbl(highCorr, caption="Predictors >0.999 Correlation") %>% kable_styling(latex_options="HOLD_position", bootstrap_options = c("striped", "condensed"), full_width = FALSE, position = "left", fixed_thead = FALSE)



## ----model_plots_lm1_-------------------------------------------------------------------------------------
#we need to pivot some columns into rows in order to make the code simpler for the plot using ggplot. 
train_set_tall <- train_set %>% 
    pivot_longer(cols = starts_with("sensor_"), 
          names_to = c(".value", "sensor_"), names_sep = "_")

#using a lm fitting model, plot the cooler values against each predictor. 
ggplot(train_set_tall, aes(x=sensor,y=profile_cooler)) + 
  ggtitle("Cooler") +
  xlab("") +
   facet_wrap(~sensor_,scales = "free") + 
  geom_smooth(method = "lm")



## ----model_plots_lm2_-------------------------------------------------------------------------------------
#using a lm fitting model, plot the valve condition values against each predictor. 
ggplot(train_set_tall, aes(x=sensor,y=profile_valve_condition)) + 
   ggtitle("Valve Condition") +
  xlab("") +
   facet_wrap(~sensor_,scales = "free") + 
  geom_smooth(method = "lm")


## ----model_plots_lm3_-------------------------------------------------------------------------------------
#using a lm fitting model, plot the internal pump leakage values against each predictor. 
ggplot(train_set_tall, aes(x=sensor,y=profile_internal_pump_leakage)) + 
     ggtitle("Internal Pump Leakage") +
  xlab("") +
   facet_wrap(~sensor_,scales = "free") + 
  geom_smooth(method = "lm")


## ----model_plots_lm4_-------------------------------------------------------------------------------------
#using a lm fitting model, plot the hydraulic accumulator values against each predictor. 
ggplot(train_set_tall, aes(x=sensor,y=profile_hydraulic_accumulator)) + 
    ggtitle("Hydraulic Accumulator") +
  xlab("") +
   facet_wrap(~sensor_,scales = "free") + 
  geom_smooth(method = "lm")


## ----model_plots_lm5_-------------------------------------------------------------------------------------
#using a lm fitting model, plot the Stable values against each predictor. 
ggplot(train_set_tall, aes(x=sensor,y=profile_stable)) +
    ggtitle("Stable") +
  xlab("") +
   facet_wrap(~sensor_,scales = "free") + 
  geom_smooth(method = "lm")



## ----plots_loess1_----------------------------------------------------------------------------------------
#Using a LOESS fitting model, plot the Cooler values against each predictor. 
ggplot(train_set_tall, aes(x=sensor,y=profile_cooler)) + 
  ggtitle("Cooler") +
  xlab("") +
   facet_wrap(~sensor_,scales = "free") + 
  geom_smooth(method = "loess")



## ----model_plots_loess2_----------------------------------------------------------------------------------
#Using a LOESS fitting model, plot the Valve condition values against each predictor. 
ggplot(train_set_tall, aes(x=sensor,y=profile_valve_condition)) + 
   ggtitle("Valve Condition") +
  xlab("") +
   facet_wrap(~sensor_,scales = "free") + 
  geom_smooth(method = "loess")


## ----model_plots_loess3_----------------------------------------------------------------------------------
#Using a LOESS fitting model, plot the Internal Pump Leakage condition values against each predictor. 
ggplot(train_set_tall, aes(x=sensor,y=profile_internal_pump_leakage)) + 
     ggtitle("Internal Pump Leakage") +
  xlab("") +
   facet_wrap(~sensor_,scales = "free") + 
  geom_smooth(method = "loess")


## ----model_plots_loess4_----------------------------------------------------------------------------------
#Using a LOESS fitting model, plot the Hydraulic Accumulator condition values against each predictor. 
ggplot(train_set_tall, aes(x=sensor,y=profile_hydraulic_accumulator)) + 
    ggtitle("Hydraulic Accumulator") +
  xlab("") +
   facet_wrap(~sensor_,scales = "free") + 
  geom_smooth(method = "loess")


## ----model_plots_loess5_----------------------------------------------------------------------------------
#Using a LOESS fitting model, plot the Stable condition values against each predictor. 
ggplot(train_set_tall, aes(x=sensor,y=profile_stable)) +
    ggtitle("Stable") +
  xlab("") +
   facet_wrap(~sensor_,scales = "free") + 
  geom_smooth(method = "loess")



## ----results_start, error=FALSE, messages=FALSE, results=FALSE--------------------------------------------
#Set up dataframes for each evaluation criteria (Precision, Accuracy, F1, Recall and Model Run Time). Header for each frame matches the component (ie, cooler), plus a column at the start for what model is being evaluated.

model_results_precision <- setNames(data.table(matrix(nrow = 0, ncol = 1+ncol(df_profile))), c("Model", colnames(df_profile)))

model_results_accuracy <- setNames(data.table(matrix(nrow = 0, ncol = 1+ncol(df_profile))), c("Model", colnames(df_profile)))

model_results_F1 <- setNames(data.table(matrix(nrow = 0, ncol = 1+ncol(df_profile))), c("Model", colnames(df_profile)))

model_results_recall <- setNames(data.table(matrix(nrow = 0, ncol = 1+ncol(df_profile))), c("Model", colnames(df_profile)))

model_results_run_time <- setNames(data.table(matrix(nrow = 0, ncol = 1+ncol(df_profile))), c("Model", colnames(df_profile)))

#We have used 4 models to evaluate, if required additional models can be added here.
models <- c("glm", "gamLoess", "knn", "Rborist")



## ----model_test_set, error=FALSE, messages=FALSE, results=FALSE-------------------------------------------

#model_func is the function that runs each model. 
#y col: an integer representing which Profile condition (ie Cooler) we are evaluating.
#method_name: the model we are running, ie, glm
#trainControl: the trainControl parameters (if any) used to pass to the train function.
# preProcess: any pre-processing steps to pass to the train function. ie, center
#tLength: TuneLength to pass to the train function
#blnFinalValidation: either TRUE or FALse. TRUE if this is the final validation data set we are evaluation, otherwise FALSE if we are using the Test data set for evaluation.

model_func <- function(y_col, method_name, trainControl, preProcess, tLength, blnFinalValidation){
  
  #we are assessing the run time for each model, so capture the start time.
  model_start_time <- Sys.time()
  
  #trainControl is sometimes null, so if not required then dont include in the train parameters. if trainControl is null then tunelegnth is also null.
  if(!is.null(trainControl)){
      train_model <- train(x=train_set_x,
                       y=train_set_y[[y_col]],
                      trControl = trainControl,
                      preProc = preProcess,
                      tuneLength = tLength,
                       method = method_name)
  } else {
      train_model <- train(x=train_set_x,
                       y=train_set_y[[y_col]],
                      preProc = preProcess,
                       method = method_name)
  }
    
  #evaluates the model against either the Test data set or Validation. Validation is only used at the very end once the model parameters are set. 
  if(blnFinalValidation){
      pred = predict(train_model, validation_set)
      reference = as.factor(validation_set_y[[y_col]])
  } else {
      pred = predict(train_model, test_set)
      reference = as.factor(test_set_y[[y_col]])
  }

  #Each Outcome has a discrete set of values. Determine what these values are so we can round our predicted results to the closest value. Sorting of these values ensures the rounding function work
  w=sort(as.numeric(levels(reference)))
  
  # create a dt with our sorted levels, and values
  dt_a = data.table(w, val = w) 
  
  # data.table must know that w is sorted for the binary search
  setattr(dt_a, "sorted", "w") 
  
  # binary search and "roll" to the nearest neighbour. We set each predicted value to the closest of the possible/allowable predicted values
  pred <- dt_a[J(pred), roll = "nearest"]$val
  pred <- as.factor(pred)
  
  #set up our confusion matrix to evaluate the model
  cm <- confusionMatrix(pred, reference)
  
  #store the evaluation parameters for returning later on.
  cm_prec <- ifelse(is.null(nrow(cm$byClass)), cm$byClass['Precision'], mean(cm$byClass[,'Precision']))
  cm_F1 <- ifelse(is.null(nrow(cm$byClass)), cm$byClass['F1'], mean(cm$byClass[,'F1']))
  cm_recall <- ifelse(is.null(nrow(cm$byClass)), cm$byClass['Recall'], mean(cm$byClass[,'Recall']))
  cm_accuracy <- cm$overall['Accuracy']
  
  #round these values
  cm_prec <- format(round(cm_prec, digits=3), nsmall = 3) 
  cm_F1 <- format(round(cm_F1, digits=3), nsmall = 3) 
  cm_recall <- format(round(cm_recall, digits=3), nsmall = 3) 
  cm_accuracy <- format(round(cm_accuracy, digits=3), nsmall = 3) 

  #calculate model run time.
  model_finish_time <- Sys.time()
  model_run_time <- as.double(difftime(model_finish_time, model_start_time, units = "mins"))
  model_run_time <- format(round(model_run_time, digits=2), nsmall = 2) 
  
  #return each evaluation parameter
  return(c(cm_prec, cm_accuracy, cm_F1, cm_recall, model_run_time))
  
}



## ----model_base, error=FALSE, messages=FALSE, results=FALSE-----------------------------------------------

#This is our base model for each of the specified models. It includes no tuning parameters, or optimisations.

#set up lists for storing each evaluation parameters.
r_res_precision_list <- list()
r_res_accuracy_list <- list()
r_res_F1_list <- list()
r_res_recall_list <- list()
r_res_run_time <- list()

#loop through each model
for (m in 1:length(models)){
  
  #store the model we are running as the first value in the list.
  r_res_precision_list <- paste(models[m], "Base")
  r_res_accuracy_list <- paste(models[m], "Base")
  r_res_F1_list <- paste(models[m], "Base")
  r_res_recall_list <- paste(models[m], "Base")
  r_res_run_time <- paste(models[m], "Base")
    
  #loop through each Outcome (y value) in the training data set, build/evaluate a model, and return the evaluation parameters. 
  for (y_col in 1:ncol(train_set_y)){
    ret <- model_func(y_col, models[m], NULL, NULL, NULL, FALSE)
    r_res_precision_list <- c(r_res_precision_list,ret[1])
    r_res_accuracy_list <- c(r_res_accuracy_list,ret[2])
    r_res_F1_list <- c(r_res_F1_list,ret[3])
    r_res_recall_list <- c(r_res_recall_list,ret[4])
    r_res_run_time <- c(r_res_run_time,ret[5])
  } 

  #now we transpose the performance evaluation lists into the final dataframe
  model_results_precision <- rbind(model_results_precision, transpose(as.data.frame(matrix(unlist(r_res_precision_list), ncol = 1, byrow = TRUE))), use.names=FALSE)
  
  model_results_accuracy <- rbind(model_results_accuracy, transpose(as.data.frame(matrix(unlist(r_res_accuracy_list), ncol = 1, byrow = TRUE))), use.names=FALSE)
  
  model_results_F1 <- rbind(model_results_F1, transpose(as.data.frame(matrix(unlist(r_res_F1_list), ncol = 1, byrow = TRUE))), use.names=FALSE)
  
  model_results_recall <- rbind(model_results_recall, transpose(as.data.frame(matrix(unlist(r_res_recall_list), ncol = 1, byrow = TRUE))), use.names=FALSE)
  
    model_results_run_time <- rbind(model_results_run_time, transpose(as.data.frame(matrix(unlist(r_res_run_time), ncol = 1, byrow = TRUE))), use.names=FALSE)
  
} 



## ----model_preprocess, error=FALSE, messages=FALSE, results=FALSE-----------------------------------------
##repeat the above but set up some pre-processing criteria.
preprocess <- c("center", "scale")

#Apply each pre-processing parameter to each model individually so we can assess how it impacts the model.
for (prp in 1:length(preprocess)){
  for (m in 1:length(models)){
    
      #store the model we are running as the first value in the list.
    strModel <- paste(models[m], " (Pre-Process: ", preprocess[prp], ")", sep="")
    
    r_res_precision_list <- strModel
    r_res_accuracy_list <- strModel
    r_res_F1_list <- strModel
    r_res_recall_list <- strModel
    r_res_run_time <- strModel
     
      #loop through each Outcome (y value) in the training data set, build/evaluate a model, and return the evaluation parameters. 
    for (y_col in 1:ncol(train_set_y)){
      ret <- model_func(y_col, models[m], NULL, preprocess[prp],NULL, FALSE)
      r_res_precision_list <- c(r_res_precision_list,ret[1])
      r_res_accuracy_list <- c(r_res_accuracy_list,ret[2])
      r_res_F1_list <- c(r_res_F1_list,ret[3])
      r_res_recall_list <- c(r_res_recall_list,ret[4])
      r_res_run_time <- c(r_res_run_time,ret[5])
    } 
    
      #now we transpose the performance evaluation lists into the final dataframe
    model_results_precision <- rbind(model_results_precision, transpose(as.data.frame(matrix(unlist(r_res_precision_list), ncol = 1, byrow = TRUE))), use.names=FALSE)
    
    model_results_accuracy <- rbind(model_results_accuracy, transpose(as.data.frame(matrix(unlist(r_res_accuracy_list), ncol = 1, byrow = TRUE))), use.names=FALSE)
    
    model_results_F1 <- rbind(model_results_F1, transpose(as.data.frame(matrix(unlist(r_res_F1_list), ncol = 1, byrow = TRUE))), use.names=FALSE)
  
  model_results_recall <- rbind(model_results_recall, transpose(as.data.frame(matrix(unlist(r_res_recall_list), ncol = 1, byrow = TRUE))), use.names=FALSE)
  
      model_results_run_time <- rbind(model_results_run_time, transpose(as.data.frame(matrix(unlist(r_res_run_time), ncol = 1, byrow = TRUE))), use.names=FALSE)
      
  } 
}



## ----model_optimise, error=FALSE, messages=FALSE, results=FALSE-------------------------------------------

##repeat with evaluation using the optimal pre-processing parameter, and apply some basic tuning parameters. 
preprocess <- c("center")
tLength <- 10
control <- trainControl(method = "cv", number = 10)

  for (m in 1:length(models)){
    
      #store the model we are running as the first value in the list.
    r_res_precision_list <- paste(models[m], "Pre-Process: Scale, with Tune")
    r_res_accuracy_list <- paste(models[m], "Pre-Process: Scale, with Tune")
    r_res_F1_list <- paste(models[m], "Pre-Process: Scale, with Tune")
    r_res_recall_list <- paste(models[m], "Pre-Process: Scale, with Tune")
    r_res_run_time <- paste(models[m], "Pre-Process: Scale, with Tune")
  
      #loop through each Outcome (y value) in the training data set, build/evaluate a model, and return the evaluation parameters. 
    for (y_col in 1:ncol(train_set_y)){
      ret <- model_func(y_col, models[m], control, preprocess, tLength, FALSE)
      r_res_precision_list <- c(r_res_precision_list,ret[1])
      r_res_accuracy_list <- c(r_res_accuracy_list,ret[2])
      r_res_F1_list <- c(r_res_F1_list,ret[3])
      r_res_recall_list <- c(r_res_recall_list,ret[4])
      r_res_run_time <- c(r_res_run_time,ret[5])
    } 
    
      #now we transpose the performance evaluation lists into the final dataframe
    model_results_precision <- rbind(model_results_precision, transpose(as.data.frame(matrix(unlist(r_res_precision_list), ncol = 1, byrow = TRUE))), use.names=FALSE)
    
    model_results_accuracy <- rbind(model_results_accuracy, transpose(as.data.frame(matrix(unlist(r_res_accuracy_list), ncol = 1, byrow = TRUE))), use.names=FALSE)
    
    model_results_F1 <- rbind(model_results_F1, transpose(as.data.frame(matrix(unlist(r_res_F1_list), ncol = 1, byrow = TRUE))), use.names=FALSE)
  
  model_results_recall <- rbind(model_results_recall, transpose(as.data.frame(matrix(unlist(r_res_recall_list), ncol = 1, byrow = TRUE))), use.names=FALSE)
  
      model_results_run_time <- rbind(model_results_run_time, transpose(as.data.frame(matrix(unlist(r_res_run_time), ncol = 1, byrow = TRUE))), use.names=FALSE)
  } 



## ----highlight_colours_-----------------------------------------------------------------------------------

#This function locates the highest value for for each model for each evaluation parameter, and highlight the cell blue. This is aesthetic, makes it nicer to review data in the report
colMaxValue <- function(dt, cols, printcaption){
  
  #find the maximum and minimum values (note, multiple values can be returned) in the supplied data (column)
  colMax <- function(data) sapply(data, max, na.rm = TRUE)
  colMin <- function(data) sapply(data, min, na.rm = TRUE)
  
  dt <- dt %>% arrange(Model)
  
  #clean up the header names because they are too long, and cause issues with how the table displays.
  names(dt) <- gsub(names(dt), pattern = "_", replacement = " ")  
  names(dt) <- str_replace(names(dt), "profile", "")
  
  #set up a print kbl object
  kb <- kbl(dt, caption=printcaption) %>% kable_styling()
  
  #loop through each column
  for (iCol in cols){
  
  #select the column from the df, and the same column from the run time. Run time is used a a secondary parameter for the common situation where multiple models return have the same maximum value  
  dtSelect <- dt %>% select(all_of(iCol))
  dtRunTime <- model_results_run_time %>% arrange(Model) %>% select(all_of(iCol))
  
  #return the row index for the maximum values
  maxvalue <- which(dtSelect == colMax(dtSelect), arr.ind = TRUE)
  
  #find the associated rows the the run time, and determine which has the shortest run time. 
  minTime <- which(dtRunTime == colMin(dtRunTime[c(maxvalue[,1])]), arr.ind = TRUE)
  minTime <- minTime[1]
   
  #now colour the cell that has the maximum value, and lowest run time. 
   kb <- kb %>% column_spec(iCol, background = ifelse(as.numeric(rownames(dt))==minTime, "blue","white"),  color = ifelse(as.numeric(rownames(dt))==minTime, "white","black"))
  
   }

  #return the table to screen. the table contains the colours. Adjust column widths.
 kb %>% kable_styling(latex_options=c("scale_down", "HOLD_position")) %>% kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)  %>% column_spec(-1, width = "6em")

}


## ----print_results_---------------------------------------------------------------------------------------

#Print each table (with optimal values highlighted) to screen. Add lines between each model (every 4 models)
colMaxValue(model_results_precision, c(2:6), "Precision") %>% row_spec(4, hline_after=TRUE) %>% row_spec(8, hline_after=TRUE) %>% row_spec(12, hline_after=TRUE)

colMaxValue(model_results_accuracy, c(2:6), "Accuracy") %>% row_spec(4, hline_after=TRUE) %>% row_spec(8, hline_after=TRUE) %>% row_spec(12, hline_after=TRUE)

colMaxValue(model_results_F1, c(2:6), "F1") %>% row_spec(4, hline_after=TRUE) %>% row_spec(8, hline_after=TRUE) %>% row_spec(12, hline_after=TRUE)

colMaxValue(model_results_recall, c(2:6), "Recall") %>% row_spec(4, hline_after=TRUE) %>% row_spec(8, hline_after=TRUE) %>% row_spec(12, hline_after=TRUE)

#clean up the headers in the table so it all fits on the page. 
names(model_results_run_time) <- gsub(names(model_results_run_time), pattern = "_", replacement = " ")  
names(model_results_run_time) <- str_replace(names(model_results_run_time), "profile", "")

#print the run time. This doesnt need any values highlighted.
kbl(model_results_run_time, caption="Run Time (min)") %>% kable_styling(latex_options=c("scale_down", "HOLD_position"), bootstrap_options = c("striped", "condensed"), full_width = FALSE)  %>% column_spec(-1, width = "6em") 



## ----final_model_test_, error=FALSE, messages=FALSE, results=FALSE----------------------------------------
#We specify the optimal model (highest performance values and lowest run time) for each outcome, then re-run the models for no reason other than to create a nice summary. We could just filter the previously run data.

optimal_models <- c("glm", "gamLoess", "glm", "Rborist", "Rborist")
optimal_preprocess <-c(NA, NA, NA, "center", "center")

#set up our lists for storing the performance data.
optimal_accuracy <- list()
optimal_precision <- list()
optimal_F1 <- list()
optimal_recall <- list()
optimal_run_time <- list()

#store the model we are running as the first row in the list, the pre-processing parameter, with the outcome name as the header.
final_model_results <- setNames(data.table(matrix(nrow = 0, ncol = 1+ncol(df_profile))), c("Description", colnames(df_profile)))

final_model_results <- rbind(final_model_results, transpose(as.data.frame(matrix(unlist(c("Model", optimal_models)), ncol = 1, byrow = TRUE))), use.names=FALSE)
 
final_model_results <- rbind(final_model_results, transpose(as.data.frame(matrix(unlist(c("Pre-Processing Method", optimal_preprocess)), ncol = 1, byrow = TRUE))), use.names=FALSE)
     
  #loop through each Outcome (y value) in the training data set, build/evaluate a model, and return the evaluation parameters. 
    for (y_col in 1:ncol(train_set_y)){
      
      if (is.na(optimal_preprocess[y_col])) {
        ret <- model_func(y_col, optimal_models[y_col], NULL, NULL,  NULL, FALSE)
      } else {
        ret <- model_func(y_col, optimal_models[y_col], NULL, optimal_preprocess[y_col], NULL,FALSE)
      }
     
      optimal_precision <- c(optimal_precision,ret[1]) 
      optimal_accuracy <- c(optimal_accuracy,ret[2])
      optimal_F1 <- c(optimal_F1,ret[3])
      optimal_recall <- c(optimal_recall,ret[4])
      optimal_run_time <- c(optimal_run_time,ret[5])

    } 

#now we transpose the performance evaluation lists into the final dataframe
final_model_results <- rbind(final_model_results, transpose(as.data.frame(matrix(unlist(c("Precision", optimal_precision)), ncol = 1, byrow = TRUE))), use.names=FALSE)

final_model_results <- rbind(final_model_results, transpose(as.data.frame(matrix(unlist(c("Accuracy", optimal_accuracy)), ncol = 1, byrow = TRUE))), use.names=FALSE)

final_model_results <- rbind(final_model_results, transpose(as.data.frame(matrix(unlist(c("F1", optimal_F1)), ncol = 1, byrow = TRUE))), use.names=FALSE)

final_model_results <- rbind(final_model_results, transpose(as.data.frame(matrix(unlist(c("Recall", optimal_recall)), ncol = 1, byrow = TRUE))), use.names=FALSE)

final_model_results <- rbind(final_model_results, transpose(as.data.frame(matrix(unlist(c("Time (min)", optimal_run_time)), ncol = 1, byrow = TRUE))), use.names=FALSE)


## ----final_model_data-------------------------------------------------------------------------------------

#clean up the headers in the table so it all fits on the page. 
names(final_model_results) <- gsub(names(model_results_run_time), pattern = "_", replacement = " ")  
names(final_model_results) <- str_replace(names(model_results_run_time), "profile", "")

kbl(final_model_results, caption="Final Model using Test/Train Data Set") %>% kable_styling(latex_options=c("scale_down", "HOLD_position"), bootstrap_options = c("striped", "condensed"), full_width = FALSE)  %>% column_spec(-1, width = "6em") %>% row_spec(2, hline_after=TRUE)


## ----final_model_validation_test_, error=FALSE, messages=FALSE, results=FALSE-----------------------------

#We repeat the above using the Validation Data: using the optimal model (highest performance values and lowest run time) for each outcome. THis is the final evaluation.

optimal_models <- c("glm", "gamLoess", "glm", "Rborist", "Rborist")
optimal_preprocess <-c(NA, NA, NA, "center", "center")

#set up our lists for storing the performance data.
optimal_accuracy <- list()
optimal_precision <- list()
optimal_F1 <- list()
optimal_recall <- list()
optimal_run_time <- list()

#store the model we are running as the first row in the list, the pre-processing parameter, with the outcome name as the header.
final_model_results <- setNames(data.table(matrix(nrow = 0, ncol = 1+ncol(df_profile))), c("Description", colnames(df_profile)))

final_model_results <- rbind(final_model_results, transpose(as.data.frame(matrix(unlist(c("Model", optimal_models)), ncol = 1, byrow = TRUE))), use.names=FALSE)
 
final_model_results <- rbind(final_model_results, transpose(as.data.frame(matrix(unlist(c("Pre-processing", optimal_preprocess)), ncol = 1, byrow = TRUE))), use.names=FALSE)
     
  #loop through each Outcome (y value) in the training data set, build/evaluate a model, and return the evaluation parameters. Note the final parameter is model_func is now TRUE.
    for (y_col in 1:ncol(validation_set_y)){
      
      if (is.na(optimal_preprocess[y_col])) {
        ret <- model_func(y_col, optimal_models[y_col], NULL, NULL,  NULL, TRUE)
      } else {
        ret <- model_func(y_col, optimal_models[y_col], NULL, optimal_preprocess[y_col], NULL,TRUE)
      }
     
      optimal_precision <- c(optimal_precision,ret[1]) 
      optimal_accuracy <- c(optimal_accuracy,ret[2])
      optimal_F1 <- c(optimal_F1,ret[3])
      optimal_recall <- c(optimal_recall,ret[4])
      optimal_run_time <- c(optimal_run_time,ret[5])

    } 

#now we transpose the performance evaluation lists into the final dataframe
final_model_results <- rbind(final_model_results, transpose(as.data.frame(matrix(unlist(c("Precision", optimal_precision)), ncol = 1, byrow = TRUE))), use.names=FALSE)

final_model_results <- rbind(final_model_results, transpose(as.data.frame(matrix(unlist(c("Accuracy", optimal_accuracy)), ncol = 1, byrow = TRUE))), use.names=FALSE)

final_model_results <- rbind(final_model_results, transpose(as.data.frame(matrix(unlist(c("F1", optimal_F1)), ncol = 1, byrow = TRUE))), use.names=FALSE)

final_model_results <- rbind(final_model_results, transpose(as.data.frame(matrix(unlist(c("Recall", optimal_recall)), ncol = 1, byrow = TRUE))), use.names=FALSE)

final_model_results <- rbind(final_model_results, transpose(as.data.frame(matrix(unlist(c("Time (min)", optimal_run_time)), ncol = 1, byrow = TRUE))), use.names=FALSE)



## ----final_model_validation_data--------------------------------------------------------------------------

#clean up the headers in the table so it all fits on the page. 
names(final_model_results) <- gsub(names(model_results_run_time), pattern = "_", replacement = " ")  
names(final_model_results) <- str_replace(names(model_results_run_time), "profile", "")

#Print the final validation model data.
kbl(final_model_results, caption="Final Model Using Validation Data Set") %>% kable_styling(latex_options=c("scale_down", "HOLD_position")) %>% kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)  %>% column_spec(-1, width = "6em") %>% row_spec(2, hline_after=TRUE)


## ----conclusion-------------------------------------------------------------------------------------------
#clean up the headers in the table so it all fits on the page. 
names(final_model_results) <- gsub(names(model_results_run_time), pattern = "_", replacement = " ")  
names(final_model_results) <- str_replace(names(model_results_run_time), "profile", "")

#Print the final validation model data.
kbl(final_model_results, caption="Final Model Using Validation Data Set") %>% kable_styling(latex_options=c("scale_down", "HOLD_position")) %>% kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)  %>% column_spec(-1, width = "6em") %>% row_spec(2, hline_after=TRUE)


## ----end_time, cache=FALSE--------------------------------------------------------------------------------
#save the time this code finished running. This is to calculate total run time for this model.
end_time <- Sys.time()


## ----run_time_final---------------------------------------------------------------------------------------
#####
#Run Time
####

#Print code run time, useful metric.
kbl(round(as.double(difftime(end_time, start_time, units = "mins")),1), caption="Total Code Run Time (min") %>% kable_styling(latex_options=c("HOLD_position")) %>% kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE, font_size = 7) %>% row_spec(2, hline_after=TRUE)


