####Instructions####

#This is a template R script file to create timing files. The script will create 1 timing file 
#for each condition for each participant. 

#In order to run this script you will need one folder with data for all participants. 

#Naming convention of the files: The files must follow a naming convention such that the filenames 
#are all the same except they all end with sequential four digit* ID (0000) that start with 0001 and 
#increase numerically by one. As an example: Project_1_subject_0001; Project_1_subject_0002; etc.

# * This naming convention only works if you have under 1000 participants. 
# * For projects with 1000 or more participants use a 5 digit ID

#The files must contain: 
#a) a column that indicates condition for each row of data, 
#b) a column that indicates how much time elapsed before the condition occurred (i.e. time onset values), and 
#c) a column that indicates how much time elapsed during the condition (i.e. time duration values).

#### Script Set-up ####
rm(list = ls()) 

library(tidyverse)
library(dplyr)


input <- "/Users/keelylab/Downloads/MIDdata/Data/"    
output <- "/Users/keelylab/Downloads/MIDdata/timingfiles/" 
setwd(input)

#Read in files from folder; creates a list of all .csv files in input folder
file_list <- list.files(path=input, pattern="*.csv") # 

# Create empty list for files to go to
onsets<-list()

#read in each .csv file in file_list 
for (i in 1:length(file_list)){
  onsets[[i]]<-read.table(paste(file_list[i], sep=''), sep=',', header=T)
}

# name each element in list
names(onsets)<-file_list

#Below creates a list of the subject IDs (using only the last 3 digits of their ID) below. The loop will cycle 
#through this list,and perform a function on each subject ID. The numbers 5 and 7 need to be changed to the
#character number representing the min and max length of the participant IDs. So for the example data files, 
#sub_102.csv, sub_1 makes up 5 characters, and the total length of sub_102 equals 7 characters. This tells R 
#to extract the part of the file name that represents the full participant ID.

sub_IDs <- unique(substr(names(onsets), 13, 16)) 

#Start of the forloop
#The script below will read in each file for each subject, then pull the onset and duration files for each 
#participant and put them into 2 separate files (1 for each condition). It will also create a column of 1s 
#for the parametric modulator. If you have a third condition, simply copy the script for creating the 
#condition 1 data and paste it after condition 2 and adjust the script as needed.

for (subject in sub_IDs) {
  
  #The line below will read in each subjects data. 'read.table' takes at minimum, one arguments (filepath). 
  #Here we are specifying the file path for each subject, by taking the generic filepath and pasting in the subject ID 
  #from our list (with no spaces). We are adding a second, optional argument 'skip' to skip the first row in each txt doc 
  #(with start date and time). Delete the 'skip' argument (, skip=1) if the first row of your data isn't reserved for 
  #date/time information. 
  
  data <- read.csv(paste(input, "MID1.0_fmri_",
                         subject, ".csv", sep = ""))
  
  condition1 <- data %>%
    filter(cue.color=="Green") %>%
    select(time.onset,time.trial) %>%
    mutate(pm=1)
  
  write.table(condition1, paste(output, subject, "_condition1.txt", 
                                sep = ""), row.names=FALSE, col.names = FALSE)
  
  condition2 <- data %>%
    filter(cue.color=="Blue") %>%
    select(time.onset,time.trial) %>%
    mutate(pm=1)
  
  write.table(condition2, paste(output, subject, "_condition2.txt", 
                                sep = ""), row.names=FALSE, col.names = FALSE)
}

#####Generating Files when missing data#####

sub_files <- setNames(lapply(sub_IDs, function(x) {
  files <- dir()[startsWith(dir(), paste0("MID1.0_fmri_", x))]
}), sub_IDs)

onsets_sub <- lapply(sub_files, function(x) {
  out <- dplyr::bind_rows(onsets[names(onsets) %in% x], .id = "file")
  out
})

onsets_df <- dplyr::bind_rows(onsets_sub, .id = "participant") ##this requires that we have a column with ID #


##Step 2: Get number of times each participant sees each condition and export it to csv

qa <- subset(as.data.frame(with(onsets_df, table(participant,cue.color)), Freq = 0))


##Step 3: Identify who and what is missing data#####

missing <- qa %>%
  filter(Freq=="0") 

#####Step 4: Creating blank EV for those missing conditions#####

blank<- data.frame(0,0,0)

for (i in 1:nrow(missing)){
  write.table(blank, 
              file = paste0(output, missing$ID[i],
                            "_condition_", missing$condition[i],
                            ".txt"), sep='\t', col.names = FALSE, row.names = FALSE)
}
