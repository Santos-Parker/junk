#'##########################################################################
#'This program reads a binary 75Hz (variable hz) file, takes the magnitude of accelerations (sqrt of the sum of squared x,y,z coordinates), subtracts gravity (magnitude of 1),and computes the absolute value. Also included is average (per second) "light" reading in Lux, and average temperature in deg C. Output is second-by-second CSV file where 75Hz acceleration values are summed over one-second and light and temperature are averaged
#'##########################################################################
#'##########################################################################
#'Directions: set working directory to folder containing raw bin files, be 
#'sure that "source" of redbin script is correct. Any .bin file in folder will
#'be read, but other file extensions are ignored
#'##########################################################################

#clean up (remove all variables from workspace)
rm(list=ls())

#print syntax
print

#load GENEAread and signal packages
library(GENEAread)
library(signal)

#source('~/Desktop/zero/one/stats/GENEActive/SealsData/CODE_Revised/redbin.R')

#Set Hz value:################
hz=75
##############CHANGE this to the correct Hz value for your file!!!!####

#list files in the folder, must be in correct directory without any other files other than the bin files to filter -- Set working directory for R session correctly!
files <-list.files(pattern = "\\.bin$")
a <-length(files)
for(i in 1:a){
  rawdata=redbin(files[i],calib=T)
  #This is the line that actually reads the data into R
  
  #Pull out timepoint for each second "on the whole second"
  y  = matrix(rawdata$data.out[,1],hz)[1,]
  y=ceiling(y) #
  y = as.POSIXct(y,origin="1970-01-01 00:00:00.000",tz="GMT") #convert to UTC time zone
  
  x = as.character(y)
  #Actual timepoints are printed into the file in whole seconds - no rounding errors
  
  #Calculate acceleration magnitude, minus gravity
  ENMOabs = matrix(abs(((rowSums(rawdata$data.out[,2:4]^2))^0.5)-1))
  
  # create a 4-col matrix (sum ENMOabs over (hz)Hz, average light, average temp) for each second
  ENMOabs_sum_light_temp<-data.frame(colSums(matrix(ENMOabs,hz)),colSums(matrix(rawdata$data.out[,5],hz))/hz,colSums(matrix(rawdata$data.out[,7],hz))/hz,x) 
  
  # write CSV
  csvfilename <-paste(files[i],"_ENMOabs_sum_light_temp.csv",sep="")
  write.table(ENMOabs_sum_light_temp,file=csvfilename,row.names=FALSE, col.names = FALSE)
}
