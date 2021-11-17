## Marton Barta Exercise 9



my_little_function <- function(dir){
  setwd(dir)
  files <- list.files(dir, pattern=".csv")                                                                                       ### checks all the .csv files
  print(files)                                                                                                                   ### Giving the options
  filenum <- as.integer(readline("Which of the following files would you like to work with? - enter the index number "))         ### Question
  open_this <- list.files(dir, pattern=".csv")[filenum]                                                                          ### Saving the file that was chosen
  hey <- read.csv(open_this)                                                                                                     ### Saving the file that was chosen
  print(colnames(hey))                                                                                                           ### Giving the options
  colnum <- as.integer(readline("Which of the following column are you interested in? - enter the index number "))               ### Question
  ## to see if the number is out of the columns' range                                                                           ### Cheching if the column chosen is out of the range
  if(colnum > ncol(hey)){
    colnum <- as.integer(readline("Please enter a valid column number: "))
  }
  print("the name of the column is: ")
  print(colnames(hey)[colnum])
  
  ##########################################################################################################################################  
  ### EXTRA CREDIT PART  ###################################################################################################################
  ##########################################################################################################################################
  ## to see if we do not have enough observations - the count of the observations excludes the NAs
  if(sum(!is.na(hey[colnum])) > 50){
    areyousure <- readline("The dataset's column does not contain 50 observations (excluding NAs) - the rule of thumb is to have at least 50 observations...  Do you still want to go on? - input 'Y' or 'N': ")
    if(areyousure == "N"){
      print("Thank you for trusting my function!")
      break
    }
  }
  ##########################################################################################################################################
  print("The variation of the column's observations is:")
  #calculates the coefficeint of variation
  answer <- c(sd(as.numeric(unlist(hey[!is.na(hey[colnum]),][colnum])))/mean(as.numeric(unlist(hey[!is.na(hey[colnum]),][colnum])))) ### Coefficient calculation
  return(answer)
  
}


dir <- "/Users/bartamarci/Desktop/Biocomp-Exercise09/"

my_little_function(dir)


  
