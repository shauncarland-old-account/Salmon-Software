
calcTimeInWaterMarine <-function(dataSet,tenDates,SW_flag){
  
#   Name: calcTimeInWaterMarine
#   Input: A list of Marine Circuli, a list of return dates
#   Output: A vector
#   Description: This function calculates the number of days each fish in the data set was in the water.  
#   It currently works for 1SW fish.  A slight modification, as described in the function
#   documentation, will allow it to be used for 2SW fish.  
#   

  #this will be returned...
  date_set <-c()
  for (i in 1: length(dataSet[,1])){
    #get the ith return date
    cur_ten_date <-tenDates[i]
    #format it as a date...
    cur_ten_date <-as.Date(cur_ten_date, format = "%m/%d/%Y")
    #extract the ith return date year, month, and day 
    cur_ten_year <- format(cur_ten_date,"%Y")
    cur_ten_year <-as.numeric(cur_ten_year)
    
    cur_ten_month <-format(cur_ten_date,"%m")
    cur_ten_day <-format(cur_ten_date,"%d")
    
    #get starting year
    #change this to cur_ten_year - 2 for 2SW fish
    if (SW_flag == 1){
    start_year <- cur_ten_year -1
    }
    else if (SW_flag == 2){
      start_year <- cur_ten_year -2
    }
    else {
      stop("SW_flag must be 1(for SW1 fish) or 2 (for SW2 fish)")
    }
    
    #start date: May 1st, 2015 from original year 
    start_date <-paste(start_year,"1","5")
    start_date <-as.Date(start_date, format = "%Y %d %m")
    
    #flag for NA values
    if( is.na(cur_ten_date)){
      date_set[i] <-NA
    }
    else {
      #find the days between cur_ten_date, and start date
      days_between <- difftime(cur_ten_date,start_date)
#       if (days_between < 0){
#         browser()
#       }
      date_set[i] <-days_between
      #continue...
    }
  }
  return(date_set)
}
