# Function Name: getCirculi
# Input: A data frame of fish data from the Northeast Fisheries
# Science Center
# Output: A data frame just containing only the circuli
# from that data set
# Description: This function extracts all of the circuli from
# the fish scale data set, so you can deal with them by itself.

getCirculi <- function(data_set){
  #column for first circuli
  first_circuli_column <- data_set$c1
  
  #index of first circuli column
  first_column_index <- which(colnames(data_set) == "c1")
  
  #column for last circuli
  last_circuli_column <- data_set$c133
  
  #index of last column
  last_column_index <- which(colnames(data_set) == "c133")
  
  #frame to be returned
  return_frame <- matrix(nrow = length(data_set[,i]),ncol = circuli_count)
  return_frame <- as.matrix(return_frame)

  #number of circuli: subtract index of c1 from index of c133
  #then add one (so that c1 and c133 are counted)
  number_of_circuli <- abs(last_column_index - first_column_index) + 1 

  
  #the first circuli.
 
  #for i rows (salmon)
  for (i in 1: length(data_set[,i])){
    #this is a vector that will contain the 
    #circuli measurements for the ith line(fish)
    cur_fish <-c()

    #index to keep track of which column we are on
    #this will help us, because when we are putting in the 
    #jth slot in the cur_fish vector, we don't want the jth
    #column in the data_set.  We want the jth column after the
    #first circuli column
    col_index <- first_column_index
    
    #for j columns (circuli)
    for (j in 1: number_of_circuli){
      cur_fish[j] <- data_set[i,col_index]
      col_index <- col_index + 1
      
    }
    #end j loop
    #put the cur_fish into return_frame
    return_frame[i,] <- cur_fish
  }
  #end i loop
  
  #return our return_frame
  return(return_frame)
}

#example:
sea_winter_1_circuli <- getCirculi(SW1)
