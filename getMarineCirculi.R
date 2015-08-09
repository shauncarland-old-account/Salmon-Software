
getMarineCirculi <- function(circuli_set,fmc_col){
# Function Name: getMarineCirculi
# Input: A data frame of circuli (not the full data frame),
#         A column containing the fmc_col info
# Output: A data frame containing the marine circuli only
# 
# Description: This function takes in the a set of circuli
# (that was obtained via the getCirculi function).  It then
# scans each line upto the fmc_col for that specific line. 
# Then, it takes all of the circuli from that col and afterwards.
# This is put into a data frame and returned. 
  
# Todo: There appears to be one completely empty column for some reason...
  
  
  #####################
  #bounds checks
  #####################
  
  #check to see if number of columns and length 
  #of fmc_col are the same.
  
  if(length(circuli_set[,1]) != length(fmc_col)){
    error_message <- "The number of row in the circuli_set is not equal to
    the length of the fmc_col.  Are you sure they are from the same data set?"
    stop(error_message)
  }
  #make sure this is actually a circuli set, by checking if first slot is.numeric
  browser()
  if( ! (is.numeric(circuli_set[1,1])) ){
    error_message <- "The first item in the set is not a number.  Are you sure this
    is a circuli set, and not a full data set?"
    stop(error_message)
  }
  
 #we will get the marine circului in the form of a list of vectors.  This will
 #then be converted into a data frame.
 list_of_vectors <- list()
 index_vect <- c()
 max_mar_circ <- 0
  #for i rows...
  for (i in 1: length(circuli_set[,1])){
    #get the current row (fish)
    cur_fish <- circuli_set[i,]
    
    #this vector will store the current fish's circuli
    cur_vect <- c()
      
    #the current fmc_col value for this fish
    cur_fmc <- fmc_col[i]
    #scan through the circuli on this set
    for (j in 1: length(cur_fish)){
      
      #extract the current circuli
      cur_circuli <- cur_fish[j]
      #is this circuli >= the first marine circuli?
      if (cur_circuli >= cur_fmc){
        #store j index
        index_vect[i] <- j
        #put all of these marine circuli into cur_vect
        #marine circuli counter
        k <- j
        #cur_vect index counter
        m <- 1
        while (TRUE){
          cur_marine_circ <- cur_fish[k]
          #if current marine circuli is NA
          #we've gotten all of the marine circuli
          if (is.na (cur_marine_circ)){
            break 
          }
          #add this marine circuli
          cur_vect[m] <- cur_marine_circ
          #increment counters
          k <- k + 1
          m <- m + 1 
        }
        #end while loop
        #does this fish have the max # of marine circuli?
        if (length(cur_vect) > max_mar_circ){
          max_mar_circ <- length(cur_vect)
        }
        #end this iteration of the j loop
        break
      }
      #end cur_circuli => cur_fmc clause
    }
    #end j loop
    #put this fish's marine circuli into the list
    #note that list elements are accessed by [[i]],
    #not [i]
    
    list_of_vectors[[i]] <- cur_vect
  }
 #subtract 1 from max_mar_circ for offset
 #end i loop
 #put this info into the return frame
 return_frame <- matrix(nrow = length(circuli_set[,1]),ncol = max_mar_circ)
 return_frame <- as.data.frame(return_frame)
 
 #start filling the frame in
 #for i rows...
 for (i in 1: length(circuli_set[,1])){
   cur_mar_circs <- list_of_vectors[[i]]
   #note: cur_mar_circs is a vector
   #put the marine circuli in data frame
   for (j in 1: length(cur_mar_circs)){
     return_frame[i,j] <- cur_mar_circs[j]
   }
   #end j loop
   #fill the rest of the values with NA values
   for (k in j: max_mar_circ){
     return_frame[i,k] <- NA
   }
   #end k loop
 }
 #end i loop
 
 return(return_frame)
  
}





#Example 1
SW1_marine_circuli <- getMarineCirculi(getCirculi(SW1),SW1$fmc)

#Example 2 (Throws error)
getMarineCirculi(getCirculi(SW1),SW2$fmc)

#Example 3 (Throws error)
getMarineCirculi(SW1,SW1$fmc)