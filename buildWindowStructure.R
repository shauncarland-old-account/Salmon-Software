buildWindowStructure <-function(window_size,line){
#   Name: buildWindowStructure
#   Input: Integer, Vector of Floats
#   Output: List Object
#   Description: buildWindowStructure does a series of tasks.  Given a vector of
#   floats (referred to as line), the function finds which two adjacent values have
#   the smallest and largest difference.  It also determines which N-tuple (window) of
#   adjacent values has the smallest and largest difference, where N is the size of the window.  
#   The function returns a list giving the value of the largest/second largest/smallest/second smallest gap, 
#   the index of where the largest gap is, a list of differences from the windows, which window had the 
#   largest/smallest/second largest/second smallest score (and where it is located).
#   
  
  #type check
  if (!is.numeric(line)){
    stop("The input line must be a numeric vector")
  }
  if (!is.numeric(window_size) || window_size < 1){
    stop("window size must be a positive integer")
  }
  #error check
  if(length(line) == 0){
    stop("line must not be empty.")
  }
  
  #continuing on...
  
  #simulated value of infinity
  infinity <-100000000000000000000
  
  #window to put distance scores into
  window <- vector(length=window_size)
  #(vector) that will be used to store the values of
  #the windows
  distVals <-vector(length = length(line) - window_size - 1)
  prevLarg<- 0
  z <-1
  #for i positions on the line 
  loop <-length(line) - window_size - 1
  p <-1
  tag <-1
  #array of all of the gaps between 2 adj circuli
  gaps <-calcGaps(line)
  #i positions on the line...
  for (i in z: loop){
    #for j positions in the window
    y <-i
    k <-1
    #browser()
    j <-0
    flag <-0
    while(1==1){
      #if the window size is just 1, by default
      #we just put that one value into the window...
      if (window_size == 1){
        browser()
        window[1] <-line[1]
        flag <-1
      }
      if (flag == 1){
        #trivially end the loop
        break
      }
      #fill the window with the next values on line
      window[k] <-line[i+j]
      j <-j+1
      k <-k+1
      if (j == window_size){
        #window is full. stop/
        break
      }
    }
    
    if (window_size == 1){
      #if the window size is already full, 
      #then the score of this window is just
      #the first value
      distVals[i] <- window[1]
    }
    else {
      #calculate the score of this window
      distVals[i] <-distScore(window)
    }
    #is the largest jump in this window?
    curLarg <-largestGap(window,prevLarg,p,i)
    if (prevLarg < curLarg){
      #update the largest recorded value & its location
      prevLarg <-curLarg
      tag <-i
    }
    #reset the window
    window <- vector(length=window_size)  
  }
  
  #where is the 2nd largest gap?
  secondLargestGap <-0
  secondLargestGapInd <-0
  for (i in 1: length(gaps)){
    #loop here
    curGap <-gaps[i]
    if (curGap > secondLargestGap && abs(curGap - prevLarg) > 0.0000001 && abs(i - tag) > 10){
      #if the current gap is larger than the second largest gap found, and the second
      #largest gap is not the largest gap, then update the second largest recorded value & position
      secondLargestGap <- curGap
      secondLargestGapInd <-i
    }
  }
  
  #where is the smallest gap?
  #UPDATE: Added condition that
  #smallest gap must be before largest gap 
  #& indexes are 1 away
  smallestGap <-infinity
  smallestGapInd <-0
  for (i in 1: length(gaps)){
    curGap <-gaps[i]
    if((curGap < smallestGap) && curGap > 0 && (curGap + 2< tag)){
      smallestGap <-curGap
      smallestGapInd <-i
    }
  }
  #which window has the largest value?
  bestWind <-0
  bestWindInd <-1
  
  #which window has the second largest value
  secondBestWind <-0
  secondBestWindInd <-1
  
  #which window has the smallest value?
  smallestWind <- infinity
  smallestWindInd <-1
  
  for (i in 1: length(distVals)){
    curVal <- distVals[i]
    #check for best window
    if (curVal > bestWind){
      bestWindInd <-i
      bestWind <-curVal
    }
  }
  
  #check for 2nd largest window and smallest window
  
  for (i in 1: length(distVals)){
    curVal <-distVals[i]
    if (curVal > secondBestWind && abs(curVal - bestWind) > 0.0000000001){
      secondBestWind <-curVal
      secondBestWindInd <-i
    }
    #smallest wind
    if ((curVal < smallestWind) && (curVal > 0) && (curVal < tag)){
      smallestWind <-curVal
      smallestWindInd <-i
    }
    
  }
  
  #increment tag by 1 for offset
  #tagg_arr gives us the two indexes where line[tag[2]]-line[tag[1]]
  #gives us the largest gap.  sec_tag_arr gives the second largest 
  #gap and small_tag_arr gives the smallest gap in the same manner.
  
  tag_arr <- c(tag+1,tag+2)
  sec_tag_arr <-c(secondLargestGapInd,secondLargestGapInd+1)
  small_tag_arr <-c(smallestGapInd, smallestGapInd+1)

  #prevLarg - Largest Gap
  #Tag: Location of Largest Gap
  #DistVals: List of Window Scores
  #Best Wind: Location of Best Window
  #Gaps: List of Gap Values
  
  #package everything up into a list
  ret_list <-list(prevLarg,tag_arr,distVals,bestWindInd,bestWind,gaps,window_size,
                  line,secondBestWindInd,secondBestWind,smallestWindInd,smallestWind,
                  secondLargestGap,sec_tag_arr,small_tag_arr,smallestGap)
  names(ret_list)[1] <- "largest_gap"
  names(ret_list)[2] <- "largest_gap_index"
  names(ret_list)[3] <- "window_scores"
  names(ret_list)[4] <- "largest_window_index"
  names(ret_list)[5] <- "largest_window"
  names(ret_list)[6] <- "gap_scores"  
  names(ret_list)[7] <- "window_size"
  names(ret_list)[8] <- "line"
  names(ret_list)[9] <- "second_largest_window_index"
  names(ret_list)[10] <- "second_largest_window"
  names(ret_list)[11] <-"smallest_window_index"
  names(ret_list)[12] <- "smallest_window"
  names(ret_list)[13] <- "second_largest_gap"
  names(ret_list)[14] <- "second_largest_gap_index"
  names(ret_list)[15] <- "smallest_gap_index"
  names(ret_list)[16] <- "smallest_gap"
  return(ret_list)
}

largestGap <-function(window,prevLargest,prevLargInd,curInd){
  #helper function for buildWindowStruct
  #browser()
  dista <-0
  window_len <-length(window)
  loop <-window_len - 1
  i <-1
  while (1==1){
    x <-window[i]
    y <-window[i+1]
    if (is.na(x) == TRUE || is.na(y) == TRUE){
      dista <-0
    }
    else if (x == 0 || y ==0){
      dista <-0
    }
    else {
      dista <-abs(x-y)
    }
    if (dista > prevLargest){
      prevLargest <-dista
      prevLargInd<-curInd
    }
    if (abs(dista - 0.0775) < 0.0001){
      #browser()
    }
    if (i == loop){
      return(prevLargest)
    }
    else {
      i <-i+1
    }
  }
}

distScore <-function(window){
  #helper function for buildWindowStructure
  dist <-0
  window_len <-length(window)
  loop <-window_len - 1
  
  i <-1
  while (1==1){
    x <-window[i]
    y <-window[i+1]
    if (is.na(x) == TRUE || is.na(y) == TRUE){
      return(0)
    }
    
    
    
    dist <- dist + abs(x-y)
    
    if (i == loop){
      return(dist)
    }
    else {
      i <-i+1
    }
  }
  
  return (dist)
}

