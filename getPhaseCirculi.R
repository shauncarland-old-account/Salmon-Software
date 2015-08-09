
phase_list <- list(SW1$summax1,SW1$winmin1,0)
getPhaseCirculi_X <- function (marine_data,col_vect,data_frame_convert){
  if (missing(data_frame_convert)){
    data_frame_convert <- FALSE
  }
  
  #simulated value of infinity
  infinity <- 100000
  #list to be returned (or converted into a data frame)
  ret_list <-c()
  
  #we want to keep track of fish w/NA values
  #the indexes of fish w/NA values ill be put in na_vec
  na_vec <- c()
  na_cnt <- 1
  #for i fish...
  
  for (i in 1: length(marine_data[,1])){
    fish <-marine_data[i,]
    #start from the beginning of the marine circuli
    #up to the first phase indicator  
    fish_phases <-list()
    na_flag <-0
    for (j in 1: length(col_vect)){
      if (na_flag == 1){
        #keep track of fish w/NA values
        na_vec[na_cnt] <- i
        na_cnt <- na_cnt + 1
        break 
      }
      
      if (j ==1){
        #if we are on the start of the scale
        start_index <-1
        
      }
      if (j == length(col_vect)){
        #if we are on the end of the scale, set the last
        #circuli to infinity so the final phase will be 
        #cut off at the end of the scale.
        end_circuli <- infinity
      }
      else {
        #end_circuli is the last circuli in the current phase
        #end_circuli <- dataSet[i,col_vect[j]]
        end_circuli <- col_vect[[j]][i]
      }
      if(is.na(end_circuli)){
        #we have a missing column value
        na_flag <- 1
        end_circuli <- infinity
      }
      #scan the current line up to the end_circuli
      #to get the current phase
      #for k marine circuli on the scale...
      k <-start_index
      p <-1
      cur_phase <-c()
      while (TRUE){
        if(k > length(fish)){
          #if this scale is the last scale on the fish,
          #we want to break the loop and end to avoid errors 
          break
        }
        #get the current circ
        cur_circ <- fish[k]
        #if current circuli is NA, stop
        if (is.na(cur_circ)){
          break
        }
        #if this is the last circuli of this phase, break
        else if (end_circuli < as.numeric(cur_circ)){
          break
        }
        else {
          #put the current circuli into the current phase
          cur_phase[p] <- fish[k]
          #increment the counters
          p <- p+1
          k <- k+1
        }
      }
      #outside of the third inner loop
      cur_phase <-as.numeric(cur_phase)
      #put this phase into the list of phases
      fish_phases[[j]] <-cur_phase
      #the start index of the next phase is k
      start_index <-k
      
    }
    #number of phases this fish has
    fish_ph_len <- length(fish_phases)
    #fish_phases[fish_ph_len + 1] <- current_year
    ret_list[[i]] <- fish_phases
  }
  #filter out the na vals
  
  if (!is.null(na_vec)){
    for (i in 1: length(na_vec)){
      #cur_na <- ret_list[[i]]
      for (j in 1: length(ret_list[[1]])){
        #browser()
        ret_list[[na_vec[i]]][[j]] <- NA
      }
    }
  }
  
  #TODO: make this into a list of data frames if requested
  
  return(ret_list)
}

getPhaseCirculi(SW1_marine_circuli,col_vect)