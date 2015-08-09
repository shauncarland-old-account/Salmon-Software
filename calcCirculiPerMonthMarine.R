
calcCirculiPerMonthMarine <-function(dataSet,tenDates){
  #number of days in water
  time_in_water <-calcTimeInWaterMarine(dataSet,tenDates)
  #number of circuli on fish
  numCirc <-countCirculiMarine(dataSet)
  circPerDay <-c()
  for (i in 1: length(time_in_water)){
    circPerDay[i] <-numCirc[i]/time_in_water[i]
  }
  
  #calc circ per month
  browser()
  circPerMonth <-c()
  for (i in 1: length(time_in_water)){
    circPerMonth[i] = circPerDay[i]*31
  }
  
  
  return(circPerMonth)
}

calcCirculiPerMonthMarine(spMar_1,tenDates_1)
