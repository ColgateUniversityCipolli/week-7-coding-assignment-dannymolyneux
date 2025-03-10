#HW 7
#Question 1
pois.prob <- function(x, lambda, type){
  if(type == "="){
    prob = dpois(x, lambda)
  }
  else if(type == "!="){
    prob = 1 - dpois(x, lambda)
  }
  else if(type == "<"){
    prob = ppois(x-1, lambda)
  }
  else if(type == "<="){
    prob = ppois(x, lambda)
  }
  else if(type == ">"){
    prob = 1 - ppois(x, lambda)
  }
  else if(type == ">="){
    prob = 1 - ppois(x-1, lambda)
  }
  else{
    print("Invalid input. Please enter a valid operator for the 'type' parameter.")
    return(0)
  }
  return(prob)
}


