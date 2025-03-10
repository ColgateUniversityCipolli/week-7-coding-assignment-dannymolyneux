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

#Question 2
beta.prob <- function(x, alpha, beta, type){
  if(type == "="){
    #For a continuous distribution, probability is zero at any given point
    prob = 0
  }
  else if(type == "!="){
    #The compliment of "="
    prob = 1
  }
  else if(type == "<" | type == "<="){
    #These two operators are equivalent for continuous distributions
    prob = pbeta(x, alpha, beta)
  }
  else if(type == ">" | type == ">="){
    #These two operators are equivalent for continuous distributions
    prob = 1 - pbeta(x, alpha, beta)
  }
  else{
    print("Invalid input. Please enter a valid operator for the 'type' parameter.")
    return(0)
  }
  return(prob)
}




