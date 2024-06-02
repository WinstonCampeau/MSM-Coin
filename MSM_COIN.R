#Where seed commences the generation of all future numbers
#number is the number of resultant generated numbers
#show_list allows you to see the list of generated numbers
#raw changes the numbers from strings to numeric (1 = strings, 0 = numeric)

#I used this method to teach student's a little about simulation, probabilities, and p-values. 
#I have them do the middle square method by hand using a 4-digit seed
#They quickly learn the pitfalls of the MSM and such a small length seed (can easily converge to 0 or cycle -- although sometimes with desirable results!)
#They then use excel or google sheets and the binom.dist function to get a dirty p-value. (Pick the lower count of heads and tails as your successes and then multiply by 2 for two-tailed)
#Student's can then comment on the simulation, how to improve the simulation; they discover the concept of probabilities, fair coins, and the utility of statistical testing.



MSM <- function(seed, number, show_list, raw) {
  options("scipen" = 2*nchar(seed))
  
  #Sets number of numbers generated to 15 by default and raw output and do not show list
  
  if(missing(number)) {
    number <- 15
  }
  
  if(missing(show_list)) {
    show_list <- 0
  }
  
  if(missing(raw)) {
    raw <- 1
  }
  
  #The middle square only works appropriately with even numbers, this checks if the seed is an even number, if not, stops the function
  
  if(nchar(seed)%%2 != 0){
    cat("Please enter a seed with an even number of digits")
    cat("\n")
    stop
  } else  
  while_char <- seed
  library(stringi)
  MSM_LIST <- c()
  for(i in 1:number){
    seed_sqr <- seed^2
    
    #this while loop adds trailing zeros
    while(nchar(seed_sqr) < 2*nchar(while_char)) {
      seed_sqr<- stri_pad_left(seed_sqr, 2*nchar(while_char), 0)
    }
    
    
    
    #this section identifies the middle number and populates a list 
    
    left_digit <- nchar(seed_sqr)/4 + 1
    right_digit <- nchar(seed_sqr) - (nchar(seed_sqr)/4)
    ran_num <- as.numeric(substr(seed_sqr, left_digit, right_digit))
    
    while(nchar(ran_num) < nchar(while_char)){
      ran_num <- stri_pad_left(ran_num, nchar(while_char), 0)
    }
    
    MSM_LIST[i] <- ran_num 
    ran_num <- as.numeric(ran_num)
    seed <- ran_num
    while_char <- substr(seed_sqr, left_digit, right_digit) 
  }
  
  ifelse(raw==1, MSM_LIST,
         ifelse(raw==0, MSM_LIST <- as.numeric(MSM_LIST))
  )
  
  if(show_list == 1){
    cat("\nThe following are your randomly generated numbers, \n")
    
    print(MSM_LIST)
    
    cat("\n")
    
  }
  
  COIN_FLIP <- as.numeric(MSM_LIST)
  HEADS <- c()
  TAILS <- c()
  
  FOR_HEADS <- 10^(nchar(seed))/2
  
  #The following bits of code assign coin flips and their resultant p-value
  
  for(i in 1:length(COIN_FLIP)){
    if(COIN_FLIP[i]<FOR_HEADS){
      HEADS[i] <- 1
    } else TAILS[i] <-1
    if(nchar(seed)%%2 != 0){
      stop
    }
    
    if(is.null(HEADS)){
      HEADS <- c(0)
    }
    if(is.null(TAILS)){
      TAILS <- c(0)
    }
  }
  

      
  
  COUNT_HEADS <- sum(HEADS[which(!is.na(HEADS))])
  COUNT_TAILS <- sum(TAILS[which(!is.na(TAILS))])
  
  table <- data.frame(FLIP=c("HEADS", "TAILS"), COUNT=c(COUNT_HEADS, COUNT_TAILS))
  print(table)
  
  p <- c()
  
  ifelse(COUNT_HEADS>COUNT_TAILS, p <- 2*pbinom(COUNT_TAILS, number, 0.5), p <- 2*pbinom(COUNT_HEADS, number, 0.5))
  
  if(number%%2 == 0){
    p <- binom.test(COUNT_HEADS, number, 0.5)[3]
  } else p <- binom.test(COUNT_TAILS, number, 0.5)[3]
 
  
  
  ifelse(p<0.05, paste("Reject the null hypothesis of a fair coin, P(coin)=0.5, p=", p), paste("Fail to reject the null hypothesis of a fair coin, P(coin)=0.5, p=", p))
  
}

