#The zWald_test function calculates the Wald test for statistical significance of coefficients in a regression model. 
#It accepts the following input:
  
#x: The regression model.
#The function first computes the Wald statistic for each coefficient in the model
#by dividing the coefficients by their corresponding standard errors. It then applies
#logical checks to determine whether the computed Wald statistics fall within 
#the critical value range of a standard normal distribution. 
#If both checks are satisfied, the function returns TRUE; otherwise, it returns FALSE.
zWald_test <- function(x){
  zWald_model<- (summary(x)$coefficients / 
                    summary(x)$standard.errors)
  a <- t(apply(zWald_modelo, 1, function(x) {x < qnorm(0.025, lower.tail = FALSE)} ))
  b <- t(apply(zWald_modelo, 1, function(x) {x > -qnorm(0.025, lower.tail = FALSE)} ))
  ifelse(a==TRUE & b==TRUE, TRUE, FALSE)
}


#The pValue_extract function calculates the p-values of the coefficients in a regression model. It accepts the following input:
  
#  x: The regression model.
#The function computes the Wald statistic for each coefficient by dividing the 
#coefficients by their corresponding standard errors. 
#It then calculates the p-value associated with each coefficient using a two-tailed Wald z-test. 
#The p-values are returned as the output.


pValue_extract <- function(x){
  z <- summary(x)$coefficients/summary(x)$standard.errors
  # 2-tailed Wald z tests to test significance of coefficients
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  p
}

 