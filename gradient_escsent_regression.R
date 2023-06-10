Raw <- read.csv("abalone.csv")

# We notice that the gender represented in the first column of the data is not a numeric variable.
# A common approach in machine learning is to process characters into one-hot encoding.
raw <- cbind(1, Raw[, 1], 0, 0, Raw[, 2:ncol(Raw)])
names(raw)[1] <- "x_0"
names(raw)[2] <- "sex_1"
names(raw)[3] <- "sex_2"
names(raw)[4] <- "sex_3"
# So, here I choose (1,0,0) to present M and so on. 
raw$sex_1[raw$sex_1 == "M"] <- 1
raw$sex_2[raw$sex_1 == "F"] <- 1
raw$sex_3[raw$sex_1 == "I"] <- 1

raw$sex_1[raw$sex_1 == "F"] <- 0
raw$sex_1[raw$sex_1 == "I"] <- 0

raw <- data.matrix(raw)
raw[, 2] <- raw[, 2]-1
training <- raw[1:3000, ]

# Initial values
learnrate <- 0.00003
conv_threshold <- 0.001
iterations <- 0

w <- runif(11, 0, 1)
y <- training[, 1:11]%*%w
delta <- training[, 12]-y
J <- sum(delta^2)/2
grad_J <- -t(training[, 1:11])%*%delta
  
converged <- F

while(converged == F) {
  # Implement the gradient descent algorithm.
  w_new <- w-learnrate*grad_J
  y_new <- training[, 1:11]%*%w_new
  delta_new <- training[, 12]-y_new
  J_new <- sum((delta_new)^2)/2
  grad_J_new <- -t(training[, 1:11])%*%delta_new
  if(abs(J-J_new) <= conv_threshold) {
    converged = T
    return(print(w_new))
  }else{
    w <- w_new
    J <- J_new
    grad_J <- grad_J_new
  }
  iterations <- iterations + 1
}

# Evaluate the w.
validation <- raw[3001:nrow(raw), ]
Prediction <- validation[, 1:11]%*%w
Validation <- validation[, 12]

# Calculate the standard deviation(MAE).
SD <- sum(abs(Prediction-Validation))/(nrow(validation))
print(SD)

# Calculate the relative error.
RE <- SD/mean(Validation)
print(RE)

# Plot
p1 <- hist(Prediction, breaks = 20)
p2 <- hist(Validation, breaks = 20)
plot(p1, col=rgb(0,0,1,1/4), xlim=c(0,25), ylim=c(0,0.2), freq = FALSE)
plot(p2, col=rgb(1,0,1,1/4), xlim=c(0,25), ylim=c(0,0.2), add=T, freq = FALSE)
legend("topright", legend = c("Prediction","Validation"), col = c(rgb(0,0,1,1/4), rgb(1,0,1,1/4)), lwd = 4)
