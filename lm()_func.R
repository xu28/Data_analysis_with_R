Raw <- read.csv("abalone.csv")

# We notice that the gender represented in the first column of the data is not a numeric variable.
# A common approach in machine learning is to process characters into one-hot encoding.
raw <- cbind(Raw[, 1], 0, 0, Raw[, 2:ncol(Raw)])
names(raw)[1] <- "sex_1"
names(raw)[2] <- "sex_2"
names(raw)[3] <- "sex_3"
# So, here I choose (1,0,0) to present M and so on. 
raw$sex_1[raw$sex_1 == "M"] <- 1
raw$sex_2[raw$sex_1 == "F"] <- 1
raw$sex_3[raw$sex_1 == "I"] <- 1

raw$sex_1[raw$sex_1 == "F"] <- 0
raw$sex_1[raw$sex_1 == "I"] <- 0

raw <- data.matrix(raw)
raw[, 1] <- raw[, 1]-1
raw <- data.frame(raw)

mod <- lm(Rings ~ sex_1 + sex_2 + sex_3 + Length + Diameter + Height + Whole.weight + Shucked.weight + Viscera.weight + Shell.weight, data = raw)

validation <- raw[3001:nrow(raw), ]
Prediction <- validation[, 1:10]
Validation <- validation[, 11]

# Make the prediction
Pre <- predict(mod, Prediction)

# Calculate the standard deviation(MAE).
SD <- sum(abs(Pre-Validation))/(nrow(validation))
print(SD)

# Calculate the relative error.
RE <- SD/mean(Validation)
print(RE)

# Plot
p1 <- hist(Pre, breaks = 20)
p2 <- hist(Validation, breaks = 20)
plot(p1, col=rgb(0,0,1,1/4), xlim=c(0,25), ylim=c(0,0.2), freq = FALSE)
plot(p2, col=rgb(1,0,1,1/4), xlim=c(0,25), ylim=c(0,0.2), add=T, freq = FALSE)
legend("topright", legend = c("Prediction","Validation"), col = c(rgb(0,0,1,1/4), rgb(1,0,1,1/4)), lwd = 4)

