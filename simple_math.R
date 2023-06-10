#Problem 1
x<-c(-50:50)

x_1<-log(x)
plot(x_1)

x_2<-exp(x)
plot(x_2)

x_3<-sin(x)
plot(x_3)

x_4<-sin(2*x)
plot(x_4)

x_5<-sin(x)/cos(x)
plot(x_5)

x_5_1<-cumsum(x_5)
plot(x_5_1)


#Problem 2
if (interactive() ){
  k_1<-readline(prompt = "Please enter a positive integer: ")
  k<-as.integer(k_1)
  print(paste("There are",k%/%3,"elements in the vector 1:",k,"that are divisible by 3."))
} else{
  #  non-interactive
  cat("Please enter a positive integer: ")
  k_2 <- readLines("stdin",n=1)
  k<-as.integer(k_2)
  print(paste("There are",k%/%3,"elements in the vector 1:",k,"that are divisible by 3."))
}

#Problem 3
A<-c(1,2,3)
B<-c(3,4,5,6)
AB<-intersect(A,B)
if(length(AB)==0){
  print(A)
}else{
    print(B)}

#Proble 4
a<-rnorm(100)
y<-log(a)
y[is.na(y)==TRUE]<-9999

#Problem 5
x<-runif(10^8)
y<-c(1:10^8)
system.time({for (i in 1:10^8) y[i] <- sin(x[i])})
system.time({y <- sin(x)})