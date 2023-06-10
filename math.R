# 1
X <- matrix(c(1:4), nrow = 2, byrow = FALSE)

X_1<-X^(1:1)
X_2<-X^(1:2)
X_3<-X^(1:3)
X_4<-X^(1:4)

X_a<-X^1:1
X_b<-X^1:2
X_c<-X^1:3
X_d<-X^1:4

# 2
X <- matrix(rnorm(2*10^7), ncol = 2, byrow = TRUE)
y <- matrix(rnorm(10^7), ncol = 1, byrow = TRUE)
system.time(c<-t(X)%*%y)
# elapsed 0.087s

X_t <- t(X)
c_1 <- matrix(c(0,0), ncol = 1)
system.time({for (i in 1:10^7) c_1[1,1] <- c[1,1] + X_t[1,i]*y[i,1]; c_1[2,1] <- c[2,1] + X_t[2,i]*y[i,1]})
# elapsed 0.663s

# 1
A<-c(1,2,3)
B<-c(3,4,5,6)
AB<-intersect(A,B)
if(length(AB)==0){
  print(A)
}else{
  print(B)}

# 2
a <- rnorm(100)
y <- log(a)
y[is.na(y)==TRUE]<-9999

# 3
x <- runif(10^5,0,50)
y <- c(0)
y_1 <- c(0)
system.time({for (i in 1:10^5) y[i] <- sin(x[i])})
system.time(y_1 <- sin(x))

# 4
n <- c(1:15)
A <- 5000*(1+0.115)^n

# 5
n <- sample(1:100, 50, replace = FALSE)
Max <- 0
for (i in 1:50) {
  if (n[i]>Max){
    Max <- n[i]
  }
  else{}
}