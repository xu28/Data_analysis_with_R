# Exercise 1
x<-c()
system.time({for (i in 1:200000){if (i%%2==0) x<-c(x,i)} })
# The above code runs in 5.842 seconds.
y<-c()
system.time({a<-1:200000 ; y<-a[a%%2==0]})
# The above code runs in 0.002 seconds.

# Obviously the vector way is much faster.


# Exercise 2
# 2_1 One million entries.
a<-rnorm(1000000)
b<-rnorm(1000000)
system.time(c<-a%*%b)
# The above code runs in 0.005 seconds.
system.time({for (i in 1:1000000) d<-d+a[i]*b[i]})
# The above code runs in 0.024 seconds.

# 2_2 100 million entries.
p<-rnorm(100000000)
q<-rnorm(100000000)
system.time(h<-p%*%q)
# The above code runs in 0.480 seconds.
k<-0
system.time({for (i in 1:100000000) k<-k+p[i]*q[i]})
# The above code runs in 2.159 seconds.

# The vectorized code is also faster.