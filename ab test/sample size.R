library(stats)
library(ggplot2)


## Create storage for simulation

storage <- matrix(data=0, nrow=50, ncol=8)
colnames(storage) = c("x","base rate = 0.80","base rate=0.81", "base rate=0.82","base rate=0.83", "base rate=0.84", "base rate=0.85", "1000")


## Simulation: Sample Size x Effect Size

for (t in 1:50){
  storage[t,1] <- 0.001*t
  sample <- power.prop.test(p1=.80, p2=(.80-0.001*t), power=0.8, sig.level=0.05)
  storage[t,2] <- sample$n[1]
  sample <- power.prop.test(p1=.81, p2=(.81-0.001*t), power=0.8, sig.level=0.05)
  storage[t,3] <- sample$n[1]
  sample <- power.prop.test(p1=.82, p2=(.82-0.001*t), power=0.8, sig.level=0.05)
  storage[t,4] <- sample$n[1]
  sample <- power.prop.test(p1=.83, p2=(.83-0.001*t), power=0.8, sig.level=0.05)
  storage[t,5] <- sample$n[1]
  sample <- power.prop.test(p1=.84, p2=(.84-0.001*t), power=0.8, sig.level=0.05)
  storage[t,6] <- sample$n[1]
  sample <- power.prop.test(p1=.85, p2=(.85-0.001*t), power=0.8, sig.level=0.05)
  storage[t,7] <- sample$n[1]
  storage[t,8] <- 1000
}


graf1 <- plot(storage[25:50,1],storage[25:50,2], type="l", col="green", lwd=5, xlab="Effect Size", ylab="sample size", main="Sample Size", ylim=c(0,5000))
lines(storage[25:50,1],storage[25:50,3], col="red", lwd=2)
lines(storage[25:50,1],storage[25:50,4], col="blue", lwd=2)
lines(storage[25:50,1],storage[25:50,5], col="purple", lwd=2)
lines(storage[25:50,1],storage[25:50,6], col="orange", lwd=2)
lines(storage[25:50,1],storage[25:50,7], col="pink", lwd=2)
lines(storage[25:50,1],storage[25:50,8], col="black", lwd=2, lty =2)
legend("topright", ncol =2, c(list="base rate = 0.80","base rate=0.81", "base rate=0.82","base rate=0.83", "base rate=0.84", "base rate=0.85"),
       fill=c("green","red","blue","purple","orange","pink"))
