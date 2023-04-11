# sample size
n <- 5
# 5%ile points of the standard logistic distribution
K005 <- qlogis(0.05, 0, sqrt(3)/pi)

location <- 4.321 #a value chosen almost randomly
scale <- 1.234 #a value chosen almost randomly
mu <- location
sigma <- pi/sqrt(3)*scale

Xs <- c()
for(i in 1:100000){
	data <- rlogis(n, location, scale)
	m <- mean(data)
	s <- sd(data)
	X <- (m-mu)/s - sigma/s*K005
	Xs <- append(Xs, X)
}
ks <- as.numeric(quantile(Xs, 0.95))

#check the correctness of the ks value
thisHC5 <- qlogis(0.05, location, scale)
fail <- 0
for(i in 1:10000){
	sample <- rlogis(n, location, scale)
	PNEC <- mean(sample) - sd(sample)*ks
	if(PNEC > thisHC5) fail <- fail+1
}
fail #ks is true if fail is close to 10000*0.05
