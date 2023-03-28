dt <- 0.001
sqdt <- sqrt(dt)

# parameters
K <- 100
TK <- 1/K
r <- 0.1
alpha <- 0 # chemical effect
se2 <- 0.1 #square value of environmental stochasticity

extinctTime <- c()
for(i in 1:10000){
	# initial condition
	t <- 0
	x <- K
	
	while(x>0.0){
		dx <- (r*x*(1.0-TK*x)+0.5*se2*x-alpha*x)*dt+sqrt(se2*x*x+x)*sqdt*rnorm(1)
		x <- x + dx
		t <- t + dt
	}
	extinctTime <- append(extinctTime, t)
}
mean(extinctTime)


#Exercise
#try various parameter. Have fun!