np <- 10000 #number of particles
r <- 1/2 # probability that a particle moves to right
T <- 1000 # steps of particle move

locations <- rep(c(0),times = np)

for(j in 1:T){
	rnd <- runif(np)
	for(i in 1:np){
		if(rnd[[i]] < r){
			add <- 1
		} else {
			add <- -1
		}
		locations[[i]] <- locations[[i]]+add
	}
}
# theoretical mean and variance
(2*r-1)*T
4*r*(1-r)*T
# simulation mean and variance
mean(locations)
var(locations)

x <- 150*(seq(100)/50-1)
y <- dnorm(x, mean=(2*r-1)*T, sd=sqrt(4*r*(1-r)*T))
hist(locations,freq=F,xlim=c(-150,150), ylim=c(0,0.014))
par(new=T)
plot(x,y, type="l",xlim=c(-150,150),ylim=c(0,0.014), ann=F, xaxt="n", yaxt="n")

# Exercise
# In this simulation, a particle moves to either right or left, and does not stay at the same location.
# Consider a case that a particle stay at the same location (i.e., r + l < 1)
# 1. compute mean and variance of the location after T movements
# 2. write a code for simulation of the case
# 3. compare the theoretical and simulation results







# Answer
# stay at the same location
r <- 1/3
l <- 1/4
locations <- rep(c(0), times=np)
for(j in 1:T){
	rnd <- runif(np)
	for(i in 1:np){
		if(rnd[[i]] < r){
			add <- 1
		} else if(rnd[[i]]<r+l) {
			add <- -1
		} else {
			add <- 0
		}
		locations[[i]] <- locations[[i]]+add
	}
}
#theoretical mean and variance
T*(r-l) # (1)*R + (-1)*L + 0*(T-R-L) where R=rT, L=lT.
T*r*(1-r)+T*l*(1-l)+2*T*r*l # Var(R-L) = Var(R) + Var(L) + 2*Cov(R,L), and Cov(R, L) = -T*r*l
# simulation mean and variance
mean(locations)
var(locations)

x <- 2*seq(100)
y <- dnorm(x, mean=T*(r-l), sd=sqrt(T*r*(1-r)+T*l*(1-l)+2*T*r*l))
hist(locations,freq=F,xlim=c(0,200), ylim=c(0,0.02))
par(new=T)
plot(x,y, type="l",ylim=c(0,0.02), ann=F, xaxt="n", yaxt="n")



