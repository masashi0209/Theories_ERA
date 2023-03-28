#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define DEL 0.0001 //1 time step
#define ONE_T 10000 //inverse of DEL

int main(){
	int i, L;
	double x, alpha, sum, val;
	double K, se2, sd2, r, maxIntegrate, R, D, C;
	double *u;

	K=100.0;
	r=0.5;
	alpha=0;
	se2=0.5; //sigma_e*sigma_e
	sd2=1.0; //sigma_ed*sigma_d, this is 1 if the unit of time is generation time
	
	// some parameters
	R = 2.0*r/(K*se2);
	D = sd2/se2;
	C = R*(K+D)+1.0;
	
	// we need to determine the range of integration (L)
	// calculate the value of increment (val), and stop calculation if val becomes negligibly small (<1e-10)
	// i is the number of iteration for the integration
	// At the same time, we integrate the value of U from 0 to L (maxIntegrate)
	L = 1;
	sum = 0.0;
	val = 1.0; // any values greater than 1e-10
	while(val > 1e-10){
		x = DEL*(double)L;
		val = exp(-R*x)*pow(1.0+x/D, C)/(se2*x*(x+D));
		sum+=val;
		L++;
	}
	
	maxIntegrate=2.0*DEL*sum;
	
	// we need to record the values of U at x
	// and we need to allocate the memory for it (because we do not know the value of L in advance, we need to allocate it after we know L)
	u=(double *)malloc(sizeof(double) * L);
	
	sum=0;
	u[0]=maxIntegrate;
	for(i=1; i<L; i++){
		x=(double)(DEL*i);
		// u = u1*u2
		// u2(x) = exp(R*x)*pow(1.0+x/D, -C);
		// u1(x) = int_x0^L f(x)dx =  int_0^L f(x)dx - int_0^X0 f(x)dx
		// because int_0^L f(x)dx = maxIntegrate,
		// u1(x) = maxIntegrate - int_0^X0 f(x)dx
		u[i]=exp(R*x)*pow(1.0+x/D, -C)*(maxIntegrate-2.0*DEL*sum);
		// sum = int_0^X0 f(x)dx
		sum+=exp(-R*x)*pow(1.0+x/D, C)/(se2*x*(x+D));
	}
	
	// because T(x) = int_0^x0 u(x)dx, summing u(x) values yields T(x)
	// in this example, the results are shown up to x0=200
	sum=0;
	for(i=0; i<200*ONE_T+1; i++){
		if(i%ONE_T==0) printf("%d %g %g\n", i/ONE_T, u[i], sum);
		sum+=DEL*u[i];
	}
	
	free(u);
	return 0;
}
  

