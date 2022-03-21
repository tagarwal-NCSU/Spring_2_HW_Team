data {
	int <lower=0> y;
	int <lower=0> n;
  	}
parameters {
	real <lower=0, upper=1> p;
		    }
model {
	p ~ beta(1,1);
	y ~ binomial(n, p);
	   }