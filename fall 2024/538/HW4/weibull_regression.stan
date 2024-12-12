//
// Weibull regression with fixed shape 
// and uniform prior on regression coefficients
//
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // sample size
  vector[N] y;    // survival times
  vector[N] x;    // group indicator
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real beta0;
  real beta1;
  real<lower=0> alpha;
}

transformed parameters {
    vector[N] sigma = exp(beta0+beta1*x);  // death rate by dosage
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  //beta0 ~ normal(0, 1e6);
  //beta1 ~ normal(0, 1e6);
  alpha ~ inv_gamma(2, 1);
  y ~ weibull(alpha, sigma);
   
  //y ~ weibull(4, sigma);
  
}

