data {
int<lower=0> J;
// number of schools
array[J] real y;
// estimated treatment effects
array[J] real<lower=0> sigma; // s.e.’s of effect estimates
}
parameters {
real mu;
// population mean
real<lower=0> tau;
// population sd
vector[J] eta;
// school-level errors
}
transformed parameters {
vector[J] theta;
// school effects
theta = mu + tau*eta;
}
model {
eta ~ normal(0, 1);
y ~ normal(theta, sigma);
}
