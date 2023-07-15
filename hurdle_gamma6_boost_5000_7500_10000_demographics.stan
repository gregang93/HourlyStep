data {
  int<lower =0> Nd; //// Number of different days
  real<lower =0> y[Nd]; /// data
  real<lower =0> cumsum_lag[Nd]; /// 1st lag of accumulated
  int<lower =0> K; //// Number of columns in X
  matrix[Nd,K] X; //the model matrix
}

parameters{
  // real<lower = 0, upper = 1> theta0;
  // real<lower = 0, upper = 1> theta1;
  // real<lower = 0, upper = 1> theta2;
  // real<lower = 0, upper = 1> theta3;
  vector[K] alpha0;
  vector[K] alpha1;
  vector[K] alpha2;
  vector[K] alpha3;
  
  vector[K] beta0;
  vector[K] beta1;
  vector[K] beta2;
  vector[K] beta3;
  real<lower = 0> prec0;
  real<lower = 0> prec1;
  real<lower = 0> prec2;
  real<lower = 0> prec3;
}

transformed parameters {
  real<lower = 0> phi0;
  real<lower = 0> phi1;
  real<lower = 0> phi2;
  real<lower = 0> phi3;

  
  phi0 = 1/prec0;
  phi1 = 1/prec1;
  phi2 = 1/prec2;
  phi3 = 1/prec3;
}


model{
  alpha0 ~ normal(0,100);
  alpha1 ~ normal(0,100);
  alpha2 ~ normal(0,100);
  alpha3 ~ normal(0,100);
  beta0 ~ normal(0,100);
  beta1 ~ normal(0,100);
  beta2 ~ normal(0,100);
  beta3 ~ normal(0,100);
  prec0 ~ normal(0,100) T[0,];
  prec1 ~ normal(0,100) T[0,];
  prec2 ~ normal(0,100) T[0,];
  prec3 ~ normal(0,100) T[0,];
  for (nd in 1:Nd){
    if (cumsum_lag[nd] < 5000){
      if (y[nd]==0){
        // target += bernoulli_lpmf(1|theta0);
        target += bernoulli_logit_lpmf(1 | X[nd] * alpha0);
      } else {
        real logmu = X[nd] *beta0 ;
        real mu = exp(logmu);
        // target += bernoulli_lpmf(0 | theta0) + gamma_lpdf(y[nd]| prec0, prec0/mu);
        target += bernoulli_logit_lpmf(0 | X[nd] * alpha0) + gamma_lpdf(y[nd]| prec0, prec0/mu);
      }
    } else if (cumsum_lag[nd] < 7500){
      if (y[nd]==0){
        // target += bernoulli_lpmf(1|theta1);
        target += bernoulli_logit_lpmf(1 | X[nd] * alpha1);
      } else {
        real logmu = X[nd] *beta1;
        real mu = exp(logmu);
        // target += bernoulli_lpmf(0 | theta1) + gamma_lpdf(y[nd]| prec1, prec1/mu);
        target += bernoulli_logit_lpmf(0 | X[nd] * alpha1) + gamma_lpdf(y[nd]| prec1, prec1/mu);
      }
    } else if (cumsum_lag[nd] < 10000){
      if (y[nd]==0){
        // target += bernoulli_lpmf(1|theta2);
        target += bernoulli_logit_lpmf(1 | X[nd] * alpha2);
      } else {
        real logmu = X[nd] *beta2;
        real mu = exp(logmu);
        // target += bernoulli_lpmf(0 | theta2) + gamma_lpdf(y[nd]| prec2, prec2/mu);
        target += bernoulli_logit_lpmf(0 | X[nd] * alpha2) + gamma_lpdf(y[nd]| prec2, prec2/mu);
      }
    } else{
      if (y[nd]==0){
        // target += bernoulli_lpmf(1|theta3);
        target += bernoulli_logit_lpmf(1 | X[nd] * alpha3);
      } else {
        real logmu = X[nd] *beta3;
        real mu = exp(logmu);
        // target += bernoulli_lpmf(0 | theta3) + gamma_lpdf(y[nd]| prec3, prec3/mu);
        target += bernoulli_logit_lpmf(0 | X[nd] * alpha3) + gamma_lpdf(y[nd]| prec3, prec3/mu);
      }
    }
    
  }
}

