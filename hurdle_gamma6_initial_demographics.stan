data {
  int<lower =0> Nd; //// Number of rows
  int<lower =0> K; //// Number of columns in X
  real<lower =0> y[Nd]; /// data
  matrix[Nd,K] X; //the model matrix , 1st column is always one.
}

parameters{
  //real<lower = 0, upper = 1> theta;
  vector[K] alpha; //Regression coefficients for logistic (1st part)
  vector[K] beta; //Regression coefficients for Gamma (2nd part)
  real<lower = 0> prec;
}

transformed parameters {
  real<lower = 0> phi;
  phi = 1/prec;
}

model{
  alpha ~ normal(0,100);
  beta ~ normal(0,100);
  prec ~ normal(0,100) T[0,];
  for (nd in 1:Nd){
    if (y[nd]==0){
      // target += bernoulli_lpmf(1|theta);
      target += bernoulli_logit_lpmf(1 | X[nd] * alpha);
    } else {
      real logmu = X[nd] * beta;
      real mu = exp(logmu);
      // target += bernoulli_lpmf(0 | theta) + gamma_lpdf(y[nd]| prec, prec/mu);
      target += bernoulli_logit_lpmf(0 | X[nd] * alpha) + gamma_lpdf(y[nd]| prec, prec/mu);
    }
  }
}

