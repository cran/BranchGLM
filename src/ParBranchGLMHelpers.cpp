#include <RcppArmadillo.h>
#include "CrossProducts.h"
#include <boost/math/distributions/normal.hpp>
#include <cmath>
using namespace Rcpp;

// Checking bounds and modifying values that are out of bounds
void ParCheckBounds(arma::vec* mu, std::string Dist){
  if(Dist == "binomial"){
    mu->transform([](double val){
      if(val <= 0){val = FLT_EPSILON;}
      else if(val >= 1) {val = 1 - FLT_EPSILON;}
      return(val);
    });
  }
  else if(Dist == "poisson" || Dist == "gamma"){
    mu->transform([](double val){
      if(val <= 0){val = FLT_EPSILON;}
      return(val);
    });
  }
}

// Defining Link functions
arma::vec ParLinkCpp(const arma::mat* X, arma::vec* beta, const arma::vec* Offset, 
                     std::string Link, std::string Dist){
  
  // Calculating linear predictors and initializing vector for mu
  arma::vec mu = (*X * *beta) + *Offset;
  
  
  // Calculating mu
  if(Link == "log"){
    mu = exp(mu);
  }
  else if(Link == "logit"){
    mu = 1 / (1 + exp(-mu));
  }
  else if(Link == "probit"){
    mu = arma::normcdf(mu);
  }
  else if(Link == "cloglog"){
    mu = 1 - exp(-exp(mu));
  }
  else if(Link == "inverse"){
    mu = 1 / (mu);
  }
  else if(Link == "sqrt"){
    mu = pow(mu, 2);
  }
  
  // Checking bounds for mu and modifying values that are out of bounds
  ParCheckBounds(&mu, Dist);
  return(mu);
}

// Defining Derivative functions
arma::vec ParDerivativeCpp(const arma::mat* X, arma::vec* beta, const arma::vec* Offset,
                           arma::vec* mu, std::string Link, std::string Dist){
  
  // Initializing vector to store derivative
  arma::vec Deriv(mu->n_elem);
  
  // Calculating derivative
  if(Link == "log"){
    Deriv = *mu; 
  }
  else if(Link == "logit"){
    Deriv = *mu % (1 - *mu);
  }
  else if(Link == "probit"){
    Deriv = arma::normpdf(*X * *beta + *Offset);
  }
  else if(Link == "cloglog"){
    Deriv = -(1 - *mu) % log(1 - *mu);
  }
  else if(Link == "inverse"){
    Deriv = -pow(*mu, 2);
  }
  else if(Link == "identity"){
    Deriv.fill(1);
  }
  else if(Link == "sqrt"){
    Deriv = 2 * sqrt(*mu);
  }
  
  return(Deriv);
}

// Defining Variance functions for each family
arma::vec ParVariance(arma::vec* mu, std::string Dist){
  
  // Initializing vector to store variance
  arma::vec Var(mu->n_elem);
  
  // Calculating variance
  if(Dist == "poisson"){
    Var = *mu; 
  }
  else if(Dist == "binomial"){
    Var = *mu % (1 - *mu);
  }
  else if(Dist == "gamma"){
    Var = pow(*mu, 2);
  }
  else{
    Var.fill(1);
  }
  
  // Replacing zeros with FLT_epsilon
  Var.replace(0, FLT_EPSILON);
  
  return(Var);
  
}

// Defining log likelihood
double ParLogLikelihoodCpp(const arma::mat* X, const arma::vec* Y, 
                           arma::vec* mu, std::string Dist){
  
  // Initializing double to store log-likelihood
  double LogLik = 0;
  
  
  // Calculating log-likelihood
  if(Dist == "poisson"){
    for(unsigned int i = 0; i < Y->n_elem; i++){
      LogLik += -Y->at(i) * log(mu->at(i)) + mu->at(i);
    }
  }
  else if(Dist == "binomial"){
    for(unsigned int i = 0; i < Y->n_elem; i++){
      double theta = mu->at(i) / (1 - mu->at(i));
      LogLik += -Y->at(i) * log(theta) + log1p(theta);
    }
  }else if(Dist == "gamma"){
    arma::vec theta = -1 / *mu;
    LogLik = -arma::dot(*Y, theta) - arma::accu(log(-theta));
  }else{
    for(unsigned int i = 0; i < Y->n_elem; i++){
      LogLik += pow(Y->at(i) - mu->at(i), 2) /2;
    }
  }
  return(LogLik);
}

// Defining log likelihood for saturated model
double ParLogLikelihoodSat(const arma::mat* X, const arma::vec* Y, std::string Dist){
  
  // Initializing double to hold saturated log-likelihood
  double LogLik = 0;
  
  // Calculating saturated log-likelihood
  if(Dist == "poisson"){
    for(unsigned int i = 0; i< Y->n_elem;i++){
      if(Y->at(i) !=0){
        LogLik += Y->at(i) * (log(Y->at(i)) - 1);
      }
    }
  }
  else if(Dist == "binomial"){
    LogLik = 0;
  }else if(Dist == "gamma"){
    arma::vec theta = -1 / *Y;
    LogLik = arma::dot(*Y, theta) + arma::accu(log(-theta));
  }else{
    LogLik = 0 ;
  }
  
  return(LogLik);
}

// Defining score function
arma::vec ParScoreCpp(const arma::mat* X, const arma::vec* Y, arma::vec* Deriv,
                      arma::vec* Var, arma::vec* mu){
  
  // Initializing vector for score
  arma::vec FinalVec(X->n_cols);
  
  // Calculating w and diff for score computation
  arma::vec w = *Deriv / *Var;
  arma::vec diff = *Y - *mu;
  w.replace(arma::datum::nan, 0);
  
  // Calculating score
  for(unsigned int i = 0; i < X->n_cols; i++){
    
    FinalVec(i) = -arma::dot(X->col(i) % w, diff);
    
  }
  return FinalVec;
}

// Defining fisher information function
arma::mat ParFisherInfoCpp(const arma::mat* X, arma::vec* Deriv, 
                           arma::vec* Var){
  
  // Initializing matrix to store results
  arma::mat FinalMat(X->n_cols, X->n_cols);
  
  // Calculating weight vector, this is the diagonal of the W matrix
  arma::vec w = pow(*Deriv, 2) / *Var;
  w.replace(arma::datum::nan, 0);
  
  // Calculating X'WX
  for(unsigned int i = 0; i < X->n_cols; i++){
    
    FinalMat(i, i) = arma::dot((X->col(i) % w), X->col(i));
    
    for(unsigned int j = i + 1; j < X->n_cols; j++){
      
      FinalMat(i, j) = arma::dot((X->col(j) % w), X->col(i));
      FinalMat(j, i) = FinalMat(i, j);
      
    } 
    
  }
  return FinalMat;
}

// Function used to get step size
void ParGetStepSize(const arma::mat* X, const arma::vec* Y, const arma::vec* Offset,
                    arma::vec* mu, arma::vec* Deriv, arma::vec* Var, arma::vec* g1, 
                    arma::vec* p, arma::vec* beta, 
                    std::string Dist, std::string Link, 
                    double* f0, double* f1, double* t, double* alpha, 
                    std::string method){
  
  // Defining maximum number of iterations and counter variable
  unsigned int maxiter = 40;
  unsigned int k = 0;
  
  // Defining C1 and C2 for backtracking
  // 0 < C1 < C2 < 1
  double C1 = pow(10, -4);
  double C2 = 0.9;
  
  // Setting initial step size to be 1
  *alpha = 1;
  
  // Creating temporary variables for alpha, beta, f1, and mu
  double temp = *alpha;
  double tempf1 = *f1;
  arma::vec tempbeta = *beta;
  arma::vec tempmu = *mu;
  
  // Checking condition for initial alpha
  tempbeta = *beta + temp * *p;
  tempmu = ParLinkCpp(X, &tempbeta, Offset, Link, Dist);
  tempf1 = ParLogLikelihoodCpp(X, Y, &tempmu, Dist);
  
  // Checking for descent direction
  if(*t <= 0){
    *alpha = 0;
    return;
  }
  
  if(method == "backtrack"){
    
    // Finding alpha with backtracking line search using strong wolfe conditions
    for(; k < maxiter; k++){
      
      // Checking first wolfe condition or armijo-goldstein condition
      if(*f0 >= tempf1 + C1 * temp * *t){
        
        // Calculating stuff to check second strong wolfe condition
        *Deriv = ParDerivativeCpp(X, &tempbeta, Offset, &tempmu, Link, Dist);
        *Var = ParVariance(&tempmu, Dist);
        *g1 = ParScoreCpp(X, Y, Deriv, Var, &tempmu);
        
        // Checking 2nd wolfe condition
        if(std::fabs(arma::dot(*p, *g1) <= C2 * std::fabs(*t))){
          break;
        }
      }
      
      // Performing step halving if we have not yet reached maxiter - 1
      if(k < maxiter - 1){
        temp /= 2;
        tempbeta = *beta + temp * *p;
        tempmu = ParLinkCpp(X, &tempbeta, Offset, Link, Dist);
        tempf1 = ParLogLikelihoodCpp(X, Y, &tempmu, Dist);
      }
    }
    
    // Changing variables if an appropriate step size is found
    // Setting alpha to 0 if no adequate step size is found
    if(k < maxiter){
      *alpha = temp;
      *beta = tempbeta;
      *mu = tempmu;
      *f1 = tempf1;
    }else if(k == maxiter){
      *alpha = 0;
    }
    
  }else{
    // Add other methods here
  }
}

// Creating LBFGS helper function
arma::vec ParLBFGSHelperCpp(arma::vec* g1, arma::mat* s, arma::mat* y, 
                            int* k, unsigned int* m, 
                            arma::vec* r, arma::vec* alpha, const arma::mat* Info){
  if(*k > 0) {
    unsigned int max = std::min(*k, (int)*m);
    unsigned int index;
    for(unsigned int i = 1; i <= max; i++){
      index = (*k - i) % *m;
      alpha->at(index) = arma::dot(s->col(index), *g1)/arma::dot(y->col(index), s->col(index));
      *g1 -= alpha->at(index) * y->col(index);
      
    }
    index = (*k - 1)% *m;
    *r = *Info * *g1;
    for(unsigned int j = max; j > 0; j--){
      index = (*k - j) % *m;
      *r += s->col(index) * (alpha->at(index) - arma::dot(y->col(index), *r)/arma::dot(y->col(index), s->col(index)));
    }
    
    return *r;
  }
  
  return *Info * *g1;
}

// Creating LBFGS for GLMs for Parallel functions
int ParLBFGSGLMCpp(arma::vec* beta, const arma::mat* X, const arma::mat* XTWX,
                   const arma::vec* Y, const arma::vec* Offset,
                   std::string Link, std::string Dist, 
                   double tol, int maxit, unsigned int m, bool UseXTWX){
  
  int k = 0;
  arma::vec mu = ParLinkCpp(X, beta, Offset, Link, Dist);
  arma::vec Deriv = ParDerivativeCpp(X, beta, Offset, &mu, Link, Dist);
  arma::vec Var = ParVariance(&mu, Dist);
  m = std::min(beta->n_elem, m);
  arma::vec p(beta->n_elem);
  arma::vec g0(beta->n_elem);
  arma::vec g1 = ParScoreCpp(X, Y, &Deriv, &Var, &mu);
  arma::vec r(beta->n_elem);
  arma::vec alphavec(m);
  arma::mat s(beta->n_elem, m);
  arma::mat y(beta->n_elem, m);
  arma::mat Info(beta->n_elem, beta->n_elem);
  
  if(UseXTWX){
    if(!solve(Info, *XTWX, arma::eye(arma::size(Info)), 
              arma::solve_opts::no_approx + arma::solve_opts::likely_sympd)){
      return(-2);
    }
  }
  else{
    if(!solve(Info, ParFisherInfoCpp(X, &Deriv, &Var), arma::eye(arma::size(Info)), 
              arma::solve_opts::no_approx + arma::solve_opts::likely_sympd)){
      return(-2);
    }
  }
  double f0;
  double f1 = ParLogLikelihoodCpp(X, Y, &mu, Dist);
  double t;
  double alpha;
  
  while(arma::norm(g1) > tol){
    
    // Checks if we've reached maxit iterations and stops if we have
    if(k >= maxit){ 
      k = -1;
      break;
    }
    
    // Re-assigning log-likelihood and score
    g0 = g1;
    f0 = f1;
    
    // Calculating p (search direction) based on L-BFGS approximation to inverse info
    p = -ParLBFGSHelperCpp(&g1, &s, &y, &k, &m, &r, &alphavec, &Info);
    t = -arma::dot(g0, p);
    
    // Finding alpha with backtracking linesearch using strong wolfe conditions
    // This function also calculates mu, Deriv, Var, and g1 for the selected step size
    ParGetStepSize(X, Y, Offset, &mu, &Deriv, &Var, &g1, &p, beta, Dist, Link, &f0 ,&f1, &t, &alpha, "backtrack");
    
    if(std::fabs(f1 -  f0) < tol || all(abs(alpha * p) < tol) || alpha == 0){
      if(std::isinf(f1)|| beta->has_nan() || alpha == 0){
        k = -2;
      }
      k++;
      break;}
    
    // Updating s and y for L-BFGS update
    s.col(k % m) = alpha * p;
    y.col(k % m) = g1 - g0;
    
    // Incrementing iteration number
    k++;
  }
  return(k);
}


// Creating BFGS for GLMs for Parallel functions
int ParBFGSGLMCpp(arma::vec* beta, const arma::mat* X, const arma::mat* XTWX,  
                  const arma::vec* Y, const arma::vec* Offset,
                  std::string Link, std::string Dist,
                  double tol, int maxit, bool UseXTWX){
  
  int k = 0;
  arma::vec mu = ParLinkCpp(X, beta, Offset, Link, Dist);
  arma::vec Deriv = ParDerivativeCpp(X, beta, Offset, &mu, Link, Dist);
  arma::vec Var = ParVariance(&mu, Dist);
  arma::vec g1 = ParScoreCpp(X, Y, &Deriv, &Var, &mu);
  arma::vec p(beta->n_elem);
  arma::vec s(beta->n_elem);
  arma::vec y(beta->n_elem);
  arma::vec g0(beta->n_elem);
  arma::mat H1(beta->n_elem, beta->n_elem);
  
  if(UseXTWX){
    if(!solve(H1, *XTWX, arma::eye(arma::size(H1)), 
              arma::solve_opts::no_approx + arma::solve_opts::likely_sympd)){
      return(-2);
    }
  }
  else{
    if(!solve(H1, ParFisherInfoCpp(X, &Deriv, &Var), arma::eye(arma::size(H1)), 
              arma::solve_opts::no_approx + arma::solve_opts::likely_sympd)){
      return(-2);
    }
  }
  
  double f0;
  double f1 = ParLogLikelihoodCpp(X, Y, &mu, Dist);
  double rho;
  double alpha;
  double t;
  
  while(arma::norm(g1) > tol){
    
    // Checks if we've reached maxit iterations and stops if we have
    if(k >= maxit){ 
      k = -1;
      break;
    }
    // Re-assigning log-likelihood and score
    g0 = g1;
    f0 = f1;
    
    // Finding direction based on approximate inverse hessian
    p = -H1 * g1;
    t = -arma::dot(g0, p);
    
    // Finding alpha with backtracking linesearch using strong wolfe conditions
    // This function also calculates mu, Deriv, Var, and g1 for the selected step size
    ParGetStepSize(X, Y, Offset, &mu, &Deriv, &Var, &g1, &p, beta, Dist, Link, &f0 ,&f1, &t, &alpha, "backtrack");
    
    // Checking for convergence or non-convergence
    if(std::fabs(f1 -  f0) < tol || all(abs(alpha * p) < tol) || alpha == 0){
      if(std::isinf(f1) || beta->has_nan() || alpha == 0){
        k = -2;
      }
      k++;
      break;}
    
    // Performing BFGS update
    s = alpha * p;
    y = g1 - g0;
    rho = 1/arma::dot(s, y);
    
    H1 = (arma::diagmat(arma::ones(beta->n_elem)) - rho * s * y.t()) * H1 * 
      (arma::diagmat(arma::ones(beta->n_elem)) - rho * y * s.t()) + rho * s * s.t();
    
    // Incrementing iteration number
    k++;
  }
  return(k);
}


// Creating Fisher Scoring for GLMs for Parallel functions
int ParFisherScoringGLMCpp(arma::vec* beta, const arma::mat* X, 
                           const arma::mat* XTWX, const arma::vec* Y, const arma::vec* Offset,
                           std::string Link, std::string Dist,
                           double tol, int maxit, bool UseXTWX){
  
  int k = 0;
  arma::vec mu = ParLinkCpp(X, beta, Offset, Link, Dist);
  arma::vec Deriv = ParDerivativeCpp(X, beta, Offset, &mu, Link, Dist);
  arma::vec Var = ParVariance(&mu, Dist);
  arma::vec g1 = ParScoreCpp(X, Y, &Deriv, &Var, &mu);
  arma::vec p(beta->n_elem);
  arma::mat H1(beta->n_elem, beta->n_elem);
  if(UseXTWX){
    H1 = *XTWX;
  }
  else{
    H1 = ParFisherInfoCpp(X, &Deriv, &Var);
  }
  double f0;
  double f1 = ParLogLikelihoodCpp(X, Y, &mu, Dist);
  double alpha;
  double t;
  while(arma::norm(g1) > tol){
    // Checks if we've reached maxit iterations and stops if we have
    if(k >= maxit){ 
      k = -1;
      break;
    }
    
    // Re-assigning log-likelihood
    f0 = f1;
    
    // Solving for newton direction
    if(!arma::solve(p, -H1, g1, arma::solve_opts::no_approx + arma::solve_opts::likely_sympd)){
      return(-2);
    }
    t = -arma::dot(g1, p);
    
    // Finding alpha with backtracking linesearch using strong wolfe conditions
    // This function also calculates mu, Deriv, Var, and g1 for the selected step size
    ParGetStepSize(X, Y, Offset, &mu, &Deriv, &Var, &g1, &p, beta, Dist, Link, &f0 ,&f1, &t, &alpha, "backtrack");
    
    // Checking for convergence or non-convergence
    if(std::fabs(f1 -  f0) < tol || all(abs(alpha * p) < tol) || alpha == 0){
      if(std::isinf(f1)|| beta->has_nan() || alpha == 0){
        k = -2;
      }
      k++;
      break;}
    
    // Calculating information
    H1 = ParFisherInfoCpp(X, &Deriv, &Var);
    
    // Incrementing iteration number
    k++;
  }
  return(k);
}

int ParLinRegCppShort(arma::vec* beta, const arma::mat* x, const arma::mat* XTWX, const arma::mat* y,
                      const arma::vec* offset){
  
  
  // Calculating inverse of X'X
  arma::mat InvXX(x->n_cols, x->n_cols, arma::fill::zeros); 
  arma::vec XY = x->t() * (*y - *offset);  
  arma::vec tempbeta = *beta;
  if(!arma::solve(*beta, *XTWX, XY, arma::solve_opts::no_approx + arma::solve_opts::likely_sympd)){
    *beta = tempbeta;
    return(-2);
  }
  
  return(1);
}

// Gets initial values for gamma and gaussian regression with log/inverse/sqrt link with 
// transformed y linear regression
void PargetInit(arma::vec* beta, const arma::mat* X, const arma::mat* XTWX, const arma::vec* Y, 
                const arma::vec* Offset, std::string Dist, std::string Link, 
                bool* UseXTWX){
  
  if(Link == "log"){
    arma::vec NewY = *Y;
    NewY = log(NewY.clamp(1e-4, arma::datum::inf));
    ParLinRegCppShort(beta, X, XTWX, &NewY, Offset);
    *UseXTWX = false;
    
  }else if(Link == "inverse"){
    arma::vec NewY = *Y;
    NewY.transform( [](double val) {
      if(std::fabs(val) <= 1e-2){
        val = (val / std::fabs(val)) * 1e-2;
      }
      return(val);
    } );
    NewY = 1 / NewY;
    ParLinRegCppShort(beta, X, XTWX, &NewY, Offset);
    *UseXTWX = false;
    
  }else if(Link == "sqrt"){
    const arma::vec NewY = sqrt(*Y);
    ParLinRegCppShort(beta, X, XTWX, &NewY, Offset);
    *UseXTWX = false;
    
  }else if(Link == "identity" && Dist != "gaussian"){
    ParLinRegCppShort(beta, X, XTWX, Y, Offset);
    *UseXTWX = false;
    
  }else if(Link == "logit"){
    arma::vec NewY = *Y;
    NewY = NewY.clamp(1e-4, 1 - 1e-4);
    NewY = log(NewY / (1 - NewY));
    ParLinRegCppShort(beta, X, XTWX, &NewY, Offset);
    *UseXTWX = false;
    
  }else if(Link == "probit"){
    arma::vec NewY = *Y;
    double val0 = boost::math::quantile(boost::math::normal(0.0, 1.0), 1e-4);
    double val1 = boost::math::quantile(boost::math::normal(0.0, 1.0), 1 - 1e-4);
    for(unsigned int i = 0; i < NewY.n_elem; i++){
      if(NewY.at(i) == 0){
        NewY.at(i) = val0;
      }else{
        NewY.at(i) = val1;
      }
    }
    ParLinRegCppShort(beta, X, XTWX, &NewY, Offset);
    *UseXTWX = false;
    
  }else if(Link == "cloglog"){
    arma::vec NewY = *Y;
    NewY = NewY.clamp(1e-4, 1 - 1e-4);
    NewY = log(-log(1 - NewY));
    ParLinRegCppShort(beta, X, XTWX, &NewY, Offset);
    *UseXTWX = false;
    
  }
  
}
