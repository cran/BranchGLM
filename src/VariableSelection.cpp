#include <RcppArmadillo.h>
#include <cmath>
#include "BranchGLMHelpers.h"
using namespace Rcpp;

// Function used to get number of models given a certain maxsize and the number 
// of variables
// [[Rcpp::export]]
unsigned long long GetNum(unsigned long long size, unsigned long long max){
  double temp = 0;
  if(max >= size){
    temp = pow(2, size);
  }else{
    double helper = 1;
    temp = 1;
    for(unsigned int i = 1; i <= max; i++){
      helper *= (double)(size - i + 1) /(i);
      temp += round(helper);
    }
  }
  return(temp);
}
// Class to display progress for branch and bound method
class Progress{
private:
  unsigned long long max_size,cur_size;
  double last_print = -0.0000000001; 
  double diff = 0.0000000001;
  bool display_progress;
public:
  Progress(unsigned long long maxnum, bool display):max_size(maxnum), cur_size(0), 
  display_progress(display){}
  void update(unsigned long long num = 1){
    cur_size += num;
  };
  void print(){
    double next_print = 100 * (float)cur_size / (float)max_size;
    if(display_progress && next_print - last_print >= diff){
      Rcout << "Checked " << next_print << "% of all possible models"  << std::endl;
      while(diff <= (next_print - last_print) && diff <= 1.0){
        diff *= 10;
      }
      last_print = next_print;
    }
  }
    void finalprint(){
      double next_print = 100 * (float)cur_size / (float)max_size;
      if(display_progress){
        Rcout << "Checked " << next_print << "% of all possible models"  << std::endl;
        Rcout << "Found best model"  << std::endl << std::endl;
      }
  }
};
// Gets metric for given log likelihood and x matrix
double GetMetric(const arma::mat* X, double logLik, 
                 std::string Dist, std::string metric){
  
  double value = 0;
  unsigned int k = X->n_cols;
  if(Dist == "gaussian" || Dist == "gamma"){
    k++;
  }
  
  if(metric == "AIC"){
    value = -2 * logLik + 2 * k;
  }else if(metric == "AICc"){
    value = -2 * logLik + 2 * k + (2 * k + 2 * pow(k, 2)) / (X->n_rows - k - 1);
  }else if(metric == "BIC"){
    value = -2 * logLik + log(X->n_rows) * k;
  }
  
  return(value);
}
// Gets matrix for a given model
arma::mat GetMatrix(const arma::mat* X, arma::ivec* CurModel, 
                    arma::ivec* Indices){
  
  // Getting size of matrix and the desired columns
  double Size = 0;
  unsigned int k = 0;
  arma::ivec CurCols(Indices->n_elem, arma::fill::zeros);
  for(unsigned int i = 0; i < Indices->n_elem; i++){
    if(CurModel->at(Indices->at(i)) != 0){
      CurCols.at(i) = 1;
      Size++;
    }
  }
  
  // Making desired matrix
  arma::mat xTemp(X->n_rows, Size);
  k = 0;
  for(unsigned int j = 0; j < Indices->n_elem; j++){
    if(CurCols.at(j) == 1){
      xTemp.col(k) = X->col(j);
      k++;
    }
  }
  
  return(xTemp);
}

double BoundHelper(const arma::mat* X, double logLik, 
                   std::string Dist, std::string metric, int minsize){
  
  double value = 0;
  unsigned int k = minsize;
  if(Dist == "gaussian" || Dist == "gamma"){
    k++;
  }
  
  if(metric == "AIC"){
    value = -2 * logLik + 2 * k;
  }else if(metric == "AICc"){
    value = -2 * logLik + 2 * k + (2 * k + 2 * pow(k, 2)) / (X->n_rows - k - 1);
  }else if(metric == "BIC"){
    value = -2 * logLik + log(X->n_rows) * k;
  }
  
  return(value);
}

// Updates bound if upper model is the same as it was for the previous lower bound, 
// is also a naive bound checked before calculating better lower bound
double UpdateBound(const arma::mat* X, arma::ivec* indices, int cur, double LowerBound, 
                   std::string metric, int minsize){
  
  double value = 0;
  unsigned int k = 0;
  for(unsigned int i = 0; i < indices->n_elem; i++){
    if(indices->at(i) == (cur - 1)){
      k++;
    }
  }
  
  if(metric == "AIC"){
    value =  LowerBound  + 2 * k;
  }else if(metric == "AICc"){
    unsigned int newk = minsize - k;
    value = LowerBound + 2 * k - (2 * newk + 2 * pow(newk, 2)) / (X->n_rows - newk - 1) + 
      (2 * minsize + 2 * pow(minsize, 2)) / (X->n_rows - minsize - 1);
  }else if(metric == "BIC"){
    value = LowerBound + log(X->n_rows) * k;
  }
  return(value);
}
