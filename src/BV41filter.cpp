#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// Functions written by Dominik Schulz, 24/04/2023

// C++ version of BV4.1 trend-cycle and seasonality smoothing
// [[Rcpp::export]]
Rcpp::List BV41filterCpp(const arma::vec& xt, const arma::mat& wst,
  const arma::mat& wss, const int nrowst, const int nrowss, 
  const int maxt, const int maxs) {

  const int n = xt.size();
  arma::vec trend_e = arma::zeros<arma::vec>(n);
  arma::vec season_e = arma::zeros<arma::vec>(n);
  
  const int Nt = 2 * nrowst + 1;
  const int Ns = 2 * nrowss + 1;
  
  arma::mat ymd = arma::zeros<arma::mat>(Nt, n - 2 * nrowst);
  arma::mat ymd2 = arma::zeros<arma::mat>(Ns, n - 2 * nrowss);
  
  for (int i = nrowst + 1; i < n - nrowst + 1; ++i) {
    
    ymd.col(i - (nrowst + 1)) = xt.subvec(i - nrowst - 1, i + nrowst - 1);
    
  }
  
  trend_e.subvec(0, nrowst - 1) = wst.rows(0, nrowst - 1) * xt.head(maxt);
  trend_e.subvec(nrowst, n - nrowst - 1) = trans(wst.submat(nrowst, 0, nrowst, Nt - 1) * ymd); 
  trend_e.subvec(n - nrowst, n - 1) = wst.rows(nrowst + 1, Nt - 1) * xt.tail(maxt); 

  arma::vec zt = xt - trend_e;
  
  for (int i = nrowss + 1; i < n - nrowss + 1; ++i) {
    
    ymd2.col(i - (nrowss + 1)) = zt.subvec(i - nrowss - 1, i + nrowss - 1);
    
  }   
  
  season_e.subvec(0, nrowss - 1) = wss.rows(0, nrowss - 1) * zt.head(maxs);
  season_e.subvec(nrowss, n - nrowss - 1) = trans(wss.submat(nrowss, 0, nrowss, Ns - 1) * ymd2); 
  season_e.subvec(n - nrowss, n - 1) = wss.rows(nrowss + 1, Ns - 1) * zt.tail(maxs);  
  
  Rcpp::List listOut = List::create(_["trend_e"] = trend_e,
                              _["season_e"] = season_e);
  
  return listOut;
  
}
  
  
