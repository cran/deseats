#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// Functions written by Dominik Schulz, 26/09/2023

// Function to help obtain a sequence from 'from' to 'to' by step 1
// in C++
// [[Rcpp::export]]
arma::vec seqCpp(const int from, const int to){
  const int n = to - from + 1;
  arma::vec seqOut(n);
  for (int i = from; i < to + 1; ++i) {
    seqOut(i - from) = i;
  }
  return seqOut;
}

// Function to help obtain a sequence from 'from' to 'to' by step 1
// in C++ (returns a rowvec instead)
// [[Rcpp::export]]
arma::rowvec rseqCpp(const int from, const int to) {
  const int n = to - from + 1;
  arma::rowvec seqOut(n);
  for (int i = from; i < to + 1; ++i) {
    seqOut(i - from) = i;
  }
  return seqOut;
}

// Function to calculate the factorial of an 'int' in C++
// [[Rcpp::export]]
int factorialCpp(const int k) {
  int fac = 1;
  if (k > 1) {
    for (int i = 2; i < k + 1; ++i) {
      fac *= i;
    }
  }
  return fac;
}


// Fit ARMA
// [[Rcpp::export]]
Rcpp::List armaCpp(const arma::vec& Xt, const int p, const int q, const int armamean) {
  Rcpp::Environment pkg = Rcpp::Environment::namespace_env("deseats");
  // Rcpp::Function f = pkg["arima"];    
  Rcpp::Function f = pkg["arima_no_warn"];
  
  Rcpp::NumericVector order = Rcpp::NumericVector::create(p, 0, q);
  
  return f(Xt, order, armamean);
  
}

// [[Rcpp::export]]
double BICarmaCpp(const arma::vec& Xt, const int p, const int q, const int armamean) {
  
  Rcpp::List arma = armaCpp(Xt, p, q, armamean);
  const double llhood = arma["loglik"];
  const int n = Xt.size();
  return -2.0 * llhood + std::log(n) * (p + q);
  
} 

// [[Rcpp::export]]
Rcpp::NumericVector selectOrderBIC(const arma::vec& Xt, const int pmin, const int pmax, const int qmin, const int qmax, const int armamean) {
  
  arma::mat bicmat = arma::zeros<arma::mat>(pmax - pmin + 1, qmax - qmin + 1);
  
  int ari = 0;
  int mai;
  
  for (int p0 = pmin; p0 < pmax + 1; ++p0) {
    mai = 0;
    for (int q0 = qmin; q0 < qmax + 1; ++q0) {
      
      bicmat(ari, mai) = BICarmaCpp(Xt, p0, q0, armamean);
      
      mai = mai + 1;
      
    }
    ari = ari + 1;
  }
  
  arma::uvec ind = arma::find(bicmat == bicmat.min());
  
  int popt = (int)ind(0);
  
  int qopt = popt / (pmax - pmin + 1) + qmin;

  for (int j = 0; popt >= (pmax - pmin + 1); ++j) {
    popt = popt - (pmax - pmin + 1);
  }

  popt = popt + pmin;
  return Rcpp::NumericVector::create(popt, 0, qopt);
  
}

// [[Rcpp::export]]
Rcpp::List armaoptCpp(const arma::vec& Xt, const int narmin, const int narmax, const int nmamin, const int nmamax, const int armamean) {
  
  Rcpp::NumericVector order = selectOrderBIC(Xt, narmin, narmax, nmamin, nmamax, armamean);
  
  Rcpp::Environment pkg = Rcpp::Environment::namespace_env("deseats");
  // Rcpp::Function f = pkg["arima"];    
  Rcpp::Function f = pkg["arima_no_warn"];
  
  Rcpp::List armaopt = f(Xt, order, armamean);
  
  arma::vec coefs = armaopt["coef"];
  
  return armaopt;  
  
}
  
// [[Rcpp::export]]  
double arma_sumacovCpp(Rcpp::List arma) {
  
  const arma::vec info = arma["arma"];
  
  const int p = info(0);
  const int q = info(1);
  
  const arma::vec coef = arma["coef"];
  const double sig2 = arma["sigma2"];
  
  arma::vec ar = arma::zeros<arma::vec>(1);
  arma::vec ma = arma::zeros<arma::vec>(1);  
  
  if (p >= 1) {
    ar = coef.subvec(0, p - 1);
  }
  if (q >= 1) {
    ma = coef.subvec(p, p + q - 1);
  }
  
  return std::pow((1 + arma::accu(ma)) / (1 - arma::accu(ar)), 2.0) * sig2;
  
}  
  
// Autocovariance calculation from given data
// [[Rcpp::export]]
arma::vec acovCpp(const arma::vec& Xt, const int lagMax) {
  const double meanX = arma::mean(Xt);
  arma::vec XtDM = Xt - meanX;
  arma::vec acovOut(lagMax + 1);
  const int n = Xt.size();
  for (int i = 0; i < lagMax + 1; ++i) {
    acovOut(i) = arma::accu(XtDM.subvec(0, n - 1 - i) % XtDM.subvec(i, n - 1));
  }
  arma::vec out = acovOut / n;
  return out;
}


// Buehlmann algorithm for estimating cf0
// [[Rcpp::export]]
double cf0Cpp(const arma::vec& Xt) {
  const int n = Xt.size();
  arma::vec ga = acovCpp(Xt, n - 1);
  const int nit = 20;
  int runc = 1;
  arma::vec L(nit + 1);
  L(0) = std::trunc(n / 2.0 + 0.5);
  const double c1 = (std::pow(ga(0), 2.0) + 2.0 * arma::accu(arma::pow(ga.subvec(1, n - 1), 2.0))) / (4.0 * M_PI);
  double c2 = 0;
  int L1 = 0;
  int LGopt = 0;

  for(int i = 0; i < nit; i++) {
    if(runc == 1) {
      L1 = std::trunc(L(i) / std::pow(n, 2.0 / 21.0)) + 1;
      arma::vec x1 = seqCpp(0, L1 - 1) / L1;
      arma::vec w1 = 1.0 - x1;
      arma::vec gai = seqCpp(0, L1 - 1) % ga.subvec(0, L1 - 1) % w1;
      c2 = 3.0 * (2.0 * arma::accu(arma::pow(gai.subvec(0, L1 - 1), 2.0))) / (2.0 * M_PI);
      L(i + 1) = std::trunc(std::pow(n, 1.0 / 3.0) * std::pow(c2 / c1, 1.0 / 3.0)) + 1;

      if (L(i + 1) == L(i)) {
        runc = 0;
        LGopt = L(i + 1);
      }


    }
  }
  if (runc == 1) {
    LGopt = L(nit);
  }

  L1 = std::trunc(LGopt / std::pow(n, 2.0 / 21.0)) + 1;
  arma::vec x1 = seqCpp(0, L1 - 1) / L1;
  arma::vec w1 = 1.0 - x1;
  arma::vec ga1 = seqCpp(0, L1 - 1) % ga.subvec(0, L1 - 1) % w1;
  const double c20 = 3.0 * std::pow(2.0 * arma::accu((ga1.subvec(0, L1 - 1))), 2.0) / (2.0 * M_PI);
  const arma::vec w0 = (1.0 + cos(M_PI * x1)) / 2.0;
  arma::vec ga0 = ga.subvec(0, L1 - 1) % w0;
  const double c10 = std::pow(2.0 * arma::accu((ga0.subvec(0, L1 - 1))) - ga0(0), 2.0) / (2.0 * M_PI);
  const int L0opt = std::trunc(std::pow(n, 1.0 / 3.0) * std::pow((c20 / c10) / 2.0, 1.0 / 3.0)) + 1;
  const arma::vec wacf = arma::reverse(seqCpp(1, L0opt + 1)) / (L0opt + 1.0);
  const arma::vec acfX = ga.subvec(0, L0opt);
  const double cf0LW = 2.0 * arma::accu(acfX % wacf) - acfX(0);

  return cf0LW;
}

// C++ version of deseats smoothing
// [[Rcpp::export]]
Rcpp::List fitteddeseatsCpp(const arma::vec& y, const int p,
  const int s, const int mu, const double b, const int bb) {

  const int n = y.size();
  arma::vec grG(n);
  arma::vec grS(n);
  const int bwidthAbs = std::trunc(n * b + 0.5);
  const int bwidthMax = 2 * bwidthAbs + 1;
  
  arma::mat wsG(bwidthMax, bwidthMax);
  arma::mat wsS = wsG;
  arma::mat wsK = wsG;
  arma::rowvec wk = arma::zeros<arma::rowvec>(bwidthMax);
  arma::mat xt = arma::zeros<arma::mat>((p + s), bwidthMax);
  
  arma::mat xw = xt;

  const arma::vec seqhh = seqCpp(0, bwidthAbs);
  const arma::vec allLowB = bwidthAbs - seqhh;
  const arma::vec hr = bwidthAbs + bb * allLowB;
  const arma::vec hr2 = arma::pow(hr + 1, 2.0);
  const arma::vec ht = seqhh + hr;
  arma::mat xa = xt;
  
  const arma::rowvec allsequ = rseqCpp(-bwidthAbs, hr(0));
  const int m = allsequ.size();
  const arma::rowvec allsequ2 = arma::pow(allsequ, 2.0);
  const arma::rowvec allsequhh = allsequ / bwidthAbs;
  
  arma::mat dataMat = arma::ones<arma::mat>((p + s), m);  
  dataMat.row(1) = allsequhh;
  
  for (int i = 2; i < p + 1; ++i) {
    dataMat.row(i) = arma::pow(allsequhh, i);
  }
  
  const double l1 = 2.0 * M_PI / s;
  
  for (int i = 1; i < std::trunc((s - 1.0) / 2) + 1; ++i) {
    dataMat.row(p + 2 * i - 1) = arma::cos(i * l1 * allsequ); //look here
    dataMat.row(p + 2 * i) = arma::sin(i * l1 * allsequ);
  }
  
  if ((s / 2.0) == std::trunc(s / 2.0)) {
    dataMat.row(p + s - 1) = arma::cos(M_PI * allsequ);
  }  
  
  arma::rowvec ej = arma::zeros<arma::rowvec>(p + s);
  arma::rowvec phi = ej;  
  
  ej(0) = 1;
  
  for (int i = 1; i < s; i++) {
    if (i % 2 == 1) {
      phi(p + i) = 1;
    }
  }
  
  const arma::vec allUpB = bwidthAbs + hr;

  int lowB;
  int upB;
  
  for (int i = 0; i < bwidthAbs + 1; ++i) {

    lowB = allLowB(i);
    upB = allUpB(i);
    
    if (mu == 0) {
      wk.subvec(0, ht(i)).fill(1.0);
    } else if (mu == 1) {
      wk.subvec(0, ht(i)) = 1.0 - allsequ2.subvec(lowB, upB) / hr2(i);
    } else {
      wk.subvec(0, ht(i)) = arma::pow(1.0 - allsequ2.subvec(lowB, upB) / hr2(i), mu);
    }
    
    wsK.row(i) = wk;
    xt.submat(0, 0, p + s - 1, ht(i)) = dataMat.cols(lowB, upB);

    xw.submat(0, 0, p + s - 1, bwidthMax - 1) = xt * arma::diagmat(wk);
    xa.submat(0, 0, p + s - 1, bwidthMax - 1) = arma::solve(xw * xt.t(), xw);

    wsG.row(i) = ej * xa;
    wsS.row(i) = phi * xa;
    
  }
  
  wsG.rows(bwidthAbs + 1, bwidthMax - 1) = arma::flipud(arma::fliplr(wsG.submat(0, 0, bwidthAbs - 1, bwidthMax - 1)));
  wsS.rows(bwidthAbs + 1, bwidthMax - 1) = arma::flipud(arma::fliplr(wsS.submat(0, 0, bwidthAbs - 1, bwidthMax - 1)));  
  wsK.rows(bwidthAbs + 1, bwidthMax - 1) = arma::flipud(arma::fliplr(wsK.submat(0, 0, bwidthAbs - 1, bwidthMax - 1)));  
  
  arma::mat ws = wsG + wsS;  
  
  arma::mat ym(bwidthMax, n - bwidthMax + 1);
  for (int i = bwidthAbs; i < n - bwidthAbs; ++i) {
    ym.col(i - bwidthAbs) = y.subvec(i - bwidthAbs, i + bwidthAbs);
  }
  grG.subvec(0, bwidthAbs - 1) = wsG.rows(0, bwidthAbs - 1) * y.subvec(0, bwidthMax - 1);
  grG.subvec(bwidthAbs, n - bwidthAbs - 1) = (wsG.row(bwidthAbs) * ym).t();
  grG.subvec(n - bwidthAbs, n - 1) = wsG.rows(bwidthAbs + 1, bwidthMax - 1) * y.subvec(n - bwidthMax, n - 1);
  
  grS.subvec(0, bwidthAbs - 1) = wsS.rows(0, bwidthAbs - 1) * y.subvec(0, bwidthMax - 1);
  grS.subvec(bwidthAbs, n - bwidthAbs - 1) = (wsS.row(bwidthAbs) * ym).t();
  grS.subvec(n - bwidthAbs, n - 1) = wsS.rows(bwidthAbs + 1, bwidthMax - 1) * y.subvec(n - bwidthMax, n - 1);  
  
  arma::vec gr = grG + grS;
  arma::vec resid = y - gr;
  
  Rcpp::List listOut = List::create(_["Compl"] = gr,
                              _["Trend"] = grG,
                              _["Season"] = grS,
                              _["Residuals"] = resid,
                              _["wsG"] = wsG,
                              _["wsS"] = wsS,
                              _["ws"] = ws,
                              _["wsK"] = wsK);
  
  return listOut;
  
}

// C++ version of deseats smoothing
// [[Rcpp::export]]
arma::vec residdeseatsCpp(const arma::vec& y, const int p,
  const int s, const int mu, const double b, const int bb) {

  const int n = y.size();
  arma::vec gr(n);
  const int bwidthAbs = std::trunc(n * b + 0.5);
  const int bwidthMax = 2 * bwidthAbs + 1;
  
  arma::mat ws(bwidthMax, bwidthMax);

  arma::rowvec wk = arma::zeros<arma::rowvec>(bwidthMax);
  arma::mat xt = arma::zeros<arma::mat>((p + s), bwidthMax);
  
  arma::mat xw = xt;
  
  const arma::vec seqhh = seqCpp(0, bwidthAbs);
  const arma::vec allLowB = bwidthAbs - seqhh;
  const arma::vec hr = bwidthAbs + bb * allLowB;
  const arma::vec hr2 = arma::pow(hr + 1, 2.0);
  const arma::vec ht = seqhh + hr;
  arma::mat xa = xt;
  
  const arma::rowvec allsequ = rseqCpp(-bwidthAbs, hr(0));
  const int m = allsequ.size();
  const arma::rowvec allsequ2 = arma::pow(allsequ, 2.0);
  const arma::rowvec allsequhh = allsequ / bwidthAbs;
  
  arma::mat dataMat = arma::ones<arma::mat>((p + s), m);  
  dataMat.row(1) = allsequhh;
  
  for (int i = 2; i < p + 1; ++i) {
    dataMat.row(i) = arma::pow(allsequhh, i);
  }
  
  const double l1 = 2.0 * M_PI / s;  
  
  for (int i = 1; i < std::trunc((s - 1.0) / 2) + 1; ++i) {
    dataMat.row(p + 2 * i - 1) = arma::cos(i * l1 * allsequ); //look here
    dataMat.row(p + 2 * i) = arma::sin(i * l1 * allsequ);
  }
  
  if ((s / 2.0) == std::trunc(s / 2.0)) {
    dataMat.row(p + s - 1) = arma::cos(M_PI * allsequ);
  }  
  
  arma::rowvec ej = arma::zeros<arma::rowvec>(p + s);
  arma::rowvec phi = ej;  
  arma::rowvec prevec = ej;   
  
  ej(0) = 1;
  
  for (int i = 1; i < s; i++) {
    if (i % 2 == 1) {
      phi(p + i) = 1;
    }
  }

  prevec.subvec(0, p) = ej.subvec(0, p); 

  if (s > 1) {
    prevec.subvec(p + 1, p + s - 1) = phi.subvec(p + 1, p + s - 1); 
  }

  const arma::vec allUpB = bwidthAbs + hr;

  int lowB;
  int upB;
  
  for (int i = 0; i < bwidthAbs + 1; ++i) {

    lowB = allLowB(i);
    upB = allUpB(i);
    
    if (mu == 0) {
      wk.subvec(0, ht(i)).fill(1.0);
    } else if (mu == 1) {
      wk.subvec(0, ht(i)) = 1.0 - allsequ2.subvec(lowB, upB) / hr2(i);
    } else {
      wk.subvec(0, ht(i)) = arma::pow(1.0 - allsequ2.subvec(lowB, upB) / hr2(i), mu);
    }
    
    xt.submat(0, 0, p + s - 1, ht(i)) = dataMat.cols(lowB, upB);
    xw.submat(0, 0, p + s - 1, bwidthMax - 1) = xt * arma::diagmat(wk);
    xa.submat(0, 0, p + s - 1, bwidthMax - 1) = arma::solve(xw * xt.t(), xw);

    ws.row(i) = prevec * xa;
    
  }
  
  ws.rows(bwidthAbs + 1, bwidthMax - 1) = arma::flipud(arma::fliplr(ws.submat(0, 0, bwidthAbs - 1, bwidthMax - 1)));

  arma::mat ym(bwidthMax, n - bwidthMax + 1);
  for (int i = bwidthAbs; i < n - bwidthAbs; ++i) {
    ym.col(i - bwidthAbs) = y.subvec(i - bwidthAbs, i + bwidthAbs);
  }
  gr.subvec(0, bwidthAbs - 1) = ws.rows(0, bwidthAbs - 1) * y.subvec(0, bwidthMax - 1);
  gr.subvec(bwidthAbs, n - bwidthAbs - 1) = (ws.row(bwidthAbs) * ym).t();
  gr.subvec(n - bwidthAbs, n - 1) = ws.rows(bwidthAbs + 1, bwidthMax - 1) * y.subvec(n - bwidthMax, n - 1);
  
  arma::vec resid = y - gr;
  
  return resid;
  
}

// C++ version of deseats derivative smoothing
// [[Rcpp::export]]
arma::vec derivdeseatsCpp(const arma::vec& y, const int p,
  const int s, const int mu, const double b, const int bb, const int v) {

  const int n = y.size();
  arma::vec grG(n);
  const int bwidthAbs = std::trunc(n * b + 0.5);
  const int bwidthMax = 2 * bwidthAbs + 1;
  
  arma::mat wsG(bwidthMax, bwidthMax);
  arma::rowvec wk = arma::zeros<arma::rowvec>(bwidthMax);
  arma::mat xt = arma::zeros<arma::mat>((p + s), bwidthMax);
  
  arma::mat xw = xt;

  const arma::vec seqhh = seqCpp(0, bwidthAbs);
  const arma::vec allLowB = bwidthAbs - seqhh;
  const arma::vec hr = bwidthAbs + bb * allLowB;
  const arma::vec hr2 = arma::pow(hr + 1, 2.0);
  const arma::vec ht = seqhh + hr;
  arma::mat xa = xt;
  
  const arma::rowvec allsequ = rseqCpp(-bwidthAbs, hr(0));
  const int m = allsequ.size();
  const arma::rowvec allsequ2 = arma::pow(allsequ, 2.0);
  const arma::rowvec allsequhh = allsequ / bwidthAbs;
  
  arma::mat dataMat = arma::ones<arma::mat>((p + s), m);  
  dataMat.row(1) = allsequhh;
  
  for (int i = 2; i < p + 1; ++i) {
    dataMat.row(i) = arma::pow(allsequhh, i);
  }
  
  const double l1 = 2.0 * M_PI / s;  
  
  for (int i = 1; i < std::trunc((s - 1.0) / 2) + 1; ++i) {
    dataMat.row(p + 2 * i - 1) = arma::cos(i * l1 * allsequ); //look here
    dataMat.row(p + 2 * i) = arma::sin(i * l1 * allsequ);
  }
  
  if ((s / 2.0) == std::trunc(s / 2.0)) {
    dataMat.row(p + s - 1) = arma::cos(M_PI * allsequ);
  }  
  
  arma::rowvec ej = arma::zeros<arma::rowvec>(p + s);
  
  ej(v) = 1;
  
  const arma::vec allUpB = bwidthAbs + hr;

  int lowB;
  int upB;
  
  for (int i = 0; i < bwidthAbs + 1; ++i) {

    lowB = allLowB(i);
    upB = allUpB(i);
    
    if (mu == 0) {
      wk.subvec(0, ht(i)).fill(1.0);
    } else if (mu == 1) {
      wk.subvec(0, ht(i)) = 1.0 - allsequ2.subvec(lowB, upB) / hr2(i);
    } else {
      wk.subvec(0, ht(i)) = arma::pow(1.0 - allsequ2.subvec(lowB, upB) / hr2(i), mu);
    }
    
    xt.submat(0, 0, p + s - 1, ht(i)) = dataMat.cols(lowB, upB);
    xw.submat(0, 0, p + s - 1, bwidthMax - 1) = xt * arma::diagmat(wk);
    xa.submat(0, 0, p + s - 1, bwidthMax - 1) = arma::solve(xw * xt.t(), xw);

    wsG.row(i) = ej * xa;
    
  }
  
  wsG.rows(bwidthAbs + 1, bwidthMax - 1) = arma::flipud(arma::fliplr(wsG.submat(0, 0, bwidthAbs - 1, bwidthMax - 1))) * std::pow(-1.0, v);
  wsG = factorialCpp(v) * wsG * std::pow((n / double(bwidthAbs)), v);
  
  arma::mat ym(bwidthMax, n - bwidthMax + 1);
  for (int i = bwidthAbs; i < n - bwidthAbs; ++i) {
    ym.col(i - bwidthAbs) = y.subvec(i - bwidthAbs, i + bwidthAbs);
  }
  grG.subvec(0, bwidthAbs - 1) = wsG.rows(0, bwidthAbs - 1) * y.subvec(0, bwidthMax - 1);
  grG.subvec(bwidthAbs, n - bwidthAbs - 1) = (wsG.row(bwidthAbs) * ym).t();
  grG.subvec(n - bwidthAbs, n - 1) = wsG.rows(bwidthAbs + 1, bwidthMax - 1) * y.subvec(n - bwidthMax, n - 1);
  
  return grG;
  
}

// [[Rcpp::export]]
Rcpp::List algorithmCpp(const arma::vec& yt, const int p, const int s, const int mu, const double bStart, const int CF, const int errors, const double cb, const int bb, const int errm, const double expo, const int narmin, const int narmax, const int nmamin, const int nmamax, const int armamean) {
  
  double bopt = bStart;
  double boptOld = -100;
  double boptOld2 = -100;
  const int n = yt.size();
  const int k = p + 1;
  
  double adjFactor = 1.0;

  if (CF == 1) {  
  
    if (s == 4) {

      if (p == 1) {
        if (mu == 0) {
          adjFactor = 1.3195;
        } else if (mu == 1) {
          adjFactor = 1.4310;
        } else if (mu == 2) {
          adjFactor = 1.4541;
        } else if (mu == 3) {
          adjFactor = 1.4640;
        }
      } else if (p == 3) {
        if (mu == 0) {
          adjFactor = 1.2599;
        } else if (mu == 1) {
          adjFactor = 1.3077;
        } else if (mu == 2) {
          adjFactor = 1.3188;
        } else if (mu == 3) {
          adjFactor = 1.3239;
        }
      }

    } else if (s == 12) {

      if (p == 1) {
        if (mu == 0) {
          adjFactor = 1.3195;
        } else if (mu == 1) {
          adjFactor = 1.4310;
        } else if (mu == 2) {
          adjFactor = 1.4541;
        } else if (mu == 3) {
          adjFactor = 1.4640;
        }
      } else if (p == 3) {
        if (mu == 0) {
          adjFactor = 1.2599;
        } else if (mu == 1) {
          adjFactor = 1.3140;
        } else if (mu == 2) {
          adjFactor = 1.3257;
        } else if (mu == 3) {
          adjFactor = 1.3307;
        }
      }
    } else if (s == 7) {
      
      if (p == 1) {
        if (mu == 0) {
          adjFactor = 1.3195;
        } else if (mu == 1) {
          adjFactor = 1.4310;
        } else if (mu == 2) {
          adjFactor = 1.4541;
        } else if (mu == 3) {
          adjFactor = 1.4640;
        }
      } else if (p == 3) {
        if (mu == 0) {
          adjFactor = 1.2599;
        } else if (mu == 1) {
          adjFactor = 1.3116;
        } else if (mu == 2) {
          adjFactor = 1.3230;
        } else if (mu == 3) {
          adjFactor = 1.3281;
        }
      }
      
    } else if (s == 1) {
      
      if (p == 1) {
        if (mu == 0) {
          adjFactor = 1.3195;
        } else if (mu == 1) {
          adjFactor = 1.4310;
        } else if (mu == 2) {
          adjFactor = 1.4541;
        } else if (mu == 3) {
          adjFactor = 1.4640;
        }
      } else if (p == 3) {
        if (mu == 0) {
          adjFactor = 1.2599;
        } else if (mu == 1) {
          adjFactor = 1.2915;
        } else if (mu == 2) {
          adjFactor = 1.3006;
        } else if (mu == 3) {
          adjFactor = 1.3052;
        }
      }
      
    }
  }    
  
  double bmax = 0.49;       // The maximum bandwidth 
  double bmin = 0.01;       // The minimum bandwidth 

  const int v = k;
  const int pv = k + 1;

  const int n1 = std::trunc(n * cb);  
  
  const int m = 1000000;
  const arma::vec u = seqCpp(-m, m) / (m + 0.5);

  arma::vec wk = arma::pow((1 - arma::pow(u, 2.0)), mu);
  wk = wk / arma::accu(wk) * m;
  const double R2 = arma::accu(arma::pow(wk, 2.0)) / m;  
  
  arma::vec wkp;
  
  if (p == 1) {
    wkp = wk;
  } else if (p == 3) {
    if (mu == 0) {
      wkp = 3.0 / 8.0 * (3.0 - 5.0 * arma::pow(u, 2.0));
    } else if (mu == 1) {
      wkp = 15.0 / 32.0 * (3.0 - 10.0 * arma::pow(u, 2.0) + 7.0 * arma::pow(u, 4.0));      
    } else if (mu == 2) {
      wkp = 105.0 / 64.0 * (1.0 - 5.0 * arma::pow(u, 2.0) + 7.0 * arma::pow(u, 4.0) - 3.0 * arma::pow(u, 6.0));      
    } else if (mu == 3) {
      wkp = 315.0 / 512.0 * (3.0 - 20.0 * arma::pow(u, 2.0) + 42.0 * arma::pow(u, 4.0) - 36.0 * arma::pow(u, 6.0) + 11.0 * arma::pow(u, 8.0));      
    }
  }
  
  wkp = wkp / arma::accu(wkp) * m;
  
  const double Rp = arma::accu(arma::pow(wkp, 2.0)) / m;
  const double mukp = arma::accu(arma::pow(u, k) % wkp) / m;  
  
  const double c1 = std::pow(factorialCpp(k), 2.0) / (2 * k);
  const double c2 = (1.0 - 2.0 * cb) * (Rp + (s - 1.0) * R2) / std::pow(mukp, 2.0);
  
  const int itMax = 20;
  
  int it = 1;
  double bv;
  
  arma::vec gv;
  double I2;
  
  double bAdj;
  arma::vec resAdj;
  double cf0 = 0;
  double c3;
  arma::vec resAdj2;
  int stopreason = 0;
  const double diffvalue = 1.0 / n;
  arma::vec bwidths = arma::zeros<arma::vec>(itMax + 1);
  bwidths(0) = bopt;
  
  while (it < itMax + 1 && stopreason == 0) {
    
    bv = std::pow(bopt, expo);
    bv = std::max(bv, bmin);
    bv = std::min(bv, bmax);  
    
    gv = derivdeseatsCpp(yt, pv, s, mu, bv, bb, v);
  
    I2 = arma::accu(arma::pow(gv.subvec(n1, n - n1 - 1), 2.0)) / (n - 2 * n1);
    
    bAdj = bopt * adjFactor;
    bAdj = std::max(bAdj, bmin);
    bAdj = std::min(bAdj, bmax);     
    
    resAdj = residdeseatsCpp(yt, p, s, mu, bAdj, bb);
      
    if (errors == 1) {
      
      if (errm == 1) {
      
        cf0 = cf0Cpp(resAdj);
        
      } else if (errm == 0) {
        
        Rcpp::List arma = armaoptCpp(resAdj, narmin, narmax, nmamin, nmamax, armamean);
        cf0 = arma_sumacovCpp(arma);
        
      }
      
    } else if (errors == 0) {
      
      resAdj2 = arma::pow(resAdj, 2.0);
      cf0 = arma::mean(resAdj2);
      
    }
    
    c3 = cf0 / I2;
    
    boptOld2 = boptOld;
    boptOld = bopt;
    bopt = std::pow((c1 * c2 * c3), (1.0 / (2 * k + 1))) * std::pow(n, (-1.0 / (2 * k + 1)));
   
    bopt = std::max(bopt, bmin);
    bopt = std::min(bopt, bmax);   
   
    if (std::abs(bopt - boptOld) / bopt < diffvalue) {
      stopreason = 1;
    }
    
    if (std::abs(bopt - boptOld2) / bopt < diffvalue) {
      stopreason = 1;
      bopt = (bopt + boptOld) / 2.0;
    }  
    
    bwidths(it) = bopt;
    
    it += 1;
    
  }
  
  arma::vec bout = bwidths.subvec(0, it - 1);
  
  Rcpp::List out = Rcpp::List::create(_["bwidths"] = bout, _["Imk"] = I2, _["sum_autocov"] = cf0, _["RK"] = Rp, _["RW"] = R2, _["beta"] = mukp);
  
  return(out);

}


// =============================================================

//Weighted regression function
// [[Rcpp::export]]
arma::vec wls_Cpp(const arma::vec& x, const arma::vec& y, const arma::vec& wk) {
  const int n = y.size();
  arma::mat xt = arma::ones<arma::mat>(2, n);
  xt.row(1) = x.t();
  const arma::mat xw = xt * arma::diagmat(wk);
  const arma::vec coefs = arma::solve(xw * xt.t(), xw * y);
  return  coefs;
}

//Find subset indices
// [[Rcpp::export]]
arma::uvec find_sub_Cpp(const arma::vec& t, const double t0, const double interior_bwidth, const int seasonality) {
  
  arma::uvec subs;
  
  const arma::vec t_c = t - t0;
  
  if (seasonality == 0) {
    subs = arma::find(arma::abs(t_c) <= interior_bwidth);
  } else if (seasonality == 1) {
    subs = arma::find((arma::abs(t_c) <= interior_bwidth) && ((t_c - arma::trunc(t_c)) == 0));
  } 
  
  return subs;
  
}


// Codes for local linear regression in trend and seasonality
// [[Rcpp::export]]
arma::vec llin_calc_Cpp(const arma::vec& yt, const arma::vec& t, const double bwidth, const int P, const int mu, const int bb, const int seasonality) {
  
  const int n = yt.size();
  arma::vec trend_e = arma::zeros<arma::vec>(n);
  
  double t0 = 0;
  int j = 0;
  
  double interior_bwidth = 0.0;
  
  arma::uvec subs;
  arma::vec t_subset;
  arma::vec t_cent;
  arma::vec yt_subset;
  arma::vec u;
  arma::vec Ku;
  
  arma::vec coefs;
  
  for (int i = 0; i < n; ++i) {
    
    t0 = t(i);
    if (i + 1 <= bwidth * P) {
      j = i + 1;
    } else if (i + 1 >= n - bwidth * P + 1) {
      j = n - (i + 1) + 1;
    }
    if ((i + 1 <= bwidth * P) || (i + 1 >= n - bwidth * P + 1)) {
      interior_bwidth = (bb + 1) * bwidth - bb * (j - 1) * (1.0 / P);
    } else {
      interior_bwidth = bwidth;
    }

    subs = find_sub_Cpp(t, t0, interior_bwidth, seasonality);

    t_subset = t.elem(subs);

    t_cent = t_subset - t0;
    yt_subset = yt.elem(subs);
    
    
    
    u = t_cent / interior_bwidth;
    Ku = arma::pow(arma::pow(1 - u, 2.0), mu);
    
    coefs = wls_Cpp(t_cent, yt_subset, Ku);
    
    trend_e(i) = coefs(0);
    
  }  
  
  return trend_e;
  
}
