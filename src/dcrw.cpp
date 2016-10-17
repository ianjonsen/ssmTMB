#include <TMB.hpp>

using namespace density;

template<class Type>
Type objective_function<Type>::operator() () {
  DATA_MATRIX(y);	       // (lon, lat) observations
  DATA_VECTOR(idx);      // Interpolation indices
  DATA_VECTOR(w);        // Interpolation weights
  DATA_MATRIX(K);	       // Error weighting
  DATA_SCALAR(nu);      // Error scaling

  PARAMETER(theta);          // Rotation angle
  PARAMETER(l_gamma);        // Autocorrelation parameter (link scale)
  PARAMETER_VECTOR(l_sigma); // Innovation variance (link scale)
  PARAMETER(l_rho);          // Innovation correlation (link scale)
  PARAMETER_VECTOR(l_tau);   // Error dispersion (link scale)
  PARAMETER_MATRIX(x);       // Predicted locations

  // Backtransform parameters from link scale
  Type gamma = Type(1.0) / (Type(1.0) + exp(-l_gamma));
  vector<Type> sigma = exp(l_sigma);
  Type rho = Type(2.0) / (Type(1.0) + exp(-l_rho)) - Type(1.0);

  vector<Type> tau(2);
  tau(0) = exp(l_tau(0));
  tau(1) = exp(l_tau(1));

  matrix<Type> yhat(y.rows(), 2);       // Interpolations
  matrix<Type> d(x.rows() - 1, 2);      // Increments
  matrix<Type> e(x.rows() - 2, 2);      // Innovations

  // 2x2 scaled rotation matrix
  matrix<Type> gT(2, 2);
  gT(0, 0) =  gamma * cos(theta);
  gT(0, 1) = -gamma * sin(theta);
  gT(1, 0) =  gamma * sin(theta);
  gT(1, 1) =  gamma * cos(theta);

  // 2 x 2 covariance matrix for innovations
  matrix<Type> cov(2, 2);
  cov(0, 0) = sigma(0) * sigma(0);
  cov(0, 1) = rho * sigma(0) * sigma(1);
  cov(1, 0) = cov(0, 1);
  cov(1, 1) = sigma(1) * sigma(1);

  Type nll = 0.0;               // (Complete data) negative log likelihood
  MVNORM_t<Type> nll_dens(cov);	// Multivariate Normal density

  // Compute the increments
  for(int i = 0; i < d.rows(); ++i)
    d.row(i) = x.row(i + 1) - x.row(i);

  // Contribution to the likelihood assuming d(-1) = 0.
  nll += nll_dens(d.row(0));

  // Compute the innovations
  for(int i = 0; i < e.rows(); ++i) {
    e.row(i) = d.row(i + 1) - d.row(i) * gT;
    nll += nll_dens(e.row(i));
  }

  // Contribution to the likelihood from the interpolations
  for(int i=0; i < y.rows(); ++i) {
    int k = CppAD::Integer(idx(i));
    yhat.row(i) = w(i) * x.row(k) + (1.0 - w(i)) * x.row(k + 1);
    for(int j = 0; j < 2; ++j) {
      Type s = tau(j) * K(i, j);
      nll -= log(dt((y(i, j) - yhat(i, j)) / s, nu, false) / s);
    }
  }

  ADREPORT(theta);
  ADREPORT(gamma);
  ADREPORT(sigma);
  ADREPORT(rho);
  ADREPORT(tau);

  return nll;
}
