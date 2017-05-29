// [[Rcpp::depends(sf)]]

#include "sf.h"

// [[Rcpp::export]]
Rcpp::NumericVector CPL_add_one(Rcpp::NumericVector inv) {
	return sf::add_one(inv);
}
