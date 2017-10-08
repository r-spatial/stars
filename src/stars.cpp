#include "cpl_port.h"
#include "gdal.h"

#include "Rcpp.h"

#include "stars.h"

// from sf:gdal.cpp
// Rcpp::CharacterVector -> NULL-terminated array of strings
std::vector<char *> create_options(Rcpp::CharacterVector options, bool prnt) {
    std::vector<char *> ret;
    for (int i = 0; i < options.size(); i++) {
        ret.push_back((char *) (options[i]));
		if (prnt)
			Rcpp::Rcout << options[i] << std::endl;
	}
    ret.push_back((char *) NULL);
    return ret;
}

Rcpp::CharacterVector charpp2CV(char **cp) {
// NULL-terminated array of strings -> Rcpp::CharacterVector
	int n = 0;
	while (cp && cp[n] != NULL)
		n++; // count
	Rcpp::CharacterVector ret(n);
	for (int i = 0; i < n; i++)
		ret(i) = cp[i];
	return ret;
}
