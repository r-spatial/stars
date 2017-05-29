#include "Rcpp.h"

#include "stars.h"

// from sf:gdal.cpp
// Rcpp::CharacterVector -> NULL-terminated array of strings
std::vector<char *> create_options(Rcpp::CharacterVector options, bool prnt) {
    std::vector<char *> ret(options.size() + 1);
    for (int i = 0; i < options.size(); i++) {
        ret[i] = (char *) (options[i]);
		if (prnt)
			Rcpp::Rcout << options[i] << std::endl;
	}
    ret[options.size()] = NULL;
    return ret;
}

Rcpp::CharacterVector charpp2CV(char **cp) {
// NULL-terminated array of strings -> Rcpp::CharacterVector
	int n = 0;
	while (cp[n] != NULL)
		n++; // count
	Rcpp::CharacterVector ret(n);
	for (int i; i < n; i++)
		ret(i) = cp[i];
	return ret;
}
