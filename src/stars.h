std::vector<char *> create_options(Rcpp::CharacterVector options, bool prnt = false);
Rcpp::CharacterVector charpp2CV(char **cp);
Rcpp::CharacterVector get_meta_data(GDALDatasetH ds, Rcpp::CharacterVector domain_item);
