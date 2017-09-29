#include "Rcpp.h"

using namespace Rcpp;

// [[Rcpp::export]]
List CPL_xy2sfc(NumericMatrix cc, NumericVector dim, bool to_points) {
	NumericVector bb(4);
	bb.names() = CharacterVector::create("xmin", "ymin", "xmax", "ymax");
	bb.attr("class") = "bbox";
	if (to_points) {
		List ret(cc.nrow());
		// unlist(apply(cc, 1, function(x) list(sf::st_point(x))), recursive = FALSE)
		CharacterVector cls(3);
		cls[0] = "XY";
		cls[1] = "POINT";
		cls[2] = "sfg";
		NumericVector pt;
		for (size_t i; i < cc.nrow(); i++) {
			pt = cc(i,_);
			pt.attr("class") = cls;
			ret(i) = clone(pt);
		}
		bb(0) = cc(0, 0);
		bb(3) = cc(0, 1);
		bb(1) = cc(cc.nrow() - 1, 1);
		bb(2) = cc(cc.nrow() - 1, 0);
		ret.attr("class") = CharacterVector::create("sfc_POINT", "sfc");
		ret.attr("precision") = NumericVector::create(0.0);
		ret.attr("bbox") = bb;
		return(ret);
	} else {
		CharacterVector cls(3);
		cls[0] = "XY";
		cls[1] = "POLYGON";
		cls[2] = "sfg";
		NumericMatrix pol(5, 2);
		List lst(1);
		lst.attr("class") = cls;
		List ret((dim[0] - 1) * (dim[1] - 1));
		for (size_t y = 0; y < dim[1] - 1; y++) { // rows
			for (size_t x = 0; x < dim[0] - 1; x++) { // cols
				pol(0,_) = cc(y * dim[0] + x,           _);
				pol(1,_) = cc(y * dim[0] + x + 1,       _);
				pol(2,_) = cc((y + 1) * dim[0] + x + 1, _);
				pol(3,_) = cc((y + 1) * dim[0] + x,     _);
				pol(4,_) = cc(y * dim[0] + x,           _);
				lst(0) = pol;
				ret( y * (dim[0] - 1) + x ) = clone(lst);
			}
		}
		bb(0) = cc(0, 0);
		bb(3) = cc(0, 1);
		bb(1) = cc(cc.nrow() - 1, 1);
		bb(2) = cc(cc.nrow() - 1, 0);
		ret.attr("class") = CharacterVector::create("sfc_POLYGON", "sfc");
		ret.attr("precision") = NumericVector::create(0.0);
		ret.attr("bbox") = bb;
		return(ret);
	}
}
