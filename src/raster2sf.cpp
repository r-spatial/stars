#include "Rcpp.h"

using namespace Rcpp;

// [[Rcpp::export]]
List CPL_xy2sfc(NumericMatrix cc, NumericVector dim, bool to_points) {
	NumericVector bb(4);
	bb(0) = cc(0, 0);
	bb(3) = cc(0, 1);
	bb(1) = cc(cc.nrow() - 1, 1);
	bb(2) = cc(cc.nrow() - 1, 0);
	bb.names() = CharacterVector::create("xmin", "ymin", "xmax", "ymax");
	bb.attr("class") = "bbox";
	if (to_points) {
		List ret(cc.nrow());
		CharacterVector cls = CharacterVector::create("XY", "POINT", "sfg");
		NumericVector pt;
		for (size_t i; i < cc.nrow(); i++) {
			pt = cc(i,_);
			pt.attr("class") = cls; // surprisingly, this needs to be inside the for loop
			ret(i) = clone(pt);
		}
		ret.attr("class") = CharacterVector::create("sfc_POINT", "sfc");
		ret.attr("precision") = NumericVector::create(0.0);
		ret.attr("bbox") = bb;
		return(ret);
	} else {
		NumericMatrix points(5, 2);
		List polygon(1);
		polygon.attr("class") = CharacterVector::create("XY", "POLYGON", "sfg");
		List ret((dim[0] - 1) * (dim[1] - 1));
		for (size_t y = 0; y < dim[1] - 1; y++) { // rows
			for (size_t x = 0; x < dim[0] - 1; x++) { // cols
				points(0,_) = cc(y * dim[0] + x,           _);
				points(1,_) = cc(y * dim[0] + x + 1,       _);
				points(2,_) = cc((y + 1) * dim[0] + x + 1, _);
				points(3,_) = cc((y + 1) * dim[0] + x,     _);
				points(4,_) = cc(y * dim[0] + x,           _);
				polygon(0) = points;
				ret( y * (dim[0] - 1) + x ) = clone(polygon);
			}
		}
		ret.attr("class") = CharacterVector::create("sfc_POLYGON", "sfc");
		ret.attr("precision") = NumericVector::create(0.0);
		ret.attr("bbox") = bb;
		return(ret);
	}
}
