# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks()

get_stage("before_install") %>%
	add_step(step_install_cran("starsdata", repos = "http://gis-bigdata.uni-muenster.de/pebesma"))

if (ci_on_ghactions() && ci_has_env("BUILD_PKGDOWN")) {
	# creates pkgdown site and pushes to gh-pages branch
	# only for the runner with the "BUILD_PKGDOWN" env var set
	do_pkgdown()
}
