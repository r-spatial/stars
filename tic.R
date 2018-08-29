add_package_checks()

get_stage("install") %>%
  add_step(step_install_cran("lwgeom", configure.args="--without-liblwgeom", lib = "/home/travis/R/Library/tic-lib")) %>%  # install tic into custom R library used for R CMD check only
  add_step(step_install_cran("knitr")) %>%
  add_step(step_install_cran("rmarkdown")) %>%
  add_step(step_install_cran("abind"))

###
# deploy pkgdowm site
###
if (Sys.getenv("id_rsa") != "") {

  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

  get_stage("deploy") %>%
    add_step(step_build_pkgdown()) %>%
    add_step(step_push_deploy(path = "docs", branch = "gh-pages"))
}
