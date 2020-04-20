{
	if (NR == 4) {
		print("output: rmarkdown::html_vignette")
	} else if (NR > 4 && NR <= 10) {
	} else if (NR == 16) {
		print("\n**For a better version of the stars vignettes see** https://r-spatial.github.io/stars/articles/\n")
	} else if (NR == 19) {
		print
		print("knitr::opts_chunk$set(fig.height = 4.5)")
		print("knitr::opts_chunk$set(fig.width = 6)")
	} else
		print
}
