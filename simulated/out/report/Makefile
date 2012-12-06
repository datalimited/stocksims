knitr: report.Rmd
	Rscript -e "library(knitr); knit('report.Rmd')"

pdf: knitr
	pandoc report.md -o report.pdf
