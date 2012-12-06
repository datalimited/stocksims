FILE=report

all: knitr pdf

knitr: report.Rmd
	Rscript -e "library(knitr); knit('$(FILE).Rmd')"

pdf: knitr
	pandoc -o $(FILE).pdf $(FILE).md


clean all:
	shopt -s extglob
	rm !($(FILE).Rmd|Makefile)
