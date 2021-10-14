all: hw4.html

hw4.html: hw4.Rmd data/lq.rds data/dennys.rds
	Rscript -e "rmarkdown::render('hw4.Rmd')"
	
data/lq.rds: parse_lq.R data/lq/*.html
	Rscript parse_lq.R

data/lq/*.html: get_lq.R
	Rscript get_lq.R

data/dennys.rds: parse_dennys.R data/dennys/*.html
	Rscript parse_dennys.R

data/dennys/*.html: get_dennys.R
	Rscript get_dennys.R
	
clean:
	rm -f data/lq.rds
	rm -f data/dennys.rds
	rm -rf data/lq/
	rm -rf data/dennys/
	rm -f hw4.html
	
.PHONY: all clean
