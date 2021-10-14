all: paper.html paper.pdf

paper.html: paper.Rmd fig1/fig.png fig2/fig.png
	Rscript -e "rmarkdown::render('paper.Rmd')"

paper.pdf: paper.Rmd fig1/fig.png fig2/fig.png
	Rscript -e "rmarkdown::render('paper.Rmd', output_format='pdf_document')"

fig1/fig.png: fig1/fig.R
	cd fig1;Rscript fig.R

fig2/fig.png: fig2/fig.R
	cd fig2;Rscript fig.R

.PHONY: all
