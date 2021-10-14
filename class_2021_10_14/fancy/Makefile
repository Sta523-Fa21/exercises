all: paper.html paper.pdf

paper.pdf: paper.Rmd fig1/fig.png fig2/fig.png
	Rscript -e "rmarkdown::render('paper.Rmd', output_format = 'pdf_document')"

paper.html: paper.Rmd fig1/fig.png fig2/fig.png
	Rscript -e "rmarkdown::render('paper.Rmd')"

fig%/fig.png: fig%/fig.R
	cd $(<D);Rscript $(<F)

clean: 
	rm -f paper.html
	rm -f paper.pdf

.PHONY: all clean
