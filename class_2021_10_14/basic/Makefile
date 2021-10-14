paper.html: paper.Rmd fig1/fig.png fig2/fig.png
	Rscript -e "rmarkdown::render('paper.Rmd')"

fig1/fig1.png: fig1/fig1.R
	cd fig1;Rscript fig.R

fig2/fig2.png: fig2/fig2.R
	cd fig2;Rscript fig.R
