all: roadmap.pdf

roadmap.pdf: connexiveFinal.tex
	latexmk -xelatex -silent $<

# roadmap.tex: roadmap.lagda.tex
# 	agda --latex-dir=. --latex $<

.PHONY: clean
clean:
	rm -f connexiveFinal.tex *.out *.pdf *.xdv *.ptb *.fdb_latexmk *.fls *.aux *.blg *.log *.agdai *.bbl
