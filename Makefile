.PHONY: snippets

sprawko/sprawozdanie.pdf: sprawko/sprawozdanie.tex snippets
	cd sprawko; pdflatex -shell-escape sprawozdanie.tex
	cd sprawko; pdflatex -shell-escape sprawozdanie.tex

snippets:
	-rm -r sprawko/snippets
	mkdir sprawko/snippets
	./snippets '//' sprawko/snippets/  Program.fsx
# end
