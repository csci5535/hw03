.PHONY: all clean cleanall

all: hw03-student.pdf

%.pdf: %.tex
	latexmk -pdf $<

clean:
	latexmk -c
cleanall:
	latexmk -C
