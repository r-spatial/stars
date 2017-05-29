all:
	pdflatex proposal

stars.tex:	PROPOSAL.md
	pandoc -t latex PROPOSAL.md  > stars.tex
