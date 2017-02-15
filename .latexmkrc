### platex
# $latex  = 'platex -file-line-error -halt-on-error -interaction=nonstopmode';
# $bibtex = 'pbibtex';
# $dvipdf = "dvipdfmx %O -o %D %S";
# $dvips  = 'pdvips';
# $pdf_mode = 3;  # generates pdf via dvipdfmx

### pdflatex
# $pdflatex = 'pdflatex -etex -halt-on-error %O %S';
# $pdf_mode = 1;

### lualatex
$pdflatex = 'lualatex -file-line-error -halt-on-error -interaction=nonstopmode %O %S';
$bibtex = 'pbibtex %O %B';
$pdf_mode = 1;

$max_repeat = 5;
