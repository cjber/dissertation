#!/bin/bash

cd ./scripts/
Rscript run_all.r
cd ..
rm -r ./data/cache/
rm -r ./figure/

Rscript -e "knitr::knit2pdf('main.rnw')"
zathura main.pdf
