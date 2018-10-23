SHELL := /bin/bash
PACKAGE=pmplots
VERSION=$(shell grep Version DESCRIPTION |awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.
CHKDIR=.

everyfun:
	Rscript -e 'rmarkdown::render("inst/examples/everyfunction.Rmd")'

readme:
	Rscript -e 'library(rmarkdown)' -e 'render("README.Rmd")'

exampler:
	Rscript -e 'rmarkdown::render("inst/examples/example1.R")'

examples:
	make everyfun
	make readme
	make exampler

pkgdown:
	Rscript -e "pkgdown::build_site()"

ec:
	echo ${VERSION}

all:
	make doc
	make build
	make install

travis:
	make build
	R CMD check ${TARBALL} -o ${CHKDIR}

test:
	make install
	Rscript -e 'testthat:::test_dir("tests")'

.PHONY: doc
doc:
	Rscript -e 'library(devtools); document()'

build:
	R CMD build --md5 $(PKGDIR)


install:
	R CMD INSTALL --install-tests ${TARBALL}

install-build:
	R CMD INSTALL --build --install-tests ${TARBALL}

check:
	make doc
	make build
	R CMD check ${TARBALL} -o ${CHKDIR}

checkk:
	make doc
	make build
	R CMD check ${TARBALL} -o ${CHKDIR} --no-examples


