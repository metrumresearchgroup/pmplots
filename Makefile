SHELL := /bin/bash
PACKAGE=pmplots
VERSION=$(shell grep Version DESCRIPTION |awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.
CHKDIR=.

everyfun:
	Rscript -e 'rmarkdown::render("inst/examples/everyfunction.Rmd")'

readme:
	Rscript -e 'library(rmarkdown)' -e 'render("README.R")'

exampler:
	Rscript -e 'rmarkdown::render("inst/examples/example1.R")'

ec:
	echo ${VERSION}

all:
	make doc
	make build
	make install

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
	R CMD CHECK ${TARBALL} -o ${CHKDIR}

checkk:
	make doc
	make build
	R CMD CHECK ${TARBALL} -o ${CHKDIR} --no-examples


