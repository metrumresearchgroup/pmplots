SHELL := /bin/bash
PACKAGE=pmplots
VERSION=$(shell grep Version DESCRIPTION |awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.
CHKDIR=.

covr:
		Rscript inst/script/covr.R

everyfun:
	Rscript inst/script/make_pmplots_complete.R

readme:
	Rscript -e 'library(rmarkdown)' -e 'render("README.Rmd")'

examples:
	make everyfun
	make readme
	make exampler

pkgdown:
	Rscript -e "options(pkdown.internet = FALSE); pkgdown::build_site()"

ec:
	echo ${VERSION}

all:
	make doc
	make build
	make install

travis:
	make build
	R CMD check --no-manual ${TARBALL} -o ${CHKDIR}

test:
	make install
	Rscript -e 'testthat:::test_dir("tests")'

.PHONY: doc
doc:
	Rscript -e 'library(devtools); document()'

build:
	R CMD build --no-build-vignettes --md5 $(PKGDIR)

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

testing:
	make doc
	make build
	cp ${TARBALL} ../../qualification/pmplots_qualification/testing/${TARBALL}
	cd ../../qualification/pmplots_qualification/testing/ && git commit -am "testing update" && git push

