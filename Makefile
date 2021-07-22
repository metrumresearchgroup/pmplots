SHELL := /bin/bash
PACKAGE=pmplots
VERSION=$(shell grep Version DESCRIPTION |awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.
CHKDIR=.

drone:
	R CMD build --no-build-vignettes --md5
	R CMD check ${TARBALL}

covr:
		Rscript inst/script/covr.R

everyfun:
	Rscript  -e 'rmarkdown::render("inst/examples/pmplots_complete.Rmd")'

examples:
	make everyfun
	make readme
	make exampler

pkgdown:
	Rscript -e "options(pkdown.internet = FALSE); pkgdown::build_site()"

all:
	make doc
	make build
	make install

spelling:
	Rscript -e "spelling::spell_check_package()"

bump-dev:
	Rscript -e 'usethis::use_version("dev")'

tag-version:
	git tag $(VERSION)
	git push origin $(VERSION)

test:
	make install
	Rscript -e 'testthat:::test_dir("tests")'

.PHONY: doc
doc:
	Rscript inst/script/document.R

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

