SHELL := /bin/bash
PACKAGE=pmplots
VERSION=$(shell grep Version DESCRIPTION |awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.
CHKDIR=.

testn:
	Rscript inst/validation/latest.R

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

package:
	make doc
	make build-vignettes
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

build-vignettes:
	R CMD build --md5 $(PKGDIR)

install:
	R CMD INSTALL --install-tests ${TARBALL}

install-build:
	R CMD INSTALL --build --install-tests ${TARBALL}

check:
	make doc
	make build
	R CMD check  --ignore-vignettes ${TARBALL} -o ${CHKDIR}

check-package:
	make doc
	make build-vignettes
	R CMD check ${TARBALL} -o ${CHKDIR}

readme:
	Rscript -e 'rmarkdown::render("README.Rmd")'
