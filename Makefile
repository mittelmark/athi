VERSION := $(shell grep -E '^Version' DESCRIPTION | sed 's/Version: //')
PKG     := $(shell basename `pwd`)
build: man/athi.Rd
	R CMD build .

check: build man/athi.Rd
	R CMD check $(PKG)_$(VERSION).tar.gz

man/%.Rd: R/%.R
	Rscript bin/rman.R $<
