VERSION := 0.3.0
PKG     := $(shell basename `pwd`)
build:
	R CMD build .

check: build man/athi.Rd
	R CMD check $(PKG)_$(VERSION).tar.gz

man/%.Rd: R/%.R
	Rscript bin/rman.R $<
