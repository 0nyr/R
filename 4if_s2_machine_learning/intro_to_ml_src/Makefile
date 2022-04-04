#!/usr/bin/env make

%.pdf: %.Rmd
	@R --quiet --file=make.R --args $<
