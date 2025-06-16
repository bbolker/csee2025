MCCOY_DATA=McCoy_response_surfaces_Gamboa.csv

talk: shapeconst_talk.rmd redeye_odo.Rout reedfrog.Rout
	Rscript -e "rmarkdown::render('shapeconst_talk.rmd')"

redeye_odo.Rout: odo_semimech.Rout redeye_odo.R

odo_semimech.Rout: odo_semimech.R funs.R ${MCCOY_DATA}

%.Rout: %.R
	R CMD BATCH --vanilla $<

%.html: %.rmd
	Rscript -e "rmarkdown::render('$<')"

clean:
	rm -f *~ \#*

distclean:
	rm *.Rout
