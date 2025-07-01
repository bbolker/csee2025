MCCOY_DATA=McCoy_response_surfaces_Gamboa.csv

talk: shapeconst_talk.rmd redeye_odo.Rout reedfrog.Rout
	Rscript -e "rmarkdown::render('shapeconst_talk.rmd')"

upload: shapeconst_talk.html
	scp shapeconst_talk.html ms.mcmaster.ca:~/public_html/misc/csee2025_shapeconst.html

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
