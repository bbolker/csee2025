Argh, have to convert HTML to PPT

1. Print HTML to PDF: https://stackoverflow.com/a/28096322/190277

```
chromium --headless --allow-pre-commit-input --print-to-pdf=output.pdf shapeconst_talk.html
```
(maybe do this on the command line? get "Processing math" message at bottom of very slide!)

https://peter.sh/experiments/chromium-command-line-switches/


2. Convert PDF to PNG:

```
DENSITY=300 ## 
convert -density $DENSITY output.pdf output-%d.png
```

https://www.imagemagick.org/discourse-server/viewtopic.php?t=36498

https://usage.imagemagick.org/draw/#coordinates
https://stackoverflow.com/a/76361737/190277

convert original.png -fill red -draw "$(convert original.png -format 'rectangle 10,20 %[fx:w-30],40' info:)" result.png


```
for N in {0..18}; do
   ## try to match gradient at bottom of screen ...
   convert output-${N}.png \
     -fill gray92 -draw "$(convert output-${N}.png -format 'rectangle 0,%[fx:h] 800,%[fx:h-200]' info:)"  \
   output-${N}-proc.png
   mv output-${N}-proc.png output-${N}.png
done
```

Non-working machinery from link:

```
## -alpha off \
## -fill none -draw "matte '$(convert output-${N}.png -format 0,%[fx:h] info:)' floodfill" \
``` 
3. Open office Impress

* new document
* Insert > Media > Photo Album
   * add images (make sure file browser has them sorted in the right order, e.g. sort by title rather than date created)
   * delete first (empty) slide

