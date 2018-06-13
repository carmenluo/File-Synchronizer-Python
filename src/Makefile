all: index.html ; 

#index.html: Main.elm Calendar.elm CircleOfSix.elm GraphicSVG.elm
#	elm-make ./Main.elm
index.html: Journal.elm Main.elm Calendar.elm CircleOfSix.elm GraphicSVG.elm
	elm-make ./Journal.elm

run: index.html
	xdg-open ./index.html

clean:
	rm -r index.html elm-stuff
