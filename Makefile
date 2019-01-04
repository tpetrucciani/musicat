elm:
	elm make src/Main.elm --output=_site/main.js

style:
	sass --sourcemap=none style.scss _site/style.css

.PHONY: elm style
