build_dir=build
dest=$(build_dir)/main.js
sass_root=sass
css_dest=$(build_dir)/stylesheet.css

.PHONY: watch build build-sass watch-sass

watch:
	find src -name "*.elm" | grep -v "#" | entr make build

build:
	mkdir -p $(build_dir)
	elm-make src/Main.elm --output $(dest)

build-sass:
	find $(sass_root) -name '*.scss' | sort | awk '{print "@import \"" $$0 "\";"}' | SASS_PATH='.' sass --scss --stdin > $(css_dest)



watch-sass:
	find $(sass_root) -name '*.scss' | grep -v '#' | entr make build-sass
