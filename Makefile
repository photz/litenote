build_dir=build
dest=$(build_dir)/main.js
sass_root=sass
css_dest=$(build_dir)/stylesheet.css
optimized_dest=$(build_dir)/app.js
closure_path=closure.jar

.PHONY: watch build build-sass watch-sass closure-build

# compiling Elm

watch:
	find src -name "*.elm" \
	| grep -v "#" \
	| entr make build

build:
	mkdir -p $(build_dir)
	elm-make src/Main.elm --output $(dest)

closure-build:
	java -jar $(closure_path) \
	--js $(dest) \
	--js_output_file $(optimized_dest) \
	--compilation_level SIMPLE

# compiling Sass

build-sass:
	find $(sass_root) -name '*.scss' \
	| sort \
	| awk '{print "@import \"" $$0 "\";"}' \
	| SASS_PATH='.' sass --scss --stdin > $(css_dest)

watch-sass:
	find $(sass_root) -name '*.scss' \
	| grep -v '#' \
	| entr make build-sass


