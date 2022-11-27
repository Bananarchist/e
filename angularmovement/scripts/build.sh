#!/bin/sh

# declarations
output_dir="dist"
entry="src/Main.elm"
js_out="$output_dir/main.js"
debug=false
dist=false

for arg in $@; do
	if [[ $arg =~ "debug" ]]; then
		debug=true
	elif [[ $arg =~ "dist" ]]; then
		dist=true
	else
		echo "Unexpected argument $arg"
	fi
done


# compile elm code
if $debug; then
	echo "Building for debug"
	elm make $entry --output=$output_dir/main.js --debug
else
	echo "Building for prod"
	elm make $entry --output=$js_out
fi

# copy resources to dist
# markup and design
cp src/index.html $output_dir/index.html
cp src/gamepad.js $output_dir/gamepad.js
cp src/audio.js $output_dir/audio.js

if $dist; then
	echo "Compressing client for distribution"
	zip -r client.zip $output_dir
fi
