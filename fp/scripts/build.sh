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

# create output directories
mkdir -p $output_dir

# compile elm code
if $debug; then
	echo "Building for debug"
	elm make $entry --output=$output_dir/main.js --debug
else
	echo "Building for prod"
	elm make $entry --output=$js_out
fi

# copy resources to dist
cp src/index.html $output_dir
cp src/index.css $output_dir

if $dist; then
	echo "Compressing client for distribution"
	zip -r client.zip $output_dir
fi
