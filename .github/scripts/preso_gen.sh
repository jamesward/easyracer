#!/bin/sh

mkdir -p _site

ls -al _site

cd .presos

for md_file in *_*.md; do
  html_file="../_site/${md_file%.md}.html"
  if [ ! -f "$html_file" ] || [ "$md_file" -nt "$html_file" ]; then
    echo "Creating html for: ${md_file}"
    npx @marp-team/marp-cli --engine ./engine.mjs --output=${html_file} ${md_file}

    # todo: create pdfs
    # currently hanging
    # npx @marp-team/marp-cli --engine ./engine.mjs --pdf *_*.md
  fi
done

cp -r img ../_site/

cd ..
