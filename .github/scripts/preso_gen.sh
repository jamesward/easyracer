#!/bin/sh

mkdir -p _site

#ls -al .presos
#
#ls -al _site

cd .presos

for md_file in *_*.md; do
  html_file="../_site/${md_file%.md}.html"
  md_hashfile="../_site/${md_file%.md}.hash"
  current_hash=$(sha256sum "$md_file" 2>/dev/null | cut -d' ' -f1 || shasum -a 256 "$md_file" 2>/dev/null | cut -d' ' -f1)
  stored_hash=""
  if [ -f "$md_hashfile" ]; then
    stored_hash=$(cat "$md_hashfile")
  fi

  if [ ! -f "$html_file" ] || [ "$current_hash" != "$stored_hash" ]; then
    echo "Creating html for: ${md_file}"
    npx @marp-team/marp-cli --engine ./engine.mjs --output=${html_file} ${md_file}

    echo "$current_hash" > "$md_hashfile"

    # todo: create pdfs
    # currently hanging
    # npx @marp-team/marp-cli --engine ./engine.mjs --pdf *_*.md
  else
      echo "Skipping ${md_file} - content unchanged"
  fi
done

cp -r img ../_site/

cd ..
