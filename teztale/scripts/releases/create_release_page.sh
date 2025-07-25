#!/usr/bin/env bash

set -e

if [ -z "${S3_BUCKET:-}" ]; then
  echo "S3_BUCKET variable is not set, impossible to create a release page."
  exit 1
fi

# If [URL] is not defined, we use the [S3_BUCKET] address.
URL="${URL:-${S3_BUCKET}}"

versions_list_filename="${1:-}"

if [ -z "${versions_list_filename}" ]; then
  echo "$0 takes a Versions list as argument."
  exit 1
fi

# [versions] is a 2D array representation of the [$versions_list_filename] JSON:
# - one line per release or release candidate
# - each line has three elements: "[major].[minor] [rc] [latest]"
#   - [rc] is "null" if it is not a release candidate
#   - [latest] is "null" if it is not the latest release
# shellcheck disable=SC2162
mapfile -t versions < <(jq -r '[.[] | "\(.major).\(.minor) \(.rc // "null") \(.latest // "null")"] | reverse | .[]' "${versions_list_filename}")

# Define the content of the release page.
# We iterate on the [$versions] array, distinguishing between three cases:
# - release candidate
# - release
# - latest release
while read -r version rc latest; do
  if [[ ${rc} != null ]]; then
    echo "# Teztale Release Candidate ${version}~rc${rc}" >> index.md
    version="${version}-rc${rc}"
  else
    if [[ ${latest} != null ]]; then
      echo "# Teztale $version (latest)" >> index.md
    else
      echo "# Teztale $version" >> index.md
    fi
  fi

  echo "## Binaries" >> index.md

  for arch in x86_64 arm64; do
    echo "### $arch" >> index.md

    aws s3 cp "s3://${S3_BUCKET}/teztale/teztale-v${version}/binaries/${arch}/sha256sums.txt" "./sha256sums.txt"

    for binary in $(aws s3 ls "s3://${S3_BUCKET}/teztale/teztale-v${version}/binaries/${arch}" --recursive | awk '{print $NF}'); do
      binary_name=$(basename "$binary")
      # Write sha256sum only if it's an actual executable (and not a checksums file)
      if [[ "$binary_name" != "sha256sums.txt" ]]; then
        checksum=$(grep " ${binary_name}$" sha256sums.txt | awk '{print $1}')
        echo "- [${binary_name}](https://${URL}/${binary}) <span class=\"sha256\">(**sha256:** \`$checksum\`)</span>" >> index.md
      else
        echo "- [${binary_name}](https://${URL}/${binary})" >> index.md
      fi
    done
    echo -e "\n" >> index.md
  done

done <<< "$(printf "%s\n" "${versions[@]}")"

echo "Generating html file."
pandoc index.md -s --template="./docs/release_page/template.html" --metadata title="Octez Releases" --css=../style.css -o index.html
