#!/bin/bash

DAY=$1

if [ -z "$DAY" ]; then
  echo "supply folder name"
  exit 1
fi
if [ -d "./$DAY" ]; then
  echo "exists!! abort"
  exit 1
fi

mkdir -p $DAY

cat <<EOF > $DAY/day$DAY.ml
open Containers

let () =
  let data = Aoc.read_lines "input.txt" |> List.map String.trim in
  print_endline (String.concat ";" data);
EOF

cat <<EOF > $DAY/dune
(executable
 (name day$DAY)
 (libraries containers aoc))
EOF

touch $DAY/input.txt

echo "DONE"
