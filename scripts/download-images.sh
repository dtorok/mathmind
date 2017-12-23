#!/bin/bash

FOLDER=resources/images

mkdir -p $FOLDER

images=`grep 'initImage "' src/exercise-puzzle.elm | sed '1,$s/.*initImage *//' | sed '1,$s/"//g' | awk '{print $1";"$2";"$3";"$4}'`
for image in $images; do
  filename=`echo $image | cut -d';' -f1`
  url=`echo $image | cut -d';' -f2`
  width=`echo $image | cut -d';' -f3`
  height=`echo $image | cut -d';' -f4`

  filepath=$FOLDER/$filename

  if [ ! -e $filepath ]; then
    echo "Downloading $url..."
    curl $url -o $filepath
  else
    echo "Already exists, so skipping $url"
  fi
done
