#!/bin/bash

WORK_DIR="/home/onyxia/work"
DESTINATION_GIT="${WORK_DIR}/tuto"
DOWNLOAD_URL="https://linogaliana.github.io/parquet-recensement-tutomate/tuto/python.ipynb"
CHAPTER="tuto-parquet-python"

git clone --depth 1 https://github.com/linogaliana/parquet-recensement-tutomate.git ${DESTINATION_GIT}
pip install -r "${DESTINATION_GIT}/requirements.txt"
# rm -rf tuto

echo $DOWNLOAD_URL
curl -L $DOWNLOAD_URL -o "${WORK_DIR}/${CHAPTER}.ipynb"
