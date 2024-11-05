git clone --depth 1 https://github.com/linogaliana/parquet-recensement-tutomate.git tuto
pip install -r tuto/requirements.txt
rm -rf tuto

WORK_DIR="/home/onyxia/work"
DOWNLOAD_URL="https://linogaliana.github.io/parquet-recensement-tutomate/tuto/python.ipynb"
CHAPTER="tuto-parquet-python.ipynb"
curl -L $DOWNLOAD_URL -o "${WORK_DIR}/${CHAPTER}.ipynb"
