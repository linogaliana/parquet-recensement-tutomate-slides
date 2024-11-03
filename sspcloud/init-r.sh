REPO_URL=https://github.com/linogaliana/parquet-recensement-tutomate.git
WORK_DIR=/home/onyxia/work
FORMATION_DIR=${WORK_DIR}/parquet-recensement-tutomate
QUARTO_FILE=tuto/r.qmd

# Clone the repository
git clone $REPO_URL $FORMATION_DIR
chown -R onyxia:users $FORMATION_DIR

# Install dependencies
install2.r here

# Open the project
echo \
"
setHook('rstudio.sessionInit', function(newSession) {
 if (newSession)
  {
    rstudioapi::navigateToFile('tp/${QUARTO_FILE}')
    renv::restore()
  }
}, action = 'append')

setHook('rstudio.sessionInit', function(newSession) {
  if (newSession && identical(getwd(), '${WORK_DIR}'))
  {
    message('Activation du projet RStudio')
    rstudioapi::openProject('${FORMATION_DIR}')
    renv::restore()
  }
}, action = 'append')
" >> /home/onyxia/.Rprofile