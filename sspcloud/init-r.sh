# Clone repository and give permissions to the onyxia user
GIT_REPO=parquet-recensement-tutomate
WORK_DIR="/home/onyxia/work"
PROJECT_DIR="${WORK_DIR}/${GIT_REPO}"
QUARTO_FILE="${PROJECT_DIR}/tuto/r.qmd"

git clone --depth 1 https://github.com/linogaliana/${GIT_REPO}.git
chown -R onyxia:users ${GIT_REPO}/

Rscript -e "install.packages('remotes')"
Rscript -e "install.packages('renv')"
Rscript -e "renv::restore('${PROJECT_DIR}')"

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
    rstudioapi::openProject('${PROJECT_DIR}')
    renv::restore()
  }
}, action = 'append')
" >> /home/onyxia/.Rprofile