packages <- c('dplyr', 'tidyr', 'openxlsx', 'arrow', 'mgcv', 'leaps', 'logger', 
              'purrr', 'lubridate', 'ompr', 'ompr.roi', 'ROI.plugin.glpk', 
              'slider', 'xgboost', 'readr')

failed <- c()
for(pkg in packages) {
  if(!require(pkg, character.only=TRUE, quietly=TRUE)) {
    failed <- c(failed, pkg)
    cat('FAILED:', pkg, '\n')
  } else {
    cat('OK:', pkg, '\n')
  }
}

if(length(failed) > 0) {
  cat('\nFailed packages:', paste(failed, collapse=', '), '\n')
  quit(status=1)
} else {
  cat('\nAll packages loaded successfully!\n')
}
