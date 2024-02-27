.onLoad <- function(libname, pkgname){
  imgPath <- system.file('www', package='CANDIS')
  if(!file.exists(imgPath)){
    imgPath <- system.file('inst/www', package='CANDIS')
  }
  if(!is.null(imgPath)&&imgPath!=""){
    addResourcePath("www",imgPath)
  }
}


