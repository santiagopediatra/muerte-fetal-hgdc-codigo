# Instalación manual, explícita y separada de análisis. No ajusta modelos.
f <- grep("^--file=",commandArgs(FALSE),value=TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=","",f[1])),"..",".."),mustWork=TRUE)
if(Sys.getenv("HGDC_INSTALL_DEPENDENCIES")!="YES") stop("Instalación deshabilitada. Consulte docs/01_reproducibilidad.md")
if(!requireNamespace("renv",quietly=TRUE)||as.character(packageVersion("renv"))!="1.1.2") stop("Instale renv 1.1.2 según documentación")
renv::restore(project=root,lockfile=file.path(root,"renv.lock"),prompt=FALSE)
