# Prueba de rutas y sintaxis: no evalúa los scripts de análisis.
f <- grep("^--file=",commandArgs(FALSE),value=TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=","",f[1])),"..",".."),mustWork=TRUE)
.hroot <- root
source(file.path(root,"scripts","verificacion","rutas.R"))
stopifnot(dir.exists(hgdc_root()),file.exists(file.path(root,"config","project.json")))
files <- list.files(file.path(root,"scripts"),pattern="[.]R$",recursive=TRUE,full.names=TRUE)
for(x in files) invisible(parse(x))
if(dir.exists(file.path(root,"private_data"))) stopifnot(identical(hgdc_private_path(file.path(root,"private_data")),normalizePath(file.path(root,"private_data"))))
cat("Raíz localizada;",length(files),"scripts R parseados; ningún modelo ejecutado.\n")
