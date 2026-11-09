# Utilidades de rutas; no abre datos ni ajusta modelos.
hgdc_root <- function() {
 if(exists(".hroot",envir=.GlobalEnv)) return(get(".hroot",envir=.GlobalEnv))
 stop("Raíz no inicializada")
}
hgdc_private_path <- function(path) {
 base <- normalizePath(file.path(hgdc_root(),"private_data"),mustWork=TRUE,winslash="/")
 # Requiere destino existente para no admitir rutas ambiguas con .. o symlinks.
 if(!dir.exists(path)) stop("Cree la carpeta privada autorizada antes de ejecutar")
 real <- normalizePath(path,mustWork=TRUE,winslash="/")
 if(!(identical(real,base)||startsWith(real,paste0(base,"/")))) stop("Los resultados deben permanecer dentro de private_data")
 real
}
hgdc_results <- function() file.path(hgdc_root(),"private_data","reconstruccion_reanalisis")
hgdc_revision <- function() file.path(hgdc_root(),"private_data","revision_cientifica_final")
