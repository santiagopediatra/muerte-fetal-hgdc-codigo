# Adaptación de fase 5: rutas privadas y protección contra ejecución accidental.
.hfile <- grep("^--file=", commandArgs(trailingOnly=FALSE), value=TRUE)
if(!length(.hfile)) stop("Ejecute mediante Rscript; no source() interactivo.")
.hroot <- dirname(normalizePath(sub("^--file=", "", .hfile[1]), mustWork=TRUE))
while(!file.exists(file.path(.hroot, "config", "project.json"))) {
 .hparent <- dirname(.hroot); if(identical(.hparent,.hroot)) stop("No se encontró la raíz del proyecto")
 .hroot <- .hparent
}
source(file.path(.hroot,"scripts","verificacion","rutas.R"))
root <- .hroot
setwd(root)
if(Sys.getenv("HGDC_PRIVATE_WORKFLOW") != "YES") stop("Flujo privado deshabilitado. Consulte README; esta fase no ejecuta modelos.")
if(dir.exists(file.path(root,"renv","library"))) {
 if(!requireNamespace("renv",quietly=TRUE)) stop("Se requiere renv para cargar la biblioteca restaurada")
 renv::load(project=root)
}


suppressPackageStartupMessages({library(openxlsx);library(ggplot2)})
out <- hgdc_results()
out <- hgdc_private_path(out)
r <- readRDS(file.path(out,'modelos_nuevos.rds'));d<-r$d;pop<-r$poblaciones;est<-r$estimaciones;flow<-r$flujo;diag<-r$diagnosticos;passed<-r$modelos_verificados;refs<-r$refs
cat_table<-r$descriptivos$categoricas;cont_table<-r$descriptivos$continuas;annual<-r$descriptivos$anual;totals<-r$descriptivos$totales
cells<-read.xlsx(file.path(out,'05_modelos_principales.xlsx'),sheet='celdas');missing<-read.xlsx(file.path(out,'05_modelos_principales.xlsx'),sheet='faltantes')
sample<-do.call(rbind,lapply(names(r$fits),function(n){q<-d$FILA_EXCEL%in%r$fits[[n]]$filas;data.frame(poblacion=n,fila_excel=d$FILA_EXCEL[q],Y=d$Y[q])}))
r$celdas<-cells;r$faltantes<-missing;r$filas<-sample;saveRDS(r,file.path(out,'modelos_nuevos.rds'))
focus<-grepl('^DPP|^NACIONALIDAD|^RPM|^ANTECEDENTE_CIRUGIA_UTERINA',est$termino)&est$metodo=='Firth'
write.xlsx(list(comparacion=est[focus,],todos=est[est$metodo=='Firth',],flujo=flow,escenarios=data.frame(escenario=c('Principal/C ninguno','C todos indeterminados','A únicas originales','B tardía'),regla=c('Indeterminados excluidos simétricamente','Todos indeterminados únicos considerados elegibles;no se inventan EG/pesos','Todas únicas, incluidos indeterminados y conocidos bajo umbral','EG>=28;si faltaEG,peso>=1000'))),file.path(out,'06_analisis_sensibilidad.xlsx'),overwrite=FALSE)
write.xlsx(list(flujo=flow,poblaciones=totals,filas=sample,faltantes_covariables=missing),file.path(out,'03_flujo_poblaciones.xlsx'),overwrite=FALSE)
write.xlsx(list(multiples_categoricas=cat_table[cat_table$poblacion=='Multiples',],multiples_continuas=cont_table[cont_table$poblacion=='Multiples',],anual=annual,diagnosticos_pequenos=cells,faltantes_covariables=missing),file.path(out,'08_tablas_suplementarias.xlsx'),overwrite=FALSE)
write.csv(est,file.path(out,'estimaciones_frecuentistas.csv'),row.names=FALSE);write.csv(diag,file.path(out,'diagnosticos_frecuentistas.csv'),row.names=FALSE);write.csv(flow,file.path(out,'flujo_modelos.csv'),row.names=FALSE)
writeLines(c('# Diagnósticos de modelos',paste('Control global Firth/perfiles:',passed),'Fórmula original conservada salvo multiplicidad, constante al restringir a únicas. EDAD por año; factores con referencias originales. Casos completos específicos de cada población. ML usa exactamente la misma fórmula, filas y desenlace que Firth. Sus IC sonWald y requieren cautela si hay separación/celdas escasas.','RPM/DPP se interpretan como asociaciones con diagnósticos registrados; temporalidad respecto del fallecimiento no acreditada. No se atribuye protección causal aRPM/cirugía. No se seleccionaron covariables porp.','C incluye simétricamente todos los eventos únicos de elegibilidad desconocida, sin asignarEG ni peso; escenarios extremos por desenlace no se modelan. A/B/C son poblaciones hospitalarias, no tasas poblacionales.','Imputación múltiple no ejecutada: no hay justificación documentada del mecanismo de ausencia diferencial ni de un modelo predictor válido; no se asumeMAR.','',capture.output(print(diag,row.names=FALSE)),'',capture.output(print(flow,row.names=FALSE))),file.path(out,'07_diagnosticos_modelos.md'))
dir.create(file.path(out,'09_figuras'),showWarnings=FALSE)
pp<-est[focus,];pp$variable<-sub('SI$|EXTRANJERA$','',pp$termino)
g<-ggplot(pp,aes(x=OR,y=variable,color=poblacion))+geom_vline(xintercept=1,linetype=2)+geom_point(position=position_dodge(width=.6))+geom_errorbar(aes(xmin=LI95,xmax=LS95),orientation='y',width=.12,position=position_dodge(width=.6))+scale_x_log10()+labs(x='OR ajustada (IC95% penalizado)',y=NULL,color='Población')+theme_minimal(base_size=12)
ggsave(file.path(out,'09_figuras/comparacion_OR.png'),g,width=10,height=5,dpi=300);ggsave(file.path(out,'09_figuras/comparacion_OR.pdf'),g,width=10,height=5)
for(n in names(pop)){a<-flow[flow$poblacion==n,];ind<-sum(d$POB_INDETERMINADA);s<-paste0('flowchart TD\n  T["23.743 eventos"] --> U["23.666 gestaciones únicas"]\n  T --> M["70 múltiples y7 indeterminadas"]\n  U --> E["',n,': ',a$N_elegible,' elegibles; ',a$Y1_elegible,' muertes registradas"]\n  E --> C["',a$N_modelo,' casos completos; ',a$Y1_modelo,' muertes"]\n  E --> X["',a$N_excluido_covariables,' excluidos por covariables incompletas"]\n');writeLines(s,file.path(out,'09_figuras',paste0('flujo_',n,'.mmd')))}
capture.output(sessionInfo(),file=file.path(out,'sessionInfo_modelos.txt'))
print(flow);print(diag);print(est[focus,c('poblacion','termino','OR','LI95','LS95','N','Y1')])
if(!passed)stop('ConvergenciaFirth/perfiles no verificada; detener reporte inferencial definitivo ybayes.')
