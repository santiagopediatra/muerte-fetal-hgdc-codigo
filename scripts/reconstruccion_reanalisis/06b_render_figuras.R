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


suppressPackageStartupMessages({library(ggplot2)})
out<-hgdc_results()
out <- hgdc_private_path(out)
r<-readRDS(file.path(out,'modelos_nuevos.rds'));d<-r$d
# Actual flow diagrams for each population, with eligibility and complete-case losses explicit.
for(n in names(r$poblaciones)){
 a<-r$flujo[r$flujo$poblacion==n,];labels<-c('Base fuente\n23.743 eventos;494 muertes registradas','Gestaciones únicas\n23.666 eventos;489 muertes','Fuera del análisis principal\n70 múltiples;7 de multiplicidad indeterminada',paste0(n,'\n',a$N_elegible,' elegibles;',a$Y1_elegible,' muertes'),paste0('Excluidos de elegibilidad\n',23666-a$N_elegible,' eventos;',489-a$Y1_elegible,' muertes'),paste0('Casos completos\n',a$N_modelo,' eventos;',a$Y1_modelo,' muertes'),paste0('Covariables incompletas\n',a$N_excluido_covariables,' eventos;',a$Y1_excluido_covariables,' muertes'))
 boxes<-data.frame(x=c(0,0,3,0,3,0,3),y=c(6,4,6,2,4,0,2),label=labels)
 segments<-data.frame(x=c(0,0,0,0,0,0),y=c(5.55,6,3.55,4,1.55,2),xend=c(0,2,0,2,0,2),yend=c(4.45,6,2.45,4,.45,2))
 g<-ggplot()+geom_segment(data=segments,aes(x=x,y=y,xend=xend,yend=yend),arrow=grid::arrow(length=grid::unit(.15,'cm')))+geom_label(data=boxes,aes(x=x,y=y,label=label),size=3.3,linewidth=.3)+xlim(-1.7,5)+ylim(-.7,6.7)+theme_void()+labs(caption='Unidad: evento obstétrico. Mismos criterios para ambos desenlaces; no se inventaronEG/pesos.')
 ggsave(file.path(out,'09_figuras',paste0('flujo_',n,'.png')),g,width=11,height=7,dpi=300,bg='white');ggsave(file.path(out,'09_figuras',paste0('flujo_',n,'.pdf')),g,width=11,height=7)
}
a<-r$descriptivos$anual;a<-a[a$poblacion=='Principal',]
g<-ggplot(a,aes(x=factor(ano_ingreso),y=N))+geom_col(fill='#28648b')+labs(x='Año de ingreso registrado',y='Eventos elegibles',caption='Se muestran todos los años registrados y las fechas no interpretables; no prueba cobertura institucional.')+theme_minimal(base_size=11)
ggsave(file.path(out,'09_figuras/eventos_anuales.png'),g,width=10,height=5,dpi=300,bg='white')
ct<-r$descriptivos$categoricas;tmp<-ct[ct$poblacion=='Todos_eventos'&ct$nivel=='[FALTANTE]'&ct$grupo!='Todos'&ct$variable%in%c('INSTRUCCION','PAREJA_ESTABLE','ETNIA_MINORITARIA'),]
continuous<-r$descriptivos$continuas;tmp2<-continuous[continuous$poblacion=='Todos_eventos'&continuous$grupo!='Todos'&continuous$variable%in%c('EDAD_ANALITICA','EDAD_GESTACIONAL_SEMANAS','PESO_1_ANALITICO'),];tmp2$porcentaje_total<-100*tmp2$faltantes/tmp2$N_grupo;tmp2$n<-tmp2$faltantes
miss<-rbind(tmp[,c('variable','grupo','porcentaje_total','n')],tmp2[,c('variable','grupo','porcentaje_total','n')]);g<-ggplot(miss,aes(x=variable,y=porcentaje_total,fill=grupo))+geom_col(position='dodge')+coord_flip()+labs(x=NULL,y='Datos analíticos faltantes (%)',fill=NULL)+theme_minimal(base_size=11)
ggsave(file.path(out,'09_figuras/faltantes_por_desenlace.png'),g,width=10,height=5,dpi=300,bg='white')

est<-r$estimaciones;focus<-grepl('^DPP|^NACIONALIDAD|^RPM|^ANTECEDENTE_CIRUGIA_UTERINA',est$termino)&est$metodo=='Firth'
