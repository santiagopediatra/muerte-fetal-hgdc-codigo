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


suppressPackageStartupMessages({library(logistf);library(openxlsx);library(ggplot2);library(jsonlite)})
root<-.hroot;args<-commandArgs(trailingOnly=TRUE);out<-if(length(args))args[1] else hgdc_results()
out <- hgdc_private_path(out)
r<-readRDS(file.path(out,'modelos_nuevos.rds'));d<-r$d;before<-readRDS(file.path(out,'replica_validacion/comparador_fuente_anterior_reconstruido.rds'));replica<-readRDS(file.path(out,'replica_validacion/base_analitica_corregida.rds'));actual<-readRDS(file.path(out,'base_analitica_corregida.rds'))
stopifnot(identical(actual,replica),r$modelos_verificados)
for(v in names(r$refs))before[[v]]<-relevel(factor(before[[v]],levels=levels(d[[v]])),r$refs[[v]])
z<-before[before$POB_PRINCIPAL&complete.cases(before[all.vars(r$formula)]),]
if(file.exists(file.path(out,'firth_comparador_fuente_anterior.rds'))){oldfit<-readRDS(file.path(out,'firth_comparador_fuente_anterior.rds'));stopifnot(all(sapply(names(oldfit$model),function(v)identical(as.character(oldfit$model[[v]]),as.character(z[[v]])))))}else{oldfit<-logistf(r$formula,z,control=logistf.control(maxit=500,lconv=1e-7,gconv=1e-7,xconv=1e-7),plcontrol=logistpl.control(maxit=500))}
stopifnot(all(abs(oldfit$conv)<=1e-7),all(is.finite(oldfit$ci.lower)),all(is.finite(oldfit$ci.upper)),max(oldfit$pl.iter)<500)
saveRDS(oldfit,file.path(out,'firth_comparador_fuente_anterior.rds'))
oldtab<-data.frame(termino=names(coef(oldfit)),OR_fuente_anterior=exp(coef(oldfit)),LI95_fuente_anterior=exp(oldfit$ci.lower),LS95_fuente_anterior=exp(oldfit$ci.upper),N_fuente_anterior=nrow(z),Y1_fuente_anterior=sum(z$Y))
new<-r$estimaciones[r$estimaciones$poblacion=='Principal'&r$estimaciones$metodo=='Firth',]
comparison<-merge(oldtab,new,by='termino',sort=FALSE);comparison$cociente_OR<-comparison$OR/comparison$OR_fuente_anterior
historical<-read.csv(file.path(root,'results/referencias_historicas/modelo_principal_frecuentista_historico.csv'))
historical<-historical[,intersect(c('Termino','OR','LI95','LS95','N','Eventos'),names(historical))];names(historical)[names(historical)=='Termino']<-'termino';historical$advertencia<-'Modelo histórico con múltiples y sin restricción de elegibilidad: no comparable para atribuir diferencias exclusivamente a245correcciones'
write.xlsx(list(fuente_antes_despues=comparison,referencia_historica=historical,reproducibilidad=data.frame(base_replica_identica=identical(actual,replica)),muestras=data.frame(version=c('Fuente anterior reconstruida','Fuente corregida reconstruida'),N_elegible=c(sum(before$POB_PRINCIPAL),sum(d$POB_PRINCIPAL)),Y1_elegible=c(sum(before$Y[before$POB_PRINCIPAL]),sum(d$Y[d$POB_PRINCIPAL])),N_modelo=c(nrow(z),new$N[1]),Y1_modelo=c(sum(z$Y),new$Y1[1]))),file.path(out,'06c_comparacion_antes_despues.xlsx'),overwrite=FALSE)
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
# Source integrity and numerical outputs, independent of DOCX export.
source<-attr(actual,'procedencia')$archivo;sha<-strsplit(system2('sha256sum',shQuote(source),stdout=TRUE),' ')[[1]][1];stopifnot(identical(sha,attr(actual,'procedencia')$sha256))
check<-data.frame(control=c('Reconstrucción reproducida exactamente','Fuente original sin cambios','Todas poblaciones flujos correctos','Firth principal/sensibilidades convergente','Todas estimaciones Firth acotan OR','FilasFirth/ML idénticas porpoblación'),ok=c(identical(actual,replica),identical(sha,attr(actual,'procedencia')$sha256),all(r$flujo$N_elegible==r$flujo$N_modelo+r$flujo$N_excluido_covariables),r$modelos_verificados,all(new$LI95<new$OR&new$OR<new$LS95),all(sapply(r$fits,function(x)identical(rownames(x$firth$model),rownames(x$ml$model))))))
check$ok[check$control=='FilasFirth/ML idénticas porpoblación']<-all(sapply(r$fits,function(x){identical(names(x$firth$model),names(x$ml$model))&&all(sapply(names(x$ml$model),function(v)identical(as.character(x$firth$model[[v]]),as.character(x$ml$model[[v]]))))&&identical(as.numeric(x$firth$y),as.numeric(x$ml$y))}))

write.csv(check,file.path(out,'control_numerico_final.csv'),row.names=FALSE)
write_json(list(N_total=nrow(d),Y1_total=sum(d$Y),poblaciones=r$flujo,Firth_principal=new,fuente=list(archivo=source,hoja='BASE_ORIGINAL',sha256=sha),comparacion=comparison,controles=check),file.path(out,'resumen_reanalisis.json'),pretty=TRUE,dataframe='rows',auto_unbox=TRUE,digits=15)
print(check);print(comparison[grepl('^DPP|^NACIONALIDAD|^RPM|^ANTECEDENTE_CIRUGIA_UTERINA',comparison$termino),c('termino','OR_fuente_anterior','OR','cociente_OR')]);stopifnot(all(check$ok))
