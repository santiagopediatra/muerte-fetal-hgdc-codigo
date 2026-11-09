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


suppressPackageStartupMessages({library(logistf);library(openxlsx)})
args<-commandArgs(trailingOnly=TRUE);out<-if(length(args))args[1] else hgdc_results()
out <- hgdc_private_path(out)
r<-readRDS(file.path(out,'modelos_nuevos.rds'));stopifnot(r$modelos_verificados);d<-r$d;d$PESO_SENSIBILIDAD_100G<-d$PESO_1_ANALITICO/100;d$SEGURO_ANALITICO<-relevel(factor(d$SEGURO_ANALITICO),'NO');d$PRESENTACION_FETAL<-relevel(factor(d$PRESENTACION_FETAL),'Vértice');f<-r$formula
ctl<-logistf.control(maxit=500,lconv=1e-7,gconv=1e-7,xconv=1e-7);plc<-logistpl.control(maxit=500)
extract<-function(m,n,z,method){b<-coef(m);se<-sqrt(diag(vcov(m)));if(method=='Firth'){lo<-m$ci.lower;hi<-m$ci.upper;p<-m$prob}else{lo<-b-qnorm(.975)*se;hi<-b+qnorm(.975)*se;p<-coef(summary(m))[,4]};data.frame(modelo=n,metodo=method,termino=names(b),OR=exp(b),LI95=exp(lo),LS95=exp(hi),p=p,N=nrow(z),Y1=sum(z$Y),row.names=NULL)}
fitfun<-function(ff,z,method){if(method=='Firth')logistf(ff,z,control=ctl,plcontrol=plc) else glm(ff,z,family=binomial(),na.action=na.fail)}
okay<-function(m,method){if(method=='Firth')all(abs(m$conv)<=c(ctl$lconv,ctl$gconv,ctl$xconv))&&max(m$pl.iter)<plc$maxit&&all(is.finite(m$ci.lower))&&all(is.finite(m$ci.upper))&&max(abs(m$pl.conv[,c(1,3)]))<plc$lconv&&max(abs(m$pl.conv[,c(2,4)]))<plc$xconv else m$converged&&all(is.finite(coef(m)))}
forms<-list(Materno=Y~EDAD_ANALITICA+HIPERTENSIVOS+DIABETES_EN_EL_EMBARAZO+IVU+ANTECEDENTE_CIRUGIA_UTERINA,Social=Y~NACIONALIDAD+PAREJA_ESTABLE+INSTRUCCION+ETNIA_MINORITARIA,Atencion_prenatal=Y~NO_CONTROLES_NUM+SESIONES_NUM,Clinico=Y~SCORE_ANALITICO,Obstetrico=Y~RPM+DPP+PRESENTACION_FETAL)
fitting<-list();res<-list();diag<-list();samples<-list()
for(n in names(forms)){ff<-forms[[n]];q<-d$POB_PRINCIPAL&complete.cases(d[all.vars(ff)]);z<-d[q,];method<-if(n%in%c('Materno','Obstetrico'))'Firth' else 'ML_Wald';cat('DOMINIO',n,nrow(z),sum(z$Y),'\n');m<-fitfun(ff,z,method);fitting[[n]]<-m;res[[n]]<-extract(m,n,z,method);diag[[n]]<-data.frame(modelo=n,N=nrow(z),Y1=sum(z$Y),metodo=method,convergencia=okay(m,method));samples[[n]]<-data.frame(modelo=n,fila_excel=z$FILA_EXCEL,Y=z$Y)}
paired<-list();pairfits<-list();paircheck<-list()
for(pair in list(c('SCORE','SCORE_ANALITICO'),c('Controles','NO_CONTROLES_NUM'),c('Peso','PESO_SENSIBILIDAD_100G'),c('Seguro','SEGURO_ANALITICO'))){n<-pair[1];v<-pair[2];ff<-update(f,paste('.~.+',v));q<-d$POB_PRINCIPAL&complete.cases(d[all.vars(ff)]);z<-d[q,];cat('PAR',n,nrow(z),sum(z$Y),'\n');a<-fitfun(f,z,'Firth');b<-fitfun(ff,z,'Firth');pairfits[[n]]<-list(sin=a,con=b,filas=z$FILA_EXCEL);paired[[paste0(n,'_sin')]]<-extract(a,paste0(n,'_sin'),z,'Firth');paired[[paste0(n,'_con')]]<-extract(b,paste0(n,'_con'),z,'Firth');paircheck[[n]]<-data.frame(comparacion=n,N=nrow(z),Y1=sum(z$Y),mismas_filas=TRUE,mismo_desenlace=TRUE,sin_convergente=okay(a,'Firth'),con_convergente=okay(b,'Firth'));samples[[paste0(n,'_par')]]<-data.frame(modelo=paste0(n,'_par'),fila_excel=z$FILA_EXCEL,Y=z$Y)}
res<-do.call(rbind,res);paired<-do.call(rbind,paired);diag<-do.call(rbind,diag);paircheck<-do.call(rbind,paircheck);samples<-do.call(rbind,samples)
write.xlsx(list(dominios=res,diagnosticos=diag,pares=paired,control_pares=paircheck,filas=samples),file.path(out,'08b_modelos_secundarios.xlsx'),overwrite=FALSE)
write.csv(res,file.path(out,'estimaciones_dominios.csv'),row.names=FALSE);write.csv(paired,file.path(out,'estimaciones_pares_covariables.csv'),row.names=FALSE)
saveRDS(list(d=d,dominios=fitting,pares=pairfits,diagnosticos=diag,control_pares=paircheck,verificados=all(diag$convergencia)&&all(paircheck$sin_convergente&paircheck$con_convergente)),file.path(out,'modelos_secundarios_nuevos.rds'))
cat('\n## Análisis secundarios\n',file=file.path(out,'07_diagnosticos_modelos.md'),append=TRUE)
cat(paste(c('Cinco dominios originales recalculados en la población principal única; multiplicidad omitida por ser constante. Los modelos materno/obstétrico usanFirth,los restantesML como el análisis anterior. Cada dominio usa sus propios casos completos. ParesSCORE/controles/peso/seguro conservan filas idénticas entre referencia yampliado. Peso por100g;SCORE/control porunidad. No se atribuyen efectos protectores ni temporalidad causal a variables acumulativas o al peso.',capture.output(print(diag,row.names=FALSE)),capture.output(print(paircheck,row.names=FALSE))),collapse='\n'),file=file.path(out,'07_diagnosticos_modelos.md'),append=TRUE)
print(diag);print(paircheck)
if(!all(diag$convergencia)||!all(paircheck$sin_convergente&paircheck$con_convergente))stop('Modelos secundarios requieren revisión deconvergencia.')
