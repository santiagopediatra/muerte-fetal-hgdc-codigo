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


if(Sys.getenv("HGDC_RECONSTRUCTION_APPROVED")!="YES") stop("Borrador pendiente de aprobación: reconstrucción deshabilitada")
suppressPackageStartupMessages({library(readxl);library(openxlsx);library(jsonlite)})
root <- .hroot
args <- commandArgs(trailingOnly=TRUE)
out <- if(length(args)) normalizePath(args[1],mustWork=FALSE) else hgdc_results()
if(file.exists(file.path(out,'base_analitica_corregida.rds'))) stop('Destino ya contiene una reconstrucción; indique una carpeta nueva como argumento.')
# Validar padre existente antes de crear un destino; nunca escribir fuera de private_data.
hgdc_private_path(dirname(out))
dir.create(out,recursive=FALSE,showWarnings=FALSE)
out <- hgdc_private_path(out)
source_file <- file.path(root,'private_data','fuentes','BASE_OBITO_MAESTRA.xlsx')
historical_file <- file.path(root,'private_data','fuentes','BASE_OBITO_HISTORICA.xlsx')
sha <- function(f) digest::digest(file=f,algo='sha256',serialize=FALSE)
source_hash<-sha(source_file)
raw<-as.data.frame(read_excel(source_file,sheet='BASE_ORIGINAL',guess_max=25000,trim_ws=FALSE))
old<-as.data.frame(read_excel(historical_file,sheet='BASE_ORIGINAL',guess_max=25000,trim_ws=FALSE))
# Historical clean sheet is a comparison target only. It never supplies new data columns.
h<-as.data.frame(read_excel(historical_file,sheet='BASE_OBITO_LIMPIA',guess_max=25000,trim_ws=FALSE))
checks<-list();check<-function(name,ok,critical=TRUE,detail=''){checks[[length(checks)+1]]<<-data.frame(control=name,satisfactorio=isTRUE(ok),critico=critical,detalle=detail)}
changed<-function(a,b){a<-as.character(a);b<-as.character(b);is.na(a)!=is.na(b)|(!is.na(a)&!is.na(b)&a!=b)}
check('Dimensiones BASE_ORIGINAL',identical(dim(raw),c(23743L,65L)))
check('Columnas fuente anterior y nueva',identical(names(raw),names(old)))
chg<-lapply(names(raw),function(v)changed(old[[v]],raw[[v]]));names(chg)<-names(raw)
modified<-Reduce(`|`,chg)
check('245 filas modificadas',sum(modified)==245,detail=as.character(sum(modified)))
check('294 celdas modificadas',sum(sapply(chg,sum))==294,detail=as.character(sum(sapply(chg,sum))))
anchors<-names(raw)[!sapply(chg,any)]
check('59 columnas ancla sin cambios',length(anchors)==59&&all(sapply(anchors,function(v)identical(as.character(raw[[v]]),as.character(old[[v]])))))
key<-function(z)do.call(paste,c(lapply(z,function(x){x<-as.character(x);ifelse(is.na(x),'NA:',paste0(nchar(x),':',x))}),sep='|'))
oldkey<-key(old[anchors]);newkey<-key(raw[anchors]);check('Continuidad estructural por firmas exactas',identical(oldkey,newkey))
ledger<-do.call(rbind,lapply(names(raw),function(v){q<-chg[[v]];if(!any(q))return(NULL);full<-v%in%c('PESO','SEMANAS GESTACION','EDAD');x<-as.character(old[[v]][q]);z<-as.character(raw[[v]][q]);if(!full){x<-sub('^.*([0-9]{4})$','AÑO:\\1',x);z<-sub('^.*([0-9]{4})$','AÑO:\\1',z)};data.frame(fila_excel=which(q)+1,variable=v,valor_anterior=x,valor_corregido=z,valor_completo=full,ancla_unica=!(duplicated(oldkey)|duplicated(oldkey,fromLast=TRUE))[q])}))
normalize_name<-function(x){x<-toupper(iconv(x,to='ASCII//TRANSLIT'));x<-gsub('[^A-Z0-9]+','_',x);gsub('^_|_$','',x)}
source_names<-names(raw);names_out<-normalize_name(source_names)
aliases<-c(FECHA_NACIMI_MADRE='FECHA_NACIMI_MADRE',ANTECEDENTE_DE_CIRUGIA_UTERINA='ANTECEDENTE_CIRUGIA_UTERINA',DESPRENDIMIENTO_PREMATURO_DE_PLACENTA='DPP',ANESTESIA_CESAREA='ANESTESIA_CESAREA')
for(a in names(aliases))names_out[names_out==a]<-aliases[a]
check('Encabezados normalizados únicos',!anyDuplicated(names_out))
missing_codes<-c('','NA','N/A','N.A.','NAN','NULL','NONE','S/D','SD','SIN DATO','SIN DATOS','NO REGISTRA','NO REGISTRADO','NO CONSTA','DESCONOCIDO')
clean<-function(x){x<-trimws(gsub('[[:space:]]+',' ',as.character(x)));x[toupper(x)%in%missing_codes]<-NA_character_;x}
strictnum<-function(x){x<-clean(x);q<-!is.na(x)&grepl('^[0-9]+([.][0-9]+)?$',x);z<-rep(NA_real_,length(x));z[q]<-as.numeric(x[q]);z}
parse_date<-function(x){x<-as.character(x);q<-!is.na(x)&grepl('^[0-9]{2}/[0-9]{2}/[0-9]{4}$',x);z<-as.Date(rep(NA_character_,length(x)));z[q]<-as.Date(x[q],'%d/%m/%Y');ok<-!is.na(z)&format(z,'%d/%m/%Y')==x;ok[is.na(ok)]<-FALSE;z[!ok]<-NA;z}
reconstruct <- function(z){
 original<-z;names(z)<-names_out;z[]<-lapply(z,clean)
 z$ROW_ID<-seq_len(nrow(z));z$FILA_EXCEL<-z$ROW_ID+1L
 z$MUERTE_INTRAUTERINA_BIN<-ifelse(z$MUERTE_INTRAUTERINA=='SI',1L,ifelse(z$MUERTE_INTRAUTERINA=='NO',0L,NA_integer_));z$Y<-z$MUERTE_INTRAUTERINA_BIN
 z$EDAD_ANALITICA<-strictnum(z$EDAD);z$EDAD_ANALITICA[!is.na(z$EDAD_ANALITICA)&(z$EDAD_ANALITICA<10|z$EDAD_ANALITICA>60)]<-NA_real_
 z$EDAD_CATEGORIA<-ifelse(is.na(z$EDAD_ANALITICA),NA,ifelse(z$EDAD_ANALITICA<20,'<20',ifelse(z$EDAD_ANALITICA<35,'20-34','>=35')))
 z$SCORE_ANALITICO<-strictnum(z$SCORE);z$NO_CONTROLES_NUM<-strictnum(z$NO_CONTROLES);z$SESIONES_NUM<-strictnum(z$SESIONES)
 z$SEGURO_ANALITICO<-ifelse(z$SEGURO%in%c('SI','NO'),z$SEGURO,NA)
 z$PSICOPROFILAXIS_ANALITICA<-ifelse(z$PSICOPROFILAXIS%in%c('SI','NO'),z$PSICOPROFILAXIS,NA)
 z$ACOMPANAMIENTO<-ifelse(is.na(z$ACOMPANADA),NA,ifelse(z$ACOMPANADA=='SOLA','SOLA','ACOMPANADA'))
 gx<-z$SEMANAS_GESTACION;fmt<-!is.na(gx)&grepl('^[0-9]+([.][0-6])?$',gx)
 wk<-rep(NA_real_,nrow(z));day<-wk;wk[fmt]<-as.numeric(sub('[.].*$','',gx[fmt]));day[fmt]<-0
 dec<-fmt&grepl('[.]',gx);day[dec]<-as.numeric(sub('^.*[.]','',gx[dec]));eg<-wk+day/7
 z$EG_SEMANAS_ENTERAS<-wk;z$EG_DIAS<-day;z$EG_NUMERICA_FUENTE<-eg
 z$EG_ESTADO<-ifelse(is.na(gx),'FALTANTE',ifelse(!fmt,'CODIGO_INVALIDO',ifelse(eg==0,'CERO',ifelse(eg<20|eg>=45,'FUERA_RANGO','OBSERVADA_UTILIZABLE'))))
 z$EG_CODIFICACION_INVALIDA<-ifelse(is.na(gx),NA_integer_,as.integer(!fmt));z$EG_IMPUTADO<-0L
 z$EG_REGLA_IMPUTACION<-NA_character_
 z$EDAD_GESTACIONAL_SEMANAS<-ifelse(z$EG_ESTADO=='OBSERVADA_UTILIZABLE',eg,NA_real_)
 z$EDAD_GESTACIONAL_ANALITICA<-z$EDAD_GESTACIONAL_SEMANAS
 z$EG_CATEGORIA_ANALITICA<-ifelse(is.na(z$EDAD_GESTACIONAL_SEMANAS),NA,ifelse(z$EDAD_GESTACIONAL_SEMANAS<37,'PRETERMINO',ifelse(z$EDAD_GESTACIONAL_SEMANAS<42,'TERMINO','POSTERMINO')))
 z$EG_CAT<-ifelse(is.na(z$EDAD_GESTACIONAL_SEMANAS),NA,ifelse(z$EDAD_GESTACIONAL_SEMANAS<37,'<37',ifelse(z$EDAD_GESTACIONAL_SEMANAS<42,'37–41+6','≥42')))
 px<-gsub(' ','',z$PESO,fixed=TRUE);single_p<-!is.na(px)&grepl('^[0-9]+$',px);pair_p<-!is.na(px)&grepl('^[0-9]+/[0-9]+$',px)
 p1<-rep(NA_real_,nrow(z));p2<-p1;p1[single_p]<-as.numeric(px[single_p]);p1[pair_p]<-as.numeric(sub('/.*$','',px[pair_p]));p2[pair_p]<-as.numeric(sub('^.*/','',px[pair_p]))
 z$PESO_FORMATO_INVALIDO<-!is.na(px)&!single_p&!pair_p;z$PESO_1<-p1;z$PESO_2<-p2
 sx<-z$SEXO;sp<-!is.na(sx)&grepl('^[^/]+/[^/]+$',sx)
 z$SEXO_FETO_1<-ifelse(sp,sub('/.*$','',sx),sx);z$SEXO_FETO_2<-ifelse(sp,sub('^.*/','',sx),NA)
 # Do not convert a date-looking weight into numeric fetal components.
 second_evidence<-!is.na(p2)|!is.na(z$SEXO_FETO_2)|z$PESO_FORMATO_INVALIDO
 strong<-!is.na(p2)&!is.na(z$SEXO_FETO_2)
 z$EMBARAZO_MULTIPLE_BIN<-ifelse(z$EMBARAZO_MULTIPLE=='Doble',1L,ifelse(z$EMBARAZO_MULTIPLE=='NO',0L,NA_integer_))
 z$EMBARAZO_MULTIPLE_ANALITICO<-ifelse(z$EMBARAZO_MULTIPLE_BIN==1,1L,ifelse(z$EMBARAZO_MULTIPLE_BIN==0&strong,1L,ifelse(z$EMBARAZO_MULTIPLE_BIN==0&second_evidence,NA_integer_,z$EMBARAZO_MULTIPLE_BIN)))
 z$FUENTE_MULTIPLE_ANALITICO<-ifelse(z$EMBARAZO_MULTIPLE_BIN==1,'REGISTRO_INSTITUCIONAL_DOBLE',ifelse(strong,'DOS_PESOS_MAS_DOS_SEXOS',ifelse(second_evidence,'DISCORDANTE_INDETERMINADO','REGISTRO_CONCORDANTE_NO_MULTIPLE')))
 for(j in 1:2){p<-z[[paste0('PESO_',j)]];e<-z$EDAD_GESTACIONAL_SEMANAS
 st<-ifelse(is.na(p),'MISSING',ifelse(p>=300&p<=7000,'PLAUSIBLE',ifelse(p>0&p<300&!is.na(e)&e<=27,'POTENCIALMENTE_PLAUSIBLE_PREMATURIDAD_EXTREMA',ifelse(p>0&p<300&is.na(e),'INDETERMINADO_SIN_EG','INCOMPATIBLE_EG_REQUIERE_REVISION'))))
 use<-st%in%c('PLAUSIBLE','POTENCIALMENTE_PLAUSIBLE_PREMATURIDAD_EXTREMA')
 z[[paste0('ESTADO_PESO_',j)]]<-st;z[[paste0('PESO_',j,'_PLAUSIBLE')]]<-ifelse(st%in%c('MISSING','INDETERMINADO_SIN_EG'),NA_integer_,as.integer(use));z[[paste0('PESO_',j,'_ANALITICO')]]<-ifelse(use,p,NA_real_)
 }
 for(v in c('IVU','RPM','DPP','ANTECEDENTE_CIRUGIA_UTERINA','PAREJA_ESTABLE','ETNIA_MINORITARIA'))z[[paste0(v,'_BIN')]]<-ifelse(z[[v]]=='SI',1L,ifelse(z[[v]]=='NO',0L,NA_integer_))
 for(v in c('FECHA_INGRESO','FECHA_NACIMIENTO','FECHA_DE_EGRESO','FECHA_NACIMI_MADRE')){z[[paste0(v,'_PARSEADA')]]<-parse_date(z[[v]]);z[[paste0('ANIO_',v)]]<-as.integer(format(z[[paste0(v,'_PARSEADA')]],'%Y'))}
 s<-!is.na(z$EMBARAZO_MULTIPLE_ANALITICO)&z$EMBARAZO_MULTIPLE_ANALITICO==0;e<-z$EDAD_GESTACIONAL_SEMANAS;p<-z$PESO_1_ANALITICO
 eligibility<-function(t,g)s&((!is.na(e)&e>=t)|(is.na(e)&!is.na(p)&p>=g))
 z$POB_PRINCIPAL<-eligibility(22,500);z$POB_A<-s;z$POB_B<-eligibility(28,1000);z$POB_INDETERMINADA<-s&is.na(e)&is.na(p)
 z$MOTIVO_FLUJO<-ifelse(is.na(z$EMBARAZO_MULTIPLE_ANALITICO),'MULTIPLICIDAD_INDETERMINADA',ifelse(!s,'MULTIPLE',ifelse(z$POB_PRINCIPAL,'ELEGIBLE_PRINCIPAL',ifelse(!is.na(e)&e<22,'EG_MENOR22',ifelse(is.na(e)&is.na(p),'EG_PESO_INDETERMINADOS','SIN_EG_PESO_MENOR500')))))
 z
}
d<-reconstruct(raw);before<-reconstruct(old)
check('N eventos',nrow(d)==23743)
check('Desenlace completo binario',!anyNA(d$Y)&&all(d$Y%in%0:1))
check('494 muertes registradas',sum(d$Y)==494)
check('Desenlace sin cambios frente a fuente previa',identical(d$Y,before$Y))
check('Duplicados exactos fuente',sum(duplicated(raw))==0)
check('Multiplicidad reconstruida',sum(d$EMBARAZO_MULTIPLE_ANALITICO==0,na.rm=TRUE)==23666&&sum(d$EMBARAZO_MULTIPLE_ANALITICO==1,na.rm=TRUE)==70&&sum(is.na(d$EMBARAZO_MULTIPLE_ANALITICO))==7)
pchg<-chg$PESO;pr<-pchg&is.na(old$PESO)&!is.na(raw$PESO)
check('42 pesos recuperados',sum(pr)==42)
check('4 pesos numéricos corregidos',sum(pchg&!is.na(old$PESO))==4)
check('42 pesos recuperados válidos según regla',all(!is.na(d$PESO_1_ANALITICO[pr]))&&all(d$PESO_1_PLAUSIBLE[pr]==1))
pesos<-data.frame(fila_excel=which(pchg)+1,PESO_antes=as.character(old$PESO[pchg]),PESO_nuevo=as.character(raw$PESO[pchg]),recuperado=pr[pchg],EG=d$EDAD_GESTACIONAL_SEMANAS[pchg],peso_analitico=d$PESO_1_ANALITICO[pchg],estado=d$ESTADO_PESO_1[pchg],Y=d$Y[pchg])
gchg<-chg[['SEMANAS GESTACION']];egreview<-data.frame(fila_excel=which(gchg)+1,original_anterior=as.character(old[['SEMANAS GESTACION']][gchg]),corregido=as.character(raw[['SEMANAS GESTACION']][gchg]),estado=d$EG_ESTADO[gchg],EG_analitica=d$EDAD_GESTACIONAL_SEMANAS[gchg],Y=d$Y[gchg])
check('44 EG convertidas a vacío',sum(gchg)==44&&all(is.na(raw[['SEMANAS GESTACION']][gchg])))
check('Valores EG retirados no informativos',all(as.character(old[['SEMANAS GESTACION']][gchg])%in%c('0','SIN DATOS')))
# Independent Excel reader confirms physical import, including all 44 blanks.
independent<-openxlsx::read.xlsx(source_file,sheet='BASE_ORIGINAL',check.names=FALSE,skipEmptyRows=FALSE,skipEmptyCols=FALSE,detectDates=FALSE)
check('Importación independiente dimensiones',identical(dim(independent),dim(raw)))
for(v in c('PESO','SEMANAS GESTACION','FECHA NACIMIENTO','MUERTE INTRAUTERINA'))check(paste('Importación independiente',v),!any(changed(independent[[v]],raw[[v]])))
check('125 fechas de nacimiento modificadas',sum(chg[['FECHA NACIMIENTO']])==125)
check('Nacimiento 1899:124 a2',sum(before$ANIO_FECHA_NACIMIENTO==1899,na.rm=TRUE)==124&&sum(d$ANIO_FECHA_NACIMIENTO==1899,na.rm=TRUE)==2)
check('Fechas modificadas interpretables',all(!is.na(d$FECHA_NACIMIENTO_PARSEADA[chg[['FECHA NACIMIENTO']]])))
# Validate categorical covariates strictly, without treating unknown as NO.
levels_expected<-list(NACIONALIDAD=c('ECUATORIANA','EXTRANJERA'),PAREJA_ESTABLE=c('SI','NO'),INSTRUCCION=c('NINGUNA','PRIMARIA','SECUNDARIA','SUPERIOR'),ETNIA_MINORITARIA=c('SI','NO'),HIPERTENSIVOS=c('Ninguno','HTA preexistente','HTA inducida por el embarazo','Preeclampsia','Eclampsia'),DIABETES_EN_EL_EMBARAZO=c('NO','Gestacional','Preexistente'),IVU=c('SI','NO'),ANTECEDENTE_CIRUGIA_UTERINA=c('SI','NO'),RPM=c('SI','NO'),DPP=c('SI','NO'))
for(v in names(levels_expected))check(paste('Categorías',v),all(is.na(d[[v]])|d[[v]]%in%levels_expected[[v]]))
labels<-do.call(rbind,lapply(c('HIPERTENSIVOS','DIABETES_EN_EL_EMBARAZO'),function(v){j<-match(v,names_out);x<-as.character(raw[[j]]);do.call(rbind,lapply(sort(unique(d[[v]])),function(k)data.frame(variable=v,nivel=k,antes=sum(x==k,na.rm=TRUE),despues=sum(d[[v]]==k,na.rm=TRUE),espacios_normalizados=sum(trimws(x)==k&x!=trimws(x),na.rm=TRUE))))}))
check('Trimws conserva Eclampsia',sum(trimws(raw$HIPERTENSIVOS)=='Eclampsia',na.rm=TRUE)==sum(d$HIPERTENSIVOS=='Eclampsia',na.rm=TRUE))
check('Trimws conserva diabetes preexistente',sum(trimws(raw[['DIABETES EN EL EMBARAZO']])=='Preexistente',na.rm=TRUE)==sum(d$DIABETES_EN_EL_EMBARAZO=='Preexistente',na.rm=TRUE))
flowfun<-function(z,label){do.call(rbind,lapply(sort(unique(z$MOTIVO_FLUJO)),function(r){q<-z$MOTIVO_FLUJO==r;data.frame(version=label,motivo=r,N=sum(q),Y1=sum(z$Y[q]),Y0=sum(z$Y[q]==0))}))}
flows<-rbind(flowfun(before,'Fuente original anterior reconstruida'),flowfun(d,'Fuente original corregida reconstruida'))
check('Flujo cubre23.743',sum(flows$N[flows$version=='Fuente original corregida reconstruida'])==nrow(d))
check('Simetría criterios elegibilidad',all(d$POB_PRINCIPAL==(!is.na(d$EMBARAZO_MULTIPLE_ANALITICO)&d$EMBARAZO_MULTIPLE_ANALITICO==0&((!is.na(d$EDAD_GESTACIONAL_SEMANAS)&d$EDAD_GESTACIONAL_SEMANAS>=22)|(is.na(d$EDAD_GESTACIONAL_SEMANAS)&!is.na(d$PESO_1_ANALITICO)&d$PESO_1_ANALITICO>=500)))))
# Explicit unresolved issues; originals are retained, no date-based exclusions.
check('Fechas fuera de2018–2023 presentes',!any(!is.na(d$ANIO_FECHA_NACIMIENTO)&!d$ANIO_FECHA_NACIMIENTO%in%2018:2023),FALSE,'Se conservan todos los eventos; período institucional no acreditado por las fechas')
check('Peso con apariencia de fecha resuelto',!any(d$PESO_FORMATO_INVALIDO),FALSE,'Los pesos con formato de fecha no se reinterpretan sin evidencia; se conservan marcas de formato inválido')
check('Programa inicial completo localizado',FALSE,FALSE,'No localizado tras revisar los blobs R/Python históricos; reglas parciales recuperadas y reemplazo explícito reproducible')
# Historic sheet differences are evaluated only; no columns are copied into d.
common<-intersect(names(d),names(h));recon_comp<-do.call(rbind,lapply(common,function(v){a<-as.character(before[[v]]);b<-as.character(h[[v]]);data.frame(variable=v,discordancias_anteriores_fuente_limpia=sum(changed(a,b)),diferencias_nueva_vs_limpia=sum(changed(d[[v]],h[[v]])))}))
old55<-h$MUERTE_INTRAUTERINA_BIN==1&(is.na(h$EDAD_GESTACIONAL_SEMANAS)|h$EDAD_GESTACIONAL_SEMANAS<=0|h$EG_IMPUTADO==1)&is.na(h$PESO_1_ANALITICO)
# Exact correspondence source/clean established with invariant common fields; row reference is audit only.
matchnorm<-function(x)gsub('[^A-Z0-9]','',toupper(iconv(x,to='ASCII//TRANSLIT')))
j<-match(matchnorm(names(old)),matchnorm(names(h)));keep<-which(!is.na(j)&names(old)%in%anchors);keep<-keep[sapply(keep,function(k)identical(as.character(old[[k]]),as.character(h[[j[k]]])))]
check('Correspondencia auditoría55 por anclas',identical(key(old[keep]),key(h[j[keep]]))&&length(keep)>=40)
t55<-data.frame(fila_excel=which(old55)+1,EG_fuente_anterior=before$EDAD_GESTACIONAL_SEMANAS[old55],EG_fuente_nueva=d$EDAD_GESTACIONAL_SEMANAS[old55],peso_fuente_anterior=before$PESO_1_ANALITICO[old55],peso_fuente_nuevo=d$PESO_1_ANALITICO[old55],ambos_indeterminados_nueva=is.na(d$EDAD_GESTACIONAL_SEMANAS[old55])&is.na(d$PESO_1_ANALITICO[old55]),elegible_nueva=d$POB_PRINCIPAL[old55])
check('55 históricos sin nuevas recuperaciones fuente',sum(old55)==55&&!any(pchg[old55])&&!any(!is.na(d$EDAD_GESTACIONAL_SEMANAS[old55])&is.na(before$EDAD_GESTACIONAL_SEMANAS[old55]))&&all(changed(d$PESO_1_ANALITICO[old55],before$PESO_1_ANALITICO[old55])==FALSE))
# Full origin dictionary, including technical IDs and selection indicators.
dict<-data.frame(variable=names(d),origen=ifelse(names(d)%in%names_out,source_names[match(names(d),names_out)],'DERIVADA'),transformacion=ifelse(names(d)%in%names_out,'Normalizar encabezado; trimws y espacios; códigos desconocidos→NA; fuente original conservada','Ver función reconstruct en01_reconstruir_validar.R'),faltantes=sapply(d,function(x)sum(is.na(x))),clase=sapply(d,function(x)class(x)[1]),uso='Descriptivo/auditoría',referencia_regla='323aca2:preparar_base.py; reconstrucción versionada',stringsAsFactors=FALSE)
setdict<-function(v,origin,rule,reference,use='Descriptivo/auditoría'){q<-dict$variable%in%v;dict$origen[q]<<-origin;dict$transformacion[q]<<-rule;dict$referencia_regla[q]<<-reference;dict$uso[q]<<-use}
setdict(c('EDAD_ANALITICA','EDAD_CATEGORIA'),'EDAD','Número estricto; rango10–60; categorías<20/20–34/>=35','323aca2:analisis_frecuentista/02_preparar_base_analitica.R','Modelo y descriptivo')
setdict(c('EDAD_GESTACIONAL_SEMANAS','EDAD_GESTACIONAL_ANALITICA','EG_SEMANAS_ENTERAS','EG_DIAS','EG_ESTADO','EG_NUMERICA_FUENTE','EG_CODIFICACION_INVALIDA','EG_CATEGORIA_ANALITICA','EG_CAT'),'SEMANAS GESTACION','Semanas.días;0–6 días;20<=EG<45; cero/inválido→NA analítico; sin imputación','85560f0^:scripts/03_corregir_edad_gestacional.R; análisis vigente ignora imputados','Elegibilidad/descriptivo')
setdict(c('EG_IMPUTADO','EG_REGLA_IMPUTACION'),'Decisión de reconstrucción','0/NA: no se ejecutan imputaciones determinísticas; invalidación explícita de códigos','01_datos.R vigente excluye valores imputados')
setdict(c('PESO_1','PESO_2','PESO_1_ANALITICO','PESO_2_ANALITICO','ESTADO_PESO_1','ESTADO_PESO_2','PESO_1_PLAUSIBLE','PESO_2_PLAUSIBLE','PESO_FORMATO_INVALIDO'),'PESO; EG observada','Separación estricta n/n;300–7000g utilizable;<300g solo EG<=27; fecha con3partes no se convierte','AUDITORIA_DESENLACE.md; AUDITORIA_CALIDAD;03_corregir_edad_gestacional.R','Elegibilidad/descriptivo')
setdict(c('EMBARAZO_MULTIPLE_ANALITICO','FUENTE_MULTIPLE_ANALITICO'),'EMBARAZO MULTIPLE; PESO; SEXO','Doble→1;NO+dos pesos+dos sexos→1;evidencia discordante→NA;NO sin evidencia→0','AUDITORIA_MULTIPLES.md:regla empíricamente documentada','Selección/descripción independiente')
setdict(c('ROW_ID','FILA_EXCEL'),'Posición física en BASE_ORIGINAL','ID técnico1..N y filaExcel=ID+1;no es ID de paciente/gestación longitudinal','Regla técnica explícita','Trazabilidad')
setdict(c('MUERTE_INTRAUTERINA_BIN','Y'),'MUERTE INTRAUTERINA','SI→1;NO→0;otras categorías→NA; sin validación clínica independiente','scripts/final_metodos_resultados/01_datos.R','Desenlace')
setdict(c('POB_PRINCIPAL','POB_A','POB_B','POB_INDETERMINADA','MOTIVO_FLUJO'),'Multiplicidad;EG;PESO_1_ANALITICO','Únicas;EG prioritaria;principal22/500;tardía28/1000;A todas únicas;indeterminada falta ambos','Solicitud fase2 del usuario','Selección')
for(v in names(d)[grepl('_PARSEADA$|^ANIO_',names(d))])setdict(v,sub('_PARSEADA$|^ANIO_','',v),'Solo d/m/Y exacto y válido; derivados fecha/año; sin reemplazar1899','05_fechas_comprobaciones.R vigente')
for(v in c('SCORE_ANALITICO','NO_CONTROLES_NUM','SESIONES_NUM'))setdict(v,sub('_ANALITICO$|_NUM$','',v),'Conversión numérica estricta;textoNO no pasa a0;SCORE241 no corregido sin evidencia nueva','scripts/final_metodos_resultados/01_datos.R')
for(v in names(levels_expected)){dict$uso[dict$variable==v]<-'Modelo ajustado/descriptivo';dict$transformacion[dict$variable==v]<-paste('Trimws; conservarNA y niveles:',paste(levels_expected[[v]],collapse='|'))}
check('Fuente inalterada',identical(source_hash,sha(source_file)))
controls<-do.call(rbind,checks);passed<-!any(controls$critico&!controls$satisfactorio)
attr(d,'raw_fuente_original')<-raw
attr(d,'procedencia')<-list(archivo=source_file,hoja='BASE_ORIGINAL',sha256=source_hash,script='scripts/reconstruccion_reanalisis/01_reconstruir_validar.R (borrador fase5)' ,columnas_de_hoja_limpia_reutilizadas=FALSE,RDS_historico_como_fuente=FALSE)
attr(d,'validacion')<-list(aprobada=passed,limitaciones=controls$detalle[!controls$satisfactorio&!controls$critico])
saveRDS(d,file.path(out,'base_analitica_corregida.rds'))
saveRDS(before,file.path(out,'comparador_fuente_anterior_reconstruido.rds'))
write.xlsx(list(controles=controls,cambios245=ledger,pesos46=pesos,revision44EG=egreview,flujo=flows,etiquetas=labels,comparacion_historica=recon_comp,seguimiento55=t55,estados_EG=as.data.frame(table(d$EG_ESTADO,d$Y)),indeterminados=as.data.frame(table(Y=d$Y,ambos=is.na(d$EDAD_GESTACIONAL_SEMANAS)&is.na(d$PESO_1_ANALITICO)))),file.path(out,'01_validacion_reconstruccion.xlsx'),overwrite=FALSE)
write.xlsx(dict,file.path(out,'02_diccionario_variables.xlsx'),overwrite=FALSE)
write.csv(controls,file.path(out,'controles_validacion.csv'),row.names=FALSE)
write.csv(flows,file.path(out,'flujo_reconstruccion.csv'),row.names=FALSE)
write.csv(data.frame(FILA_EXCEL=d$FILA_EXCEL,Y=d$Y,motivo=d$MOTIVO_FLUJO,principal=d$POB_PRINCIPAL,A=d$POB_A,B=d$POB_B),file.path(out,'filas_poblaciones.csv'),row.names=FALSE)
report <- c('# Validación privada de reconstrucción desde BASE_ORIGINAL',
 paste('Validación técnica crítica:',if(passed)'APROBADA CON LIMITACIONES DOCUMENTADAS' else 'NO SUPERADA'),
 'Período definitivo 2018–2023. Fuente corregida BASE_ORIGINAL; la hoja limpia histórica se utiliza solo como comparación. No se leen RDS históricos como fuente.',
 'No se encontró el creador inicial completo. Se conserva el reemplazo explícito basado en reglas documentadas.',
 'Los controles siguientes verifican cambios agregados, importación independiente, plausibilidad y simetría. No constituyen adjudicación clínica.',
 capture.output(print(controls,row.names=FALSE)),capture.output(print(flows,row.names=FALSE)),
 'Las discrepancias históricas de derivación no se presentan como recuperaciones nuevas. No se imputan EG ni se incorporan selectivamente muertes indeterminadas.',
 'La EG válida tiene prioridad sobre el peso. Los pesos con formato de fecha no se reinterpretan sin evidencia.',
 'Se preservan fechas no interpretables/fuera del período sin redefinir la población ni las estimaciones; cobertura institucional previa no acreditada.',
 'Persisten incertidumbre de medición, temporalidad diagnóstica, selección y confusión. Los productos de auditoría por fila son privados y nunca destinados a Git.',
 capture.output(sessionInfo()))
writeLines(report,file.path(out,'01_informe_validacion.md'));writeLines(source_hash,file.path(out,'sha256_fuente.txt'))
capture.output(sessionInfo(),file=file.path(out,'sessionInfo_reconstruccion.txt'))
print(controls);print(flows);print(table(Y=d$Y,ambos=is.na(d$EDAD_GESTACIONAL_SEMANAS)&is.na(d$PESO_1_ANALITICO)))
if(!passed)stop('Errores críticos: consultar01_informe_validacion.md. No ejecutar modelos.')
