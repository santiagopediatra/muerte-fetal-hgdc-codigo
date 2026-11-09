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
# Sensibilidad de la forma funcional de la edad materna en el modelo principal (Firth)

# Reutiliza EXACTAMENTE base, población, referencias, fórmula y controles de
# scripts/reconstruccion_reanalisis/02_descriptivos_modelos.R (población 'Principal').
# Escribe solo en private_data/analisis_complementarios/; las tablas revisadas se copian a results/.
suppressPackageStartupMessages({library(logistf); library(splines)})

proj <- root
src  <- hgdc_private_path(hgdc_results())
out  <- file.path(root, 'private_data', 'analisis_complementarios', 'sensibilidad_edad'); dir.create(out, showWarnings = FALSE, recursive = TRUE); out <- hgdc_private_path(out)

# ---- Base y especificación (copiadas literalmente del script del modelo principal) ----
d <- readRDS(file.path(src, 'base_analitica_corregida.rds'))
stopifnot(isTRUE(attr(d, 'validacion')$aprobada), identical(attr(d, 'procedencia')$hoja, 'BASE_ORIGINAL'))
refs <- c(NACIONALIDAD='ECUATORIANA',PAREJA_ESTABLE='SI',INSTRUCCION='SECUNDARIA',ETNIA_MINORITARIA='NO',HIPERTENSIVOS='Ninguno',DIABETES_EN_EL_EMBARAZO='NO',IVU='NO',ANTECEDENTE_CIRUGIA_UTERINA='NO',RPM='NO',DPP='NO')
for (v in names(refs)) d[[v]] <- relevel(factor(d[[v]]), refs[[v]])
f <- Y~EDAD_ANALITICA+HIPERTENSIVOS+DIABETES_EN_EL_EMBARAZO+IVU+ANTECEDENTE_CIRUGIA_UTERINA+NACIONALIDAD+PAREJA_ESTABLE+INSTRUCCION+ETNIA_MINORITARIA+RPM+DPP
control   <- logistf.control(maxit=500,lconv=1e-7,gconv=1e-7,xconv=1e-7)
plcontrol <- logistpl.control(maxit=500)
z <- d[d$POB_PRINCIPAL & complete.cases(d[all.vars(f)]), ]
conv_ok <- function(m) all(abs(m$conv) <= c(control$lconv, control$gconv, control$xconv)) &&
  all(is.finite(m$ci.lower)) && all(is.finite(m$ci.upper)) && max(m$pl.iter) < plcontrol$maxit

# ---- Paso A: verificación ----
fit <- function(form) logistf(form, z, control = control, plcontrol = plcontrol)
cat('Ajustando modelo principal (edad lineal)...\n'); flush.console()
m_lin <- fit(f)
i <- 'DPPSI'; dpp <- round(exp(c(coef(m_lin)[i], m_lin$ci.lower[i], m_lin$ci.upper[i])), 2)
okA <- nrow(z) == 23138 && sum(z$Y) == 418 && all(dpp == c(6.86, 3.82, 11.53)) && conv_ok(m_lin)
verifA <- sprintf('Paso A: n = %d; muertes = %d; DPP ORa %s (%s–%s); convergente = %s -> %s',
                  nrow(z), sum(z$Y), sub('.', ',', dpp[1], fixed = TRUE), sub('.', ',', dpp[2], fixed = TRUE),
                  sub('.', ',', dpp[3], fixed = TRUE), conv_ok(m_lin), if (okA) 'VERIFICADO' else 'NO COINCIDE')
cat(verifA, '\n')
if (!okA) stop('El modelo principal no se reproduce: se detiene.')

# Términos de las 16 covariables distintas de la edad (orden del modelo principal)
covs <- data.frame(
  termino = c('HIPERTENSIVOSEclampsia','HIPERTENSIVOSHTA inducida por el embarazo','HIPERTENSIVOSHTA preexistente','HIPERTENSIVOSPreeclampsia',
              'DIABETES_EN_EL_EMBARAZOGestacional','DIABETES_EN_EL_EMBARAZOPreexistente','IVUSI','ANTECEDENTE_CIRUGIA_UTERINASI',
              'NACIONALIDADEXTRANJERA','PAREJA_ESTABLENO','INSTRUCCIONNINGUNA','INSTRUCCIONPRIMARIA','INSTRUCCIONSUPERIOR',
              'ETNIA_MINORITARIASI','RPMSI','DPPSI'),
  Covariable = c('Eclampsia','HTA inducida por embarazo','HTA preexistente','Preeclampsia','Diabetes gestacional','Diabetes preexistente',
                 'IVU registrada','Cirugía uterina previa','Nacionalidad extranjera','Sin pareja estable','Escolaridad: ninguna',
                 'Escolaridad: primaria','Escolaridad: superior','Etnia no mestiza registrada','RPM','DPP'),
  stringsAsFactors = FALSE)
stopifnot(setequal(covs$termino, setdiff(names(coef(m_lin)), c('(Intercept)', 'EDAD_ANALITICA'))))

# Componentes no lineales del spline cúbico restringido (base de potencias truncadas) con los MISMOS
# nudos que ns(): genera el mismo espacio de funciones que ns(x, df) y contiene a x, de modo que el
# modelo lineal queda anidado por nombre de término y anova(method = 'nested') es aplicable.
rcs_nl <- function(x, kn) {
  k <- length(kn); p3 <- function(u) pmax(u, 0)^3
  sapply(seq_len(k - 2), function(j) p3(x - kn[j]) - p3(x - kn[k - 1]) * (kn[k] - kn[j]) / (kn[k] - kn[k - 1]) +
           p3(x - kn[k]) * (kn[k - 1] - kn[j]) / (kn[k] - kn[k - 1]))
}

f2 <- function(x) formatC(x, format = 'f', digits = 2, decimal.mark = ',')
f3 <- function(x) formatC(x, format = 'f', digits = 3, decimal.mark = ',')
fp <- function(p) ifelse(p < 0.001, '<0,001', f3(p))
orci <- function(m, t) paste0(f2(exp(coef(m)[t])), ' (', f2(exp(m$ci.lower[t])), '–', f2(exp(m$ci.upper[t])), ')')

res <- list(); tests <- list(); age_spline <- list()
for (k in c(3, 4)) {
  # ---- Paso B: spline natural ns(edad, df = k), nudos en percentiles por defecto ----
  f_ns <- update(f, as.formula(sprintf('. ~ . - EDAD_ANALITICA + ns(EDAD_ANALITICA, df = %d)', k)))
  cat(sprintf('\nAjustando spline ns(df = %d)...\n', k)); flush.console()
  m_ns <- fit(f_ns)
  kn <- sort(c(attr(ns(z$EDAD_ANALITICA, df = k), 'Boundary.knots'), attr(ns(z$EDAD_ANALITICA, df = k), 'knots')))
  cat('Nudos (años):', paste(f2(kn), collapse = '; '), '\n')

  # ---- Paso C: prueba de razón de verosimilitud penalizada (anidada) ----
  KN <- kn; nl <- function(x) rcs_nl(x, KN)
  f_rcs <- update(f, . ~ . + nl(EDAD_ANALITICA))
  m_rcs <- fit(f_rcs)
  # La log-verosimilitud penalizada de Firth difiere solo en una constante (log|det A| del cambio de base),
  # igual en el modelo completo y el nulo; por eso la equivalencia se comprueba en coeficientes y predictor lineal.
  equiv <- max(abs(coef(m_rcs)[covs$termino] - coef(m_ns)[covs$termino])) < 1e-6 &&
    max(abs(m_rcs$linear.predictors - m_ns$linear.predictors)) < 1e-6 &&
    qr(cbind(1, ns(z$EDAD_ANALITICA, df = k), z$EDAD_ANALITICA, nl(z$EDAD_ANALITICA)))$rank == k + 1
  if (!equiv) stop('La reparametrización no es equivalente a ns(df = ', k, '): se detiene.')
  a <- anova(m_lin, m_rcs, method = 'nested')
  tests[[k]] <- data.frame(Modelo = sprintf('Spline natural ns(df = %d) vs. lineal', k), Nudos = paste(f2(kn), collapse = '; '),
                           Chi2 = f3(a$chisq), gl = a$df, p = fp(a$pval),
                           Convergente = conv_ok(m_ns) && conv_ok(m_rcs), Equivalencia_ns = equiv)

  # ---- Paso D: comparación de ORa ----
  bl <- coef(m_lin)[covs$termino]; bs <- coef(m_ns)[covs$termino]
  cambio <- 100 * (bs - bl) / bl
  res[[k]] <- data.frame(orci(m_ns, covs$termino), paste0(formatC(cambio, format = 'f', digits = 1, decimal.mark = ','), ' %'),
                         ifelse(abs(cambio) > 10, 'Sí', 'No'), cambio)
  names(res[[k]]) <- c(sprintf('ORa (IC95 %%) spline df = %d', k), sprintf('Cambio log-OR df = %d', k),
                       sprintf('Cambio >10 %% df = %d', k), sprintf('cambio_num_df%d', k))
}

tabla <- data.frame(Covariable = covs$Covariable, 'ORa (IC95 %) edad lineal' = orci(m_lin, covs$termino),
                    res[[3]][, 1:3], res[[4]][, 1:3], check.names = FALSE, row.names = NULL)
pruebas <- rbind(tests[[3]], tests[[4]])

# ---- Paso F: exportación ----
write.csv(tabla, file.path(out, 'edad_spline_ORa.csv'), row.names = FALSE, fileEncoding = 'UTF-8')
write.csv(pruebas, file.path(out, 'edad_spline_no_linealidad.csv'), row.names = FALSE, fileEncoding = 'UTF-8')
capture.output(sessionInfo(), file = file.path(out, 'sessionInfo_edad_spline.txt'))

# ---- Paso G: consola ----
options(width = 220)
cat('\n===== Verificación (paso A) =====\n', verifA, '\n', sep = '')
cat('\n===== Prueba de no linealidad (razón de verosimilitud penalizada, anidada) =====\n')
print(pruebas, row.names = FALSE, right = FALSE)
cat('\n===== ORa: edad lineal vs. spline natural =====\n')
print(tabla, row.names = FALSE, right = FALSE)
for (k in c(3, 4)) {
  s <- covs$Covariable[abs(res[[k]][[4]]) > 10]
  cat(sprintf('\nCambio >10 %% en log-OR con df = %d: %s', k, if (length(s)) paste(s, collapse = ', ') else 'ninguna'))
}
cat('\nNota: el cambio relativo en log-OR se inestabiliza cuando la log-OR lineal es cercana a 0 (OR ≈ 1).\n')
cat('Salida:', out, '\n')
