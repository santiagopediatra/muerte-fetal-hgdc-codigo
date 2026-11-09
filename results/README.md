# Resultados agregados

Contiene solo tablas y figuras agregadas, revisadas antes de su difusión ([../docs/revision_divulgacion.md](../docs/revision_divulgacion.md)). Ningún archivo contiene registros individuales.

## tablas/principales/

**Origen de los archivos:** son las salidas del validador final (versión validada del 10/10/2026, 760 de 760 controles), copiadas sin modificar. Los nombres `tabla_NN.csv` y `indice_tablas.csv` siguen la numeración interna de esa versión.

**Correspondencia con el manuscrito vigente:** el manuscrito vigente (Metodología y tablas, y Suplemento reorganizado, 11/10/2026) usa otra numeración. La correspondencia se estableció por **título y contenido**, no por número. Las cifras se cotejaron una a una con el manuscrito vigente:

- Las únicas diferencias son de presentación: redondeo a menos decimales, `<0,001` y `>0,999` en lugar de `0,0000` y `1,0000`, espaciado y N o eventos trasladados al encabezado.
- No hay discrepancias de valor.

| Archivo | Contenido (título) | Manuscrito vigente | Estado |
|---|---|---|---|
| tabla_01 | Características maternas, sociodemográficas y de atención prenatal de las gestaciones únicas elegibles | Tabla 1 | Incluida |
| tabla_02 | Características clínicas, obstétricas y de la gestación en la población principal | Tabla 2 | Incluida |
| tabla_03 | Modelo multivariable principal: Firth y Bayes, 23.138 casos completos y 418 muertes | Tabla 3 | Incluida |
| tabla_05 | Sensibilidad de las ORa de Firth a la definición de la población | Tabla 4 | Incluida |
| tabla_06 | Poblaciones, desenlace y pérdidas por covariables | Tabla S1 | Incluida |
| tabla_07 | Denominadores, datos faltantes y resúmenes continuos completos de la población principal | Tabla S2 | **Excluida** |
| tabla_08 | Características de los eventos de gestaciones múltiples | Tabla S3 | **Excluida** |
| tabla_09 | Todas las covariables de Firth en las cuatro poblaciones | Tabla S4 | Incluida |
| tabla_10 | Sensibilidad bayesiana completa a las distribuciones previas | Tabla S5 | Incluida |
| tabla_11 | Comprobaciones predictivas posteriores para los tres priors | Tabla S6 | Incluida |
| tabla_12 | Modelos bayesianos sin y con Score Mamá | Tabla S7 | Incluida |
| tabla_13 | Pares de Firth con variables adicionales | Tabla S8 | Incluida |
| tabla_14 | Diagnósticos de los cinco ajustes bayesianos | Tabla S9 | Incluida |
| tabla_15 | Distribuciones categóricas adicionales de gestaciones únicas elegibles | Tabla S10 | **Excluida** |
| tabla_16 | Distribución por año de ingreso registrado y anomalías residuales de fechas | Tabla S11 | Incluida |
| tabla_17 | Regresión logística convencional comparativa en las cuatro poblaciones | Tabla S12 | Incluida |
| tabla_04 | Modelos secundarios por dominio reestimados en la población principal | Tabla S23 | Incluida |

**No incluidas porque su código no forma parte de este depósito:** Tablas 5 y 6, y S13–S22 y S24 del manuscrito vigente. Corresponden a los modelos guiados por DAG, la sensibilidad temporal de 2021 y el modelo sin RPM ni DPP.

## tablas/complementarias/

| Archivo | Contenido | Script |
|---|---|---|
| `tabla15S_or_crudas.csv` | OR crudas y ajustadas de las 17 covariables (STROBE 16a) | 07 |
| `edad_spline_ORa.csv`, `edad_spline_no_linealidad.csv` | Edad lineal frente a splines naturales con 3 y 4 grados de libertad | 08 |
| `edades_extremas_ORa.csv` | Modelo principal sin edades < 14 y > 50 años | 09 |

**Formato de los CSV:** UTF-8, separador coma, decimales con coma y celdas entre comillas.

## figuras/

Salidas de los scripts 06 y 06b:

- `flujo_poblacion_principal` (.png, .pdf y .mmd): flujo de selección de la población principal.
- `comparacion_OR_poblaciones` (.png y .pdf): ORa de DPP, RPM, cirugía uterina previa y nacionalidad en las cuatro poblaciones.

Son idénticas byte a byte a las Figuras 1 y 2 del manuscrito del 09/10/2026. **No son las figuras del manuscrito vigente**:

- su Figura 1 es un diagrama rediseñado, generado fuera de este depósito;
- ya no incluye la comparación de OR.

## informes/

- Resumen de resultados definitivos.
- Revisión STROBE.

## referencias_historicas/

- `modelo_principal_frecuentista_historico.csv`: coeficientes agregados de una especificación anterior, que incluía múltiples y no restringía la elegibilidad. El script 06 lo usa solo como referencia en la comparación antes/después; no es un resultado vigente.
