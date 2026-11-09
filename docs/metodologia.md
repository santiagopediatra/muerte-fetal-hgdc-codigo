# Metodología — HGDC 2018–2023

Resumen técnico para orientar el código. La descripción completa está en el manuscrito, que no se incluye en este repositorio. Las cifras citadas proceden de `results/`.

## Fuente y reconstrucción

- **Fuente:** hoja `BASE_ORIGINAL` de la base maestra corregida. La versión anterior y su hoja limpia se usan solo como comparadores.
- **Reconstrucción:** el script 01 rehace la base analítica con reglas explícitas, sin imputación. Ejecuta 46 controles; 43 son críticos.
- **Limitaciones no críticas declaradas:**
  - fechas fuera del período;
  - pesos con formato de fecha, que no se reinterpretan;
  - no se localizó el programa inicial que creó la base maestra.
- **Origen del código:** el script 01 es la versión depurada del reconstructor histórico. Se retiraron las referencias a filas concretas sin cambiar reglas ni resultados.

## Población y desenlace

- **Unidad:** evento obstétrico, que corresponde a una fila de la base.
- **Población principal:** gestaciones únicas con EG válida ≥ 22 semanas o, si falta la EG, peso fetal ≥ 500 g. La EG válida tiene prioridad sobre el peso.
- **Sensibilidades de elegibilidad:**
  - A: todas las gestaciones únicas.
  - B: EG ≥ 28 semanas o, si falta la EG, peso ≥ 1.000 g.
  - C: población principal más todas las gestaciones únicas con EG y peso indeterminados, incorporadas de forma simétrica en ambos grupos del desenlace.
- **Gestaciones múltiples:** fuera del análisis principal; se describen por separado.
- **Desenlace:** muerte fetal registrada (`MUERTE INTRAUTERINA`: SI = 1, NO = 0), sin adjudicación clínica independiente.

## Modelos

- **Principal:** regresión logística de Firth (`logistf`) sobre casos completos (23.138 eventos, 418 muertes).
  - Covariables: edad (continua, por año), trastornos hipertensivos, diabetes, IVU, cirugía uterina previa, nacionalidad, pareja estable, escolaridad, etnia minoritaria, RPM y DPP.
  - Referencias: Ninguno, NO, ECUATORIANA, SI (pareja estable) y SECUNDARIA.
  - IC95 % por perfil de verosimilitud penalizada.
  - No se seleccionaron covariables por significación.
- **Comparadores:**
  - logística convencional con la misma muestra;
  - Bayes (CmdStan): priors normales con DE 0,7, 1,5 y 2,5; 4 cadenas; 2.000 iteraciones de calentamiento y 2.000 de muestreo; semilla 1409.
- **Secundarios:** modelos por dominio, pares de Firth con variables adicionales y Bayes sin y con Score Mamá.
- **Complementarios:**
  - OR crudas univariables de Firth (Tabla 15S, STROBE 16a);
  - edad modelada con splines cúbicos naturales de 3 y 4 grados de libertad (prueba de no linealidad: p = 0,927 y 0,804);
  - exclusión de edades < 14 y > 50 años (22 eventos, ninguna muerte).

## Limitaciones

- Medición clínica no adjudicada.
- Flujo institucional previo a la base desconocido.
- Temporalidad diagnóstica no determinada.
- Posible selección diferencial y confusión residual.
- Independencia entre eventos supuesta: no hay identificador materno estable.

No interpretar las asociaciones inversas como protección causal ni las proporciones hospitalarias como incidencia poblacional.
