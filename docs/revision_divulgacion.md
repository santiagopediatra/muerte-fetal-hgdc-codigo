# Revisión de divulgación de los resultados agregados

Antes de incorporar cualquier tabla o figura a `results/`, se revisó el riesgo de identificación de personas. La revisión se hizo el 5 de noviembre de 2026 y buscó:

- recuentos de 1 a 4;
- valores extremos individuales (mínimos y máximos);
- grupos muy pequeños;
- combinaciones de atributos raros con el desenlace.

## Excluidas del repositorio, pendientes de decisión del titular

Ninguna tabla se modificó. Las tres tablas siguientes se retiraron completas. Se identifican por **título y contenido**, porque la numeración cambió entre versiones del manuscrito.

**El mismo contenido sigue presente en el suplemento vigente** (`07_SUPLEMENTO_REORGANIZADO.docx`, 11/10/2026). Se comprobó el 6/11/2026 que conserva los mismos mínimos y máximos, las 5 muertes en múltiples y los diagnósticos con n = 1. El manuscrito no se modificó.

| Título | Archivo (validación 10/10) | Suplemento vigente | Manuscrito 09/10 | Motivo |
|---|---|---|---|---|
| Denominadores, datos faltantes y resúmenes continuos completos de la población principal | `tabla_07.csv` | Tabla S2 | no incluida | La columna «Mínimo–máximo» publica valores individuales extremos (edad materna, peso fetal y otras variables continuas) |
| Características de los eventos de gestaciones múltiples | `tabla_08.csv` | Tabla S3 | Tabla S2 («Registros de embarazos múltiples») | Solo 5 muertes. Los cuartiles de EG y de peso sobre 5 eventos se aproximan a valores individuales |
| Distribuciones categóricas adicionales de gestaciones únicas elegibles | `tabla_15.csv` | Tabla S10 | no incluida | 67 celdas con recuentos de 1 a 4. Diagnósticos de texto libre con n = 1, cruzados con el desenlace, en un solo hospital y un período conocido, incluidos consumo de drogas, salud mental y enfermedades congénitas raras. Es la de mayor riesgo |

Para publicar estas tablas, se proponen dos opciones:

- suprimir o agrupar las celdas menores de 5 («<5» u «Otras») y los mínimos y máximos;
- o no publicarlas.

## Incluidas, con celdas pequeñas que conviene conocer

- **Tablas 1 y 2 del manuscrito vigente, Tabla 15S y edades extremas (script 09):** recuentos de 1 a 4 en categorías clínicas comunes (eclampsia, HTA preexistente, diabetes preexistente, IVU, datos faltantes). No combinan atributos raros con fechas ni con lugar.
- **Distribución por año de ingreso registrado (Tabla S11 vigente, `tabla_16.csv`):** recoge errores de fecha («1900», n = 3; «2017», n = 2; «2024», n = 2). No identifican a nadie por sí solos.
- **`edades_extremas_conteos.csv`:** no se incluye en `results/`. Sus recuentos son 3 y 19 eventos sin muertes; su contenido ya figura en el texto del análisis.

## Regla para futuras salidas

Las salidas de los scripts se escriben en `private_data/`. Una salida solo pasa a `results/` después de esta revisión, y debe añadirse a la lista cerrada de `.gitignore` y al manifiesto.
