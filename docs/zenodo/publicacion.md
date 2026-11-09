# Publicación en GitHub y Zenodo

Revisión de la integración GitHub–Zenodo: noviembre de 2026.

## Cómo funciona la integración automática

- **Disparo:** una vez conectado un repositorio, Zenodo ingiere y archiva automáticamente cada nueva release publicada, sin un paso de revisión previo.
- **Pre-releases y borradores:** según el código de invenio-github, también se archivan las *pre-releases* publicadas; los borradores (*draft*) no.
- **Metadatos:** si existe `.zenodo.json` en la raíz, Zenodo ignora `CITATION.cff`.
- **Repositorios privados:** se ha documentado que la integración no archiva repositorios privados.
- **Decisión para este proyecto:** **no se activa la integración.** El depósito es manual.

## Procedimiento del depósito manual

1. **Contenido.** Mantener GitHub privado. El contenido del depósito es el commit revisado de `main`.
2. **Borrador en Zenodo.**
   - Crear un depósito nuevo de tipo *Software* y reservar el DOI (*Reserve DOI*).
   - Subir el ZIP revisado y completar los metadatos de `metadatos_borrador.json` y `descripcion.md`.
   - **Guardar como borrador.**
   - Un token personal de Zenodo con solo el permiso `deposit:write` permite crear y editar borradores, pero no publicarlos: publicar requiere `deposit:actions`.
3. **Cierre, tras la aprobación de los autores y del titular:**
   - completar `CITATION.cff` con los autores, la licencia, la fecha y el DOI reservado;
   - hacer un commit y crear la etiqueta `v1.0.0` sobre ese commit final. La etiqueta no se crea antes ni se mueve después;
   - generar el ZIP con `git archive --format=zip --prefix=muerte-fetal-hgdc-codigo-v1.0.0/ v1.0.0`;
   - sustituir el archivo del borrador por ese ZIP.
4. **Publicación.** Revisar el borrador y publicarlo desde la interfaz de Zenodo. Una versión publicada no puede borrarse; solo admite versiones nuevas.
5. **Después, opcional.** Hacer público el repositorio de GitHub y crear la release `v1.0.0` **sin** integración, para no generar un segundo DOI.

## Licencia

Pendiente de la decisión del titular de los derechos. Mientras no se defina, el depósito no concede ninguna licencia.
