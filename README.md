# PY2 Sistema de Eventos Comerciales

![Lenguaje](https://img.shields.io/badge/Lenguaje-Haskell-blue)
![Estado](https://img.shields.io/badge/Estado-En%20desarrollo-yellow)
![Versión](https://img.shields.io/badge/Versión-1.0-orange)
![Licencia](https://img.shields.io/badge/Licencia-Académico-lightgrey)

Sistema desarrollado en **Haskell** que permite registrar, procesar y analizar eventos.

El sistema simula un flujo continuo de datos, permitiendo aplicar transformaciones, análisis estadísticos y almacenamiento en archivos CSV.

---

##  Integrantes

- **Alice Arias Salazar**  

---

## Información académica

- **Curso:** Lenguajes de Programación  
- **Semestre:** I Semestre, 2026  
- **Proyecto:** Proyecto Programado #2  
- **Fecha de entrega:** 06/05/2026  
- **Estatus:** (Pendiente / Excelente / Muy Buena, etc.)

---

##  Descripción

Este sistema simula el comportamiento de una plataforma digital que genera eventos a partir de la actividad de los usuarios.

Cada evento representa una acción (como compras, visualizaciones o devoluciones) y es procesado mediante técnicas del **paradigma funcional**, incluyendo:

- Transformación de datos  
- Filtrado  
- Agregación  
- Análisis temporal  

El sistema funciona en consola mediante un menú interactivo y maneja eventos como un flujo continuo de información.

---

##  Funcionalidades

###  Transformación de eventos
- Aplicación de impuesto del 13% a compras  
- Identificación de eventos de alto valor  

###  Análisis de datos
- Cálculo del monto total  
- Promedio por categoría y año  

### Análisis temporal
- Mes con mayor monto  
- Día más activo  
- Evento más antiguo y más reciente  
- Resumen por intervalos  

### Búsqueda
- Filtrado de eventos por rango de fechas  

###  Estadísticas
- Cantidad de eventos por categoría  
- Evento con mayor y menor valor  
- Día con mayor actividad  

---

##  Características técnicas

- Uso de **tipos algebraicos (`data`)**  
- Programación funcional pura  
- Funciones de orden superior (`map`, `filter`, `fold`)  
- Manejo de errores con `Maybe` y `Either`  
- Inmutabilidad de datos  
- Modularidad en archivos (`Types`, `Services`, `Utils`)  
- Persistencia opcional con archivos CSV  
- Generación automática de eventos (sin librerías externas)  

---

##  Persistencia de datos (extra)

El sistema permite guardar eventos en archivos `.csv`, logrando:

- Mantener datos entre ejecuciones  
- Evitar pérdida de información  
- Simular sistemas reales  

---

##  Ejecución

Desde la raíz del proyecto:

```bash
runhaskell app/Main.hs
