Efecto de las fluctuaciones del tipo de cambio en los precios en México (2003–2024)
Tesis de licenciatura en Economía
Universidad Autónoma Metropolitana — Unidad Azcapotzalco
División de Ciencias Sociales y Humanidades

Autor: Samuel Valentín Maldonado Chávez
Asesor: Dr. Pablo Javier Sánchez Buelna y Dr. Owen Eli Ceballos Mina
Ciudad de México, diciembre de 2025


Descripción
Este repositorio contiene el código y los datos del modelo econométrico desarrollado para la tesis de licenciatura, cuyo objetivo es analizar el traspaso del tipo de cambio a los precios en México (Exchange Rate Pass-Through, ERPT) durante el periodo 2003–2024, bajo el régimen de metas de inflación explícitas del Banco de México.

El análisis estima funciones de impulso–respuesta (IRF) y elasticidades de traspaso acumuladas (ETA) para distintos índices de precios, con el fin de evaluar la magnitud y heterogeneidad del traspaso a lo largo de la cadena productiva.


Pregunta de investigación
¿En qué medida las fluctuaciones del tipo de cambio nominal se transmiten a los precios internos en México durante el periodo 2003–2024, bajo el régimen de metas de inflación explícitas del Banco de México?


Hipótesis
El traspaso del tipo de cambio a los precios en México sigue un patrón decreciente a lo largo de la cadena productiva: el impacto sobre los precios al productor es mayor que sobre los precios al consumidor, debido a la absorción parcial del choque cambiario en los costos intermedios y a la política monetaria del Banco de México.


Metodología
Se estima un modelo VAR con variables exógenas (VAR-X), siguiendo la metodología de Capistrán, Ibarra-Ramírez y Ramos-Francia (2011), con datos mensuales de enero 2003 a diciembre 2024.
Variables endógenas
Variable
Descripción
d1Y_end
Δlog PIB México
d1R_end
Tasa de interés doméstica (1ª diferencia)
d1S
Δlog Tipo de cambio MXN/USD — variable impulso
d1P_C
Δlog INPC — modelo base
d1PP_c
Δlog Precios al productor comerciables
d1PP_nc
Δlog Precios al productor no comerciables
d1PC_c
Δlog Precios al consumidor comerciables
d1PC_nc
Δlog Precios al consumidor no comerciables
d1P_ayc
Δlog Precios administrados y concertados

Variables exógenas
Variable
Descripción
d1Y_exo
Δlog PIB extranjero (EE.UU.)
d1R_exo
Tasa de interés extranjera (Fed Funds)
d1P_exo
Δlog Precios externos
d1P_comm
Δlog Precios de commodities

Pruebas y análisis realizados
Prueba de estacionaridad ADF
ACF y PACF
Selección de rezagos (VARselect, lag.max = 12)
Causalidad de Granger
Funciones de impulso–respuesta (IRF) ortogonalizadas con bootstrap
Elasticidades de traspaso acumuladas (PT)
Regresión móvil con ventana de 48 meses


Estructura del repositorio
VAR-X-tesis/

├── ERPT_VAR_X.R        ← Código principal del modelo VAR-X

├── VAR-X ERPT DF.xlsx  ← Base de datos (series de tiempo mensuales)

├── README.md           ← Este archivo

├── LICENSE             ← Licencia MIT

└── .gitignore


Requisitos
Software: R (≥ 4.0)

Paquetes de R:

install.packages(c("readxl", "tseries", "forecast", "vars",

                   "lmtest", "ggplot2", "zoo"))


Cómo replicar los resultados
Clona el repositorio:

git clone https://github.com/samuelvalentinn-beep/VAR-X-tesis

Coloca el archivo VAR-X ERPT DF.xlsx en el mismo directorio que el script.

Abre ERPT_VAR_X.R en RStudio y ejecuta el script completo.


Referencia principal
Capistrán, C., Ibarra-Ramírez, R. y Ramos-Francia, M. (2011). Exchange rate pass-through to prices: Evidence from Mexico. Banco de México, Working Paper 2011-12.


Licencia
Este repositorio está bajo la licencia MIT.
Los datos son de uso académico. Si utilizas este código, por favor cita la tesis original.


