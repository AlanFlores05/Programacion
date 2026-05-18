[ProyectoFinal_v0_CamposFlores.pdf](https://github.com/user-attachments/files/27945886/ProyectoFinal_v0_CamposFlores.pdf)
[DEFUNCIONES_Chiapas.xlsx](https://github.com/user-attachments/files/27945539/DEFUNCIONES_Chiapas.xlsx)
[Censos2010y2020_Chiapas.xlsx](https://github.com/user-attachments/files/27945533/Censos2010y2020_Chiapas.xlsx)
---
title: "Proyecto Final Demografía-9219: Tablas de Vida de Chiapas"
subtitle: "2010, 2019 y 2021 por Sexo"
author:
  - "Campos Ruiz Evelyn Paola"
  - "Flores Rico André Alán"
date: today
lang: es
format: pdf
---

```{r setup, include=FALSE}
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(kableExtra)
library(scales)
library(lubridate)

colores <- c("Hombres" = "#2166ac", "Mujeres" = "#d6604d")
tipos   <- c("2010" = "solid", "2019" = "dashed", "2021" = "dotdash")
```

Introducción

Chiapas es uno de los 32 estados que conforman los Estados Unidos Mexicanos,
ubicado en el sureste del país. Con una extensión territorial de 73,289 km²
divididos en 124 municipios, limita al norte con Tabasco, al este con
Guatemala, al sur con el Océano Pacífico y al oeste con Oaxaca y Veracruz.
Es la octava entidad más extensa del país y alberga una de las poblaciones
indígenas más numerosas de México —principalmente tzotzil, tzeltal, chol y
zoque—, lo que le confiere dinámicas demográficas y de mortalidad particulares.

En este trabajo construimos las **tablas de vida abreviadas** de este estado  para
los años 2010, 2019 y 2021, divididas por sexo.

Un breve contexto previo y que consideramos sería interestante tomar en cuenta, serian los siguientes: 

**1. Pobreza y rezago social.**
Chiapas ocupa el último lugar nacional en el Índice de Desarrollo Humano.
El acceso limitado a servicios de salud en zonas serranas e indígenas se
traduce en tasas de mortalidad infantil superiores a la media nacional:
aproximadamente **18 por 1,000 nacidos vivos** en 2010 (frente a 14
a nivel nacional).

**2. Alta fecundidad y estructura joven.**
En el año 2010 Chiapas registraba la tasa global de fecundidad más alta del país
(~3.2 hijos por mujer). Esta pirámide de base ancha reduce la tasa bruta de
mortalidad pero, conforme avanza la transición demográfica, la mortalidad en
edades adultas cobra mayor peso relativo.

**3. Sobremortalidad masculina en adultos jóvenes.**
Hay un exceso de mortalidad masculina en el periodo de eades entre los 20–34 años relacionados a causas
externas ( como l son las accidentes u homicidios) lo que logramos ver en la curva $\log(nM_x)$,
aunque es moderado en comparación con otros estados del norte del país.

**4. Impacto del COVID-19 en 2021.**
En este caso laas defunciones totales en Chiapas pasaron de 28,317 (2019) a 39,865 (2021),
un incremento del **41 %**. La alta prevalencia de comorbilidades metabólicas
(diabetes tipo 2, obesidad, hipertensión) y el acceso limitado a UCI
amplificaron el impacto, concentrado en adultos de 50–74 años y en hombres.

# Diagrama de flujo del proceso

```{r diagrama, echo=FALSE, fig.cap="Diagrama de flujo: construcción de tablas de vida abreviadas", fig.height=5}
pasos <- data.frame(
  x = rep(0.5, 7), y = 7:1,
  label = c(
    "1. Descargar defunciones (INEGI)\npor edad quinquenal, sexo y año de ocurrencia",
    "2. Descargar población censal prorateada (INEGI)\nCensos 2010 y 2020, Chiapas",
    "3. Estimar población media 2019 y 2021\nmediante crecimiento exponencial intercensal",
    "4. Calcular tasa central de mortalidad\nnMx = nDx / nNx",
    "5. Calcular nax, nqx, npx, lx, ndx, nLx, Tx, ex\n(fórmulas de tabla de vida abreviada)",
    "6. Construir cuadro de e0 por sexo y año",
    "7. Graficar nMx, lx, nqx, ndx, ex, nLx y analizar"
  )
)
flechas <- data.frame(
  x=rep(0.5,6), xend=rep(0.5,6),
  y=c(6.62,5.62,4.62,3.62,2.62,1.62),
  yend=c(6.38,5.38,4.38,3.38,2.38,1.38)
)
ggplot() +
  geom_segment(data=flechas, aes(x=x,y=y,xend=xend,yend=yend),
               arrow=arrow(length=unit(0.25,"cm"),type="closed"),
               color="#444444", linewidth=0.6) +
  geom_label(data=pasos, aes(x=x,y=y,label=label),
             size=2.7, fill="#e8f4f8", color="#1a1a1a",
             label.size=0.4, label.padding=unit(0.35,"lines")) +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(limits=c(0.5,7.5)) +
  theme_void() + theme(plot.margin=margin(5,5,5,5))
```

# Fórmulas que utilizamos

## Para el Crecimiento exponencial tenemos lo siguiente:

En el caso de la estimación de la población media de 2019 y proyectar la de 2021 a partir de
los censos 2010 y 2020, se utilizamos el modelo de crecimiento exponencial:

$$K(t) = K(t_0) \cdot e^{\,r\,(t - t_0)}$$

donde la tasa intrínseca de crecimiento por grupo de edad sería:

$$r = \frac{\ln\bigl(K(t_T)\bigr) - \ln\bigl(K(t_0)\bigr)}{t_T - t_0}$$

**Fechas de referencia a considerar:** $t_0 = 2010.4795$ (25 de junio de 2010) y
$t_T = 2020.2022$ (15 de marzo de 2020), con $\Delta t = 9.7227$ años.
La población estimada corresponde a mitad de año ($t = 2019.5$ y $t = 2021.5$).

## Tabla de vida (pero abreviada)

| Columna | Fórmula | Descripción |
|---------|---------|-------------|
| $nM_x$ | $nD_x / nN_x$ | Tasa central de mortalidad |
| $nq_x$ | $\dfrac{n \cdot nM_x}{1 + (n - na_x)\,nM_x}$ | Probabilidad de morir en $[x,\,x{+}n)$ |
| $np_x$ | $1 - nq_x$ | Probabilidad de sobrevivir |
| $l_x$ | $l_{x-n} \cdot np_{x-n}$, $\;l_0 = 100{,}000$ | Sobrevivientes a la edad exacta $x$ |
| $nd_x$ | $l_x \cdot nq_x$ | Muertes esperadas en $[x,\,x{+}n)$ |
| $nL_x$ | $n \cdot l_{x+n} + na_x \cdot nd_x$ | Años-persona vividos en $[x,\,x{+}n)$ |
| $T_x$ | $\displaystyle\sum_{a \geq x} nL_a$ | Años-persona vividos desde $x$ |
| $e_x$ | $T_x / l_x$ | Esperanza de vida en la edad $x$ |

: Columnas de la tabla de vida abreviada {tbl-colwidths="[12,42,46]"}

**Grupo abierto (85+):** $nq_{85} = 1$ y $nL_{85} = l_{85}/nM_{85}$.

**Fracción $na_x$** — tiempo vivido en el intervalo por quienes mueren en él:

- **Edad 0** (Coale-Demeny, diferenciado por sexo):

$$\text{Hombres:}\quad na_0 = \begin{cases} 0.330 & nM_0 \geq 0.107 \\ 0.045 + 2.684\,nM_0 & nM_0 < 0.107 \end{cases}$$

$$\text{Mujeres:}\quad na_0 = \begin{cases} 0.350 & nM_0 \geq 0.107 \\ 0.053 + 2.800\,nM_0 & nM_0 < 0.107 \end{cases}$$

- **Edad 1–4:** $\;na_1 = 1.5$
- **Edades 5+:** $\;na_x = n/2 = 2.5$
- **Grupo 85+:** $\;na_{85} = 1/nM_{85}$

# Código y cálculos que realizamos 

## Nuestros datos de entrada fueron:

```{r datos}
# ── Grupos de edad (inicio de cada intervaloo) ─────────────────────────
# Intervalos: [0], [1-4], [5-9], [10-14], ..., [80-84], [85+]
edades_x <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40,
              45, 50, 55, 60, 65, 70, 75, 80, 85)

# ── Defunciones Chiapas por año de ocurrencia (INEGI) ─────────────────
# Fuente: Estadísticas de Mortalidad, entidad de residencia: Chiapas.
# Registro total; año de ocurrencia: 2010, 2019, 2021.

def_H_2010 <- c(751, 237, 112, 137, 285, 407, 429, 520, 479, 497,
                558, 634, 726, 727, 894, 997,1057, 990,1567)
def_M_2010 <- c(575, 227, 112, 108, 165, 180, 173, 205, 223, 298,
                442, 463, 602, 613, 818, 946, 984, 899,1579)

def_H_2019 <- c(784, 239, 122, 132, 272, 433, 503, 510, 548, 636,
                688, 784,1029,1101,1219,1257,1482,1414,2200)
def_M_2019 <- c(674, 229,  87,  93, 137, 153, 184, 221, 298, 374,
                538, 671, 934,1048,1072,1191,1448,1324,2252)

def_H_2021 <- c(730, 233,  72, 131, 304, 454, 549, 588, 736, 876,
               1132,1261,1605,1765,2042,2100,2307,2251,3182)
def_M_2021 <- c(607, 218,  79,  90, 144, 180, 237, 279, 406, 543,
                733, 939,1333,1622,1691,1628,1981,1922,2898)

# ── Población prorateada Censo 2010 (INEGI, Chiapas) ──────────────────
# Fuente: INEGI, hoja "2010" — columnas "Proorrateo" Hombre/Mujer.
pob_H_2010 <- c(1022789.5,4394602.1,5678012.8,5620705.6,5592851.4,
                4876620.4,4261390.8,4079076.0,4016975.4,3394464.2,
                2861576.4,2434104.5,1894169.1,1496122.8,1109703.8,
                 885407.0, 587326.7, 359958.0, 302675.0)
pob_M_2010 <- c( 985824.0,4260281.0,5511360.7,5459685.2,5574772.1,
                5142515.0,4639443.1,4500291.2,4382317.7,3704611.2,
                3143145.9,2695091.8,2051134.7,1660283.4,1237257.2,
                1012533.6, 674111.1, 449201.2, 409609.7)

# ── Población prorateada Censo 2020 (INEGI, Chiapas) ──────────────────
# Fuente: INEGI, hoja "2020" — columnas "Proorrateo" Hombre/Mujer.
pob_H_2020 <- c( 921180.8,4170581.5,5465198.5,5566592.2,5474277.6,
                5177353.8,4872197.8,4537778.9,4341147.3,4071323.6,
                3820808.6,3339561.4,2698955.2,2262875.1,1710639.7,
                1236230.7, 849780.6, 524975.0, 434931.5)
pob_M_2020 <- c( 898747.4,4081722.4,5322602.1,5400760.2,5355924.9,
                5267407.8,5142528.3,4903524.3,4698733.9,4450742.8,
                4138866.9,3713262.2,3009378.9,2568660.1,1942355.8,
                1416859.8, 968743.2, 652939.9, 606873.0)
```

## La Función de crecimiento exponencial nos quedó como:

```{r expo}
# K(t) = K(t0) * exp(r * (t - t0))
# r    = log(K_T / K_0) / (tT - t0)
expo <- function(K_0, K_T, t_0, t_T, t) {
  dt  <- decimal_date(as.Date(t_T)) - decimal_date(as.Date(t_0))
  r   <- log(K_T / K_0) / dt
  h   <- t - decimal_date(as.Date(t_0))
  return(K_0 * exp(r * h))
}

# Aplicación por grupo de edad: proyectar 2019 y 2021
# t0 = 25-jun-2010 = 2010.4795 | tT = 15-mar-2020 = 2020.2022
t0_str <- "2010-06-25"
tT_str <- "2020-03-15"

pob_H_2019 <- mapply(expo, pob_H_2010, pob_H_2020,
                     t_0=t0_str, t_T=tT_str, t=2019.5)
pob_M_2019 <- mapply(expo, pob_M_2010, pob_M_2020,
                     t_0=t0_str, t_T=tT_str, t=2019.5)
pob_H_2021 <- mapply(expo, pob_H_2010, pob_H_2020,
                     t_0=t0_str, t_T=tT_str, t=2021.5)
pob_M_2021 <- mapply(expo, pob_M_2010, pob_M_2020,
                     t_0=t0_str, t_T=tT_str, t=2021.5)

# Verificación de nuestros totales
cat("Población total estimada Chiapas:\n")
cat(sprintf("  2019 H: %s  |  M: %s\n",
    format(round(sum(pob_H_2019)), big.mark=","),
    format(round(sum(pob_M_2019)), big.mark=",")))
cat(sprintf("  2021 H: %s  |  M: %s\n",
    format(round(sum(pob_H_2021)), big.mark=","),
    format(round(sum(pob_M_2021)), big.mark=",")))
```

## Para la Función principal: tabla de vida abreviada

```{r funcion_tv}
construir_tabla_vida <- function(edades, def, pob, sexo_str, anio_str) {
  n_g  <- length(edades)
  # Amplitud de cada intervalo: 1 para [0], 4 para [1-4], 5 para el resto
  n    <- c(1, 4, rep(5, n_g - 3), Inf)

  # Tasa central de mortalidad
  nMx  <- def / pob

  # Fracción del intervalo vivida por quienes mueren (nax)
  nax  <- c(NA, 1.5, rep(2.5, n_g - 3), NA)

  # Edad 0: Coale-Demeny (diferenciado por sexo)
  nax[1] <- if (sexo_str == "Hombres") {
    ifelse(nMx[1] >= 0.107, 0.330, 0.045 + 2.684 * nMx[1])
  } else {
    ifelse(nMx[1] >= 0.107, 0.350, 0.053 + 2.800 * nMx[1])
  }
  # Grupo abierto 85+
  nax[n_g] <- 1 / nMx[n_g]

  # Probabilidad de morir
  nqx      <- (n * nMx) / (1 + (n - nax) * nMx)
  nqx[n_g] <- 1.0
  npx      <- 1 - nqx

  # Sobrevivientes (radix = 100,000)
  lx       <- numeric(n_g); lx[1] <- 100000
  for (i in 2:n_g) lx[i] <- lx[i-1] * npx[i-1]

  ndx  <- lx * nqx

  # Años-persona vividos
  nLx          <- numeric(n_g)
  for (i in 1:(n_g-1)) nLx[i] <- n[i] * lx[i+1] + nax[i] * ndx[i]
  nLx[n_g]     <- lx[n_g] / nMx[n_g]

  Tx <- rev(cumsum(rev(nLx)))
  ex <- Tx / lx

  data.frame(
    x    = edades,
    n    = ifelse(is.infinite(n), NA, n),
    nDx  = round(def, 1),
    nNx  = round(pob, 0),
    nMx  = round(nMx,  6),
    nax  = round(nax,  4),
    nqx  = round(nqx,  6),
    npx  = round(npx,  6),
    lx   = round(lx,   2),
    ndx  = round(ndx,  2),
    nLx  = round(nLx,  2),
    Tx   = round(Tx,   2),
    ex   = round(ex,   2),
    Sexo = sexo_str,
    Anio = anio_str,
    stringsAsFactors = FALSE
  )
}
```

## Aqui tendriamos la construcción de las seis tablas que necsitamos:

```{r construir}
tv_H_2010 <- construir_tabla_vida(edades_x,def_H_2010,pob_H_2010,"Hombres","2010")
tv_M_2010 <- construir_tabla_vida(edades_x,def_M_2010,pob_M_2010,"Mujeres","2010")
tv_H_2019 <- construir_tabla_vida(edades_x,def_H_2019,pob_H_2019,"Hombres","2019")
tv_M_2019 <- construir_tabla_vida(edades_x,def_M_2019,pob_M_2019,"Mujeres","2019")
tv_H_2021 <- construir_tabla_vida(edades_x,def_H_2021,pob_H_2021,"Hombres","2021")
tv_M_2021 <- construir_tabla_vida(edades_x,def_M_2021,pob_M_2021,"Mujeres","2021")

tv_todas <- bind_rows(tv_H_2010,tv_M_2010,
                      tv_H_2019,tv_M_2019,
                      tv_H_2021,tv_M_2021) %>%
  mutate(Anio = factor(Anio, levels = c("2010","2019","2021")))
```

# Las Esperanzas de vida al nacer {#sec-e0}

```{r tabla_e0}
e0_tabla <- tv_todas %>%
  filter(x == 0) %>%
  select(Anio, Sexo, ex) %>%
  pivot_wider(names_from = Sexo, values_from = ex) %>%
  arrange(Anio) %>%
  mutate(Brecha = round(Mujeres - Hombres, 2))

kable(e0_tabla,
      caption   = "Chiapas: Esperanza de vida al nacer ($e_0$) por sexo y año",
      col.names = c("Año","Hombres","Mujeres","Brecha (M$-$H)"),
      digits    = 2,
      booktabs  = TRUE,
      align     = "cccc") %>%
  kable_styling(latex_options = c("striped","hold_position"),
                full_width = FALSE) %>%
  column_spec(4, bold = TRUE)
```

# Gráficaas

## Tasa central de mortalidad ($nM_x$)

```{r graf_nmx, fig.cap="Tasa central de mortalidad en escala logarítmica. La curva en forma de J es característica: alta mortalidad infantil, mínimo en adolescencia y aumento sostenido en adultos. El repunte en 2021 en los grupos 50-74 refleja el impacto COVID."}
ggplot(tv_todas, aes(x=x, y=nMx, color=Sexo, linetype=Anio)) +
  geom_line(linewidth=0.8) + geom_point(size=1.5) +
  scale_y_log10(labels=label_scientific()) +
  scale_color_manual(values=colores) +
  scale_linetype_manual(values=tipos) +
  labs(x="Edad (x)", y=expression(nM[x]~"(escala log)"),
       color="Sexo", linetype="Año") +
  theme_minimal(base_size=11) + theme(legend.position="bottom")
```

## Curva de sobrevivencia ($l_x$)

```{r graf_lx, fig.cap="Curva de sobrevivencia. La caída más pronunciada en 2021 a partir de los 50 años evidencia el exceso de mortalidad pandémico."}
ggplot(tv_todas, aes(x=x, y=lx, color=Sexo, linetype=Anio)) +
  geom_line(linewidth=0.8) +
  scale_y_continuous(labels=comma) +
  scale_color_manual(values=colores) +
  scale_linetype_manual(values=tipos) +
  labs(x="Edad (x)", y=expression(l[x]),
       color="Sexo", linetype="Año") +
  theme_minimal(base_size=11) + theme(legend.position="bottom")
```

## Probabilidad de morir ($nq_x$)

```{r graf_nqx, fig.cap="Probabilidad de morir por grupo de edad. El aumento en 2021 en edades maduras es consistente con el patrón de mortalidad por COVID-19."}
ggplot(tv_todas, aes(x=x, y=nqx, color=Sexo, linetype=Anio)) +
  geom_line(linewidth=0.8) +
  scale_color_manual(values=colores) +
  scale_linetype_manual(values=tipos) +
  labs(x="Edad (x)", y=expression(nq[x]),
       color="Sexo", linetype="Año") +
  theme_minimal(base_size=11) + theme(legend.position="bottom")
```

## Distribución de las muertes ($nd_x$)

```{r graf_ndx, fig.cap="Distribución de muertes por cada 100,000 nacimientos. En 2021 el pico se desplaza hacia edades menores respecto a 2019, reflejo del exceso de mortalidad pandémico en adultos."}
ggplot(tv_todas, aes(x=x, y=ndx, color=Sexo, linetype=Anio)) +
  geom_line(linewidth=0.8) +
  scale_y_continuous(labels=comma) +
  scale_color_manual(values=colores) +
  scale_linetype_manual(values=tipos) +
  labs(x="Edad (x)", y=expression(nd[x]),
       color="Sexo", linetype="Año") +
  theme_minimal(base_size=11) + theme(legend.position="bottom")
```

## Esperanza de vida por edades ($e_x$)

```{r graf_ex, fig.cap="Esperanza de vida restante por edad. La reducción de 2021 respecto a 2019 es especialmente marcada en hombres entre los 50 y 75 años."}
ggplot(tv_todas, aes(x=x, y=ex, color=Sexo, linetype=Anio)) +
  geom_line(linewidth=0.8) +
  scale_color_manual(values=colores) +
  scale_linetype_manual(values=tipos) +
  labs(x="Edad (x)", y=expression(e[x]~"(años)"),
       color="Sexo", linetype="Año") +
  theme_minimal(base_size=11) + theme(legend.position="bottom")
```

## Años-persona vividos ($nL_x$)

```{r graf_nlx, fig.cap="Años-persona vividos por grupo de edad."}
ggplot(tv_todas, aes(x=x, y=nLx, color=Sexo, linetype=Anio)) +
  geom_line(linewidth=0.8) +
  scale_y_continuous(labels=comma) +
  scale_color_manual(values=colores) +
  scale_linetype_manual(values=tipos) +
  labs(x="Edad (x)", y=expression(nL[x]),
       color="Sexo", linetype="Año") +
  theme_minimal(base_size=11) + theme(legend.position="bottom")
```

# Y ya para finalizar aqui esta nuestro análisis de los resultadosque obtuvimos

## Evolución 2010 a 2019

Entre estsos dos años, 2010 y 2019, podemos ver un incremento en la esperanza de vida al nacer
en ambos sexos, consistente con la transición epidemiológica del estado.
La principal fuente de ganancia es la **reducción de la mortalidad infantil**
($nM_0$): Chiapas redujo su TMI de aproximadamente 18 a 13 por 1,000 nacidos
vivos, resultado de la expansión del IMSS-Bienestar y los programas
Oportunidades/Prospera en comunidades rurales e indígenas.

En el caso de la gráfica de $nM_x$ en escala logarítmica se confirma el patrón en forma de
"J" característico: mortalidad elevada en la infancia, un mínimo en la
adolescencia temprana y crecimiento sostenido en edades adultas. La
**sobremortalidad masculina** en el tramo 20–35 años —atribuible a causas
externas— es visible pero moderada en comparación con entidades del norte.

## Impacto del COVID-19 en el 2021

El año 2021 representa una ruptura clara en la tendencia positiva:

- **$nM_x$ en grupos 50–74 años:** repunte notorio en ambos sexos,
  más pronunciado en hombres. Chiapas, con alta prevalencia de
  comorbilidades metabólicas y acceso limitado a UCI, registró un
  exceso de mortalidad particularmente severo.

- **Curva $l_x$:** la caída se acelera a partir de los 50 años en 2021,
  reflejando el exceso de mortalidad pandémico y la menor probabilidad de
  sobrevivir en esas edades.

- **Distribución $nd_x$:** el pico de muertes se desplaza hacia edades
  más jóvenes en relación con 2019, en lugar de concentrarse exclusivamente
  en los grupos de mayor edad.

- **Brecha de $e_0$ (Mujeres $-$ Hombres):** se amplía en 2021 como
  consecuencia de la mayor letalidad masculina por COVID-19.

## Particularidades que identificamos de Chiapas y es importante mencionar: 

La $e_0$ de Chiapas en los tres años analizados se ubica por debajo del
promedio nacional, evidenciando el efecto acumulado del rezago social sobre
la mortalidad. Sin embargo, la **trayectoria positiva 2010–2019** demuestra
que las mejoras en cobertura de salud han tenido impacto real y medible.

El **retroceso de 2021** fue amplificado en Chiapas por tres factores
concurrentes: alta prevalencia de comorbilidades en la población adulta,
infraestructura hospitalaria limitada y rezago relativo en el proceso de
vacunación. Esto lo distingue de entidades con mayor desarrollo, donde
el impacto del COVID-19 sobre la $e_0$ fue de menor magnitud.

# Nuestras conclusionees serían:

1. Las tablas de vida de Chiapas para 2010, 2019 y 2021 confirman una mejora
   sostenida en la sobrevivencia entre los dos primeros años, impulsada
   principalmente por la reducción de la mortalidad en menores de 5 años.

2. El COVID-19 genró en 2021 el mayor retroceso en la esperanza de vida de
   Chiapas en décadas, con un impacto diferencial por sexo que amplió la
   brecha $e_0$ a favor de las mujeres.

3. El perfil de mortalidad de Chiapas refleja fielmente su posición de
   rezago socioeconómico: $nM_0$ elevado, sobremortalidad masculina adulta
   moderada y alta vulnerabilidad ante eventos de salud pública como la
   pandemia.
