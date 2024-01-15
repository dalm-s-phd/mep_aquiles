# Puedo encontrar la guía chat realizar este análisis en chatgpt

# Librerías ---------------------------------------------------------------
library(readxl)
library(tidyverse)
library(ggplot2) # gráficos


library(knitr)
library(kableExtra)
library(vtable)  #Es para realizar tablas de resumen // requiere el anterior si o si
library(skimr)   # Es para crear un resumen de datos, como summarize
library(gtsummary) # otro para generar tablas de resumen


# Importar datos, ordenar y limpiar ---------------------------------------

# Cargar la base d datos
datos_originales <- read_excel("C:/Users/santi/OneDrive/Documentos/Proyectos/Inv_Aquiles/bases_de_datos/base_de_datos_15.12.2023.xlsx", 
                               sheet = "Sheet1",
                               na = "Na", n_max = 103)

# versión propia de lo mismo
# datos_originales <- read_excel("C:/Users/santi/OneDrive - Universidad Maimónides/Proyectos investigación/Santiago d'Almeida - MEP TA/base_de_datos_15.12.2023.xlsx", 
#                                sheet = 1,
#                                col_names = c(""),
#                                col_types = c(""),
#                                na = "Na", n_max = 103)

# Crear base de datos para TABLA 1 ----------------------------------------

# Seleccionar las columnas necesarias
datos_demograficos <- datos_originales %>%
    select("n_orden","tto (A/B)","fecha_nacim (dd/mm/AAAA)","sexo (M/F)","peso_km_sem0","altura_cm","lado_afect (I/D)",
           "tto_prev (S/N)","fracaso_tto_prev (S/N)","meses_evo (num meses)","realiza_act_dep_sem0 (S/N)",
           "cuales_act_dep_sem0","cant_hr_sem_total_sem0","visa-a_sem1","faam_sem1","faam_dep_sem1","eva_sem1")

# Calcular la edad actual y agregar la nueva columna (en años)
datos_demograficos <- datos_demograficos %>%
    mutate(edad_actual = as.numeric(difftime(Sys.Date(), `fecha_nacim (dd/mm/AAAA)`, units = "secs")) / (60 * 60 * 24 * 365.25), .after = `fecha_nacim (dd/mm/AAAA)`)

# Calcular el Índice de Masa Corporal (IMC)
datos_demograficos <- datos_demograficos %>%
    mutate(imc = peso_km_sem0 / (altura_cm / 100)^2, .after = 'altura_cm')

# Cambiar los títulos de cada columna
datos_demograficos <- datos_demograficos %>%
    rename(grupo = "tto (A/B)",
           fecha_nac = "fecha_nacim (dd/mm/AAAA)",
           sexo = "sexo (M/F)",
           peso_kg = "peso_km_sem0",
           altura_cm= "altura_cm",
           lado_afectado = "lado_afect (I/D)",
           realizo_tratamientos_previos = "tto_prev (S/N)",
           fracaso_tratamiento_previo = "fracaso_tto_prev (S/N)",
           meses_evolucion_patologia = "meses_evo (num meses)",
           realiza_actividad_deportiva = "realiza_act_dep_sem0 (S/N)",
           horas_semana_actividad_deportiva= "cant_hr_sem_total_sem0",
           VISA_A = "visa-a_sem1",
           FAAAM_AVD = "faam_sem1",
           FAAM_Deporte = "faam_dep_sem1",
           EVA = "eva_sem1"
    )

# Modificar valores en toda la base de datos para re-codificar
datos_demograficos <- datos_demograficos %>%
    mutate(
        grupo = recode(grupo, "A" = "Grupo Control", "B" = "Grupo Experimental"),
        sexo = recode(sexo, "M" = "Masculino", "F" = "Femenino"),
        lado_afectado = recode(lado_afectado, "D" = "Derecha", "I" = "Izquierda"),
        realizo_tratamientos_previos = recode(realizo_tratamientos_previos, "S" = "Si", "N" = "No"),
        fracaso_tratamiento_previo = recode(fracaso_tratamiento_previo, "S" = "Si", "N" = "No"),
        realiza_actividad_deportiva = recode(realiza_actividad_deportiva, "S" = "Si", "N" = "No"))



# para generar la tabla 1, selecciono solo las filas que necesito, por ahora (para no tocar de mas 'datos_demograficos')
T1_datos_demog <- datos_demograficos[1:7,]

# Modifico las variables para que las interprete segun sus caracteristicas
T1_datos_demog <- T1_datos_demog %>%
    mutate(
        grupo = as.factor(grupo),
        sexo = as.factor(sexo),
        lado_afectado = as.factor(lado_afectado),
        realizo_tratamientos_previos = as.factor(realizo_tratamientos_previos),
        fracaso_tratamiento_previo = as.factor(fracaso_tratamiento_previo),
        realiza_actividad_deportiva = as.factor(realiza_actividad_deportiva)
    )

# Generar una tabla de descripción, dividida por el grupo de tratamiento
T1_datos_demog %>%
    select(-n_orden, -fecha_nac, cuales_act_dep_sem0) %>%  # selecciona todo menos estas variables
    tbl_summary(
        by = grupo,                                        # divide en grupos
        label=list(                                        # Cambia el nombre de las variables
            edad_actual ~ "Edad (años)",
            sexo ~ "Sexo",
            peso_kg ~ "Peso (Kg)",
            altura_cm ~ "Altura (cm)",
            imc ~ "Índice de Masa Corporal (IMC)",
            lado_afectado ~ "Lado afectado",
            realizo_tratamientos_previos ~ "Realizo tto. previos",
            fracaso_tratamiento_previo ~ "Fracaso tto. previos",
            realiza_actividad_deportiva ~ "Realiza actividad deportiva",
            meses_evolucion_patologia ~ "Evolución de la patología (meses)",
            horas_semana_actividad_deportiva ~ "Volumen de actividad deportiva (hr/semana)",
            VISA_A ~ "VISA-A",
            FAAAM_AVD ~ "FAAAM sub-escala: AVD",
            FAAM_Deporte ~ "FAAM sub-escala: Deporte",
            EVA ~ "Escala Visual Análoga (EVA)"),
        type = list(                                       # Le dice que tipo de categoría es cada variable
            sexo = "categorical",
            edad_actual = "continuous",
            peso_kg = "continuous",
            altura_cm = "continuous",
            imc = "continuous",
            lado_afectado = "categorical",
            meses_evolucion_patologia = "continuous",
            realizo_tratamientos_previos = "categorical",
            fracaso_tratamiento_previo = "categorical",
            realiza_actividad_deportiva = "categorical",
            horas_semana_actividad_deportiva = "continuous",
            VISA_A = "continuous",
            FAAAM_AVD = "continuous",
            FAAM_Deporte = "continuous",
            EVA = "continuous"
        ),
        statistic = list(                                  # podes pedirle estadisticos
            all_continuous() ~ "{mean} ({sd}±)",
            all_categorical() ~ "{n} ({p}%)"
        ),
        missing = "no") %>%                                # no toma en cuenta los "Na"
    modify_header(label = "**Variables**") %>%             # cambia la primera columna
    modify_caption("**Caracteristicas de los pacientes** (N = {N})") %>%   # cambia el titulo
    modify_footnote(all_stat_cols() ~ "Promedio (SD±) para variables numéricas; n (%) para variables categóricas.") %>%  # cambia el pie de pagina
    bold_labels() %>%                                      # etiquetas en negrita
    italicize_levels()                                     # niveles en itálica


# Crear base de datos para análisis ---------------------------------------


# Filtrar y seleccionar las columnas relevantes para el manejo de datos de análisis
datos_transformados <- datos_originales %>%
    select(
        n_orden,
        starts_with("visa-a"),
        starts_with("faam"),
        starts_with("faam_dep"),
        starts_with("eva"),
        starts_with("eco_sag"),
        starts_with("elast_sag"),
        starts_with("elast_trans"),
        starts_with("mc")
    )

# Modificar las comillas invertidas para nombres con guiones
col_names <- colnames(datos_transformados)
col_names <- gsub("visa-a", "visa_a", col_names)  # Reemplazar guión con guion bajo

# Renombrar las columnas
colnames(datos_transformados) <- col_names

# Reorganizar los datos en formato largo y agregar una columna para el marco temporal
datos_estructurados <- datos_transformados %>%
    pivot_longer(cols = -n_orden, names_to = "variable", values_to = "valor") %>%
    mutate(variable = gsub("visa_a", "visa-a", variable),
           semana = gsub(".*_(\\d+)$", "sem_\\1", variable),
           variable = gsub("_sem\\d+", "", variable))

# Corregir la variable "dep" a "faam_dep"
datos_estructurados <- datos_estructurados %>%
    mutate(variable = if_else(variable == "faam" & is.na(semana), "faam_dep", variable))

# Seleccionar las columnas relevantes
datos_estructurados <- datos_estructurados %>%
    select(n_orden, semana, variable, valor)

# Eliminar cualquier carácter o palabra previo a "sem" en la columna "semana"
datos_estructurados$semana <- gsub(".*sem", "sem", datos_estructurados$semana)

# Corregir la semana 52 de la variable "faam_dep"
datos_estructurados <- datos_estructurados %>%
    mutate(semana = if_else(semana == "sem" & variable != "faam_dep", "sem52", semana),
           variable = gsub("faam_dep_sem", "faam_dep", variable))

# Reemplazar "sema8" por "sem8" en la columna "semana"
datos_estructurados$semana <- gsub("sema8", "sem8", datos_estructurados$semana)

# Reemplazar "mc_sema8" por "mc" en la columna "variable"
datos_estructurados$variable <- gsub("mc_sema8", "mc", datos_estructurados$variable)

datos_estructurados$semana <-
    factor(
        datos_estructurados$semana,
        levels = c(
            "sem1",
            "sem2",
            "sem3",
            "sem4",
            "sem5",
            "sem6",
            "sem7",
            "sem8",
            "sem9",
            "sem10",
            "sem11",
            "sem12",
            "sem26",
            "sem52"
        )
    )

view(datos_estructurados)


# Análisis EVA ------------------------------------------------------------


# Se genera una nueva base de datos que solo incluya, n_orde hasta el 8 (es lo que tenemos hasta el 15/12/23), Grupo, los datos de la semana 1~12 y los valores de EVA
datos_eva_hasta_sem12 <-
    inner_join(datos_demograficos, datos_estructurados, by = "n_orden") %>%
    select(n_orden,
           grupo,
           semana,
           variable,
           valor)%>%
    filter(grepl("^sem([1-9]|1[0-2])$", semana)) %>%
    filter(variable == "eva") %>%
    filter(n_orden < 6)    # borrar mas adelante



# Se genera una base de datos de eva sem1 vs sem7 con sus valores originales
datos_eva_sem1_vs_sem7 <-  datos_eva_hasta_sem12 %>%
    filter(semana %in% c("sem1","sem7"))


# Se generar un grafico pre-post, para eso primero uso medidas de resultado y luego ploteo
datos_eva_sem1_vs_sem7 %>%
    group_by(grupo, semana) %>%
    summarise(prom_eva = median(valor, na.rm = T),
              desv_est = sd(valor, na.rm = T),
              n = n())

# Boxplot pre-post
ggplot(datos_eva_sem1_vs_sem7, aes(x=semana,y=valor, fill=grupo)) +
    geom_boxplot(outlier.shape = NA,notch = FALSE)+
    geom_point() +
    scale_y_continuous(name = "EVA (0/100 mm)", limits = c(0, 100)) +
    scale_x_discrete(labels=c('Pre', 'Post')) +
    labs(title = "EVA medido con 3 saltos en la semana 0 y semana 6", x = element_blank()) +
    theme(plot.title = element_text(face = "bold")) +
    facet_grid(. ~ grupo)

# Grafico dividido por persona y su tendencia
ggplot(datos_eva_sem1_vs_sem7, aes(x=semana,y=valor)) +
    geom_point(aes(col = grupo), size = 5)+
    geom_line(aes(group=n_orden)) +
    scale_y_continuous(name = "EVA (0/100 mm)", limits = c(0, 100)) +
    scale_x_discrete(labels=c('Pre', 'Post')) +
    labs(title = "EVA individuales medido con 3 saltos en la semana 0 y semana 6", x = element_blank()) +
    theme(plot.title = element_text(face = "bold")) +
    facet_grid(. ~ grupo)


#Para hacer el grafico de tiempo de sem1~sem7 primero tengo que hacer el promedio 
datos_plot_eva_sem1_3_5_7 <- datos_eva_hasta_sem12 %>%
    filter(semana %in% c("sem1", "sem3","sem5", "sem7")) %>%
    group_by(grupo, semana) %>%
    summarise(
        prom_eva = median(valor, na.rm = TRUE),
        desv_est = sd(valor, na.rm = TRUE),
        n = n()
    )

# Grafico donde se ve la evalucion de la semana 1,3,5,7
ggplot(data = datos_plot_eva_sem1_3_5_7, aes(x = semana, y = prom_eva, group = grupo, color = grupo, shape = grupo)) +
    geom_line() +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = prom_eva - desv_est, ymax = prom_eva + desv_est), width = 0.25) +
    scale_y_continuous(name = "EVA (0/100 mm)", limits = c(0, 100)) +
    labs(title = "EVA medido con 3 saltos a lo largo del tiempo",
         x = element_blank()) +
    scale_x_discrete(labels=c('Semana 0', 'Semana 2', 'Semana 4', 'Semana 6')) +
    theme(plot.title = element_text(face = "bold")) +
    facet_grid(grupo~.)



# Análisis FAAM en relacion a EVA -----------------------------------------------------------

# Se genera una nueva base de datos que solo incluya, n_orde hasta el 8 (es lo que tenemos hasta el 15/12/23), Grupo, los datos de la semana 1~12 y los valores de solo FAAM sin deporte
datos_faam_hasta_sem12 <-
    inner_join(datos_demograficos, datos_estructurados, by = "n_orden") %>%
    select(n_orden,
           grupo,
           semana,
           variable,
           valor)%>%
    filter(grepl("^sem([1-9]|1[0-2])$", semana)) %>%
    filter(variable == "faam") %>%
    filter(n_orden < 6)    # borrar mas adelante

# Se genera una base de datos de FAAM sem1 vs sem7 con sus valores originales
datos_faam_sem1_vs_sem7 <-  datos_faam_hasta_sem12 %>%
    filter(semana %in% c("sem1","sem7"))


# Se generar un grafico pre-post, para eso primero uso medidas de resultado y luego ploteo
datos_faam_sem1_vs_sem7 %>%
    group_by(grupo, semana) %>%
    summarise(prom_faam = median(valor, na.rm = T),
              desv_est = sd(valor, na.rm = T),
              n = n())

# Boxplot pre-post
ggplot(datos_faam_sem1_vs_sem7, aes(x=semana,y=valor, fill=grupo)) +
    geom_boxplot(outlier.shape = NA,notch = FALSE)+
    geom_point() +
    scale_y_continuous(name = "FAAM (%)", limits = c(0, 100)) +
    scale_x_discrete(labels=c('Pre', 'Post')) +
    labs(title = "FAAM en la semana 0 y semana 6", x = element_blank()) +
    theme(plot.title = element_text(face = "bold")) +
    facet_grid(. ~ grupo)

# Grafico dividido por persona y su tendencia
ggplot(datos_faam_sem1_vs_sem7, aes(x=semana,y=valor)) +
    geom_point(aes(col = grupo), size = 5)+
    geom_line(aes(group=n_orden)) +
    scale_y_continuous(name = "FAAM (%)", limits = c(0, 100)) +
    scale_x_discrete(labels=c('Pre', 'Post')) +
    labs(title = "FAAM individuales medido en la semana 0 y semana 6", x = element_blank()) +
    theme(plot.title = element_text(face = "bold")) +
    facet_grid(. ~ grupo)


#Para hacer el grafico de tiempo de sem1~sem7 primero tengo que hacer el promedio 
datos_plot_faam_sem1_3_5_7 <- datos_faam_hasta_sem12 %>%
    filter(semana %in% c("sem1", "sem3","sem5", "sem7")) %>%
    group_by(grupo, semana) %>%
    summarise(
        prom_eva = median(valor, na.rm = TRUE),
        desv_est = sd(valor, na.rm = TRUE),
        n = n()
    )

# Grafico donde se ve la evalucion de la semana 1,3,5,7
ggplot(data = datos_plot_faam_sem1_3_5_7, aes(x = semana, y = prom_eva, group = grupo, color = grupo, shape = grupo)) +
    geom_line() +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = prom_eva - desv_est, ymax = prom_eva + desv_est), width = 0.25) +
    scale_y_continuous(name = "FAAM (%)", limits = c(0, 100)) +
    labs(title = "FAAM a lo largo del tiempo",
         x = element_blank()) +
    scale_x_discrete(labels=c('Semana 0', 'Semana 2', 'Semana 4', 'Semana 6')) +
    theme(plot.title = element_text(face = "bold")) +
    facet_grid(grupo~.)
