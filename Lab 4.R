

# Laboratorio 4 DW Análisis Distribuidora del Sur -------------------------

library(dplyr)
library(tidyverse)
library(highcharter)
library(ggplot2)
library(qcc)
library(plotly)


# Cargar y previsualizar datos ------------------------------------------------------------

df <- read_delim("tabla_completa.csv", delim = ";", 
                       escape_double = FALSE, col_types = cols(Q = col_number()), 
                       trim_ws = TRUE,locale = locale(encoding = "Latin1"))

glimpse(df)
df <- mutate_if(df,is.character,as.factor)


# Columna del mes en que entra el flujo de caja
df <- mutate(df,Mes_entra = (CREDITO/30)+MES)

# Análisis ----------------------------------------------------------------

# Flujo de entrada real por mes
Q_month <- df %>% 
  group_by(Mes_entra) %>% 
  summarise(Q = sum(Q)) %>% 
  filter(Mes_entra<=12)

gg_flujo_de_caja <- ggplot(Q_month,aes(x = Mes_entra,y = Q))+
  geom_bar(stat='identity', width = 0.5, fill = "steelblue")+
  geom_text(aes(label=Q), vjust=1.5, color="black", size=3)+
  ggtitle("Flujo de Caja")+
  labs(x = "Meses", y = "Flujo de Caja")+
  theme_minimal()
gg_flujo_de_caja2 <- ggplotly(gg_flujo_de_caja)
gg_flujo_de_caja2

mean(Q_month$Q)

# Personal y envíos -------------------------------------------------------

# Personal actual (9 pilotos y se rotan las unidades)
Personal <- df %>% 
  summarise(Nombre = unique(PILOTO))

# Cantidad de veces que un piloto ha entregado el pedido de un cleinte
  # y monto total de esos envíos
No_cliente_por_piloto <- df %>% 
  select(PILOTO,CLIENTE,Q) %>% 
  group_by(PILOTO, CLIENTE) %>% 
  summarise(Q = sum(Q),
            Entregas = n())

# Ingreso y entregas totales por piloto (top)
Ingreso_por_piloto <- df %>% 
  group_by(PILOTO) %>% 
  summarise(Q = sum(Q),
            Entregas = n())

Ingreso_por_piloto <- Ingreso_por_piloto[order(Ingreso_por_piloto$Q,
                                               Ingreso_por_piloto$Entregas,
                                               decreasing = TRUE),]

mean(Ingreso_por_piloto$Q) # Promedio de ingreso x piloto = 66,538.69
# Los pilotos por encima del promedio son Fernando, Ismael, Pedro y Hector A.

# Ingreso por piloto
gg_ingreso_por_piloto <- ggplot(Ingreso_por_piloto,aes(x = reorder(PILOTO, Q), y = Q))+
  geom_bar(stat='identity', width = 0.5, fill = "steelblue")+
  geom_text(aes(label=Q), vjust=0.5, color="black", size=4)+
  ggtitle("Ingreso por piloto")+
  labs(x = "Piloto", y = "Ingreso")+
  coord_flip()+
  theme_minimal()
gg_ingreso_por_piloto2 <- ggplotly(gg_ingreso_por_piloto)
gg_ingreso_por_piloto2


# Entregas por piloto
gg_entregas_por_piloto <- ggplot(Ingreso_por_piloto,
                                 aes(x = reorder(PILOTO, Entregas),
                                     y = Entregas))+
  geom_bar(stat='identity', width = 0.5, fill = "steelblue")+
  geom_text(aes(label=Entregas), vjust=0.5, color="black", size=4)+
  ggtitle("Entregas por piloto")+
  labs(x = "Piloto", y = "Entrega")+
  coord_flip()+
  theme_minimal()
gg_entregas_por_piloto2 <- ggplotly(gg_entregas_por_piloto)
gg_entregas_por_piloto2

# Clientes y envíos -------------------------------------------------------

# Podemos ver que existen 3 tipos de envío, Desácho, Faltante y Devolución
  # suponiendo que los faltantes y devoluciones son de un pedido anterior podemos
  # notar que el unico cleitne con devoluciones es GALLO NEGRO, esto puede
  # significar que por parte del cliente hay algun problema ya que es comun ver
  # faltantes en otros clientes pero ninguna devolución.
Clientes <- df %>% 
  summarise(Cliente = unique(CLIENTE))

# 80-20 de clientes
tabla_clientes <- table(df$CLIENTE)
Pareto_clientes <- pareto.chart(tabla_clientes,
                                main = "Pareto de Clientes")
Pareto_clientes
# El 80% de las ventas esta conformado por una mayoria de clientes
  # desde El pinche hasta tienda la bendición



# Precio ------------------------------------------------------------------

# Análizando el precio unitario no se ve ningun indicio de robo
Precio <- df$CANTIDAD/df$Q
unique(Precio)


# Tipos de entrega ---------------------------------------------------------

Tipo_de_entrega_por_cliente <- df %>% 
  group_by(CLIENTE, `TIPO DE ENTREGA`) %>% 
  summarise(Cantidad = n())

Tipo_total <- Tipo_de_entrega_por_cliente %>% 
  group_by(`TIPO DE ENTREGA`) %>% 
  summarise(Cantidad = sum(Cantidad))

# Tipo de entrega por cliente
gg_tipo_de_entrega_por_cliente <- ggplot(Tipo_de_entrega_por_cliente,
       aes(x = reorder(CLIENTE, Cantidad),y = Cantidad, fill = `TIPO DE ENTREGA`))+
  geom_bar(stat='identity', width = 0.5)+
  ggtitle("Tipo de entrega por cliente")+
  labs(x = "Cliente", y = "Cantidad")+
  coord_flip()+
  theme_minimal()
gg_tipo_de_entrega_por_cliente2 <- ggplotly(gg_tipo_de_entrega_por_cliente)
gg_tipo_de_entrega_por_cliente2

Despacho_por_cliente <- Tipo_de_entrega_por_cliente %>% 
  filter(`TIPO DE ENTREGA` == "Despacho a cliente")
# El cliente con más Despachos es Taqueria el chinito

Faltante_por_cliente <- Tipo_de_entrega_por_cliente %>% 
  filter(`TIPO DE ENTREGA` == "Faltante")
# El cliente con más Faltantes es El pinche obelisco y tambien tiene más 
  # faltantes que depachos normales

Devolucion_por_cliente <- Tipo_de_entrega_por_cliente %>% 
  filter(`TIPO DE ENTREGA` == "DEVOLUCION")
# EL unico cliente con devoluciones es Gallo Negro

# Entregas por Mes
entregas_por_mes <- df %>% 
  group_by(MES) %>% 
  summarise(Entregas = n())

gg_entregas_por_mes <- ggplot(entregas_por_mes,
                              aes(x = MES,y = Entregas))+
  geom_bar(stat='identity', width = 0.5, fill = "steelblue")+
  ggtitle("Entregas por mes")+
  labs(x = "Mes", y = "Entregas")+
  theme_minimal()
gg_entregas_por_mes2 <- plotly::ggplotly(gg_entregas_por_mes)
gg_entregas_por_mes2


# Vehiculos -------------------------------------------------------------

Vehiculos <- df %>% 
  group_by(UNIDAD) %>% 
  summarise(Entregas = n(),
            Cantidad = sum(CANTIDAD),
            Q = sum(Q))

# Entregas por unidad
gg_entrega_por_unidad <- ggplot(Vehiculos,
                                aes(x = UNIDAD,y = Entregas, fill = UNIDAD))+
  geom_bar(stat='identity')+
  geom_text(aes(label=Entregas), vjust=1.5, color="black", size=4)+
  ggtitle("Entregas por Unidad")+
  labs(x = "Unidad", y = "Entrega")+
  theme_minimal()
gg_entrega_por_unidad2 <- plotly::ggplotly(gg_entrega_por_unidad)
gg_entrega_por_unidad2

# Cantidad por unidad
gg_cantidad_por_unidad <- ggplot(Vehiculos,
                                 aes(x = UNIDAD,y = Cantidad, fill = UNIDAD))+
  geom_bar(stat='identity')+
  geom_text(aes(label=Cantidad), vjust=1.5, color="black", size=4)+
  ggtitle("Cantidades por Unidad")+
  labs(x = "Unidad", y = "Cantidad")+
  theme_minimal()
gg_cantidad_por_unidad2 <- plotly::ggplotly(gg_cantidad_por_unidad)
gg_cantidad_por_unidad2

# Q por unidad
ggp_q_por_unidad <- ggplot(Vehiculos,aes(x = UNIDAD,y = Q, fill=UNIDAD))+
  geom_bar(stat='identity')+
  geom_text(aes(label=Q), vjust=1.5, color="black", size=4)+
  ggtitle("Ingreso por Unidad")+
  labs(x = "Unidad", y = "Ingreso")+
  theme_minimal()
ggp_q_por_unidad2 <- plotly::ggplotly(ggp_q_por_unidad)
ggp_q_por_unidad2


# Creditos ----------------------------------------------------------------

creditos <- df %>% 
  group_by(CREDITO) %>% 
  summarise(Cantidad = n())

gg_creditos <- ggplot(creditos,aes(x = CREDITO,y = Cantidad, fill = CREDITO))+
  geom_bar(stat='identity')+
  geom_text(aes(label=Cantidad), vjust=1.5, color="black", size=4)+
  ggtitle("Créditos")+
  labs(x = "Crédito", y = "Cantidad")+
  theme_minimal()
gg_creditos2 <- plotly::ggplotly(gg_creditos)
gg_creditos2
