# Función alerta.movil 
# Recibe:
#         df: data frame de transacciones, con columnas id_tx, fecha, cod_canton, monto_crc 
#         ventana: numero de dias previos usados para medir lo "normal"
#         k.alerta: multiplicador de la desviacion estandar
# Devuelve:
#         data frame: con fecha, n.tx, media, desv, alerta
#         (los primeros 'ventana' dias quedan en NA: no se evaluan)

 alerta.movil <- function(df, ventana, k.alerta){
   #transacciones por dia (356 filas), ordenado por filas
   conteo <- aggregate(id_tx ~ fecha,
                       data = df,
                       FUN = length)
   names(conteo) <- c("fecha", "n.tx")
   conteo$fecha <- as.Date(conteo$fecha)
   conteo <- conteo[order(conteo$fecha), ]
   
   n <- nrow(conteo)
   media <- rep(NA_real_, n)
   desv <- rep(NA_real_, n)
   
   for (t in seq_len(n)) {
     if(t>ventana){
       previos <- conteo$n.tx[(t-ventana):(t-1)] #solo se evaluan dias previos
       media[t] <- mean(previos)
       desv[t] <- sd(previos)
     }
   }
   
   conteo$media <- media
   conteo$desv <- desv
   conteo$alerta <- conteo$n.tx > media + k.alerta*desv #NA en los primeros dias
   rownames(conteo) <- NULL
   
   conteo
 }
 
 
 # función benford.mad(x)
 # Recibe: x vector numerico de montos(positivos)
 # Devuelve: La MAD (desviacion absoluta media) contra la Ley de Benford
 
 benford.mad <- function(x) {
   montos.validos <- x[!is.na(x) & x > 0]
   primer.digito <- montos.validos%/% 10^floor(log10(montos.validos)) # es el primer digito con valores del 1 al 9
   proporcion.observada <- as.numeric(table(factor(primer.digito, levels = 1:9))) / length(primer.digito)
   proporcion.benford <- log10(1+1 / (1:9)) # proporcion de Benford
   mean(abs(proporcion.observada - proporcion.benford)) 
 }
 
 # Funcion benford.canton(x)
 # Recibe: 
 #         df: un data frame de transacciones (cod_canton, canal, monto_crc)
 #         canal: nombre del canal a auditar
 #         cantones: data frame de cantones (cod_canton, canton, provincia)
 # Devuelve:
 #         data frame por canton (cod_canton, canton, n, mad. clase) ordenado de mayor a menor MAD
 
 benford.canton <- function(df, canal, cantones) {
   tx.canal <- df[df$canal == canal, ]
   codigos.canton <- sort(unique(tx.canal$cod_canton))
   
   tabla.canton <- data.frame(
     cod_canton = codigos.canton,
     n = sapply(codigos.canton, function(codigo) sum(tx.canal$cod_canton == codigo)),
     mad = sapply(codigos.canton, function(codigo) benford.mad(tx.canal$monto_crc[tx.canal$cod_canton == codigo]))
   )
   
   tabla.canton$clase <- as.character(cut(tabla.canton$mad,
                                          breaks = c(-Inf, 0.006, 0.012, 0.015, Inf),
                                          labels = c("conformidad cercana", "conformidad aceptable",
                                                     "conformidad marginal", "no conformidad")))
   
   tabla.canton <- merge(tabla.canton, cantones[, c("cod_canton", "canton")], by = "cod_canton")
   
   tabla.canton <- tabla.canton[order(-tabla.canton$mad), c("cod_canton", "canton", "n", "mad", "clase")]
   
   rownames(tabla.canton) <- NULL
   tabla.canton
 }
 
 
 # funcion valor.acumulado(df, tasa.anual, canal)
 # Recibe:
 #        df: data frame de transacciones (fecha, canal, monto_crc)
 #        tasa.anual: tasa efectiva anual en decimal
 #        canal: nombre del canal cuyos montos se depositan
 # Devuelve: 
 #        lista con:
 #                  tabla data frame: con mes (1 a 12), monto (total del canal
 #                        en el mes) y factor ((1 + tasa.mensual)^(12 - mes))
 #                  tasa.mensual:  tasa efectiva mensual equivalente
 #                  va: valor acumulado al 31 de diciembre
 #                  interes: intereses ganados (va - suma de montos)
 
 valor.acumulado <- function(df, tasa.anual, canal) {
   tx.canal <- df[df$canal == canal, ]
   mes.tx <- as.numeric(format(as.Date(tx.canal$fecha), "%m"))
   
   monto.mes <- sapply(1:12, function(mes) sum(tx.canal$monto_crc[mes.tx == mes]))
   
   tasa.mensual <- (1 + tasa.anual)^(1 / 12) - 1
   
   tabla.va <- data.frame(
     mes = 1:12,
     monto  = monto.mes,
     factor = (1 + tasa.mensual)^(12 - (1:12))
   )
   
   va.total <- sum(tabla.va$monto * tabla.va$factor)
   interes.total <- va.total - sum(tabla.va$monto)
   
   list(tabla = tabla.va,
        tasa.mensual = tasa.mensual,
        va = va.total,
        interes = interes.total)
 }