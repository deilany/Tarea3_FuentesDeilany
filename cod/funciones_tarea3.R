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