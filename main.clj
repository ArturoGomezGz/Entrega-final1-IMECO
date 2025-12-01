;; ==============================
;; Simulador de Máquina Expendedora
;; ==============================

;; Forward declarations para recursión mutua
;; Declaramos las funciones para evitar errores de referencia antes de su definición.
(declare agregar-denominacion-a-resultado)
(declare actualizar-cantidad-denominacion)
(declare calcular-cambio-recursivo)
(declare agregar-a-cambio)

;; ------------------------------
;; Lectura de archivos
;; ------------------------------

;; Lee un archivo completo y lo convierte a estructura de datos de Clojure.
;; Utiliza read para parsear el contenido del archivo y convertirlo a listas/mapas.
(defn leer-archivo-completo [archivo]
  (with-open [reader (clojure.java.io/reader archivo)]
    (read (java.io.PushbackReader. reader))))

;; Lee la configuración inicial de la máquina expendedora desde un archivo.
;; Retorna la estructura de datos con productos y dinero inicial.
(defn leer-maquina [archivo]
  (leer-archivo-completo archivo))

;; Lee la lista de transacciones a procesar desde un archivo.
;; Retorna una lista con todas las transacciones que se ejecutarán.
(defn leer-transacciones [archivo]
  (leer-archivo-completo archivo))

;; ------------------------------
;; Funciones para manejar dinero
;; ------------------------------

;; Convierte una lista de denominaciones individuales (ej: [1 5 5 10]) 
;; a formato agrupado [[denominacion cantidad] ...].
;; Utiliza recursión para contar cuántas veces aparece cada denominación.
(defn convertir-y-contar [lista-denominaciones resultado]
  (if (empty? lista-denominaciones)
      resultado
      (convertir-y-contar 
       (rest lista-denominaciones)
       (agregar-denominacion-a-resultado (first lista-denominaciones) resultado))))

;; Agrega o actualiza una denominación en el resultado.
;; Si la denominación ya existe, incrementa su cantidad; si no, la agrega nueva.
(defn agregar-denominacion-a-resultado [denominacion resultado]
  (if (empty? resultado)
      [[denominacion 1]]
      (if (= (first (first resultado)) denominacion)
          (cons [(first (first resultado)) (+ (second (first resultado)) 1)]
                (rest resultado))
          (cons (first resultado) 
                (agregar-denominacion-a-resultado denominacion (rest resultado))))))

(defn convertir-denominaciones-individuales [lista-denominaciones]
  (convertir-y-contar lista-denominaciones []))

;; Calcula el valor total de una lista de denominaciones agrupadas.
;; Multiplica cada denominación por su cantidad y suma todos los resultados.
(defn calcular-total-denominaciones [denominaciones]
  (if (empty? denominaciones)
      0
      (+ (* (first (first denominaciones)) (second (first denominaciones)))
         (calcular-total-denominaciones (rest denominaciones)))))

;; Calcula el total de una lista de denominaciones individuales.
;; Suma directamente todos los valores de la lista sin agrupar.
(defn calcular-total-lista-individual [lista-denominaciones]
  (if (empty? lista-denominaciones)
      0
      (+ (first lista-denominaciones) 
         (calcular-total-lista-individual (rest lista-denominaciones)))))

;; Extrae el inventario de dinero de la estructura de la máquina.
;; Busca la clave :dinero en la asociación y retorna su valor.
(defn obtener-dinero [maquina]
  (:dinero maquina))

;; Calcula el efectivo total disponible en la máquina.
;; Usa el inventario de dinero y suma todas las denominaciones.
(defn calcular-efectivo-total [maquina]
  (calcular-total-denominaciones (obtener-dinero maquina)))

;; Actualiza la cantidad de una denominación específica en el inventario.
;; Puede sumar o restar según el modo especificado (:sumar o :restar).
(defn actualizar-cantidad-denominacion [inventario denominacion cantidad modo]
  (if (empty? inventario)
      []
      (if (= (first (first inventario)) denominacion)
          (conj (rest inventario) 
                [(first (first inventario))
                 (if (= modo :sumar)
                     (+ (second (first inventario)) cantidad)
                     (- (second (first inventario)) cantidad))])
          (conj (actualizar-cantidad-denominacion (rest inventario) denominacion cantidad modo)
                (first inventario)))))

;; Agrega las denominaciones de pago al inventario de dinero de la máquina.
;; Procesa cada denominación del pago y la suma al inventario existente.
(defn agregar-dinero-inventario [inventario-dinero denominaciones-pago]
  (if (empty? denominaciones-pago)
      inventario-dinero
      (agregar-dinero-inventario
       (actualizar-cantidad-denominacion inventario-dinero 
                                        (first (first denominaciones-pago))
                                        (second (first denominaciones-pago))
                                        :sumar)
       (rest denominaciones-pago))))

;; Ordena las denominaciones de mayor a menor valor.
;; Utiliza inserción ordenada para mantener el orden descendente.
(defn insertar-ordenado [denominacion lista-ordenada]
  (if (empty? lista-ordenada)
      [denominacion]
      (if (> (first denominacion) (first (first lista-ordenada)))
          (cons denominacion lista-ordenada)
          (cons (first lista-ordenada) (insertar-ordenado denominacion (rest lista-ordenada))))))

(defn ordenar-denominaciones-descendente [inventario]
  (if (empty? inventario)
      []
      (insertar-ordenado (first inventario) (ordenar-denominaciones-descendente (rest inventario)))))

;; Agrega una denominación al cambio acumulado.
;; Si la denominación ya existe en el cambio, incrementa su cantidad.
(defn agregar-a-cambio [cambio-acumulado denominacion]
  (if (empty? cambio-acumulado)
      [[denominacion 1]]
      (if (= (first (first cambio-acumulado)) denominacion)
          (cons [(first (first cambio-acumulado)) (+ (second (first cambio-acumulado)) 1)]
                (rest cambio-acumulado))
          (cons (first cambio-acumulado) 
                (agregar-a-cambio (rest cambio-acumulado) denominacion)))))

;; Función recursiva para calcular el cambio exacto.
;; Intenta usar la denominación más grande disponible y continúa recursivamente.
;; Retorna la lista de denominaciones del cambio o nil si no es posible dar cambio exacto.
(defn calcular-cambio-recursivo [monto-restante inventario cambio-acumulado]
  (if (or (= monto-restante 0) (empty? inventario))
      (if (= monto-restante 0)
          cambio-acumulado
          nil) ; No se puede dar cambio exacto
      (if (and (> (second (first inventario)) 0) 
               (>= monto-restante (first (first inventario))))
          (calcular-cambio-recursivo 
           (- monto-restante (first (first inventario)))
           (actualizar-cantidad-denominacion inventario 
                                            (first (first inventario)) 
                                            1 
                                            :restar)
           (agregar-a-cambio cambio-acumulado (first (first inventario))))
          (calcular-cambio-recursivo monto-restante (rest inventario) cambio-acumulado))))

;; Calcula el cambio óptimo usando el algoritmo greedy.
;; Ordena las denominaciones de mayor a menor y usa las denominaciones más grandes primero.
(defn calcular-cambio [monto-cambio inventario-dinero]
  (calcular-cambio-recursivo monto-cambio (ordenar-denominaciones-descendente inventario-dinero) []))

;; Aplica las denominaciones del cambio al inventario, restándolas.
;; Actualiza el inventario después de entregar el cambio al cliente.
(defn aplicar-cambio-inventario [inventario cambio]
  (if (empty? cambio)
      inventario
      (aplicar-cambio-inventario
       (actualizar-cantidad-denominacion inventario 
                                        (first (first cambio))
                                        (second (first cambio))
                                        :restar)
       (rest cambio))))

;; Verifica si hay suficiente cantidad de una denominación específica.
;; Busca la denominación en el inventario y compara con la cantidad solicitada.
(defn suficiente-denominacion? [inventario denominacion cantidad]
  (if (some (fn [d] (= (first d) denominacion)) inventario)
      (>= (second (first (filter (fn [d] (= (first d) denominacion)) inventario))) cantidad)
      false))

;; Retira denominaciones específicas del inventario de dinero.
;; Verifica disponibilidad y actualiza las cantidades, retorna nil si no es posible.
(defn retirar-denominaciones-inventario [inventario denominaciones-retiro]
  (if (empty? denominaciones-retiro)
      inventario
      (if (suficiente-denominacion? inventario (first (first denominaciones-retiro)) (second (first denominaciones-retiro)))
          (retirar-denominaciones-inventario
           (actualizar-cantidad-denominacion inventario 
                                            (first (first denominaciones-retiro))
                                            (second (first denominaciones-retiro))
                                            :restar)
           (rest denominaciones-retiro))
          nil)))

;; Actualiza la lista de productos restando una cantidad específica.
;; Busca el producto por slot y resta el valor de la cantidad actual.
(defn actualizar-lista-restar [productos-mapa slot valor]
  (if (empty? productos-mapa)
      {}
      (let [producto (get productos-mapa slot)]
        (if producto
            (assoc productos-mapa slot [(first producto) (- (second producto) valor)])
            productos-mapa))))

;; Actualiza la lista de productos sumando una cantidad al stock existente.
;; Busca el producto por slot e incrementa su cantidad actual con el valor dado.
(defn actualizar-lista-sumar [productos-mapa slot valor]
  (if (empty? productos-mapa)
      {}
      (let [producto (get productos-mapa slot)]
        (if producto
            (assoc productos-mapa slot [(first producto) (+ (second producto) valor)])
            productos-mapa))))

;; Actualiza el inventario de productos restando una cantidad (función por defecto).
;; Modifica la cantidad del producto especificado sin alterar el inventario de dinero.
(defn actualizar-inventario-restar [maquina slot valor]
  {:productos (actualizar-lista-restar (:productos maquina) slot valor)
   :dinero (obtener-dinero maquina)})

;; Actualiza el inventario de productos sumando una cantidad.
;; Útil para operaciones de resurtido que incrementan el stock.
(defn actualizar-inventario-sumar [maquina slot valor]
  {:productos (actualizar-lista-sumar (:productos maquina) slot valor)
   :dinero (obtener-dinero maquina)})

;; Función principal para actualizar inventario (por defecto resta).
;; Proporciona una interfaz unificada para las operaciones de inventario.
(defn actualizar-inventario [maquina slot valor]
  (actualizar-inventario-restar maquina slot valor))

;; Actualiza el estado de la máquina después de una venta exitosa.
;; Reduce el stock del producto vendido y actualiza el inventario de dinero.
(defn actualizar-maquina-despues-venta [maquina slot nuevo-inventario-dinero]
  {:productos (actualizar-lista-restar (:productos maquina) slot 1)
   :dinero nuevo-inventario-dinero})

;; Convierte una lista de denominaciones a formato legible para mostrar al usuario.
;; Formatea cada denominación con el símbolo de peso y las separa con comas.
(defn mostrar-lista-denominaciones [lista]
  (if (empty? lista)
      ""
      (if (empty? (rest lista))
          (str "$" (first lista))
          (str "$" (first lista) ", " (mostrar-lista-denominaciones (rest lista))))))

;; Muestra el detalle de las denominaciones del cambio.
;; Lista cada denominación con su cantidad de forma legible.
(defn mostrar-detalle-cambio [cambio]
  (if (empty? cambio)
      true ; No hacer nada si la lista está vacía
      (do
        (println (str "  " 
                      (second (first cambio))
                      " x $" 
                      (first (first cambio))))
        (mostrar-detalle-cambio (rest cambio)))))

;; Muestra el cambio entregado al cliente.
;; Presenta el monto total del cambio y el detalle de denominaciones.
(defn mostrar-cambio [cambio monto-total]
  (println (str "Cambio: $" monto-total))
  (mostrar-detalle-cambio cambio))

;; Imprime el inventario de dinero mostrando cada denominación y su cantidad.
;; Lista todas las denominaciones disponibles con formato legible.
(defn imprimir-inventario-dinero [inventario-dinero]
  (if (empty? inventario-dinero)
      (println "Fin del inventario de dinero.")
      (do
        (println (str
                  "$" (first (first inventario-dinero))
                  ": " (second (first inventario-dinero))
                  " unidades"))
        (imprimir-inventario-dinero (rest inventario-dinero)))))

;; Imprime el inventario de productos mostrando cada producto y su stock restante.
;; Lista todos los productos con su cantidad disponible.
(defn imprimir-inventario [productos]
  (if (empty? productos)
      (println "Fin del inventario.")
      (do
        (doseq [[slot producto-info] productos]
          (println (str
                    (name slot)
                    ": " (nth producto-info 1)
                    " unidades restantes.")))
        (println "Fin del inventario."))))

;; Muestra un resumen completo del estado final de la máquina.
;; Presenta el inventario de productos, dinero disponible y total en efectivo.
(defn mostrar-estadisticas [maquina]
  (println "\n=== Estadísticas ===")
  (println "Inventario restante:")
  (imprimir-inventario (:productos maquina))
  (println "\nInventario de dinero:")
  (imprimir-inventario-dinero (obtener-dinero maquina))
  (println (str "Total en efectivo: $" (calcular-efectivo-total maquina))))

;; Convierte un vector de listas Racket a un mapa de Clojure.
;; Transforma la estructura de datos del archivo al formato esperado.
(defn convertir-estructura-racket [estructura]
  (let [vec-estructura (vec estructura)]
    (if (>= (count vec-estructura) 2)
        (let [productos-section (vec-estructura 0)
              dinero-section (vec-estructura 1)
              productos-vec (vec productos-section)
              dinero-vec (vec dinero-section)]
          ;; Saltar el primer elemento que es la etiqueta 'productos' o 'dinero'
          (let [productos (into {} (map (fn [p] 
                                          (let [pv (vec p)]
                                            [(pv 0) (vec (rest pv))]))
                                       (rest productos-vec)))
                dinero (mapv vec (rest dinero-vec))]
            {:productos productos :dinero dinero}))
        estructura)))

;; Procesa una venta cuando el pago es exacto (sin cambio).
;; Confirma la venta, agrega el dinero al inventario y reduce el stock del producto.
(defn procesar-venta-sin-cambio [maquina slot inventario-dinero-actualizado]
  (println (str "Venta realizada: " (name slot)))
  (println "Pago exacto - No hay cambio")
  (actualizar-maquina-despues-venta maquina slot inventario-dinero-actualizado))

;; Procesa una venta que requiere cambio.
;; Verifica si es posible dar el cambio exacto, y si es así, completa la venta.
(defn procesar-venta-con-cambio-calculado [maquina slot inventario-dinero-actualizado monto-cambio]
  (if (calcular-cambio monto-cambio inventario-dinero-actualizado)
      (do
        (println (str "Venta realizada: " (name slot)))
        (mostrar-cambio (calcular-cambio monto-cambio inventario-dinero-actualizado) monto-cambio)
        (actualizar-maquina-despues-venta 
         maquina 
         slot 
         (aplicar-cambio-inventario inventario-dinero-actualizado 
                                   (calcular-cambio monto-cambio inventario-dinero-actualizado))))
      (do
        (println "No se puede dar cambio exacto, transacción cancelada.")
        maquina)))

;; Procesa la venta cuando el pago es suficiente, determinando si se necesita cambio.
;; Si el pago es exacto, procesa sin cambio; si hay exceso, calcula el cambio.
(defn procesar-venta-con-pago-suficiente [maquina slot denominaciones-pago]
  (let [precio (second (get (:productos maquina) slot))]
    (if (= (- (calcular-total-denominaciones denominaciones-pago) precio) 0)
        (procesar-venta-sin-cambio maquina slot 
                                  (agregar-dinero-inventario (obtener-dinero maquina) denominaciones-pago))
        (procesar-venta-con-cambio-calculado maquina slot 
                                            (agregar-dinero-inventario (obtener-dinero maquina) denominaciones-pago)
                                            (- (calcular-total-denominaciones denominaciones-pago) precio)))))

;; Verifica si el pago es suficiente para comprar el producto.
;; Si el pago es insuficiente, cancela la transacción; si es suficiente, continúa el proceso.
(defn procesar-venta-con-cambio [maquina slot denominaciones-pago]
  (let [precio (second (get (:productos maquina) slot))]
    (if (< (calcular-total-denominaciones denominaciones-pago) precio)
        (do
          (println "Pago insuficiente, transacción cancelada.")
          maquina)
        (procesar-venta-con-pago-suficiente maquina slot denominaciones-pago))))

;; Procesa una venta de producto verificando disponibilidad y manejando el pago.
;; Verifica que el producto exista y tenga stock, luego procesa el pago y cambio.
(defn vender-producto [maquina slot lista-denominaciones]
  (println (str "Denominaciones insertadas: " (mostrar-lista-denominaciones lista-denominaciones)))
  (let [producto (get (:productos maquina) slot)]
    (if (or (nil? producto) (= (nth producto 1) 0))
        (do (println (str "Producto " (name slot) " agotado"))
            maquina)
        (procesar-venta-con-cambio maquina slot (convertir-denominaciones-individuales lista-denominaciones)))))

;; Agrega stock a un producto específico de la máquina.
;; Suma la cantidad especificada al inventario actual del producto.
(defn resurtir-producto [maquina slot cantidad]
  (println (str "Resurtido de " cantidad
                " unidades del producto " (name slot)))
  (actualizar-inventario-sumar maquina slot cantidad))

;; Procesa el retiro de denominaciones específicas del inventario de dinero.
;; Verifica que haya suficientes denominaciones antes de realizar el retiro.
(defn retirar-dinero [maquina denominaciones-retiro]
  (println "Retiro de dinero:")
  (mostrar-detalle-cambio denominaciones-retiro)
  (if (retirar-denominaciones-inventario (obtener-dinero maquina) denominaciones-retiro)
      {:productos (:productos maquina)
       :dinero (retirar-denominaciones-inventario (obtener-dinero maquina) denominaciones-retiro)}
      (do
        (println "No hay suficientes denominaciones para el retiro.")
        maquina)))

;; Determina el tipo de transacción y la procesa según corresponda.
;; Maneja tres tipos: 'venta, 'resurtir y 'retiro-dinero.
;; Utiliza pattern matching con cond para identificar y procesar cada tipo.
(defn procesar-una-transaccion [maquina transaccion]
  (cond
    (= (first transaccion) 'venta)
    (vender-producto maquina
                     (second transaccion) ; slot
                     (nth transaccion 2))  ; lista de denominaciones individuales
    (= (first transaccion) 'resurtir)
    (resurtir-producto maquina (second transaccion) (nth transaccion 2))
    (= (first transaccion) 'retiro-dinero)
    (retirar-dinero maquina (convertir-denominaciones-individuales (second transaccion)))
    :else maquina))

;; Procesa recursivamente una lista de transacciones aplicándolas una por una.
;; Recibe el estado inicial de la máquina y una lista de transacciones,
;; retorna el estado final después de procesar todas las transacciones.
(defn procesar-transacciones [maquina transacciones]
  (if (empty? transacciones)
      maquina
      (procesar-transacciones
       (procesar-una-transaccion maquina (first transacciones))
       (rest transacciones))))

;; Función principal del simulador que ejecuta todo el programa.
;; Lee los archivos de configuración inicial (maquina.txt) y transacciones,
;; procesa todas las transacciones y finalmente muestra las estadísticas.
(defn main []
  (println "=== Simulador de Máquina Expendedora ===")
  (let [maquina-raw (leer-maquina "maquina.txt")
        maquina (convertir-estructura-racket maquina-raw)
        transacciones-raw (leer-transacciones "transacciones.txt")
        transacciones (vec transacciones-raw)]
    (mostrar-estadisticas 
     (procesar-transacciones maquina transacciones))))

;; Ejecutar programa
(main)


