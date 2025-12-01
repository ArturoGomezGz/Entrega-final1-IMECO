;; ==============================
;; Simulador de Máquinas Expendedoras en Paralelo
;; ==============================

;; Forward declarations para recursión mutua
(declare agregar-denominacion-a-resultado)
(declare actualizar-cantidad-denominacion)
(declare calcular-cambio-recursivo)
(declare agregar-a-cambio)

;; ------------------------------
;; Constantes de configuración
;; ------------------------------

;; Punto de reorden para productos (cuando el stock baja a este nivel o menos, se necesita resurtido)
(def punto-reorden-producto 2)

;; Punto de reorden para dinero (cuando las monedas/billetes bajan a este nivel o menos)
(def punto-reorden-dinero 5)

;; Límite de retiro para dinero (cuando las monedas/billetes alcanzan este nivel o más)
(def limite-retiro-dinero 30)

;; ------------------------------
;; Lectura de archivos
;; ------------------------------

(defn leer-archivo-completo [archivo]
  "Lee un archivo y lo convierte a estructura de datos Clojure.
   Maneja errores si el archivo no existe o tiene formato inválido."
  (try
    (with-open [reader (clojure.java.io/reader archivo)]
      (read (java.io.PushbackReader. reader)))
    (catch java.io.FileNotFoundException e
      (println (str "Error: No se encontró el archivo '" archivo "'"))
      nil)
    (catch Exception e
      (println (str "Error al leer el archivo '" archivo "': " (.getMessage e)))
      nil)))

;; ------------------------------
;; Funciones para manejar dinero
;; ------------------------------

(defn convertir-y-contar [lista-denominaciones resultado]
  (if (empty? lista-denominaciones)
      resultado
      (convertir-y-contar 
       (rest lista-denominaciones)
       (agregar-denominacion-a-resultado (first lista-denominaciones) resultado))))

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

(defn calcular-total-denominaciones [denominaciones]
  (if (empty? denominaciones)
      0
      (+ (* (first (first denominaciones)) (second (first denominaciones)))
         (calcular-total-denominaciones (rest denominaciones)))))

(defn obtener-dinero [maquina]
  (:dinero maquina))

(defn calcular-efectivo-total [maquina]
  (calcular-total-denominaciones (obtener-dinero maquina)))

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

(defn agregar-dinero-inventario [inventario-dinero denominaciones-pago]
  (if (empty? denominaciones-pago)
      inventario-dinero
      (agregar-dinero-inventario
       (actualizar-cantidad-denominacion inventario-dinero 
                                        (first (first denominaciones-pago))
                                        (second (first denominaciones-pago))
                                        :sumar)
       (rest denominaciones-pago))))

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

(defn agregar-a-cambio [cambio-acumulado denominacion]
  (if (empty? cambio-acumulado)
      [[denominacion 1]]
      (if (= (first (first cambio-acumulado)) denominacion)
          (cons [(first (first cambio-acumulado)) (+ (second (first cambio-acumulado)) 1)]
                (rest cambio-acumulado))
          (cons (first cambio-acumulado) 
                (agregar-a-cambio (rest cambio-acumulado) denominacion)))))

(defn calcular-cambio-recursivo [monto-restante inventario cambio-acumulado]
  (if (or (= monto-restante 0) (empty? inventario))
      (if (= monto-restante 0)
          cambio-acumulado
          nil)
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

(defn calcular-cambio [monto-cambio inventario-dinero]
  (calcular-cambio-recursivo monto-cambio (ordenar-denominaciones-descendente inventario-dinero) []))

(defn aplicar-cambio-inventario [inventario cambio]
  (if (empty? cambio)
      inventario
      (aplicar-cambio-inventario
       (actualizar-cantidad-denominacion inventario 
                                        (first (first cambio))
                                        (second (first cambio))
                                        :restar)
       (rest cambio))))

(defn suficiente-denominacion? [inventario denominacion cantidad]
  (if (some (fn [d] (= (first d) denominacion)) inventario)
      (>= (second (first (filter (fn [d] (= (first d) denominacion)) inventario))) cantidad)
      false))

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

;; ------------------------------
;; Funciones para manejar productos
;; ------------------------------

(defn actualizar-lista-restar [productos-mapa slot valor]
  "Resta valor de un producto, asegurando que no quede negativo"
  (if (empty? productos-mapa)
      {}
      (let [producto (get productos-mapa slot)]
        (if producto
            (let [nueva-cantidad (- (second producto) valor)]
              (assoc productos-mapa slot [(first producto) (max 0 nueva-cantidad)]))
            productos-mapa))))

(defn actualizar-lista-sumar [productos-mapa slot valor]
  (if (empty? productos-mapa)
      {}
      (let [producto (get productos-mapa slot)]
        (if producto
            (assoc productos-mapa slot [(first producto) (+ (second producto) valor)])
            productos-mapa))))

(defn actualizar-inventario-sumar [maquina slot valor]
  {:productos (actualizar-lista-sumar (:productos maquina) slot valor)
   :dinero (obtener-dinero maquina)
   :ganancia (:ganancia maquina 0)})

(defn actualizar-maquina-despues-venta [maquina slot nuevo-inventario-dinero precio]
  {:productos (actualizar-lista-restar (:productos maquina) slot 1)
   :dinero nuevo-inventario-dinero
   :ganancia (+ (:ganancia maquina 0) precio)})

;; ------------------------------
;; Funciones de impresión
;; ------------------------------

(defn mostrar-lista-denominaciones [lista]
  (if (empty? lista)
      ""
      (if (empty? (rest lista))
          (str "$" (first lista))
          (str "$" (first lista) ", " (mostrar-lista-denominaciones (rest lista))))))

(defn mostrar-detalle-cambio [cambio]
  (if (empty? cambio)
      true
      (do
        (println (str "  " 
                      (second (first cambio))
                      " x $" 
                      (first (first cambio))))
        (mostrar-detalle-cambio (rest cambio)))))

(defn mostrar-cambio [cambio monto-total]
  (println (str "Cambio: $" monto-total))
  (mostrar-detalle-cambio cambio))

;; ------------------------------
;; Conversión de estructuras
;; ------------------------------

(defn convertir-estructura-racket [estructura]
  (let [vec-estructura (vec estructura)]
    (if (>= (count vec-estructura) 2)
        (let [productos-section (vec-estructura 0)
              dinero-section (vec-estructura 1)
              productos-vec (vec productos-section)
              dinero-vec (vec dinero-section)]
          (let [productos (into {} (map (fn [p] 
                                          (let [pv (vec p)]
                                            [(pv 0) (vec (rest pv))]))
                                       (rest productos-vec)))
                dinero (mapv vec (rest dinero-vec))]
            {:productos productos :dinero dinero :ganancia 0}))
        estructura)))

;; ------------------------------
;; Procesamiento de transacciones (una máquina)
;; ------------------------------

(defn procesar-venta-sin-cambio [maquina slot inventario-dinero-actualizado precio]
  (println (str "Venta realizada: " (name slot)))
  (println "Pago exacto - No hay cambio")
  (actualizar-maquina-despues-venta maquina slot inventario-dinero-actualizado precio))

(defn procesar-venta-con-cambio-calculado [maquina slot inventario-dinero-actualizado monto-cambio precio]
  (if (calcular-cambio monto-cambio inventario-dinero-actualizado)
      (do
        (println (str "Venta realizada: " (name slot)))
        (mostrar-cambio (calcular-cambio monto-cambio inventario-dinero-actualizado) monto-cambio)
        (actualizar-maquina-despues-venta 
         maquina 
         slot 
         (aplicar-cambio-inventario inventario-dinero-actualizado 
                                   (calcular-cambio monto-cambio inventario-dinero-actualizado))
         precio))
      (do
        (println "No se puede dar cambio exacto, transacción cancelada.")
        maquina)))

(defn procesar-venta-con-pago-suficiente [maquina slot denominaciones-pago]
  (let [precio (first (get (:productos maquina) slot))]
    (if (= (- (calcular-total-denominaciones denominaciones-pago) precio) 0)
        (procesar-venta-sin-cambio maquina slot 
                                  (agregar-dinero-inventario (obtener-dinero maquina) denominaciones-pago)
                                  precio)
        (procesar-venta-con-cambio-calculado maquina slot 
                                            (agregar-dinero-inventario (obtener-dinero maquina) denominaciones-pago)
                                            (- (calcular-total-denominaciones denominaciones-pago) precio)
                                            precio))))

(defn procesar-venta-con-cambio [maquina slot denominaciones-pago]
  (let [precio (first (get (:productos maquina) slot))]
    (if (< (calcular-total-denominaciones denominaciones-pago) precio)
        (do
          (println "Pago insuficiente, transacción cancelada.")
          maquina)
        (procesar-venta-con-pago-suficiente maquina slot denominaciones-pago))))

(defn vender-producto [maquina slot lista-denominaciones]
  (println (str "Denominaciones insertadas: " (mostrar-lista-denominaciones lista-denominaciones)))
  (let [producto (get (:productos maquina) slot)]
    (if (or (nil? producto) (= (nth producto 1) 0))
        (do (println (str "Producto " (name slot) " agotado"))
            maquina)
        (procesar-venta-con-cambio maquina slot (convertir-denominaciones-individuales lista-denominaciones)))))

(defn resurtir-producto [maquina slot cantidad]
  (println (str "Resurtido de " cantidad
                " unidades del producto " (name slot)))
  (actualizar-inventario-sumar maquina slot cantidad))

(defn retirar-dinero [maquina denominaciones-retiro]
  (println "Retiro de dinero:")
  (mostrar-detalle-cambio denominaciones-retiro)
  (if (retirar-denominaciones-inventario (obtener-dinero maquina) denominaciones-retiro)
      {:productos (:productos maquina)
       :dinero (retirar-denominaciones-inventario (obtener-dinero maquina) denominaciones-retiro)
       :ganancia (:ganancia maquina 0)}
      (do
        (println "No hay suficientes denominaciones para el retiro.")
        maquina)))

(defn procesar-una-transaccion [maquina transaccion]
  (cond
    (= (first transaccion) 'venta)
    (vender-producto maquina
                     (second transaccion)
                     (nth transaccion 2))
    (= (first transaccion) 'resurtir)
    (resurtir-producto maquina (second transaccion) (nth transaccion 2))
    (= (first transaccion) 'retiro-dinero)
    (retirar-dinero maquina (convertir-denominaciones-individuales (second transaccion)))
    :else maquina))

(defn procesar-transacciones [maquina transacciones]
  (if (empty? transacciones)
      maquina
      (procesar-transacciones
       (procesar-una-transaccion maquina (first transacciones))
       (rest transacciones))))

;; ==============================
;; SECCIÓN NUEVA: MÚLTIPLES MÁQUINAS EN PARALELO
;; ==============================

;; Almacén de todas las máquinas usando un atom para gestión de estado concurrente
(def maquinas-estado (atom {}))

;; Lock para sincronizar la salida a consola durante procesamiento paralelo
(def print-lock (Object.))

(defn println-sync [& args]
  "Imprime a consola de forma sincronizada para evitar output entrelazado"
  (locking print-lock
    (apply println args)))

;; ------------------------------
;; Gestión de múltiples máquinas
;; ------------------------------

;; ------------------------------
;; Lectura de configuración de múltiples máquinas
;; ------------------------------

(defn crear-maquina [id archivo-configuracion]
  "Crea una máquina con un ID único y la carga desde un archivo de configuración"
  (let [maquina-raw (leer-archivo-completo archivo-configuracion)
        maquina (convertir-estructura-racket maquina-raw)]
    (swap! maquinas-estado assoc id maquina)
    id))

(defn leer-configuracion-maquinas [archivo-config]
  "Lee el archivo de configuración que define las máquinas y sus archivos de config
   Formato esperado: ((maquina-id archivo-config) ...)"
  (let [config (leer-archivo-completo archivo-config)]
    (mapv vec config)))

(defn inicializar-maquinas [configuraciones]
  "Inicializa todas las máquinas basándose en la lista de configuraciones"
  (reset! maquinas-estado {})
  (doseq [[id archivo] configuraciones]
    (crear-maquina id archivo))
  (println (str "Se inicializaron " (count configuraciones) " máquinas")))

;; ------------------------------
;; Lectura de transacciones por máquina
;; ------------------------------

(defn leer-transacciones-multiples [archivo]
  "Lee transacciones que incluyen el ID de la máquina
   Formato: ((maquina-id transaccion1 transaccion2 ...) ...)"
  (let [data (leer-archivo-completo archivo)]
    (mapv vec data)))

(defn agrupar-transacciones-por-maquina [transacciones-raw]
  "Agrupa las transacciones por ID de máquina
   Input: ((maq1 (venta A1 (20)) (venta B1 (10))) (maq2 (venta A2 (50))) ...)
   Output: {maq1 [(venta A1 (20)) (venta B1 (10))] maq2 [(venta A2 (50))] ...}"
  (into {} 
        (map (fn [grupo]
               [(first grupo) (vec (rest grupo))])
             transacciones-raw)))

;; ------------------------------
;; Procesamiento paralelo de transacciones
;; ------------------------------

(defn procesar-maquina-transacciones [id transacciones]
  "Procesa todas las transacciones de una máquina específica.
   Captura el output y lo imprime de forma sincronizada para evitar mezclas."
  (let [maquina-actual (get @maquinas-estado id)
        output-and-result (with-out-str
                            (println (str "=== Procesando Máquina: " id " ==="))
                            (let [maquina-final (procesar-transacciones maquina-actual transacciones)]
                              (swap! maquinas-estado assoc id maquina-final)
                              (print "::RESULT::" maquina-final)))]
    ;; Extraer resultado del output capturado
    (let [result-idx (.indexOf output-and-result "::RESULT::")
          output (subs output-and-result 0 result-idx)]
      (println-sync output)
      [id (get @maquinas-estado id)])))

(defn procesar-todas-maquinas-paralelo [transacciones-por-maquina]
  "Procesa las transacciones de todas las máquinas en paralelo usando pmap"
  (println "\n=== Iniciando procesamiento paralelo de transacciones ===")
  (let [ids-maquinas (keys transacciones-por-maquina)
        resultados (doall (pmap (fn [id]
                                  (procesar-maquina-transacciones id (get transacciones-por-maquina id [])))
                                ids-maquinas))]
    (println "\n=== Procesamiento paralelo completado ===")
    resultados))

;; ------------------------------
;; Cálculo de estadísticas y análisis
;; ------------------------------

(defn calcular-ganancia-total []
  "Calcula la ganancia total de todas las máquinas"
  (reduce + (map (fn [[_ maquina]] (:ganancia maquina 0)) @maquinas-estado)))

(defn obtener-ganancias-por-maquina []
  "Retorna una lista de pares [id ganancia] ordenada de mayor a menor"
  (sort-by second > 
           (map (fn [[id maquina]] [id (:ganancia maquina 0)]) @maquinas-estado)))

(defn obtener-top-10-porciento []
  "Retorna el top 10% de máquinas con mayor ganancia"
  (let [ganancias (obtener-ganancias-por-maquina)
        total (count ganancias)
        top-count (max 1 (int (Math/ceil (* total 0.10))))]
    (take top-count ganancias)))

(defn productos-necesitan-resurtido [maquina]
  "Retorna los productos de una máquina que están por debajo del punto de reorden"
  (filter (fn [[slot info]]
            (<= (second info) punto-reorden-producto))
          (:productos maquina)))

(defn maquinas-necesitan-resurtido-producto []
  "Retorna las máquinas que necesitan resurtido de productos"
  (filter (fn [[id maquina]]
            (seq (productos-necesitan-resurtido maquina)))
          @maquinas-estado))

(defn dinero-necesita-resurtido [maquina]
  "Retorna las denominaciones que están por debajo del punto de reorden"
  (filter (fn [[denom cantidad]]
            (<= cantidad punto-reorden-dinero))
          (:dinero maquina)))

(defn maquinas-necesitan-resurtido-dinero []
  "Retorna las máquinas que necesitan resurtido de dinero"
  (filter (fn [[id maquina]]
            (seq (dinero-necesita-resurtido maquina)))
          @maquinas-estado))

(defn dinero-necesita-retiro [maquina]
  "Retorna las denominaciones que están en el límite de retiro o más"
  (filter (fn [[denom cantidad]]
            (>= cantidad limite-retiro-dinero))
          (:dinero maquina)))

(defn maquinas-necesitan-retiro-dinero []
  "Retorna las máquinas que necesitan retiro de dinero"
  (filter (fn [[id maquina]]
            (seq (dinero-necesita-retiro maquina)))
          @maquinas-estado))

;; ------------------------------
;; Mostrar resultados finales
;; ------------------------------

(defn mostrar-estadisticas-maquina [id maquina]
  "Muestra las estadísticas de una máquina individual"
  (println (str "\n--- Máquina: " id " ---"))
  (println "Inventario de productos:")
  (doseq [[slot info] (:productos maquina)]
    (println (str "  " (name slot) ": " (second info) " unidades")))
  (println "Inventario de dinero:")
  (doseq [[denom cantidad] (:dinero maquina)]
    (println (str "  $" denom ": " cantidad " unidades")))
  (println (str "Ganancia: $" (:ganancia maquina 0)))
  (println (str "Efectivo total: $" (calcular-efectivo-total maquina))))

(defn mostrar-resultados-finales []
  "Muestra todos los resultados finales del procesamiento"
  (println "\n" (apply str (repeat 50 "=")))
  (println "RESULTADOS FINALES")
  (println (apply str (repeat 50 "=")))
  
  ;; Estadísticas por máquina
  (println "\n=== Estado de cada máquina ===")
  (doseq [[id maquina] @maquinas-estado]
    (mostrar-estadisticas-maquina id maquina))
  
  ;; Ganancia total
  (println "\n" (apply str (repeat 50 "-")))
  (println (str "GANANCIA TOTAL DEL NEGOCIO: $" (calcular-ganancia-total)))
  (println (apply str (repeat 50 "-")))
  
  ;; Top 10% de máquinas
  (println "\n=== Top 10% de máquinas con mayor ganancia ===")
  (doseq [[id ganancia] (obtener-top-10-porciento)]
    (println (str "  " id ": $" ganancia)))
  
  ;; Máquinas que necesitan resurtido de producto
  (println "\n=== Máquinas que necesitan resurtido de PRODUCTO ===")
  (let [maquinas-resurtir (maquinas-necesitan-resurtido-producto)]
    (if (empty? maquinas-resurtir)
        (println "  Ninguna máquina necesita resurtido de producto")
        (doseq [[id maquina] maquinas-resurtir]
          (println (str "  " id ":"))
          (doseq [[slot info] (productos-necesitan-resurtido maquina)]
            (println (str "    - " (name slot) ": " (second info) " unidades (punto de reorden: " punto-reorden-producto ")"))))))
  
  ;; Máquinas que necesitan resurtido de dinero
  (println "\n=== Máquinas que necesitan resurtido de DINERO ===")
  (let [maquinas-dinero (maquinas-necesitan-resurtido-dinero)]
    (if (empty? maquinas-dinero)
        (println "  Ninguna máquina necesita resurtido de dinero")
        (doseq [[id maquina] maquinas-dinero]
          (println (str "  " id ":"))
          (doseq [[denom cantidad] (dinero-necesita-resurtido maquina)]
            (println (str "    - $" denom ": " cantidad " unidades (punto de reorden: " punto-reorden-dinero ")"))))))
  
  ;; Máquinas que necesitan retiro de dinero
  (println "\n=== Máquinas que necesitan RETIRO de dinero ===")
  (let [maquinas-retiro (maquinas-necesitan-retiro-dinero)]
    (if (empty? maquinas-retiro)
        (println "  Ninguna máquina necesita retiro de dinero")
        (doseq [[id maquina] maquinas-retiro]
          (println (str "  " id ":"))
          (doseq [[denom cantidad] (dinero-necesita-retiro maquina)]
            (println (str "    - $" denom ": " cantidad " unidades (límite de retiro: " limite-retiro-dinero ")"))))))
  
  (println "\n" (apply str (repeat 50 "="))))

;; ------------------------------
;; Función principal del simulador paralelo
;; ------------------------------

(defn main-paralelo []
  "Función principal que ejecuta el simulador con múltiples máquinas en paralelo"
  (println "=== Simulador de Máquinas Expendedoras en Paralelo ===")
  (println "Ingrese el nombre del archivo de configuración de máquinas (o presione Enter para usar 'config_maquinas.txt'):")
  (flush)
  (let [input-config (read-line)
        archivo-config (if (or (nil? input-config) (empty? input-config)) 
                         "config_maquinas.txt" 
                         input-config)]
    (println (str "Usando archivo de configuración: " archivo-config))
    
    (println "Ingrese el nombre del archivo de transacciones (o presione Enter para usar 'transacciones_multiples.txt'):")
    (flush)
    (let [input-trans (read-line)
          archivo-trans (if (or (nil? input-trans) (empty? input-trans)) 
                          "transacciones_multiples.txt" 
                          input-trans)]
      (println (str "Usando archivo de transacciones: " archivo-trans))
      
      ;; Inicializar máquinas
      (let [configuraciones (leer-configuracion-maquinas archivo-config)]
        (inicializar-maquinas configuraciones)
        
        ;; Leer y procesar transacciones
        (let [transacciones-raw (leer-transacciones-multiples archivo-trans)
              transacciones-por-maquina (agrupar-transacciones-por-maquina transacciones-raw)]
          
          ;; Procesar en paralelo
          (procesar-todas-maquinas-paralelo transacciones-por-maquina)
          
          ;; Mostrar resultados
          (mostrar-resultados-finales))))))

;; Ejecutar programa
(main-paralelo)
