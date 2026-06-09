(ns calorias-api.core
  (:gen-class)
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.adapter.jetty :refer [run-jetty]]))

;; =========================
;; BANCO DE DADOS (ATOM)
;; =========================

(def db
  (atom {:usuario    nil
         :transacoes '()}))

;; =========================
;; CONFIG API EXTERNA
;; =========================

(def api-key "0aHrm9wNHEqA8t994MztxJlYmP03ESX3BGi3nYTv")

;; =========================
;; FUNÇÕES PURAS DE DOMÍNIO
;; =========================

(defn filtrar-por-periodo
  "Filtra uma lista de transações entre duas datas (strings YYYY-MM-DD).
   Implementada com recursão de cauda explícita (loop/recur).
   Função pura — não acessa estado global."
  [transacoes inicio fim]
  (loop [restantes transacoes
         resultado '()]
    (if (empty? restantes)
      (reverse resultado)
      (let [t       (first restantes)
            no-intervalo (and (>= (compare (:data t) inicio) 0)
                              (<= (compare (:data t) fim) 0))]
        (recur (rest restantes)
               (if no-intervalo (conj resultado t) resultado))))))

(defn calcular-saldo
  "Soma as calorias de uma sequência de transações.
   Implementada com recursão de cauda explícita (loop/recur).
   Função pura — não acessa estado global."
  [transacoes]
  (loop [restantes transacoes
         acumulador 0]
    (if (empty? restantes)
      acumulador
      (let [cal (:calorias (first restantes))]
        (recur (rest restantes)
               (+ acumulador (if (number? cal) cal 0)))))))

;; =========================
;; FUNÇÕES AUXILIARES PURAS
;; =========================

(defn formatar-transacoes
  "Formata uma lista de transações para exibição no extrato.
   Usa map (HOF) para transformar cada mapa de transação.
   Função pura — não acessa estado global."
  [transacoes]
  (map (fn [t]
         {:data      (:data t)
          :nome      (:nome t)
          :tipo      (:tipo t)
          :calorias  (:calorias t)})
       transacoes))

(defn kg->lbs
  "Converte quilogramas para libras (exigido pela API Ninjas /caloriesburned)."
  [kg]
  (* kg 2.20462))

;; =========================
;; FUNÇÕES DE I/O (APIs EXTERNAS)
;; =========================

(defn buscar-calorias-alimento
  "Consulta a API Ninjas /v1/nutrition para obter as calorias de um alimento.
   Recebe nome e quantidade em gramas; retorna número (0 em caso de falha).
   Monta a URL com URLEncoder para garantir encoding correto dos espaços."
  [nome quantidade]
  (try
    (let [query    (str quantidade " grams " nome)
          encoded  (java.net.URLEncoder/encode query "UTF-8")
          url      (str "https://api.api-ninjas.com/v1/nutrition?query=" encoded)
          resp     (client/get url {:headers {"X-Api-Key" api-key}})
          body     (json/parse-string (:body resp) true)]
      (println "[alimento] query=" query "status=" (:status resp) "body=" (:body resp))
      (if (empty? body)
        0
        (let [cal (:calories (first body))]
          (if (number? cal) cal 0))))
    (catch Exception e
      (println "[alimento] ERRO:" (.getMessage e))
      0)))

(defn buscar-calorias-exercicio
  "Consulta a API Ninjas /v1/caloriesburned para calorias gastas em um exercício.
   - atividade : string com nome da atividade (ex: 'running', 'soccer')
   - peso-kg   : peso do usuário em kg (convertido internamente para lbs)
   - duracao   : duração em minutos
   Usa :query-params para encoding correto. Lê o campo 'total_calories'.
   Retorna número (0 em caso de falha)."
  [atividade peso-kg duracao]
  (try
    (let [peso-lbs (Math/round (double (kg->lbs peso-kg)))
          resp     (client/get "https://api.api-ninjas.com/v1/caloriesburned"
                               {:headers      {"X-Api-Key" api-key}
                                :query-params {"activity" atividade
                                               "weight"   peso-lbs
                                               "duration" duracao}})
          body     (json/parse-string (:body resp) true)]
      (if (empty? body)
        0
        (let [cal (:total_calories (first body))]
          (if (number? cal) cal 0))))
    (catch Exception _ 0)))

;; =========================
;; FUNÇÕES DE ESTADO (EFEITOS)
;; =========================

(defn cadastrar-usuario!
  "Registra os dados pessoais do usuário no atom db."
  [dados]
  (swap! db assoc :usuario dados))

(defn consultar-usuario
  "Retorna os dados pessoais do usuário cadastrado."
  []
  (:usuario @db))

(defn adicionar-transacao!
  "Adiciona um mapa de transação à lista no atom db."
  [transacao]
  (swap! db update :transacoes conj transacao))

(defn registrar-alimento!
  "Obtém as calorias via API externa e registra a transação de alimento."
  [nome quantidade data]
  (let [calorias (buscar-calorias-alimento nome quantidade)]
    (adicionar-transacao! {:tipo       "alimento"
                           :nome       nome
                           :quantidade quantidade
                           :calorias   calorias
                           :data       data})))

(defn registrar-exercicio!
  "Obtém as calorias gastas via API externa e registra a transação de exercício.
   Usa o peso do usuário cadastrado; assume 70 kg se não houver usuário."
  [nome duracao data]
  (let [peso     (get-in @db [:usuario :peso] 70)
        calorias (buscar-calorias-exercicio nome peso duracao)]
    (adicionar-transacao! {:tipo     "exercicio"
                           :nome     nome
                           :duracao  duracao
                           :calorias (- calorias)
                           :data     data})))

;; =========================
;; ENDPOINTS (API REST)
;; =========================

(defroutes app-routes

  ;; POST /usuario — Cadastrar dados pessoais
  (POST "/usuario" req
    (let [dados (json/parse-string (slurp (:body req)) true)]
      (cadastrar-usuario! dados)
      {:status  200
       :headers {"Content-Type" "application/json"}
       :body    (json/generate-string {:msg "Usuário cadastrado"})}))

  ;; GET /usuario — Consultar dados pessoais
  (GET "/usuario" []
    {:status  200
     :headers {"Content-Type" "application/json"}
     :body    (json/generate-string {:usuario (consultar-usuario)})})

  ;; POST /alimento — Registrar consumo de alimento (calorias via API externa)
  (POST "/alimento" req
    (let [dados      (json/parse-string (slurp (:body req)) true)
          nome       (:nome dados)
          quantidade (:quantidade dados 100)
          data       (:data dados)]
      (registrar-alimento! nome quantidade data)
      {:status  200
       :headers {"Content-Type" "application/json"}
       :body    (json/generate-string {:msg "Alimento registrado"})}))

  ;; POST /exercicio — Registrar atividade física (calorias via API externa)
  (POST "/exercicio" req
    (let [dados   (json/parse-string (slurp (:body req)) true)
          nome    (:nome dados)
          duracao (:duracao dados 30)
          data    (:data dados)]
      (registrar-exercicio! nome duracao data)
      {:status  200
       :headers {"Content-Type" "application/json"}
       :body    (json/generate-string {:msg "Exercício registrado"})}))

  ;; GET /saldo — Saldo total ou por período (?inicio=YYYY-MM-DD&fim=YYYY-MM-DD)
  (GET "/saldo" [inicio fim]
    {:status  200
     :headers {"Content-Type" "application/json"}
     :body    (json/generate-string
               {:saldo (if (and inicio fim)
                         (calcular-saldo
                           (filtrar-por-periodo (:transacoes @db) inicio fim))
                         (calcular-saldo (:transacoes @db)))})})

  ;; GET /extrato — Extrato total ou por período (?inicio=YYYY-MM-DD&fim=YYYY-MM-DD)
  (GET "/extrato" [inicio fim]
    {:status  200
     :headers {"Content-Type" "application/json"}
     :body    (json/generate-string
               {:transacoes (formatar-transacoes
                              (if (and inicio fim)
                                (filtrar-por-periodo (:transacoes @db) inicio fim)
                                (:transacoes @db)))})})

  (route/not-found
    (json/generate-string {:erro "Rota não encontrada"})))

;; =========================
;; CORS
;; =========================

(defn wrap-cors [handler]
  (fn [req]
    (if (= :options (:request-method req))
      {:status  200
       :headers {"Access-Control-Allow-Origin"  "*"
                 "Access-Control-Allow-Headers" "Content-Type"
                 "Access-Control-Allow-Methods" "GET,POST,OPTIONS"}
       :body    ""}
      (let [resp (handler req)]
        (-> resp
            (assoc-in [:headers "Access-Control-Allow-Origin"]  "*")
            (assoc-in [:headers "Access-Control-Allow-Headers"] "Content-Type")
            (assoc-in [:headers "Access-Control-Allow-Methods"] "GET,POST,OPTIONS"))))))

;; =========================
;; MAIN — único ponto com efeito colateral
;; =========================

(defn -main [& _args]
  (println "Servidor rodando em http://localhost:3000")
  (run-jetty (wrap-cors app-routes) {:port 3000 :join? false}))