(ns calorias-api.core
  (:gen-class)
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.string :as str]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.json :refer [wrap-json-body]]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]))

;; =========================
;; BANCO DE DADOS (ATOM)
;; =========================

(def db
  (atom {:usuario    {}
         :transacoes '()}))

;; =========================
;; CONFIG APIs EXTERNAS
;; =========================

(def api-key-ninjas "Fh3Jf0IWiNIMdZ7Dda77UJDIQ3sbJAixUxFd3zcE")
(def api-key-usda   "g81cImnaYz180pDWw2IqfiXHoBGhDN8pMg1xVtKU")

;; =========================
;; FUNÇÕES PURAS — VALIDAÇÃO
;; =========================

(defn valida-usuario?
  "Verifica se o mapa de usuário contém os campos obrigatórios com valores válidos."
  [u]
  (and (contains? u :peso)
       (contains? u :altura)
       (contains? u :idade)
       (contains? u :sexo)
       (pos? (:peso u))
       (pos? (:altura u))))

(defn valida-alimento?
  "Verifica se o mapa de alimento contém os campos obrigatórios com valores válidos."
  [a]
  (and (contains? a :nome)
       (contains? a :quantidade)
       (contains? a :data)
       (string? (:nome a))
       (pos? (:quantidade a))))

(defn valida-exercicio?
  "Verifica se o mapa de exercício contém os campos obrigatórios com valores válidos."
  [e]
  (and (contains? e :nome)
       (contains? e :duracao)
       (contains? e :data)
       (string? (:nome e))
       (pos? (:duracao e))))

;; =========================
;; FUNÇÕES PURAS — DOMÍNIO
;; =========================

(defn- acumular-caloria
  "Acumula calorias: soma se alimento, subtrai se exercício.
   Função auxiliar pura usada pelo reduce em calcular-saldo.
   Aceita tipo como keyword (:alimento) ou string ('alimento')."
  [total transacao]
  (let [tipo (:tipo transacao)]
    (if (or (= tipo :alimento) (= tipo "alimento"))
      (+ total (or (:calorias transacao) 0))
      (- total (or (:calorias transacao) 0)))))

(defn calcular-saldo
  "Calcula o saldo calórico de uma lista de transações usando reduce (HOF).
   Função pura — não acessa estado global."
  [transacoes]
  (reduce acumular-caloria 0 transacoes))

(defn filtrar-por-periodo
  "Filtra transações entre duas datas (strings YYYY-MM-DD).
   Implementada com recursão de cauda explícita (loop/recur).
   Função pura — não acessa estado global."
  [transacoes inicio fim]
  (loop [restantes transacoes
         resultado '()]
    (if (empty? restantes)
      (reverse resultado)
      (let [t            (first restantes)
            no-intervalo (and (>= (compare (:data t) inicio) 0)
                              (<= (compare (:data t) fim) 0))]
        (recur (rest restantes)
               (if no-intervalo (conj resultado t) resultado))))))

(defn formatar-transacoes
  "Formata uma lista de transações para exibição no extrato.
   Usa map (HOF) para transformar cada mapa de transação.
   Função pura — não acessa estado global."
  [transacoes]
  (map (fn [t]
         {:data     (:data t)
          :nome     (:nome t)
          :tipo     (:tipo t)
          :calorias (:calorias t)})
       transacoes))

;; =========================
;; FUNÇÕES DE I/O — APIs EXTERNAS
;; =========================

(defn buscar-calorias-alimento
  "Consulta a API USDA FoodData Central (gratuita) para obter calorias.
   Retorna kcal por 100g escalado para a quantidade informada (em gramas)."
  [nome quantidade]
  (try
    (let [kcal-por-100g (-> (client/get "https://api.nal.usda.gov/fdc/v1/foods/search"
                                        {:query-params     {"query"    (str/lower-case nome)
                                                            "pageSize" "1"
                                                            "api_key"  api-key-usda}
                                         :throw-exceptions false})
                            :body
                            (json/parse-string true)
                            :foods
                            first
                            :foodNutrients
                            (->> (filter (fn [n] (= (:nutrientName n) "Energy"))))
                            first
                            :value)]
      (* (double (or kcal-por-100g 0))
         (/ quantidade 100.0)))
    (catch Exception _ 0)))

(defn buscar-calorias-exercicio
  "Consulta a API Ninjas /v1/caloriesburned para calorias gastas em um exercício.
   Recebe nome da atividade e duração em minutos. Retorna total de calorias gastas."
  [nome duracao]
  (try
    (-> (client/get "https://api.api-ninjas.com/v1/caloriesburned"
                    {:headers          {"X-Api-Key" api-key-ninjas}
                     :query-params     {"activity" (str/lower-case nome)
                                        "duration" (str duracao)}
                     :throw-exceptions false})
        :body
        (json/parse-string true)
        first
        :total_calories
        double)
    (catch Exception _ 0)))

;; =========================
;; FUNÇÕES DE ESTADO (EFEITOS)
;; =========================

(defn salvar-usuario!
  "Registra os dados pessoais do usuário no atom db."
  [dados]
  (swap! db assoc :usuario dados))

(defn consultar-usuario
  "Retorna os dados pessoais do usuário cadastrado."
  []
  (:usuario @db))

(defn registrar-alimento!
  "Obtém as calorias via API USDA e registra a transação de alimento no atom."
  [dados]
  (let [calorias  (buscar-calorias-alimento (:nome dados) (:quantidade dados))
        transacao (merge dados {:tipo :alimento :calorias calorias})]
    (swap! db update :transacoes conj transacao)
    transacao))

(defn registrar-exercicio!
  "Obtém as calorias via API Ninjas e registra a transação de exercício no atom."
  [dados]
  (let [calorias  (buscar-calorias-exercicio (:nome dados) (:duracao dados))
        transacao (merge dados {:tipo :exercicio :calorias calorias})]
    (swap! db update :transacoes conj transacao)
    transacao))

;; =========================
;; HELPERS DE RESPOSTA
;; =========================

(defn como-json
  "Monta uma resposta HTTP com body em JSON e status opcional (padrão 200)."
  [conteudo & [status]]
  {:status  (or status 200)
   :headers {"Content-Type" "application/json; charset=utf-8"}
   :body    (json/generate-string conteudo)})

;; =========================
;; ENDPOINTS (API REST)
;; =========================

(defroutes app-routes

  ;; POST /usuario — Cadastrar dados pessoais
  (POST "/usuario" req
    (if (valida-usuario? (:body req))
      (do (salvar-usuario! (:body req))
          (como-json (consultar-usuario) 201))
      (como-json {:mensagem "Dados inválidos"} 422)))

  ;; GET /usuario — Consultar dados pessoais
  (GET "/usuario" []
    (como-json (consultar-usuario)))

  ;; POST /alimento — Registrar consumo de alimento (calorias via API USDA)
  (POST "/alimento" req
    (if (valida-alimento? (:body req))
      (como-json (registrar-alimento! (:body req)) 201)
      (como-json {:mensagem "Dados inválidos"} 422)))

  ;; POST /exercicio — Registrar atividade física (calorias via API Ninjas)
  (POST "/exercicio" req
    (if (valida-exercicio? (:body req))
      (como-json (registrar-exercicio! (:body req)) 201)
      (como-json {:mensagem "Dados inválidos"} 422)))

  ;; GET /saldo — Saldo total ou por período (?inicio=YYYY-MM-DD&fim=YYYY-MM-DD)
  (GET "/saldo" {params :params}
    (let [inicio (:inicio params)
          fim    (:fim params)
          trans  (:transacoes @db)]
      (como-json {:saldo (if (and inicio fim)
                           (calcular-saldo (filtrar-por-periodo trans inicio fim))
                           (calcular-saldo trans))})))

  ;; GET /extrato — Extrato total ou por período (?inicio=YYYY-MM-DD&fim=YYYY-MM-DD)
  (GET "/extrato" {params :params}
    (let [inicio (:inicio params)
          fim    (:fim params)
          trans  (:transacoes @db)
          filtradas (if (and inicio fim)
                      (filtrar-por-periodo trans inicio fim)
                      trans)]
      (como-json {:transacoes (formatar-transacoes filtradas)
                  :saldo      (calcular-saldo filtradas)})))

  (route/not-found
    (como-json {:mensagem "Recurso não encontrado"} 404)))

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
;; APP (middlewares)
;; =========================

(def app
  (-> app-routes
      (wrap-json-body {:keywords? true :bigdecimals? true})
      (wrap-defaults api-defaults)
      wrap-cors))

;; =========================
;; MAIN
;; =========================

(defn -main [& _args]
  (println "Servidor rodando em http://localhost:3000")
  (run-jetty app {:port 3000 :join? false}))