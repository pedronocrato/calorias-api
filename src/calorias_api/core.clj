(ns calorias-api.core
  (:gen-class)
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.string :as str]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.json :refer [wrap-json-body]]))

;; =========================
;; ESTADO (ATOM)
;; =========================

(def db (atom {:usuario {} :transacoes '()}))

;; =========================
;; CHAVES APIs EXTERNAS
;; =========================

(def api-key-ninjas "Fh3Jf0IWiNIMdZ7Dda77UJDIQ3sbJAixUxFd3zcE")
(def api-key-usda "ARdUjCgYYmeKzvnTfEcAN5UHqe115b5ADCUoYrVp")

;; =========================
;; FUNÇÕES PURAS
;; =========================

(defn- acumular [total t]
  (let [tipo (:tipo t)
        cal  (or (:calorias t) 0)]
    (if (or (= tipo :alimento) (= tipo "alimento"))
      (+ total cal)
      (- total cal))))

(defn calcular-saldo
  "Calcula saldo calórico de uma lista de transações. Usa reduce (HOF)."
  [transacoes]
  (reduce acumular 0 transacoes))

(defn filtrar-por-periodo
  "Filtra transações por período. Usa loop/recur (recursão de cauda)."
  [transacoes inicio fim]
  (loop [xs transacoes resultado '()]
    (if (empty? xs)
      (reverse resultado)
      (let [t (first xs)
            ok (and (>= (compare (:data t) inicio) 0)
                    (<= (compare (:data t) fim) 0))]
        (recur (rest xs) (if ok (conj resultado t) resultado))))))

(defn formatar [transacoes]
  "Formata transações para exibição. Usa map (HOF)."
  (map #(select-keys % [:data :nome :tipo :calorias]) transacoes))

(defn valida-alimento? [a]
  (and (some? (:nome a)) (some? (:quantidade a)) (some? (:data a))))

(defn valida-exercicio? [e]
  (and (some? (:nome e)) (some? (:duracao e)) (some? (:data e))))

;; =========================
;; APIs EXTERNAS
;; =========================

(defn calorias-alimento [nome quantidade]
  (try
    (let [r (-> (client/get "https://api.nal.usda.gov/fdc/v1/foods/search"
                            {:query-params {"query" (str/lower-case nome)
                                            "pageSize" "1"
                                            "api_key" api-key-usda}
                             :throw-exceptions false})
                :body (json/parse-string true)
                :foods first :foodNutrients
                (->> (filter #(= (:nutrientName %) "Energy")))
                first :value)]
      (* (double (or r 0)) (/ quantidade 100.0)))
    (catch Exception _ 0)))

(defn calorias-exercicio [nome duracao]
  (try
    (-> (client/get "https://api.api-ninjas.com/v1/caloriesburned"
                    {:headers {"X-Api-Key" api-key-ninjas}
                     :query-params {"activity" (str/lower-case nome)
                                    "duration" (str duracao)}
                     :throw-exceptions false})
        :body (json/parse-string true) first :total_calories double)
    (catch Exception _ 0)))

;; =========================
;; ENDPOINTS
;; =========================

(defn resposta [corpo & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/json; charset=utf-8"}
   :body (json/generate-string corpo)})

(defroutes rotas

  (POST "/usuario" req
    (let [b (:body req)
          u {:nome (get b :nome "") :peso (double (get b :peso 0))
             :altura (double (get b :altura 0)) :idade (int (get b :idade 0))
             :sexo (str (get b :sexo ""))}]
      (swap! db assoc :usuario u)
      (resposta u 201)))

  (GET "/usuario" []
    (resposta (:usuario @db)))

  (POST "/alimento" req
    (let [b (:body req)]
      (if (valida-alimento? b)
        (let [t (merge b {:tipo :alimento :calorias (calorias-alimento (:nome b) (:quantidade b))})]
          (swap! db update :transacoes conj t)
          (resposta t 201))
        (resposta {:mensagem "Dados invalidos"} 422))))

  (POST "/exercicio" req
    (let [b (:body req)]
      (if (valida-exercicio? b)
        (let [t (merge b {:tipo :exercicio :calorias (calorias-exercicio (:nome b) (:duracao b))})]
          (swap! db update :transacoes conj t)
          (resposta t 201))
        (resposta {:mensagem "Dados invalidos"} 422))))

  (GET "/saldo" {p :params}
    (let [trans (:transacoes @db)
          filtradas (if (and (:inicio p) (:fim p))
                      (filtrar-por-periodo trans (:inicio p) (:fim p))
                      trans)]
      (resposta {:saldo (calcular-saldo filtradas)})))

  (GET "/extrato" {p :params}
    (let [trans (:transacoes @db)
          filtradas (if (and (:inicio p) (:fim p))
                      (filtrar-por-periodo trans (:inicio p) (:fim p))
                      trans)]
      (resposta {:transacoes (formatar filtradas)
                 :saldo (calcular-saldo filtradas)})))

  (route/not-found (resposta {:mensagem "Rota nao encontrada"} 404)))

;; =========================
;; CORS + APP + MAIN
;; =========================

(defn wrap-cors [handler]
  (fn [req]
    (if (= :options (:request-method req))
      {:status 200
       :headers {"Access-Control-Allow-Origin" "*"
                 "Access-Control-Allow-Headers" "Content-Type"
                 "Access-Control-Allow-Methods" "GET,POST,OPTIONS"}
       :body ""}
      (let [r (handler req)]
        (-> r
            (assoc-in [:headers "Access-Control-Allow-Origin"] "*")
            (assoc-in [:headers "Access-Control-Allow-Headers"] "Content-Type")
            (assoc-in [:headers "Access-Control-Allow-Methods"] "GET,POST,OPTIONS"))))))

(def app (-> rotas (wrap-json-body {:keywords? true}) wrap-cors))

(defn -main [& _]
  (println "Servidor rodando em http://localhost:3000")
  (run-jetty app {:port 3000 :join? false}))