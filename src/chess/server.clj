(ns chess.server
  (:require
    [chess.games :refer :all]
    [org.httpkit.server :refer :all]
    [compojure.core :refer :all]
    [clojure.data.json :as json]
    [chess.mapper :refer :all]
    [cognitect.transit :as transit]
    [ysera.error :refer [error]]))
(import [java.io ByteArrayInputStream ByteArrayOutputStream])

(def websocket-atom (atom {:channels {}}))

(defn get-client-channels []
  (keys (:channels (deref websocket-atom))))

(comment (count (:channels (deref websocket-atom))))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn get-player-id [channel]
  (get-in (deref websocket-atom)
          [:channels channel :game-player-id]))

(defn connect! [channel]
  (println "client connected")
  (swap! websocket-atom update :channels (fn [channels]
                                           (assoc channels channel (merge {:channel channel
                                                                           :id      (uuid)}
                                                                          (condp = (count channels)
                                                                            0 {:game-player-id :large}
                                                                            1 {:game-player-id :small}
                                                                            nil))))))

(defn disconnect! [channel status]
  (println "client disconnected:" status)
  (swap! websocket-atom update :channels (fn [channels]
                                           (dissoc channels channel))))

(defn transit-write [x]
  (let [baos (ByteArrayOutputStream.)
        w (transit/writer baos :json)
        _ (transit/write w x)
        ret (.toString baos)]
    (.reset baos)
    ret))

(defn notify-clients [msg]
  (doseq [channel (get-client-channels)]
    (send! channel (transit-write (merge msg {:player-id (get-player-id channel)})))))

(add-watch chess.games/game-atom
           :websocket-client-notifier
           (fn [_ _ _ _]
             (println "something happened. notifying clients.")
             (notify-clients {:data (game->view-game (deref chess.games/game-atom))})))

;; Routes

;(defn game-response [view-game]
;  {:status  200
;   :headers {"Content-Type"                 "text/json; charset=utf-8"
;             "Access-Control-Allow-Origin"  "*"
;             "Access-Control-Allow-Methods" "*"}
;   :body    (json/write-str view-game)})

;(defroutes
;  chess
;  (POST "/createGame" []
;    (time (game-response (game->view-game (create-game!)))))
;  (POST "/move" {body :body}
;    (let [params (json/read-json (slurp body))
;          from-position (:from-position params)
;          to-position (:to-position params)
;          player-id (keyword (:player-id params))]
;      (time (game-response (game->view-game (move! player-id from-position to-position))))))
;  ;(POST "/castle" {body :body}
;  ;  (let [params (json/read-json (slurp body))
;  ;        from-position (:from-position params)
;  ;        to-position (:to-position params)
;  ;        player-id (keyword (:player-id params))]
;  ;    (time (game-response (game->view-game (castle! player-id from-position to-position))))))
;  (POST "/undo" {body :body}
;    (let [params (json/read-json (slurp body))
;          player-id (keyword (:player-id params))]
;      (time (game-response (game->view-game (undo! player-id))))))
;  (POST "/redo" {body :body}
;    (let [params (json/read-json (slurp body))
;          player-id (keyword (:player-id params))]
;      (time (game-response (game->view-game (redo! player-id)))))))

(defn handle-client-action
  [{action :action data :data} player-id]
  (condp = action
    "create-game"
    (create-game!)

    "move-piece"
    (let [from-position (:from-position data)
          to-position (:to-position data)]
      (move! player-id from-position to-position))

    "undo"
    (undo! player-id)

    "redo"
    (redo! player-id)

    (error "Unknown action:" action)))

(defn chess-websocket
  [request]
  (with-channel request channel
                (connect! channel)
                (on-close channel (fn [status] (disconnect! channel status)))
                (on-receive channel (fn [data]
                                      (println data)
                                      (let [data (-> data
                                                     (.getBytes "UTF-8")
                                                     (ByteArrayInputStream.)
                                                     (transit/reader :json)
                                                     (transit/read))
                                            player-id (get-player-id channel)]
                                        (when player-id
                                          (handle-client-action data player-id)))))))

;; Starting & Stopping

(defonce server (atom nil))

(defn stop! []
  (when-not (nil? @server)
    ;; graceful shutdown: wait 100ms for existing requests to be finished
    (@server :timeout 100)
    (reset! server nil)))

(defn start! []
  ;; (reset! server (run-server chess {:port 8001}))
  (run-server chess-websocket {:port 9876}))

(defn restart! []
  (stop!)
  (start!))
