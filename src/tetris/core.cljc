(ns tetris.core
  (:require [tetris.utils :as utils]
            [tetris.move :as move]
            [tetris.tetris_move :as tmove]
            [tetris.tiles :as tiles]
            [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.transforms :as t]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])))

;; here is a lot of unused states TODO: cleanup
(defonce *state (atom {:last-action-time 0
                       :last-auto-move 0
                       :mouse-x 0
                       :mouse-y 0
                       :pressed-keys #{}
                       :x-velocity 0
                       :y-velocity 0
                       :active-tile (tmove/spawn-new-tetro)
                       :can-jump? false
                       :direction :right
                       :player-images {}
                       :player-image-key :tile1
                       :basic-tile-loaded {}
                       :basic-tile-complete {}
                       :speed tmove/initial-speed
                       :camera-x 0
                       :camera-y 0
                       :camera (e/->camera true)
                       :map {:height tmove/height
                             :width tmove/width
                             ;; :game-board (make-array Integer/TYPE tmove/height tmove/width)
                             :game-board (vec (repeat tmove/height (vec (repeat tmove/width :B))))}}))

;; todo remove me
(defonce *game (atom {}))

(defn init-open-gl [game]
    ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA)))


(defn load-map [game state]
  (tiles/load-simple-map game state))

(defn init-game-board [board]
  "testing purposes"
  #_(let [fun (fn [val] :L)
        board (-> board
        (update-in [5 2] fun)
        (update-in [6 2] fun)
        (update-in [7 2] fun)
        (update-in [7 3] fun)
        )]
    (tmove/put-tetromino board 10 3 :J 2))
  board)

(defn init [game]
  (do
    (swap! *state update :map assoc :game-board
           (init-game-board (-> @*state :map :game-board)))
    (init-open-gl game)
    (reset! *game game)
    ;; load images and put them in the state atom
    (doseq [[k path] {:tile1 "tile.png"}]
       (utils/get-image path
                       (fn [{:keys [data width height]}]
                         (let [;; create an image entity (a map with info necessary to display it)
                               entity-bare (e/->image-entity game data width height)
                               ;; compile the shaders so it is ready to render
                               entity (c/compile game entity-bare)
                               entity-bare (assoc entity-bare :width width :height height)
                               ;; assoc the width and height to we can reference it later
                               entity (assoc entity :width width :height height)
                               ]
                           ;; add it to the state
                           (swap! *state update :basic-tile-loaded assoc k entity-bare)
                           (swap! *state update :player-images assoc k entity)
                           (swap! *state update :basic-tile-complete assoc :tris (load-map game @*state))
                           ))))

    )
  )

(def clear-color [(/ 173 255) (/ 216 255) (/ 230 255) 1])
(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color clear-color
           :depth 1}})

(defn run [game]
  (let [{:keys [pressed-keys
                camera-x
                camera-y
                direction
                player-images
                player-image-key
                camera]
         :as state} @*state
        game-width (utils/get-width game)
        game-height (utils/get-height game)
        camera (t/translate camera camera-x  camera-y)]
    ;; render the blue background
    (c/render game (update screen-entity :viewport
                           assoc :width game-width :height game-height))

    ;; render game board
    (when-let [tile (-> @*state :basic-tile-complete :tris)]
      (c/render game
                (-> (tiles/update-color-buffer-instances @*state)
                    (t/project game-width game-height)
                    (t/scale 30 30)
                    (t/camera camera)))
      (swap! *state
           (fn [state]
             (->> state
                  (move/move-tile game)
                  (move/move-tile-on-tick game)))))
    )

  ;; return the game map
  game)
