(ns tetris.move
  (:require [tetris.utils :as utils]
            [tetris.tetris_move :as tmove]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])))

(def ^:const actions-per-second 10)
(def ^:const moves-per-second 2)

(defn limit-key-press [last-action current-time]
  (let [time-frame (/ 1 actions-per-second)
        action-delta (- current-time last-action)]
    (if (> action-delta time-frame)
      current-time
      false)))

(defn react-to-action [{:keys [pressed-keys] :as state}]
  (cond
      (contains? pressed-keys :up) (update-in state [:active-tile :rot] inc)
      (contains? pressed-keys :left) (update-in state [:active-tile :column] dec)
      (contains? pressed-keys :right) (update-in state [:active-tile :column] inc)
      (contains? pressed-keys :down) (update-in state [:active-tile :row] inc)
      :else state
      ))

(defn are-coords-valid? [coords row col board]
  (and
   (every? (fn [[x y]]
             (and (> (+ row x) -1)
                  (> (+ col y) -1)
                  (< (+ row x) tmove/height)
                  (< (+ col y) tmove/width)))
           coords)
   (every? (fn [[x y]]
             (let [row-x (+ row x)
                   col-y (+ col y)]
               (= :B (nth (nth board row-x) col-y))))
           coords)))

(defn move-tile [{:keys [total-time] :as game}
                 {:keys [last-action-time active-tile map] :as state}]
  (if (limit-key-press last-action-time total-time)
    (let [state-new (react-to-action state)
          row (-> state-new :active-tile :row)
          col (-> state-new :active-tile :column)
          rot (-> state-new :active-tile :rot)
          coords (tmove/get-coords (:shape active-tile) rot)]
      (if (are-coords-valid? coords row col (:game-board map))
        (update-in state-new [:last-action-time] (constantly total-time))
        state))
    state
    ))

(defn can-move [{:keys [active-tile map] :as state}]
  (let [coords (tmove/get-coords (:shape active-tile) (:rot active-tile))
        row-x (:row active-tile)
        col-y (:column active-tile)]
    (are-coords-valid? coords (+ row-x 1) col-y (:game-board map))))

;; TODO: make proper end game, stop game loop, show results
(defn try-place-new-tile [state]
  "Try to place new tetris tile. In case of fail we have game over."
  (let
      [state-new-tetro (update-in state [:active-tile] tmove/spawn-new-tetro)]
    (if (can-move state-new-tetro)
      state-new-tetro
      ;; this is game over; return old state
      state)))

(defn move-tile-on-tick [{:keys [delta-time total-time] :as game}
                         {:keys [last-auto-move] :as state}]
  (if (> total-time (+ (/ 1 moves-per-second) last-auto-move))
    (if (can-move state)
      (let
          [state (update-in state [:last-auto-move] (constantly total-time))
           state (update-in state [:active-tile :row] inc)]
        state)
      ;; we ticked but we cannot move, spawn new tris and bake current one into board
      (let [game-board (tmove/put-tetromino-from-state (-> state :map :game-board)
                                                       (-> state :active-tile))
            game-board (tmove/squeeze-board game-board)
             state (update-in state [:map :game-board] (constantly game-board))]
         (try-place-new-tile state))
      )
    state))
