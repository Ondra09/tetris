(ns tetris.tetris_move
    (:require [tetris.utils :as utils]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])))

(def ^:const initial-speed 3)
(def ^:const width 10)
(def ^:const height 24) ;; actual board has 20 heigh

(def ^:const tetrominoes {:I {:shape [[[-1 0] [0 0] [1 0] [2 0]]
                                      [[0 -1] [0 0] [0 1] [0 2]]]}

                          :J {:shape [[[-1 0] [0 0] [1 0] [1 -1]]
                                      [[-1 -1] [0 -1] [0 0] [0 1]]
                                      [[-1 0] [-1 1] [0 0] [1 0]]
                                      [[0 -1] [0 0] [0 1] [1 1]]]}

                          :L {:shape [[[-1 0] [0 0] [1 0] [1 1]]
                                      [[0 -1] [1 -1] [0 0] [0 1]]
                                      [[-1 -1] [-1 0] [0 0] [1 0]]
                                      [[0 -1] [0 0] [0 1] [-1 1]]]}

                          :O {:shape [[[0 0] [1 0] [0 1] [1 1]]]}

                          :S {:shape [[[0 -1] [0 0] [-1 0] [-1 1]]
                                      [[-1 -1] [0 -1] [0 0] [1 0]]]}

                          :T {:shape [[[0 0] [0 -1] [0 1] [1 0]]
                                      [[0 0] [0 -1] [-1 0] [1 0]]
                                      [[0 0] [0 -1] [0 1] [-1 0]]
                                      [[0 0] [-1 0] [0 1] [1 0]]]}

                          :Z {:shape [[[0 -1] [0 0] [1 0] [1 1]]
                                      [[1 -1] [0 -1] [0 0] [-1 0]]]}})


(defn nth-rotation [rotations index]
  (let [idx (mod index (count rotations))]
    (nth rotations idx)))

(defn get-coords
  ([mark rotation] (get-coords mark rotation tetrominoes))
  ([mark rotation tetrominoes]
  (nth-rotation (-> tetrominoes mark :shape) rotation)))

(defn put-tetromino [array row col tetro-mark rotation]
  (let [coords (get-coords tetro-mark rotation)]
    ;; todo :prevent creation of new vector
    (reduce
     (fn [data [xx yy]]
       (update-in data [(+ row xx) (+ col yy)] (constantly tetro-mark)))
     array
     coords)))

(defn put-tetromino-from-state [array {:keys [row column shape rot] :as active-tile}]
  "bakes tetronimo into board"
  (put-tetromino array row column shape rot))

(defn spawn-new-tetro [& [old-tetro]]
  {:row 1 :column 5 :shape (rand-nth (keys tetrominoes)) :rot 0})


(defn full-line? [line]
  (every? (fn [x] (not (= :B x))) line))

;; TODO: this can be rewritten and optimalized
(defn squeeze-board [array]
  (let [shifted (reduce-kv
   (fn [data idx line]
     (if (full-line? line)
       (update-in data [:shift] inc)
       (update-in data [:board (- idx (:shift data))] (fn [old-line]

                                                        line))))
   { :shift 0 :board (vec (reverse array))}
   (into [] (reverse array)))
        height (- (count (:board shifted)) 1)
        width (count (first (:board shifted)))]
    (vec (reverse (reduce
     (fn [array idx]
       (assoc array (- height idx) (vec (repeat width :B))))
     (:board shifted)
     (range (:shift shifted)))
    ))))

(defn find-non-empty-lines [array]
  (reduce-kv (fn [buf idx line]
            (if (full-line? line)
              (conj buf idx)
              buf
              ))
            []
            array))

(defn squeeze-lines [array reduce-indexes]
  (reduce
   (fn [data idx]
     (let [up-idx (dec idx)]))
   array
   reduce-indexes))
