(ns tetris.tiles
  (:require [tetris.utils :as utils]
            [tetris.tetris_move :as tmove]
            [tetris.tile_entity :as tiles]
            [play-cljc.transforms :as t]
            [play-cljc.math :as m]
            [play-cljc.instances :as i]
            [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            #?@(:clj [[clojure.java.io :as io]
                      [tile-soup.core :as ts]])))

#?(:clj (defmacro read-tiled-map [fname]
          (-> (str "public/" fname)
              io/resource
              slurp
              ts/parse
              pr-str)))


(defn update-all [game state]
  (let []
    ))

(defn create-graphics [game state]
  (let [img-tile (-> state :basic-tile-loaded :tile1)
        tilewidth 15 ;; TODO : create constant
        tileheight 15
        entity (tiles/->tile-entity img-tile tilewidth tileheight)
        entity (update-in entity [:uniforms 'u_color]
                          (fn [_] [0.5 0 1]))]

    (let [entity-c (i/->instanced-entity entity)
          entity-c (c/compile game entity-c)]
      {:compiled entity-c :raw-entity entity}
      )))
;; todo: entity-template is in state
(defn generate-tiles [state entity-template]
  ""
  (let [entity (:raw-entity entity-template)
        entity-c (:compiled entity-template)
        board (-> state :map :game-board)
        board-width (-> state :map :width)
        board-height (-> state :map :height)
        entities (for [j (range board-height)
                       i (range board-width)]
                   (let [item (aget board j i)]
                     (cond
                       (= 0 item)(update-in (t/translate entity i j) [:uniforms 'u_color]
                                            (fn [_] [1 1 1]))
                       (= 5 item) (update-in (t/translate entity i j) [:uniforms 'u_color]
                                             (fn [_] [0 1 1]))
                       :else (do
                               ;; (print "ERROR :: unknown element in board: " j " " i)
                               (update-in (t/translate entity i j) [:uniforms 'u_color]
                                          (fn [_] [1 0 0]))))))
        ;; draw active shape
        ;; shape (-> state :active-tile :shape)
        ;; idx (-> state :active-tile :rot)
        ;; active-row (-> state :active-tile :row)
        ;; active-column (-> state :active-tile :column)
        ;; coords (tmove/nth-rotation (-> tmove/tetrominoes shape :shape) idx)
        ;; entities (concat entities
        ;;                  (for [[x y] coords]
        ;;                    (update-in (t/translate entity
        ;;                                            (+ active-column y)
        ;;                                            (+ active-row x))
        ;;                               [:uniforms 'u_color]
        ;;                              (fn [_] [0 1 0]))))
        ]
    (vec entities)))

;; light-blue 34 128 240

(defn get-color-for-tile [tile]
  (case tile
    :B [1 1 1]
    :I [0.1 0.6 1]
    :J [0 1 0]
    :L [0 0 1]
    :O [0 1 1]
    :S [1 0 1]
    :T [1 1 0]
    :Z [0 0 0]
    [1 1 1]))


(defn set-color-to-attribute-array [data idx color]
  "Expecting color has size 3."
  ; could be done by reduce on variable lenght color
  (let [data (assoc! data idx (nth color 0))
        data (assoc! data (+ idx 1) (nth color 1))
        data (assoc! data (+ idx 2) (nth color 2))]
    data))

(defn get-idx-into-attribute-array [i j board-width]
  (* 3 (+ (* i board-width) j)))

(defn update-color-buffer-instances [state]
  "Updates drawing graphics every frame.
   It is important not to generate new objects.
   Function is only changing instanced buffers."
  (let [entity-i (get-in state [:basic-tile-complete :tris])
        board (-> state :map :game-board)
        board-width (-> state :map :width)
        a_color (get (:attributes entity-i) 'a_color)
        ;; this is the most trickest part
        ;; using double reduce to iterate over 2d array and mutating it in place
        ;; by transient and persistant!
        a_color (update a_color :data
                        (fn [old-data]
                          (persistent!
                           (reduce-kv
                            (fn [trans-data i row]
                              (reduce-kv
                               (fn [data j val]
                                 (let [idx (get-idx-into-attribute-array i j board-width)
                                       color (get-color-for-tile val)]
                                   (set-color-to-attribute-array data idx color)))
                               trans-data
                               row))
                            (transient old-data)
                            board))))
        ;; draw active tile
        active-shape (-> state :active-tile :shape)
        active-rot (-> state :active-tile :rot)
        active-row (-> state :active-tile :row)
        active-column (-> state :active-tile :column)
        active-coords (tmove/nth-rotation (-> tmove/tetrominoes active-shape :shape) active-rot)
        active-color (get-color-for-tile active-shape)
        a_color (update a_color :data
                        (fn [old-data]
                          (persistent!
                           (reduce
                            (fn [data [xx yy]]
                              (let [j (+ active-column yy)
                                    i (+ active-row xx)
                                    idx (get-idx-into-attribute-array i j board-width)]
                                (set-color-to-attribute-array data idx active-color)))
                            (transient old-data)
                            active-coords))))]
    (update-in entity-i [:attributes 'a_color] (fn [_] a_color))))


(defn load-simple-map [game state]
  (let [board (-> state :map :game-board)
        board-width (-> state :map :width)
        board-height (-> state :map :height)
        img-tile (-> state :basic-tile-loaded :tile1)
        width (:width img-tile) ;; img-tile is larger than tilewidth = 30
        height (:width img-tile)
        tilewidth 15
        tileheight 15
        entity (tiles/->tile-entity img-tile
                                    tilewidth tileheight)
        entity (update-in entity [:uniforms 'u_color]
                          (fn [_] [0.5 0 1]))
        entities (for [j (range board-height)
                       i (range board-width)]
                   (let [item (get-in board [j i])]
                     (update-in (t/translate entity i j) [:uniforms 'u_color]
                                (fn [_] [1 1 1]))))
        ;; draw active shape
        ;; shape (-> state :active-tile :shape)
        ;; idx (-> state :active-tile :rot)
        ;; active-row (-> state :active-tile :row)
        ;; active-column (-> state :active-tile :column)
        ;; coords (tmove/nth-rotation (-> tmove/tetrominoes shape :shape) idx)
        ;; entities (concat entities
        ;;                  (for [[x y] coords]
        ;;                    (update-in (t/translate entity
        ;;                                            (+ active-column y)
        ;;                                            (+ active-row x))
        ;;                               [:uniforms 'u_color]
        ;;                               (fn [_] [0 1 0]))))
        ]
    ;; (print (type entities))
    ;; (print (type (:entities m)))

    (let [entity (i/->instanced-entity entity)
          entity (c/compile game entity)
          entity (reduce-kv i/assoc entity (vec entities))]
      entity
      )
    ))

(defn load-tiled-map [game parsed callback]
  (let [map-width (-> parsed :attrs :width)
        map-height (-> parsed :attrs :height)
        tileset (first (filter #(= :tileset (:tag %)) (:content parsed)))
        image (first (filter #(= :image (:tag %)) (:content tileset)))
        {{:keys [tilewidth tileheight]} :attrs} tileset
        layers (->> parsed :content
                    (filter #(= :layer (:tag %)))
                    (map #(vector
                            (-> % :attrs :name)
                            (-> % :content first :content first)))
                    (into {}))]
    (utils/get-image (-> image :attrs :source)
      (fn [{:keys [data width height]}]
        (let [entity (tiles/->tile-entity (e/->image-entity game data width height) tilewidth tileheight)
              tiles-vert (/ height tileheight) ;; number of tiles withing an image
              tiles-horiz (/ width tilewidth)
              images (vec
                       (for [y (range tiles-vert) ;; genereate separete image from larger map
                             x (range tiles-horiz)]
                         (t/crop entity x y 1 1))) ;; pbly crop it to sub image?
              {:keys [layers tiles entities]}
              (reduce
                (fn [m layer-name]
                  (let [layer (get layers layer-name)]
                    (reduce
                      (fn [m i]
                        (let [x (mod i map-width)
                              y (int (/ i map-width)) ;; just indexing into tile map array
                              image-id (dec (nth layer i)) ;; nth image's ID in tile map
                              tile-map (when (>= image-id 0)
                                         {:layer layer-name :tile-x x :tile-y y})]
                          (cond-> m
                                  true (assoc-in [:layers layer-name x y] tile-map)
                                  tile-map (update :tiles conj tile-map)
                                  tile-map (update :entities conj
                                                   (t/translate (nth images image-id) x y)))))
                      m
                      (range (count layer))))) ;;  number of tiles in layer
                {:layers {}
                 :tiles []
                 :entities []}
                ["background" "walls"])
              entity (i/->instanced-entity entity)
              entity (c/compile game entity)
              entity (reduce-kv i/assoc entity entities)] ;; (fn ... (let
          (callback
            {:layers layers
             :tiles tiles
             :map-width map-width
             :map-height map-height}
            entity))))))

(defn touching-tile [{:keys [layers] :as tiled-map} layer-name x y width height]
  (let [layer (get layers layer-name)
        start-x (int x)
        start-y (int y)
        end-x (int (+ x width))
        end-y (int (+ y height))
        near-tiles (for [tile-x (range start-x (inc end-x))
                         tile-y (range start-y (inc end-y))]
                     (get-in layer [tile-x tile-y]))]
(some->> near-tiles (remove nil?) first)))
