(ns tetris.tile_entity
  (:require [play-cljc.math :as m]
            [play-cljc.transforms :as t]
            [play-cljc.instances :as i]
            [play-cljc.gl.utils :as u]))

(def ^:private ^:const reverse-matrix (m/scaling-matrix -1 -1))

;; InstancedTileEntity

(def ^:private instanced-tile-vertex-shader
  {:inputs
   '{a_position vec2
     a_matrix mat3
     a_texture_matrix mat3
     a_color vec3
     }
   :uniforms
   '{u_project_matrix mat3
     u_scale_matrix mat3
     u_camera_matrix mat3
     ;;u_color vec3
     }
   :outputs
   '{v_tex_coord vec2
     v_color_out vec4}
   :signatures
   '{main ([] void)}
   :functions
   '{main ([]
           (= gl_Position
              (vec4
                (.xy (* u_project_matrix
                        u_scale_matrix
                        a_matrix
                        u_camera_matrix
                        (vec3 a_position 1)))
                0 1))
           (= v_tex_coord (.xy (* a_texture_matrix (vec3 a_position 1))))
           (= v_color_out (vec4 a_color 1)))}})

(def ^:private instanced-tile-fragment-shader
  {:precision "mediump float"
   :uniforms
   '{u_image sampler2D}
   :inputs
   '{v_tex_coord vec2
     v_color_out vec4}
   :outputs
   '{o_color vec4}
   :signatures
   '{main ([] void)}
   :functions
   '{main ([] (= o_color (* v_color_out (texture u_image v_tex_coord))))}})

(def ^:private instanced-tile-attrs->unis
  '{a_matrix u_matrix
    a_texture_matrix u_texture_matrix
    a_color u_color
    })

(defrecord InstancedTileEntity [instance-count])

(extend-type InstancedTileEntity
  t/IProject
  (project [entity width height]
    (update-in entity [:uniforms 'u_project_matrix]
      #(m/multiply-matrices 3 (m/projection-matrix width height) %)))
  t/IScale
  (scale [entity x y]
    (update-in entity [:uniforms 'u_scale_matrix]
      #(m/multiply-matrices 3 (m/scaling-matrix x y) %)))
  t/ICamera
  (camera [entity {:keys [matrix]}]
    (update-in entity [:uniforms 'u_camera_matrix]
      #(->> %
            (m/multiply-matrices 3 matrix)
            (m/multiply-matrices 3 reverse-matrix))))
  i/IInstanced
  (assoc [instanced-entity i entity]
    (reduce-kv
      (partial u/assoc-instance-attr i entity)
      instanced-entity
      instanced-tile-attrs->unis))
  (dissoc [instanced-entity i]
    (reduce-kv
      (partial u/dissoc-instance-attr i)
      instanced-entity
      instanced-tile-attrs->unis)))

;; TileEntity

(defrecord TileEntity [width height tile-width tile-height])

(extend-type TileEntity
  t/ITranslate
  (translate [entity x y]
    (update-in entity [:uniforms 'u_matrix]
      #(m/multiply-matrices 3 (m/translation-matrix x y) %)))
  t/ICrop
  (crop [{:keys [width height tile-width tile-height] :as entity}
         crop-x crop-y crop-width crop-height]
    (let [crop-x (* tile-width crop-x)
          crop-y (* tile-height crop-y)
          crop-width (* tile-width crop-width)
          crop-height (* tile-height crop-height)]
      (update-in entity [:uniforms 'u_texture_matrix]
        #(->> %
              (m/multiply-matrices 3
                (m/translation-matrix (/ crop-x width) (/ crop-y height)))
              (m/multiply-matrices 3
                (m/scaling-matrix (/ crop-width width) (/ crop-height height)))))))
  i/IInstance
  (->instanced-entity [entity]
    (-> entity
        (assoc :vertex instanced-tile-vertex-shader
               :fragment instanced-tile-fragment-shader)
        (update :uniforms dissoc 'u_matrix 'u_texture_matrix 'u_color)
        (update :uniforms merge {'u_project_matrix (m/identity-matrix 3)
                                 'u_scale_matrix (m/identity-matrix 3)
                                 'u_camera_matrix (m/identity-matrix 3)})
        (update :attributes merge {'a_matrix {:data [] :divisor 1}
                                   'a_texture_matrix {:data [] :divisor 1}
                                   'a_color {:data [] :divisor 1}})
        map->InstancedTileEntity)))

(defn ->tile-entity [image-entity tile-width tile-height]
  (-> image-entity
      (assoc :tile-width tile-width
             :tile-height tile-height)
      map->TileEntity))
