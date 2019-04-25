(ns kdtree-2d.core
  (:require [quil.core :as q]
            [thi.ng.math.core :as m]
            [thi.ng.geom.core :as g]
            [thi.ng.geom.rect :refer [rect top right bottom left
                                      top-right bottom-left]]
            [thi.ng.geom.vector :as v :refer [vec2 vec3]]
            [thi.ng.math.macros :as mm]
            [thi.ng.geom.macros.vector :as mv]
            [color-palette.core :as cp]
            [quil.middleware :as mid]))

(defmulti divide-rect (fn [r axis v] axis))

(defmethod divide-rect :x
  [r axis x]
  (let [x (- x (get-in r [:p axis]))
        r (g/translate r (m/- (:p r)))]
    [(rect (vec2) (vec2 x (top r)))
     (rect (vec2 x (bottom r)) (top-right r))]))

(defmethod divide-rect :y
  [r axis y]
  (let [y (- y (get-in r [:p axis]))
        r (g/translate r (m/- (:p r)))]
    [(rect (:p r) (vec2 (right r) y))
     (rect (vec2 (left r) y) (top-right r))]))

(defn divide [pts rect depth]
  (when (>= (count pts) 1)
    (let [top-left (:p rect)
          axis ([:x :y] (mod depth 2))
          sort-pts (vec (sort-by #(axis %) pts))
          median (quot (count pts) 2)
          translate #(g/translate % (m/- top-left))
          left-pts (map translate (subvec sort-pts 0 median))
          right-pts (map translate (subvec sort-pts (inc median)))
          div-rects (divide-rect
                     rect axis
                     (get-in sort-pts [median axis]))]
      {:pts pts
       :rect rect
       :depth depth
       :left (divide left-pts (div-rects 0) (inc depth))
       :right (divide right-pts (div-rects 1) (inc depth))})))

(defmacro transform-center [center & body]
  `(do (q/translate ~center)
       ~@body
       (q/translate (m/- ~center))))

(defn is-leaf [tree]
  (and (not (:left tree))
       (not (:right tree))))

;; define your colors
(def colors [[[150,206,180]
              [255,238,173]
              [217,83,79]
              [255,173,96]]])

(defn rand-color []
  (conj (rand-nth (rand-nth colors)) 220))

(defn draw-kdtree [tree]                      
  (when tree                                  
    (q/push-matrix)                           
    (let [rect (:rect tree)                   
          size (:size rect)]                  
      (q/translate (:p rect))
      (transform-center                       
       (m/div size 2)                         
       (q/scale (q/random 0.8 1.2)))
      (if (is-leaf tree)                      
        (do (q/fill (rand-color))
            (q/rect 0 0 (:x size) (:y size)))
        (do (draw-kdtree (:left tree))        
            (draw-kdtree (:right tree)))))    
    (q/pop-matrix)))

(defn setup []
  {:kdtree (-> (repeatedly 3000 #(vec2 (q/random (q/width))
                                       (q/random (q/height))))
               (divide (rect (q/width) (q/height)) 0))})

(defn update-state [state] state)

(defn draw-state [state]
  (q/background 255)
  (transform-center
   (vec2 (/ (q/width) 2) (/ (q/height) 2))
   (q/scale 0.7))
  (q/stroke 120 120)
  (draw-kdtree (:kdtree state))
  state)
 
(defn main []
  (q/defsketch kdtree-2d
    :size [1000 618]
    :title "kdtree-blends"
    :setup setup
    :update update-state
    :draw draw-state
    :features [:keep-on-top]
    :settings #(q/pixel-density (q/display-density))
    :middleware [mid/fun-mode]))

(main)
