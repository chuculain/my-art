(ns my-art.hydrogen-cloud
  (:require [quil.core :as q]
            [quil.middleware :as m]))


(defn clamp [x lo hi]
  (max lo (min hi x)))

(defn radians->degrees [radians]
  (* radians (/ 180.0 Math/PI)))

(def factorial (memoize (fn [n] (reduce *' 1N (range 1 (inc n))))))

(defn associated-laguerre
  "L_n^alpha(x) using stable recurrence."
  [n alpha x]
  (cond
    (= n 0) 1.0
    (= n 1) (+ 1.0 alpha (- x))
    :else
    (loop [k 1
           lkm1 1.0
           lk (+ 1.0 alpha (- x))]
      (if (= k n)
        lk
        (let [k1 (inc k)
              a (+ (* 2.0 k) 1.0 alpha (- x))
              b (+ k alpha)
              lkp1 (/ (- (* a lk) (* b lkm1)) k1)]
          (recur k1 lk lkp1))))))

(defn double-factorial [n]
  (if (<= n 0)
    1.0
    (reduce *' 1N (range n 0 -2))))

(defn associated-legendre
  "P_l^m(x) for m >= 0 and |x| <= 1."
  [l m x]
  (let [x (clamp x -1.0 1.0)
        pmm (if (zero? m)
              1.0
              (let [sgn (if (odd? m) -1.0 1.0)
                    pref (* sgn (double (double-factorial (dec (* 2 m)))))
                    term (Math/pow (max 0.0 (- 1.0 (* x x))) (/ m 2.0))]
                (* pref term)))]
    (cond
      (= l m) pmm
      (= l (inc m)) (* x (+ (* 2.0 m) 1.0) pmm)
      :else
      (loop [ll (+ m 2)
             pprev pmm
             prev (* x (+ (* 2.0 m) 1.0) pmm)]
        (if (> ll l)
          prev
          (let [curr (/ (- (* x (- (* 2.0 ll) 1.0) prev)
                           (* (+ ll m -1.0) pprev))
                        (- ll m))]
            (recur (inc ll) prev curr)))))))

(defn radial-wavefn [n l r a0]
  (let [rho (/ (* 2.0 r) (* n a0))
        k (- n l 1)]
    (if (neg? k)
      0.0
      (let [norm (* (Math/pow (/ 2.0 (* n a0)) 1.5)
                    (Math/sqrt
                     (/ (double (factorial k))
                        (* 2.0 n (double (factorial (+ n l)))))))
            lag (associated-laguerre k (+ (* 2 l) 1) rho)]
        (* norm
           (Math/exp (/ (- rho) 2.0))
           (Math/pow rho l)
           lag)))))

(defn angular-density [l m theta]
  (let [am (Math/abs (long m))
        plm (associated-legendre l am (Math/cos theta))
        num (* (+ (* 2 l) 1)
               (double (factorial (- l am))))
        den (* 4.0 Math/PI (double (factorial (+ l am))))
        nrm (/ num den)]
    (* nrm plm plm)))

(defn probability-density [n l m x y z a0]
  (let [r (Math/sqrt (+ (* x x) (* y y) (* z z)))]
    (if (< r 1.0e-9)
      (if (zero? l)
        (let [rad (radial-wavefn n l 0.0 a0)]
          (* rad rad (/ 1.0 (* 4.0 Math/PI))))
        0.0)
      (let [theta (Math/acos (clamp (/ z r) -1.0 1.0))
            rad (radial-wavefn n l r a0)
            ang (angular-density l m theta)]
        (* rad rad ang)))))

(defn valid-quantum-state? [n l m]
  (and (pos? n)
       (<= 0 l)
       (< l n)
       (<= (Math/abs (long m)) l)))

(defn clamp-quantum-counters [n l m]
  (let [n' (int (clamp n 1 8))
        l' (int (clamp l 0 (dec n')))
        m' (int (clamp m (- l') l'))]
    [n' l' m']))

(defn step-counters-up [n l m]
  (cond
    ;; Traverse all m values for current l from -l to +l.
    (< m l)         [n l (inc m)]
    ;; Then move to next l and start its m-range at -l.
    (< l (dec n))   (let [l' (inc l)] [n l' (- l')])
    ;; Then advance to next n and restart at 1s (l=0,m=0).
    (< n 8)         [(inc n) 0 0]
    :else           [n l m]))

(defn step-counters-down [n l m]
  (cond
    ;; Traverse all m values for current l from +l down to -l.
    (> m (- l)) [n l (dec m)]
    ;; Then move to previous l and land on its max m (+l).
    (> l 0)      (let [l' (dec l)] [n l' l'])
    ;; Then move to previous n and land on its final state (l=n-1,m=+l).
    (> n 1)      (let [n' (dec n)
                       l' (dec n')]
                   [n' l' l'])
    :else        [n l m]))

(defn rmax-for-n [n a0]
  (* (max 8.0 (* 5.0 n n)) a0))

;; --- Heatmap color mapping (black→blue→cyan→green→yellow→red→white) ---
(defn heatmap-color
  "Maps t in [0,1] to an ARGB int using a smooth black→blue→cyan→green→yellow→red→white gradient."
  [t]
  (let [t (clamp t 0.0 1.0)
        ;; 6 segments of equal width
        seg (int (Math/floor (* t 6.0)))
        f   (- (* t 6.0) seg)
        [r g b]
        (case seg
          0 [0.0        0.0        f]           ; black -> blue
          1 [0.0        f          1.0]          ; blue  -> cyan
          2 [0.0        1.0        (- 1.0 f)]   ; cyan  -> green
          3 [f          1.0        0.0]          ; green -> yellow
          4 [1.0        (- 1.0 f) 0.0]          ; yellow-> red
          5 [1.0        f          f]            ; red   -> white
          [1.0 1.0 1.0])]
    (unchecked-int
     (bit-or (bit-shift-left 0xff 24)
             (bit-shift-left (int (* r 255)) 16)
             (bit-shift-left (int (* g 255)) 8)
             (int (* b 255))))))

(defn max-of-grid [grid]
  (areduce grid i acc 0.0 (max acc (aget grid i))))

(defn point-probability
  [n l m a0 plane scale-u scale-v half-w half-h px py]
  (let [u (* (- (double px) half-w) scale-u)
        v (* (- (double py) half-h) scale-v)]
    (case plane
      :xy (probability-density n l m u v 0.0 a0)
      :yz (probability-density n l m 0.0 u v a0)
      :xz (probability-density n l m u 0.0 v a0)
      ;; Fallback keeps behavior sensible if view config is extended.
      (probability-density n l m u v 0.0 a0))))

(defn nearest-upsample-grid
  "Creates a resized grid from src-grid using nearest-neighbor sampling."
  [src-grid src-w src-h dst-w dst-h]
  (let [^doubles out (double-array (* dst-w dst-h))
        x-scale (/ (double src-w) dst-w)
        y-scale (/ (double src-h) dst-h)]
    (dotimes [py dst-h]
      (let [sy (int (clamp (Math/floor (* py y-scale)) 0 (dec src-h)))
            src-row (* sy src-w)
            dst-row (* py dst-w)]
        (dotimes [px dst-w]
          (let [sx (int (clamp (Math/floor (* px x-scale)) 0 (dec src-w)))]
            (aset-double out (+ dst-row px) (aget src-grid (+ src-row sx)))))))
    out))

(defn coarse-gradient-grid
  "Per-cell local gradient estimate from right/down neighbors."
  [coarse-grid coarse-w coarse-h]
  (let [^doubles grads (double-array (* coarse-w coarse-h))]
    (dotimes [y coarse-h]
      (let [row (* y coarse-w)
            yn (min (dec coarse-h) (inc y))
            row-down (* yn coarse-w)]
        (dotimes [x coarse-w]
          (let [xn (min (dec coarse-w) (inc x))
                i (+ row x)
                p (aget coarse-grid i)
                dx (Math/abs (- (aget coarse-grid (+ row xn)) p))
                dy (Math/abs (- (aget coarse-grid (+ row-down x)) p))]
            (aset-double grads i (max dx dy))))))
    grads))

;; --- Precompute a grid of probability values for one panel slice ---
(defn compute-panel-grid
  "Parallel, cache-friendly grid computation. Each CPU core handles a row-chunk.
   Trig and scale constants are hoisted outside the inner loop."
  [n l m a0 rmax grid-w grid-h plane]
  (let [^doubles arr (double-array (* grid-w grid-h))
        ;; pre-divide so the inner loop only multiplies
        scale-u (/ (* 2.0 rmax) grid-w)
        scale-v (/ (* 2.0 rmax) grid-h)
        half-w  (* 0.5 grid-w)
        half-h  (* 0.5 grid-h)
        n-cpu   (.. Runtime getRuntime availableProcessors)
        chunks  (partition-all (max 1 (quot grid-h n-cpu)) (range grid-h))]
    (dorun
     (pmap (fn [row-chunk]
             (doseq [py row-chunk]
               (let [row-offset (* py grid-w)]
                 (dotimes [px grid-w]
                   (let [p (point-probability n l m a0
                                              plane
                                              scale-u scale-v
                                              half-w half-h
                                              px py)]
                     (aset-double arr (+ row-offset px) p))))))
           chunks))
    arr))

(defn compute-panel-grid-adaptive
  "Adaptive coarse-to-fine panel computation.
   Computes a small coarse grid, upsamples it, then refines only pixels with
   high probability mass or high local gradient."
  [n l m a0 rmax target-w target-h plane lod]
  (let [coarse-scale (double (or (:coarse-scale lod) 0.35))
        refine-scale (double (or (:refine-scale lod) 2.0))
        prob-thresh  (double (or (:refine-prob-threshold lod) 0.06))
        grad-thresh  (double (or (:refine-grad-threshold lod) 0.12))
        min-coarse   (int (or (:min-coarse-size lod) 48))
        coarse-w     (int (clamp (Math/round (* target-w coarse-scale)) min-coarse target-w))
        coarse-h     (int (clamp (Math/round (* target-h coarse-scale)) min-coarse target-h))
        fine-w       (int (clamp (Math/round (* coarse-w refine-scale)) coarse-w target-w))
        fine-h       (int (clamp (Math/round (* coarse-h refine-scale)) coarse-h target-h))
        coarse-grid  (compute-panel-grid n l m a0 rmax coarse-w coarse-h plane)
        coarse-pmax  (max 1.0e-20 (max-of-grid coarse-grid))
        grad-grid    (coarse-gradient-grid coarse-grid coarse-w coarse-h)
        fine-grid    (nearest-upsample-grid coarse-grid coarse-w coarse-h fine-w fine-h)
        x-map        (/ (double coarse-w) fine-w)
        y-map        (/ (double coarse-h) fine-h)
        scale-u      (/ (* 2.0 rmax) fine-w)
        scale-v      (/ (* 2.0 rmax) fine-h)
        half-w       (* 0.5 fine-w)
        half-h       (* 0.5 fine-h)]
    (if (or (= fine-w coarse-w) (= fine-h coarse-h))
      {:grid coarse-grid
       :grid-w coarse-w
       :grid-h coarse-h
       :coarse-samples (* coarse-w coarse-h)
       :refined-samples 0
       :samples (* coarse-w coarse-h)}
      (let [refined-count
            (volatile! 0)]
        (dotimes [py fine-h]
          (let [cy (int (clamp (Math/floor (* py y-map)) 0 (dec coarse-h)))
                coarse-row (* cy coarse-w)
                fine-row (* py fine-w)]
            (dotimes [px fine-w]
              (let [cx (int (clamp (Math/floor (* px x-map)) 0 (dec coarse-w)))
                    ci (+ coarse-row cx)
                    cp (aget coarse-grid ci)
                    cg (aget grad-grid ci)
                    p-ratio (/ cp coarse-pmax)
                    g-ratio (/ cg coarse-pmax)]
                (when (or (> p-ratio prob-thresh)
                          (> g-ratio grad-thresh))
                  (aset-double fine-grid (+ fine-row px)
                               (point-probability n l m a0
                          plane
                                                  scale-u scale-v
                                                  half-w half-h
                                                  px py))
                  (vswap! refined-count inc))))))
        {:grid fine-grid
         :grid-w fine-w
         :grid-h fine-h
         :coarse-samples (* coarse-w coarse-h)
         :refined-samples @refined-count
         :samples (+ (* coarse-w coarse-h) @refined-count)}))))

(defn compute-all-panels
  "Computes all panels concurrently using adaptive sampling."
  [n l m a0 rmax grid-w grid-h views lod]
  (let [results (vec (pmap (fn [{:keys [plane]}]
                             (compute-panel-grid-adaptive
                              n l m a0 rmax grid-w grid-h plane lod))
                           views))
        out-w (:grid-w (first results) grid-w)
        out-h (:grid-h (first results) grid-h)
        total-coarse (reduce + (map :coarse-samples results))
        total-refined (reduce + (map :refined-samples results))]
    {:grids (mapv :grid results)
     :grid-w out-w
     :grid-h out-h
     :compute-stats {:coarse-samples total-coarse
                     :refined-samples total-refined
                     :total-samples (+ total-coarse total-refined)
                     :nominal-full-samples (* grid-w grid-h (count views))}}))

;; --- State management ---
(def panel-views
  [{:plane :xy :label "XY plane (z=0)" :col 0 :row 0}
   {:plane :yz :label "YZ plane (x=0)" :col 1 :row 0}
   {:plane :xz :label "XZ plane (y=0)" :col 2 :row 0}])

(defn grids->pmax [grids]
  (reduce (fn [out grid]
            (let [arr-max (max-of-grid grid)]
              (max out arr-max)))
          1.0e-20 grids))

(defn grid->pimage [grid grid-w grid-h pmax]
  (let [img (q/create-image grid-w grid-h :argb)]
    (.loadPixels img)
    (let [pxs (.pixels img)]
      (dotimes [i (* grid-w grid-h)]
        (let [p (aget grid i)
              t (clamp (if (> pmax 0.0) (Math/sqrt (/ p pmax)) 0.0) 0.0 1.0)]
          (aset pxs i (heatmap-color t)))))
    (.updatePixels img)
    img))

;; Pure math — safe to call from any background thread.
(defn compute-grids [state]
  (let [{:keys [n l m a0 rmax grid-w grid-h lod]} state
        {:keys [grids grid-w grid-h compute-stats]}
        (compute-all-panels n l m a0 rmax grid-w grid-h panel-views lod)
        pmax  (grids->pmax grids)]
    (assoc state
           :grids grids
           :pmax pmax
           :effective-grid-w grid-w
           :effective-grid-h grid-h
           :compute-stats compute-stats)))

;; Quil API (PImage creation) — must run on the sketch render thread.
(defn grids->imgs [state]
  (let [{:keys [grids pmax grid-w grid-h effective-grid-w effective-grid-h]} state
        render-w (or effective-grid-w grid-w)
        render-h (or effective-grid-h grid-h)
        imgs (mapv #(grid->pimage % render-w render-h pmax) grids)]
    (assoc state :imgs imgs :computing? false)))

(defn rebuild-grids [state]
  (-> state compute-grids grids->imgs))

(defn with-quantum-counters [state n l m]
  (let [[n l m] (clamp-quantum-counters n l m)
        rmax    (rmax-for-n n (:a0 state))
        base    (assoc state :n n :l l :m m :rmax rmax :computing? true)
        fut     (future (compute-grids base))]
    (assoc base :compute-future fut)))

(def up-key-codes #{38 63232})
(def down-key-codes #{40 63233})

(defn up-arrow? [k kc]
  (or (= k :up) (contains? up-key-codes kc)))

(defn down-arrow? [k kc]
  (or (= k :down) (contains? down-key-codes kc)))

(defn handle-key-pressed [state event]
  (let [k (:key event)
        kc (int (:key-code event))]
    (cond
      (up-arrow? k kc)
      (let [[n l m] (step-counters-up (:n state) (:l state) (:m state))]
        (with-quantum-counters state n l m))

      (down-arrow? k kc)
      (let [[n l m] (step-counters-down (:n state) (:l state) (:m state))]
        (with-quantum-counters state n l m))

      :else state)))

(defn setup []
  (q/frame-rate 10)
  (q/color-mode :rgb 255 255 255 255)
  (let [n 2 l 1 m 0 a0 52.0
        [n l m] (if (valid-quantum-state? n l m) [n l m] [1 0 0])
        ;; Target grid; adaptive LOD computes fewer points in practice.
        grid-w 192
        grid-h 192
        lod {:coarse-scale 0.35
             :refine-scale 2.0
             :refine-prob-threshold 0.06
             :refine-grad-threshold 0.12
             :min-coarse-size 48}
        base   {:n n :l l :m m
                :a0 a0
                :rmax (rmax-for-n n a0)
                :grid-w grid-w
                :grid-h grid-h
                :effective-grid-w grid-w
                :effective-grid-h grid-h
                :lod lod
                :computing? true
                :grids []
                :imgs []
                :compute-stats nil}
        fut    (future (compute-grids base))]
    (assoc base :compute-future fut)))

(defn update-state [state]
  ;; When a background compute-future is ready, convert its grids to PImages
  ;; on the render thread (Quil requirement) and swap it into state.
  (if-let [fut (:compute-future state)]
    (if (realized? fut)
      (-> @fut grids->imgs (dissoc :compute-future))
      state)
    state))

(defn draw-state [state]
  (q/background 12 12 20)
  (let [w        (q/width)
        h        (q/height)
        outer-pad 22.0
      title-y  (+ outer-pad 6.0)
      sub-y    (+ title-y 20.0)
      grid-top (+ sub-y 24.0)
        gap      14.0
        cols     (inc (reduce max 0 (map :col panel-views)))
        rows     (inc (reduce max 0 (map :row panel-views)))
        panel-w  (/ (- w (* 2.0 outer-pad) (* gap (dec cols))) cols)
        panel-h  (/ (- h grid-top outer-pad (* gap (dec rows))) rows)
        imgs     (:imgs state)]

    ;; Title / HUD
    (q/fill 240 240 255)
    (q/text-size 22)
    (q/text "Hydrogen Orbital Probability Density" outer-pad title-y)
    (let [stats (:compute-stats state)
          eff-w (:effective-grid-w state)
          eff-h (:effective-grid-h state)
          nominal (or (:nominal-full-samples stats) 1)
          used (or (:total-samples stats) nominal)
          saved (int (Math/round (* 100.0 (max 0.0 (- 1.0 (/ (double used) nominal))))))]
      (q/text-size 13)
      (q/text (str "n=" (:n state) "  l=" (:l state) "  m=" (:m state)
                   "   XY/YZ/XZ slices   map=" eff-w "x" eff-h
                   "   saved~" saved "%   (UP/DOWN to browse states)")
              outer-pad sub-y))

    ;; Panels
    (doseq [[idx {:keys [label col row]}] (map-indexed vector panel-views)]
      (let [panel-x (+ outer-pad (* col (+ panel-w gap)))
            panel-y (+ grid-top (* row (+ panel-h gap)))]
        ;; background
        (q/no-stroke)
        (q/fill 4 4 10)
        (q/rect panel-x panel-y panel-w panel-h)
        (when-let [img (nth imgs idx nil)]
          (q/image img panel-x panel-y panel-w panel-h))
        ;; label
        (q/fill 210 210 255 200)
        (q/text-size 12)
        (q/text label (+ panel-x 8.0) (+ panel-y panel-h -8.0))))

    ;; Color scale legend
        (let [legend-x (- w outer-pad 220.0)
          legend-y (- h outer-pad 44.0)
          bar-w    180.0
          bar-h    14.0]
      (q/no-stroke)
      (q/fill 20 20 30 200)
      (q/rect legend-x legend-y 220.0 44.0)
      (dotimes [i (int bar-w)]
        (let [t    (/ i bar-w)
              argb (heatmap-color t)
              r    (bit-and (bit-shift-right argb 16) 0xff)
              g    (bit-and (bit-shift-right argb 8)  0xff)
              b    (bit-and argb 0xff)]
          (q/fill r g b)
          (q/rect (+ legend-x 20.0 i) (+ legend-y 8.0) 1.0 bar-h)))
      (q/fill 220 220 255)
      (q/text-size 11)
      (q/text "0" (+ legend-x 18.0) (+ legend-y 36.0))
      (q/text "max" (+ legend-x 190.0) (+ legend-y 36.0))
      (q/text "probability" (+ legend-x 80.0) (+ legend-y 36.0)))
    (when (:computing? state)
      (let [cx (/ w 2.0)
            cy (/ (+ grid-top h) 2.0)]
        (q/fill 0 0 0 160)
        (q/no-stroke)
        (q/rect outer-pad grid-top (- w (* 2.0 outer-pad)) (- h grid-top outer-pad))
        (q/fill 255 210 60)
        (q/text-size 22)
        (q/text-align :center :center)
        (q/text (str "Computing orbital  n=" (:n state)
                     "  l=" (:l state) "  m=" (:m state) "…")
                cx cy)
        (q/text-align :left :baseline)))))

(q/defsketch hydrogen-cloud
  :title "Hydrogen Orbital Probability Density"
  :size [1024 768]
  :host "host"
  :setup setup
  :update update-state
  :key-pressed handle-key-pressed
  :draw draw-state
  :middleware [m/fun-mode])