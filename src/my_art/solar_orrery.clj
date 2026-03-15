(ns my-art.solar-orrery
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def ^:const TWO-PI (* 2.0 Math/PI))

;;; ─── TIME CONTROL ────────────────────────────────────────────────────────────
;;; Two independent clocks:
;;;   sim-year   → drives orbital positions (slow, so orbits are visible)
;;;   epoch-year → drives cosmic colour palette + sun size (fast sweep of 10 Ga)
(def ^:const SIM-YPF   2.0)       ;; sim years per frame  (~2 yr / frame)
(def ^:const EPOCH-YPF 5.0e6)     ;; epoch years per frame (~5 Myr / frame)
(def ^:const EPOCH-MAX 1.0e10)    ;; 10 billion years — full solar lifecycle

;;; ─── PLANET DATA ─────────────────────────────────────────────────────────────
;;; :period  orbital period in Earth years
;;; :orbit-r display orbit radius in pixels (from screen centre)
;;; :size    body radius in pixels
;;; :hue     base HSB hue (0-360) for the current stable era
(def planet-catalogue
  [{:name "Mercury" :period   0.2408 :orbit-r  62 :size  3 :hue  22}
   {:name "Venus"   :period   0.6152 :orbit-r  97 :size  5 :hue  45}
   {:name "Earth"   :period   1.0000 :orbit-r 132 :size  5 :hue 200}
   {:name "Mars"    :period   1.8808 :orbit-r 172 :size  4 :hue  12}
   {:name "Jupiter" :period  11.8618 :orbit-r 232 :size 12 :hue  36}
   {:name "Saturn"  :period  29.4571 :orbit-r 280 :size  9 :hue  52 :rings? true}
   {:name "Uranus"  :period  84.0107 :orbit-r 320 :size  7 :hue 185}
   {:name "Neptune" :period 164.7960 :orbit-r 358 :size  6 :hue 220}])

;;; ─── ASTEROID BELT ───────────────────────────────────────────────────────────
(defn make-asteroids [n]
  (vec (repeatedly n
                   #(hash-map :period  (+ 3.2 (rand 2.2))   ;; 3.2–5.4 yr
                               :orbit-r (+ 196 (rand-int 30))
                               :phase   (* TWO-PI (rand))))))

;;; ─── DEEP-TIME COLOUR PALETTE ────────────────────────────────────────────────
(defn lerp [a b t] (+ a (* (- b a) t)))

(defn epoch-t [epoch-year] (min 1.0 (/ epoch-year EPOCH-MAX)))

;;; Background hue: warm protostellar orange → cosmic blue → dying red
(defn bg-hue [t]
  (cond
    (< t 0.04) (lerp  28.0 220.0 (/ t 0.04))
    (< t 0.85) (lerp 220.0 200.0 (/ (- t 0.04) 0.81))
    :else       (lerp 200.0  10.0 (/ (- t 0.85) 0.15))))

;;; Per-planet / per-star hue nudge based on epoch
(defn hue-bias [t]
  (cond
    (< t 0.10) (lerp 20.0  0.0 (/ t 0.10))
    (< t 0.85)  0.0
    :else       (lerp  0.0 -20.0 (/ (- t 0.85) 0.15))))

;;; ─── SUN ─────────────────────────────────────────────────────────────────────
(defn sun-radius [t]
  (cond
    (< t 0.04) (lerp   4.0   16.0 (/ t 0.04))
    (< t 0.85) (lerp  16.0   20.0 (/ (- t 0.04) 0.81))
    :else       (lerp  20.0  130.0 (/ (- t 0.85) 0.15))))

(defn sun-hue [t]
  (if (< t 0.85) 42.0
      (lerp 42.0 4.0 (/ (- t 0.85) 0.15))))

(defn draw-sun [cx cy t frame]
  (let [r     (sun-radius t)
        hue   (sun-hue t)
        ;; ~11-year solar cycle compressed to a gentle pulse
        pulse (* 1.4 (Math/sin (* TWO-PI (/ frame 36))))]
    (q/no-stroke)
    ;; layered corona glow
    (doseq [layer (range 7)]
      (let [lr    (+ (* r 2.6) (* layer 6.0) pulse)
            alpha (max 0 (int (- 38 (* layer 7))))]
        (q/fill hue 140 255 alpha)
        (q/ellipse cx cy (* 2 lr) (* 2 lr))))
    ;; photosphere
    (q/fill hue 200 255 235)
    (q/ellipse cx cy (* 2 (+ r pulse)) (* 2 (+ r pulse)))))

;;; ─── ORBITAL MECHANICS ───────────────────────────────────────────────────────
(defn planet-angle [planet year]
  ;; modulo avoids floating-point drift at large sim-years
  (+ (:phase planet)
     (* TWO-PI (/ (mod year (:period planet)) (:period planet)))))

(defn planet-xy [planet year cx cy]
  (let [a (planet-angle planet year)
        r (:orbit-r planet)]
    [(+ cx (* r (Math/cos a)))
     (+ cy (* r (Math/sin a)))]))

;;; ─── JUPITER–SATURN RESONANCE SPIROGRAPH ────────────────────────────────────
;;; The near 5∶2 commensurability (~19.86-year synodic cycle) produces
;;; an exquisite rose curve when you trace their orbital midpoint over time.
(defn resonance-pts [jupiter saturn sim-year cx cy n lookback]
  (let [dt (/ lookback (double n))]
    (mapv (fn [i]
            (let [y       (- sim-year (* i dt))
                  [jx jy] (planet-xy jupiter y cx cy)
                  [sx sy] (planet-xy saturn  y cx cy)]
              [(* 0.5 (+ jx sx))
               (* 0.5 (+ jy sy))]))
          (range n))))

;;; ─── DRAW HELPERS ────────────────────────────────────────────────────────────
(defn draw-starfield [stars t frame]
  (q/no-stroke)
  (doseq [{:keys [x y r phase]} stars]
    (let [tw    (Math/sin (+ phase (* frame 0.035)))
          alpha (int (+ 55 (* 45 tw)))
          h     (mod (+ 220.0 (hue-bias t)) 360.0)]
      (q/fill h 70 255 alpha)
      (q/ellipse x y r r))))

(defn draw-orbit-guide [planet cx cy t]
  (let [hue (mod (+ (:hue planet) (hue-bias t)) 360.0)]
    (q/no-fill)
    (q/stroke hue 55 120 28)
    (q/stroke-weight 0.5)
    (q/ellipse cx cy (* 2 (:orbit-r planet)) (* 2 (:orbit-r planet)))))

(defn draw-asteroid-belt [asteroids sim-year cx cy t]
  (q/no-stroke)
  (doseq [{:keys [period orbit-r phase]} asteroids]
    (let [angle (+ phase (* TWO-PI (/ (mod sim-year period) period)))
          x     (+ cx (* orbit-r (Math/cos angle)))
          y     (+ cy (* orbit-r (Math/sin angle)))
          hue   (mod (+ 30.0 (hue-bias t)) 360.0)]
      (q/fill hue 150 190 80)
      (q/ellipse x y 1.5 1.5))))

(defn draw-trail [trail planet t]
  (let [n (count trail)]
    (when (> n 1)
      (q/no-fill)
      (q/stroke-weight 1.1)
      (dotimes [i (dec n)]
        (let [;; oldest segment (i=0) → faintest; newest (i=n-2) → brightest
              age   (/ (double (inc i)) n)
              alpha (int (* age 150.0))
              hue   (mod (+ (:hue planet) (hue-bias t)) 360.0)
              [x1 y1] (nth trail i)
              [x2 y2] (nth trail (inc i))]
          (q/stroke hue 200 230 alpha)
          (q/line x1 y1 x2 y2))))))

(defn draw-planet [planet sim-year cx cy t]
  (let [[x y] (planet-xy planet sim-year cx cy)
        size  (:size planet)
        hue   (mod (+ (:hue planet) (hue-bias t)) 360.0)]
    ;; Saturn's rings
    (when (:rings? planet)
      (q/no-fill)
      (q/stroke hue 110 215 110)
      (q/stroke-weight 1.5)
      (q/ellipse x y (* size 5.5) (* size 2.0))
      (q/stroke hue 90 190 70)
      (q/stroke-weight 0.8)
      (q/ellipse x y (* size 6.8) (* size 2.5)))
    ;; atmospheric glow
    (q/no-stroke)
    (q/fill hue 175 255 60)
    (q/ellipse x y (* size 3.5) (* size 3.5))
    ;; body
    (q/fill hue 220 255 215)
    (q/ellipse x y (* size 2) (* size 2))))

(defn draw-resonance-spirograph [pts t]
  (let [n (count pts)]
    (when (> n 1)
      (q/no-fill)
      (q/stroke-weight 0.7)
      (dotimes [i (dec n)]
        (let [;; i=0 is the present midpoint; fades toward oldest
              age   (/ (double i) n)
              alpha (int (* (- 1.0 age) 70.0))
              hue   (mod (+ 52.0 (hue-bias t)) 360.0)
              [x1 y1] (nth pts i)
              [x2 y2] (nth pts (inc i))]
          (q/stroke hue 200 255 alpha)
          (q/line x1 y1 x2 y2))))))

;;; ─── ERA LABELS ──────────────────────────────────────────────────────────────
(defn era-name [t]
  (cond
    (< t 0.04) "Protoplanetary Disk"
    (< t 0.09) "Late Heavy Bombardment"
    (< t 0.22) "Hadean / Archean"
    (< t 0.46) "Main Sequence — Biotic Window"
    (< t 0.65) "Late Main Sequence"
    (< t 0.85) "Subgiant Expansion"
    :else       "Red Giant"))

;;; ─── HUD ─────────────────────────────────────────────────────────────────────
(defn draw-hud [epoch-year t paused?]
  (let [ga (/ epoch-year 1.0e9)]
    (q/no-stroke)
    (q/fill 0 0 0 140)
    (q/rect 8 8 312 32 6)
    (q/fill 0 0 220 200)
    (q/text-size 13)
    (q/text (format "%.3f Ga  ·  %s" ga (era-name t)) 14 29)
    (when paused?
      (q/fill 45 200 255 200)
      (q/text "[SPACE] paused" 14 52))))

;;; ─── QUIL LIFECYCLE ──────────────────────────────────────────────────────────
(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb 360 255 255 255)
  (let [w       (q/width)
        h       (q/height)
        planets (mapv #(assoc % :phase (* TWO-PI (rand))) planet-catalogue)]
    {:sim-year   0.0
     :epoch-year 0.0
     :paused?    false
     :planets    planets
     :asteroids  (make-asteroids 180)
     :trails     (mapv (fn [_] []) planets)
     :stars      (vec (repeatedly 280
                                  #(hash-map :x     (rand-int w)
                                             :y     (rand-int h)
                                             :r     (+ 0.5 (rand 1.5))
                                             :phase (* TWO-PI (rand)))))}))

(defn update-state [state]
  (if (:paused? state)
    state
    (let [new-sim   (+ (:sim-year state) SIM-YPF)
          new-epoch (let [ny (+ (:epoch-year state) EPOCH-YPF)]
                      ;; loop epoch so the deep-time story keeps cycling
                      (if (>= ny EPOCH-MAX) 0.0 ny))
          cx        400.0
          cy        400.0
          planets   (:planets state)
          trails    (mapv (fn [p trail]
                            (let [pos  (planet-xy p new-sim cx cy)
                                  next (conj trail pos)]
                              (if (> (count next) 220)
                                (subvec next 1)
                                next)))
                          planets
                          (:trails state))]
      (assoc state
             :sim-year   new-sim
             :epoch-year new-epoch
             :trails     trails))))

(defn draw [state]
  (let [{:keys [sim-year epoch-year planets asteroids trails stars paused?]} state
        cx  400.0
        cy  400.0
        t   (epoch-t epoch-year)
        fc  (q/frame-count)]
    ;; Background tinted by epoch
    (q/background (bg-hue t) 55 (int (lerp 6.0 14.0 t)))

    ;; Starfield
    (draw-starfield stars t fc)

    ;; Faint orbit guides
    (doseq [p planets]
      (draw-orbit-guide p cx cy t))

    ;; Asteroid belt
    (draw-asteroid-belt asteroids sim-year cx cy t)

    ;; Jupiter–Saturn resonance spirograph (rose curve beneath the planets)
    (let [jupiter (nth planets 4)
          saturn  (nth planets 5)
          ;; 30 synodic periods lookback → one complete petal cycle
          pts     (resonance-pts jupiter saturn sim-year cx cy 320 (* 19.86 30))]
      (draw-resonance-spirograph pts t))

    ;; Planet orbital trails
    (doseq [[p trail] (map vector planets trails)]
      (draw-trail trail p t))

    ;; Sun (drawn after trails so it overlaps them naturally)
    (draw-sun cx cy t fc)

    ;; Planets
    (doseq [p planets]
      (draw-planet p sim-year cx cy t))

    ;; HUD overlay
    (draw-hud epoch-year t paused?)))

(defn key-pressed [state event]
  (case (:key event)
    :space (update state :paused? not)
    state))

(q/defsketch solar-orrery
  :title "Solar Orrery — Deep Time"
  :size [800 800]
  :setup setup
  :update update-state
  :draw draw
  :key-pressed key-pressed
  :middleware [m/fun-mode])
