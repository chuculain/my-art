(ns my-art.hydrogen-cloud
  (:require [quil.core :as q]
            [quil.middleware :as m]))


(defn clamp [x lo hi]
  (max lo (min hi x)))

(defn radians->degrees [radians]
  (* radians (/ 180.0 Math/PI)))

(defn factorial [n]
  (reduce *' 1N (range 1 (inc n))))

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

(defn estimate-pmax [n l m a0 rmax probes]
  (loop [i 0
         pmax (double 1.0e-12)]
    (if (>= i probes)
      (* 1.3 pmax)
      (let [x (- (* 2.0 rmax (rand)) rmax)
            y (- (* 2.0 rmax (rand)) rmax)
            z (- (* 2.0 rmax (rand)) rmax)
            p (probability-density n l m x y z a0)]
        (recur (inc i) (double (max pmax p)))))))

(defn sample-cloud [n l m a0 rmax sample-count]
  (let [pmax (estimate-pmax n l m a0 rmax 700)
        max-attempts (* sample-count 50)]
    (loop [samples []
           attempts 0]
      (if (or (>= (count samples) sample-count)
              (>= attempts max-attempts))
        samples
        (let [x (- (* 2.0 rmax (rand)) rmax)
              y (- (* 2.0 rmax (rand)) rmax)
              z (- (* 2.0 rmax (rand)) rmax)
              p (probability-density n l m x y z a0)
              accept (< (rand) (min 1.0 (/ p pmax)))]
          (if accept
            (recur (conj samples {:x x :y y :z z :p p}) (inc attempts))
            (recur samples (inc attempts))))))))

(defn normalize-quantum-state [n l m]
  (let [n (max 1 (int n))
        l (int (clamp l 0 (dec n)))
        m (int (clamp m (- l) l))]
    [n l m]))

(defn with-energy-state [state new-n]
  (let [[n l m] (normalize-quantum-state new-n (:l state) (:m state))
        samples (sample-cloud n
                             l
                             m
                             (:a0 state)
                             (:rmax state)
                             (:sample-count state))]
    (-> state
        (assoc :n n)
        (assoc :l l)
        (assoc :m m)
        (assoc :samples samples))))

(defn clamp-quantum-counters [n l m]
  (let [n' (int (clamp n 1 8))
        l' (int (clamp l 0 (dec n')))
        m' (int (clamp m 0 l'))]
    [n' l' m']))

(defn step-counters-up [n l m]
  ;; Navigate through valid states: m steps 0..l, then l steps 0..(n-1), then n
  (cond
    (< m l)         [n l (inc m)]
    (< l (dec n))   [n (inc l) 0]
    (< n 8)         [(inc n) 0 0]
    :else           [n l m]))

(defn step-counters-down [n l m]
  ;; Reverse: decrement m, then roll back l to its max m, then roll back n
  (cond
    (> m 0)  [n l (dec m)]
    (> l 0)  (let [l' (dec l)] [n l' l'])
    (> n 1)  (let [n' (dec n) l' (dec n')] [n' l' l'])
    :else    [n l m]))

(defn sampling-quantum-state [state]
  ;; clamp-quantum-counters already enforces valid states; this is a safety net
  (let [n (int (clamp (:n state) 1 8))
        l (int (clamp (:l state) 0 (dec n)))
        m (int (clamp (:m state) 0 l))]
    [n l m]))

(defn rmax-for-n [n a0]
  ;; Orbital extent scales as n². Factor of 5 covers >99% of radial probability.
  (* (max 8.0 (* 5.0 n n)) a0))

(defn with-quantum-counters [state n l m]
  (let [[n l m] (clamp-quantum-counters n l m)
        rmax    (rmax-for-n n (:a0 state))
        sample-state (assoc state :n n :l l :m m :rmax rmax)
        [sn sl sm] (sampling-quantum-state sample-state)
        samples (sample-cloud sn
                             sl
                             sm
                             (:a0 sample-state)
                             (:rmax sample-state)
                             (:sample-count sample-state))]
    (assoc sample-state :samples samples)))

(def up-key-codes #{38 63232})
(def down-key-codes #{40 63233})

(defn up-arrow? [k kc]
  (or (= k :up)
      (contains? up-key-codes kc)))

(defn down-arrow? [k kc]
  (or (= k :down)
      (contains? down-key-codes kc)))

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
  (q/frame-rate 30)
  (q/color-mode :hsb 360 100 100 1.0)
  (let [n 2
        l 1
        m 0
        a0 52.0
        rmax (* 8.0 a0)
  sample-count 3200
        [n l m] (if (valid-quantum-state? n l m)
                  [n l m]
                  [1 0 0])]
    (with-quantum-counters {:n n
                            :l l
                            :m m
                            :a0 a0
                            :rmax rmax
                            :sample-count sample-count
                            :samples []
                            :scale 0.55}
      n l m)))

(defn update-state [state]
  (let [[sn sl sm] (sampling-quantum-state state)
        samples (sample-cloud sn
                              sl
                              sm
                              (:a0 state)
                              (:rmax state)
                              (:sample-count state))]
    (assoc state :samples samples)))

(defn draw-state [state]
  (q/background 222 35 8)
  (let [w (q/width)
        h (q/height)
        rmax (:rmax state)
        outer-pad 22.0
        title-y (+ outer-pad 8.0)
        subtitle-y (+ title-y 24.0)
        stats-y (+ subtitle-y 20.0)
        controls-y (+ stats-y 20.0)
        legend-w 272.0
        legend-h 66.0
        legend-x (- w outer-pad legend-w)
        legend-y (+ outer-pad 4.0)
        grid-top (+ controls-y 22.0)
        gap 14.0
        panel-w (/ (- w (* 2.0 outer-pad) gap) 2.0)
        panel-h (/ (- h grid-top outer-pad gap) 2.0)
        base-scale (* (:scale state) (/ (min panel-w panel-h) (* 2.0 rmax)) 0.95)
        views [{:rot 0.0 :label "0" :col 0 :row 0}
               {:rot (/ Math/PI 2.0) :label "pi/2" :col 1 :row 0}
               {:rot Math/PI :label "pi" :col 0 :row 1}
               {:rot (* 3.0 (/ Math/PI 2.0)) :label "3pi/2" :col 1 :row 1}]]
    (q/fill 32 7 98 0.9)
    (q/text-size 22)
    (q/text "Hydrogen Cloud Monte Carlo" outer-pad title-y)
    (q/text-size 14)
    (q/text (str "n=" (:n state) "  l=" (:l state) "  m=" (:m state))
            outer-pad
            subtitle-y)
    (let [[sn sl sm] (sampling-quantum-state state)]
      (q/text (str "render state -> n=" sn " l=" sl " m=" sm) outer-pad stats-y))
    (q/text (str "samples=" (count (:samples state)) "  resample=every frame") outer-pad controls-y)
    (q/text "UP: next valid (n,l,m)  DOWN: prev valid (n,l,m)  n=1..8" outer-pad (+ controls-y 18.0))

    (doseq [{:keys [rot label col row]} views]
      (let [panel-x (+ outer-pad (* col (+ panel-w gap)))
            panel-y (+ grid-top (* row (+ panel-h gap)))
            cx (+ panel-x (/ panel-w 2.0))
            cy (+ panel-y (/ panel-h 2.0))]
        (q/no-stroke)
        (q/fill 32 6 16 0.45)
        (q/rect panel-x panel-y panel-w panel-h)
        (q/fill 32 7 98 0.85)
        (q/text-size 12)
        (q/text (str "rotation=" label " (" (format "%.1f" (radians->degrees rot)) " deg)")
                (+ panel-x 8.0)
                (+ panel-y 16.0))
        (q/no-stroke)
        (doseq [{:keys [x y z p]} (:samples state)]
          (let [x1 (- (* x (Math/cos rot)) (* z (Math/sin rot)))
                z1 (+ (* x (Math/sin rot)) (* z (Math/cos rot)))
                depth (clamp (/ (+ z1 rmax) (* 2.0 rmax)) 0.0 1.0)
                alpha (+ 0.05 (* 0.7 depth))
                size (+ 1.0 (* 1.8 depth))
                px (+ cx (* x1 base-scale))
                py (+ cy (* y base-scale))
                radial (/ (Math/sqrt (+ (* x x) (* y y) (* z z))) rmax)
                hue (+ 185.0 (* 120.0 (clamp (- 1.0 radial) 0.0 1.0)))
                bri (+ 65.0 (* 30.0 (clamp (/ p 0.00015) 0.0 1.0)))]
            (q/fill hue 75 bri alpha)
            (q/ellipse px py size size)))))

    (q/no-stroke)
    (q/fill 32 6 20 0.72)
    (q/rect legend-x legend-y legend-w legend-h)
    (q/fill 32 7 98 0.92)
    (q/text-size 12)
    (q/text "Dot color legend" (+ legend-x 10.0) (+ legend-y 16.0))
    (q/fill 305 75 92 0.9)
    (q/ellipse (+ legend-x 16.0) (+ legend-y 30.0) 8 8)
    (q/fill 185 75 72 0.9)
    (q/ellipse (+ legend-x 16.0) (+ legend-y 46.0) 8 8)
    (q/fill 32 7 98 0.92)
    (q/text "Hue: near nucleus -> warmer, brighter" (+ legend-x 28.0) (+ legend-y 34.0))
    (q/text "Hue: far from nucleus -> cooler, dimmer" (+ legend-x 28.0) (+ legend-y 50.0))
    (q/fill 32 7 98 0.92)))

(q/defsketch hydrogen-cloud
  :title "Hydrogen Cloud Monte Carlo"
  :size [1024 768]
  :host "host"
  :setup setup
  :update update-state
  :key-pressed handle-key-pressed
  :draw draw-state
  ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
   :middleware [m/fun-mode])