(ns my-art.geometrictwinkle
  (:require [quil.core :as q]
            [quil.middleware :as m]))


(defn setup []
  (q/frame-rate 30)
  (let [r 10
        w (q/width)
        h (q/height)
        y-vals (take-nth r (range (+ h r)))
        x-vals (flatten
                (map #(repeat (count y-vals) %)
                     (take-nth r (range (+ w r)))))
        origins (partition 2 (interleave x-vals (cycle y-vals)))
        circles (map (fn [[x y]] {:x x :y y :r r}) origins)]
    {:radius r
     :circles circles
     :t 0.0
     :dt (/ 1.0 30.0)
     :phase-scale 0.002
     :kps-to-phase 0.0002
     :max-phase-boost 0.006
     :typing-kps 0.0
     :kps-decay 0.93
     :typing-energy 0.0
     :energy-decay 0.90
     :energy-to-radius 0.6
     :phase-jump 0.0
     :phase-jump-decay 0.85
    :focus-pos [0.5 0.5]
     :last-key-ms nil
     :paused? false
     :debug? true}))

(defn classify-key [k kc]
  (cond
    (or (= kc 8) (= k :backspace)) :backspace
    (or (= kc 32) (= k :space) (= k \space)) :space
    (and (char? k) (Character/isLetterOrDigit ^char k)) :alnum
    (char? k) :symbol
    :else :other))

(def qwerty-layout
  [{:letters "qwertyuiop" :offset 0.0}
   {:letters "asdfghjkl" :offset 0.5}
   {:letters "zxcvbnm" :offset 1.0}])

(def keyboard-max-width
  (reduce max (map #(+ (:offset %) (count (:letters %))) qwerty-layout)))

(defn qwerty-relative-pos [k]
  (let [ch (when (char? k) (Character/toLowerCase ^char k))]
    (when ch
      (first
       (keep-indexed
        (fn [row-idx {:keys [letters offset]}]
          (let [i (.indexOf ^String letters (int ch))]
            (when (<= 0 i)
              [(/ (+ offset i 0.5) keyboard-max-width)
               (/ (+ row-idx 0.5) (count qwerty-layout))])))
        qwerty-layout)))))

(defn apply-typing-input [state k kc]
  (let [now-ms (q/millis)
        last-ms (:last-key-ms state)
        delta-ms (when last-ms (max 1 (- now-ms last-ms)))
        inst-kps (if delta-ms
                   (/ 1000.0 delta-ms)
                   0.0)
        old-kps (:typing-kps state)
        smoothed-kps (if delta-ms
                       (+ (* 0.75 old-kps) (* 0.25 inst-kps))
                       old-kps)
        key-class (classify-key k kc)
        old-energy (:typing-energy state)
        damped-energy (if (= key-class :backspace)
                        (* 0.6 old-energy)
                        old-energy)
        [energy-bump phase-bump]
        (case key-class
          :space [0.65 0.60]
          :symbol [0.35 0.25]
          :alnum [0.20 0.08]
          :backspace [0.00 -0.20]
          :other [0.05 0.03])
        key-pos (qwerty-relative-pos k)
        state' (-> state
        (assoc :last-key-ms now-ms)
        (assoc :typing-kps smoothed-kps)
        (assoc :typing-energy (+ damped-energy energy-bump))
        (update :phase-jump + phase-bump))]
    (if key-pos
      (assoc state' :focus-pos key-pos)
      state')))

(defn update-state [state]
  (let [t (if (:paused? state)
            (:t state)
            (+ (:t state) (:dt state)))
  [fx fy] (or (:focus-pos state) [0.5 0.5])
  cx (* fx (q/width))
  cy (* fy (q/height))
        r (:radius state)
        typing-kps (* (:typing-kps state) (:kps-decay state))
        typing-energy (* (:typing-energy state) (:energy-decay state))
        phase-jump (* (:phase-jump state) (:phase-jump-decay state))
        phase-scale (+ (:phase-scale state)
                       (min (:max-phase-boost state)
                            (* (:kps-to-phase state) typing-kps)))
        amp (+ 1.0 (* (:energy-to-radius state) typing-energy))
        circles (mapv (fn [{:keys [x y] :as c}]
                        (let [spatial-term (+ (q/sq (- cx x))
                                              (q/sq (- cy y)))
                              phase (+ t phase-jump (* phase-scale spatial-term))
                              radius (Math/abs (* r amp (q/sin phase)))]
                          (assoc c :r radius)))
                      (:circles state))]
    (assoc state
           :t t
           :typing-kps typing-kps
           :typing-energy typing-energy
           :phase-jump phase-jump
           :circles circles)))

(defn key-pressed [state event]
  (let [k (:key event)
        kc (:key-code event)
        typed-state (apply-typing-input state k kc)]
    (cond
      (or (= k \space) (= k :space) (= kc 32))
      (update typed-state :paused? not)

      (or (= k \n) (= k \N) (= k :n) (= k :N))
      (if (:paused? typed-state)
        (update typed-state :t + (:dt typed-state))
        typed-state)

      (or (= k \d) (= k \D) (= k :d) (= k :D))
      (update typed-state :debug? not)

      (or (= kc 93) (= k \=) (= k \+))
      (update typed-state :phase-scale #(+ % 0.0005))

      (or (= kc 91) (= k \-) (= k \_))
      (update typed-state :phase-scale #(max 0.0001 (- % 0.0005)))

      :else typed-state)))

(defn draw-state [state]
  (q/background 0 0 0)
  (q/no-stroke)

  (doseq [c (:circles state)]
    (q/ellipse (:x c) (:y c) (:r c) (:r c)))

  (when (:debug? state)
    (let [radii (map :r (:circles state))
          r-min (reduce min radii)
          r-max (reduce max radii)]
      (q/fill 255)
      (q/text (str "t=" (format "%.3f" (double (:t state)))
                   " paused=" (:paused? state)
           " scale=" (format "%.4f" (double (:phase-scale state)))
           " kps=" (format "%.2f" (double (:typing-kps state)))
           " energy=" (format "%.2f" (double (:typing-energy state)))
           " focus=" (if-let [[fx fy] (:focus-pos state)]
            (str (format "%.2f" (double fx)) "," (format "%.2f" (double fy)))
            "0.50,0.50")
           " min-r=" (format "%.3f" (double r-min))
           " max-r=" (format "%.3f" (double r-max))
           "  [space]=pause [n]=step [d]=debug [[/-]=scale")
              10 20))))


(q/defsketch geometric-twinkle
  :host "host"
  :size [500 500]
  :setup setup
  :update update-state
  :key-pressed key-pressed
  :draw draw-state
  :middleware [m/fun-mode])