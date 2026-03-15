(ns my-art.abyssal-oracle
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def qwerty-layout
  [{:letters "qwertyuiop" :offset 0.0}
   {:letters "asdfghjkl" :offset 0.5}
   {:letters "zxcvbnm" :offset 1.0}])

(def keyboard-max-width
  (reduce max (map #(+ (:offset %) (count (:letters %))) qwerty-layout)))

(defn clamp
  "Clamp numeric value x to the inclusive [lo hi] interval."
  [x lo hi]
  (-> x (max lo) (min hi)))

(defn lerp
  "Linear interpolation from a to b using normalized blend factor t."
  [a b t]
  (+ a (* (- b a) t)))

(defn qwerty-relative-pos
  "Map a letter key to a relative [x y] position on an ANSI-like QWERTY grid.
   Returns nil for non-letter keys."
  [k]
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

(defn classify-key
  "Classify a key event into semantic groups used by the visual synthesizer."
  [k kc]
  (cond
    (or (= kc 8) (= k :backspace)) :backspace
    (or (= kc 10) (= kc 13) (= k :enter) (= k :return)) :enter
    (or (= kc 9) (= k :tab)) :tab
    (or (= kc 32) (= k :space) (= k \space)) :space
    (and (char? k) (Character/isLetterOrDigit ^char k)) :alnum
    (char? k) :symbol
    :else :other))

(defn signal-for-key-class
  "Return modulation deltas for one key class.
   :energy influences pulse amplitude, :phase-kick affects oscillation phase,
   :hue-kick shifts chroma drift, and :noise-kick animates tendril jitter."
  [key-class]
  (case key-class
    :space {:energy 0.70 :phase-kick 0.55 :hue-kick 14.0 :noise-kick 0.35}
    :symbol {:energy 0.40 :phase-kick 0.22 :hue-kick 8.0 :noise-kick 0.25}
    :alnum {:energy 0.24 :phase-kick 0.10 :hue-kick 3.0 :noise-kick 0.12}
    :enter {:energy 0.95 :phase-kick 0.80 :hue-kick 20.0 :noise-kick 0.42}
    :tab {:energy 0.55 :phase-kick 0.35 :hue-kick 12.0 :noise-kick 0.28}
    :backspace {:energy -0.30 :phase-kick -0.30 :hue-kick -6.0 :noise-kick -0.10}
    :other {:energy 0.08 :phase-kick 0.03 :hue-kick 1.0 :noise-kick 0.05}))

(defn append-voice-char
  "Append character ch to rolling voice buffer with max-length limit.
   Non-printable chars are ignored."
  [voice ch max-len]
  (if (and (char? ch) (<= 32 (int ch) 126))
    (let [next (conj voice ch)
          overflow (max 0 (- (count next) max-len))]
      (if (pos? overflow)
        (vec (drop overflow next))
        next))
    voice))

(defn append-key-event
  "Append a key-driven visual event to a rolling event buffer."
  [events event max-len]
  (let [next (conj events event)
        overflow (max 0 (- (count next) max-len))]
    (if (pos? overflow)
      (vec (drop overflow next))
      next)))

(defn handle-typing-input
  "Incorporate a key event into state-level modulation signals and target focus."
  [state k kc]
  (let [now-ms (q/millis)
        last-ms (:last-key-ms state)
        delta-ms (when last-ms (max 1 (- now-ms last-ms)))
        inst-kps (if delta-ms (/ 1000.0 delta-ms) 0.0)
        smoothed-kps (if delta-ms
                       (+ (* 0.78 (:typing-kps state)) (* 0.22 inst-kps))
                       (:typing-kps state))
        key-class (classify-key k kc)
        {:keys [energy phase-kick hue-kick noise-kick]} (signal-for-key-class key-class)
        target-pos (qwerty-relative-pos k)
        event-pos (or target-pos (:focus-pos state) [0.5 0.5])
        printable? (and (char? k) (<= 32 (int k) 126))
        event-glyph (cond
                printable? k
                (= key-class :space) \_
                      (= key-class :enter) \!
                      (= key-class :tab) \=
                      (= key-class :backspace) \<
                      :else \*)
        [ex ey] event-pos
        key-event {:x ex
             :y ey
             :ttl 1.0
             :power (clamp (+ 0.15 (* 0.9 (Math/abs energy))) 0.12 1.2)
             :radius (+ 12 (* 18 (Math/abs phase-kick)))
             :drift (+ 0.2 (* 0.9 noise-kick))
             :hue-shift hue-kick
             :glyph event-glyph}
        voice (cond
                (and (= key-class :backspace) (seq (:voice-buffer state)))
                (vec (butlast (:voice-buffer state)))

                (= key-class :space)
                (append-voice-char (:voice-buffer state) \space (:voice-max-len state))

                :else
                (append-voice-char (:voice-buffer state) k (:voice-max-len state)))]
    (-> state
        (assoc :last-key-ms now-ms)
        (assoc :typing-kps smoothed-kps)
        (update :typing-energy #(clamp (+ % energy) 0.0 6.0))
        (update :phase-kick #(clamp (+ % phase-kick) -2.0 6.0))
        (update :hue-velocity #(clamp (+ % hue-kick) -60.0 60.0))
        (update :noise-drive #(clamp (+ % noise-kick) 0.0 2.0))
        (assoc :voice-buffer voice)
        (update :key-events append-key-event key-event (:max-key-events state))
        (cond-> target-pos (assoc :focus-target target-pos)))))

(defn setup
  "Initialize the abyssal oracle sketch state."
  []
  (q/frame-rate 60)
  (q/color-mode :hsb 360 100 100 1.0)
  (q/text-font (q/create-font "Monospaced" 16 true))
  {:t 0.0
   :dt (/ 1.0 60.0)
   :paused? false
   :debug? true
   :base-radius 120.0
   :typing-kps 0.0
   :kps-decay 0.92
   :typing-energy 0.0
   :energy-decay 0.90
   :phase-kick 0.0
   :phase-kick-decay 0.84
   :noise-drive 0.0
   :noise-decay 0.88
   :focus-pos [0.5 0.5]
   :focus-target [0.5 0.5]
   :focus-ease 0.12
   :hue 185.0
   :hue-velocity 0.0
   :hue-friction 0.86
   :last-key-ms nil
   :voice-buffer []
  :voice-max-len 96
  :key-events []
  :max-key-events 140
  :chatter-phase 0.0})

(defn update-focus
  "Smoothly move focus-pos toward focus-target."
  [state]
  (let [[fx fy] (:focus-pos state)
        [tx ty] (:focus-target state)
        ease (:focus-ease state)]
    (assoc state :focus-pos [(lerp fx tx ease) (lerp fy ty ease)])))

(defn update-state
  "Advance simulation signals and regenerate dynamic parameters each frame."
  [state]
  (if (:paused? state)
    state
    (let [t (+ (:t state) (:dt state))
          kps (* (:typing-kps state) (:kps-decay state))
          energy (* (:typing-energy state) (:energy-decay state))
          phase-kick (* (:phase-kick state) (:phase-kick-decay state))
          noise-drive (* (:noise-drive state) (:noise-decay state))
          hue-velocity (* (:hue-velocity state) (:hue-friction state))
          hue (mod (+ (:hue state) (* hue-velocity (:dt state))) 360.0)
          chatter-phase (+ (:chatter-phase state) (* (:dt state) (+ 0.35 (* 0.08 kps))))
          key-events (->> (:key-events state)
                          (map (fn [evt]
                                 (let [ttl (* 0.93 (:ttl evt))]
                                   (assoc evt
                                          :ttl ttl
                                          :radius (+ (:radius evt) (+ 0.7 (* 0.8 (:power evt))))))))
                          (filter #(> (:ttl %) 0.03))
                          vec)]
      (-> state
          (assoc :t t)
          (assoc :typing-kps kps)
          (assoc :typing-energy energy)
          (assoc :phase-kick phase-kick)
          (assoc :noise-drive noise-drive)
          (assoc :hue-velocity hue-velocity)
          (assoc :hue hue)
          (assoc :chatter-phase chatter-phase)
          (assoc :key-events key-events)
          update-focus))))

(defn draw-abyss
  "Render the machine-horizon where human typing chatter becomes weather."
  [state]
  (let [t (:t state)
        hue (:hue state)
        energy (:typing-energy state)
        w (q/width)
        hgt (q/height)]
    (q/no-stroke)
    (q/fill (mod (+ hue 205) 360) 78 7 0.34)
    (q/rect 0 0 w hgt)
    (q/fill (mod (+ hue 95 (* 28 (q/sin (* 0.2 t)))) 360) 60 16 0.24)
    (q/ellipse (* 0.52 w) (* 0.58 hgt) (* 1.8 w) (* 1.3 hgt))
    (q/fill (mod (+ hue 15 (* 8 (q/cos (* 0.17 t)))) 360) 76 22 0.14)
    (q/ellipse (* 0.3 w) (* 0.9 hgt) (* 1.6 w) (* 0.9 hgt))
    (q/fill (mod (+ hue 255) 360) 52 18 (clamp (+ 0.06 (* 0.03 energy)) 0.05 0.2))
    (q/ellipse (* 0.76 w) (* 0.16 hgt) (* 0.8 w) (* 0.5 hgt))
    (q/stroke (mod (+ hue 220) 360) 28 34 0.09)
    (q/stroke-weight 1)
    (doseq [y (range 0 hgt 5)]
      (q/line 0 y w y))))

(defn draw-resonance-rings
  "Draw a warped keyboard lattice that tracks focus and typing intensity."
  [state cx cy]
  (let [t (:t state)
        hue (:hue state)
        phase (:chatter-phase state)
        energy (:typing-energy state)
        kps (:typing-kps state)
        cols 18
        rows 10
        w (q/width)
        h (q/height)]
    (q/no-fill)
    (q/stroke-weight 1.1)
    (doseq [gx (range (inc cols))]
      (let [x (* (/ gx cols) w)
            warp (+ (* 18 (q/sin (+ (* 0.8 phase) (* 0.3 gx))))
                    (* 5 energy))]
        (q/stroke (mod (+ hue (* 3 gx)) 360) 45 88 0.22)
        (q/line x 0 (+ x warp) h)))
    (doseq [gy (range (inc rows))]
      (let [y (* (/ gy rows) h)
            drift (* 22 (q/cos (+ (* 0.66 phase) (* 0.43 gy))))]
        (q/stroke (mod (+ hue 150 (* 2 gy)) 360) 35 80 0.16)
        (q/line 0 (+ y drift) w (+ y (- drift)))))
    (doseq [i (range 4)]
      (let [base (+ 40 (* i (+ 32 (* 2 kps))))
            wobble (* 15 (q/sin (+ (* 1.2 t) i (:phase-kick state))))
            radius (+ base wobble (* 12 energy))
            alpha (clamp (- 0.42 (* i 0.08)) 0.08 0.5)]
        (q/stroke (mod (+ hue (* 9 i)) 360) 62 98 alpha)
        (q/stroke-weight (+ 1.2 (* i 0.35)))
        (q/ellipse cx cy (* 2 radius) (* 2 radius))))))

(defn draw-tendrils
  "Render descending glyph streams as the living transcript of user chatter."
  [state cx cy]
  (let [t (:t state)
        hue (:hue state)
        phase (:chatter-phase state)
        noise-drive (:noise-drive state)
        kps (:typing-kps state)
        voice (:voice-buffer state)
        n (count voice)
        cols 26
        w (q/width)
        h (q/height)]
    (q/text-align :center :center)
    (doseq [i (range cols)]
      (let [x (+ (* (/ (+ i 0.5) cols) w)
                 (* 12 (q/sin (+ (* 0.23 phase) i))))
            speed (+ 1.1 (* 0.1 i) (* 1.8 noise-drive) (* 0.8 kps))
            y-head (mod (+ (* 56 speed t) (* 39 i)) (+ h 180))
            trail 10]
        (doseq [j (range trail)]
          (let [y (- y-head (* j 20))]
            (when (<= -24 y (+ h 24))
              (let [idx (if (pos? n)
                          (mod (+ (* 3 i) j (int (* 8 t))) n)
                          0)
                    ch (if (pos? n) (nth voice idx) \.)
                    alpha (clamp (- 0.7 (* 0.08 j)) 0.08 0.7)
                    sz (+ 10 (* 2 (q/sin (+ (* 0.6 phase) (* 0.4 i) j))))]
                (q/fill (mod (+ hue (* 7 i) (* 3 j)) 360) 52 100 alpha)
                (q/text-size (max 9 sz))
                (q/text (str ch) x y)))))))
    (q/no-fill)
    (q/stroke (mod (+ hue 40) 360) 65 100 0.28)
    (q/stroke-weight 1.8)
    (q/ellipse cx cy (+ 120 (* 26 (q/sin (+ phase (* 1.7 noise-drive)))))
               (+ 90 (* 20 (q/cos (+ phase (* 1.1 noise-drive))))))))

(defn draw-core
  "Draw the transducer prism that converts typing into luminous geometry."
  [state cx cy]
  (let [t (:t state)
        hue (:hue state)
        energy (:typing-energy state)
        phase (:chatter-phase state)
        pulse (+ 1.0 (* 0.1 (q/sin (+ (* 2.7 t) (:phase-kick state)))))
        r (* (:base-radius state) pulse (+ 0.78 (* 0.18 energy)))]
    (q/push-matrix)
    (q/translate cx cy)
    (q/rotate (+ (* 0.08 t) (* 0.18 phase)))
    (q/no-fill)
    (q/stroke (mod (+ hue 180) 360) 70 98 0.65)
    (q/stroke-weight 2.4)
    (q/begin-shape)
    (doseq [i (range 6)]
      (let [a (+ (/ q/TWO-PI 6.0) (* i (/ q/TWO-PI 6.0)))
            rx (+ r (* 0.14 r (q/sin (+ (* 1.4 phase) i))))
            x (* rx (q/cos a))
            y (* (* 0.62 rx) (q/sin a))]
        (q/vertex x y)))
    (q/end-shape :close)
    (q/pop-matrix)
    (q/no-stroke)
    (q/fill (mod (+ hue 30) 360) 74 98 0.24)
    (q/ellipse cx cy (* 2.1 r) (* 1.2 r))
    (q/fill (mod (+ hue 70) 360) 86 100 0.38)
    (q/ellipse cx cy (* 1.2 r) (* 0.72 r))
    (q/fill (mod (+ hue 210) 360) 18 100 0.98)
    (q/ellipse cx cy (* 0.24 r) (* 0.24 r))))

(defn draw-voice-wave
  "Draw expanding key packets so each keystroke persists as a local universe."
  [state cx cy]
  (let [events (:key-events state)
        t (:t state)
        hue (:hue state)
        w (q/width)
        h (q/height)]
    (doseq [{:keys [x y ttl power radius drift hue-shift glyph]} events]
      (let [px (* x w)
            py (* y h)
            jitter-x (* 14 drift (q/sin (+ (* 2.5 t) (* 9 x))))
            jitter-y (* 10 drift (q/cos (+ (* 1.9 t) (* 7 y))))
            r (+ radius (* 36 (- 1 ttl)))]
        (q/no-fill)
        (q/stroke (mod (+ hue hue-shift (* 45 ttl)) 360) 82 100 (clamp (* 0.55 ttl) 0.06 0.6))
        (q/stroke-weight (clamp (+ 0.8 (* 2.1 ttl power)) 0.8 3.0))
        (q/ellipse (+ px jitter-x) (+ py jitter-y) (* 2 r) (* 2 r))
        (q/stroke (mod (+ hue 180 hue-shift) 360) 50 100 (clamp (* 0.35 ttl) 0.04 0.35))
        (q/line cx cy (+ px jitter-x) (+ py jitter-y))
        (q/no-stroke)
        (q/fill (mod (+ hue 100 hue-shift) 360) 25 100 (clamp (+ 0.2 (* 0.55 ttl)) 0.15 0.9))
        (q/text-size (+ 10 (* 14 ttl power)))
        (q/text-align :center :center)
        (q/text (str glyph) (+ px jitter-x) (+ py jitter-y))))))

(defn draw-debug
  "Render compact debug telemetry."
  [state]
  (when (:debug? state)
    (q/fill 0 0 100 0.9)
    (let [[fx fy] (:focus-pos state)]
      (q/text
       (str "t=" (format "%.2f" (double (:t state)))
            "  kps=" (format "%.2f" (double (:typing-kps state)))
            "  energy=" (format "%.2f" (double (:typing-energy state)))
            "  hue=" (format "%.1f" (double (:hue state)))
            "  focus=" (format "%.2f,%.2f" (double fx) (double fy))
            "  chars=" (count (:voice-buffer state))
            "  events=" (count (:key-events state))
          "  [space]=pause [esc]=debug [c]=clear")
       16 24))))

(defn draw-state
  "Render the complete sentient-cyberspace scene for the current state."
  [state]
  (draw-abyss state)
  (let [[fx fy] (:focus-pos state)
        cx (* fx (q/width))
        cy (* fy (q/height))]
    (draw-resonance-rings state cx cy)
    (draw-tendrils state cx cy)
    (draw-core state cx cy)
    (draw-voice-wave state cx cy)
    (draw-debug state)))

(defn key-pressed
  "Update interaction state from key events and preserve control shortcuts."
  [state event]
  (let [k (:key event)
        kc (:key-code event)
        typed-state (handle-typing-input state k kc)]
    (cond
      (or (= k \space) (= k :space) (= kc 32))
      (update typed-state :paused? not)

      (or (= kc 27) (= k :escape))
      (update typed-state :debug? not)

      (or (= k \c) (= k \C) (= k :c) (= k :C))
      (assoc typed-state :voice-buffer [] :key-events [])

      :else typed-state)))

    (declare abyssal-oracle)

(q/defsketch abyssal-oracle
  :title "Abyssal Oracle"
  :size [960 640]
  :setup setup
  :update update-state
  :draw draw-state
  :key-pressed key-pressed
  :features [:keep-on-top]
  :middleware [m/fun-mode])
