(ns thermos-frontend.format)

(let [format (js/Intl.NumberFormat.)]
  (defn local-format [value] (.format format value)))

(defn metric-prefix [value scale prefix]
  (when (>= value scale)
    (str (local-format (/ value scale)) " " prefix)))

(defn si-number [value & [dimension]]
  (let [sign (if (< value 0) "-" )
        value (Math/abs value)
        ]
    (if dimension
           (let [scale (case (aget dimension 0)
                         "k" 1000
                         "M" 1000000
                         "G" 1000000000
                         "T" 1000000000000)]
             (str sign
                  (or (metric-prefix value (/ 1000000000000 scale) "T")
                      (metric-prefix value (/ 1000000000 scale) "G")
                      (metric-prefix value (/ 1000000 scale) "M")
                      (metric-prefix value (/ 1000 scale) "k")
                      (local-format value))
                  (.substring dimension 1)))
           (str sign
                (or (metric-prefix value 1000000000000 "T")
                    (metric-prefix value 1000000000 "G")
                    (metric-prefix value 1000000 "M")
                    (metric-prefix value 1000 "k")
                    (local-format value))))))

(defn seconds [s]
  (let [s (int s)
        seconds-part (mod s 60)
        minutes-part (int (/ s 60))
        hours-part (int (/ minutes-part 60))
        minutes-part (mod minutes-part 60)]
    (str
     (if (pos? hours-part)
       (str hours-part "h, ") "")
     (if (pos? minutes-part)
       (str minutes-part "m, ") "")
     seconds-part "s")))

