(ns thermos-ui.frontend.format)

(let [format (js/Intl.NumberFormat.)]
  (defn local-format [value] (.format format value)))

(defn metric-prefix [value scale prefix]
  (when (>= value scale)
    (str (local-format (/ value scale)) " " prefix)))

(defn si-number [value]
  (let [sign (if (< value 0) "-" )
        value (Math/abs value)
        ]
    (str sign
     (or (metric-prefix value 1000000000000 "T")
         (metric-prefix value 1000000000 "G")
         (metric-prefix value 1000000 "M")
         (metric-prefix value 1000 "k")
         (local-format value)))))
