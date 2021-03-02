(ns thermos-util.pipes
  (:require [thermos-util.steam :as steam]
            [thermos-util.hot-water :as hw]
            [thermos-specs.document :as document]
            [thermos-util.pwlin :refer [linear-approximate linear-evaluate]]
            [thermos-util :refer [assoc-by]]

            [thermos-specs.path :as path]))

(defn- pipe-rows [doc]
  (-> doc ::document/pipe-costs :rows (->> (into (sorted-map)))))

(defn select-parameters
  "This is a bit magic and would be better tidied up.

  These are all the things in the document which control the pipe
  model, which we want to be able to pick out for efficiency when
  rerendering in the UI.
  "
  [doc]
  (select-keys doc
               [::document/flow-temperature
                ::document/return-temperature
                ::document/ground-temperature
                ::document/steam-pressure
                ::document/steam-velocity
                ::document/medium
                ::document/pipe-costs]))

(defn power-for-diameter
  "Given a pipe diameter, what power can it carry using the builtin models.

  This function is intended for spot calculations in the UI.
  For setting up the model, use curves below, as it accounts for overrides.
  "
  ^double [doc ^double dia-mm]
  
  (case (document/medium doc)
    :hot-water
    (let [delta-t (document/delta-t doc)
          t-bar   (document/mean-temperature doc)
          dia-m   (/ dia-mm 1000.0)]
      (hw/kw-per-m dia-m delta-t t-bar))
    
    :saturated-steam
    (let [steam-pressure (document/steam-pressure doc)
          steam-velocity (document/steam-velocity doc)
          dia-m          (/ dia-mm 1000.0)]
      (steam/pipe-capacity-kw
       steam-pressure steam-velocity
       dia-m))))

(defn losses-for-diameter
  "Given a pipe diameter, what are the expected losses

  This function is intended for spot calculations in the UI.
  For setting up the model, use curves below."
  [doc dia-mm]
  (case (document/medium doc)
    :hot-water
    (hw/heat-loss-w-per-m (document/delta-ground doc) (/ dia-mm 1000.0))
    
    :saturated-steam
    (try (steam/heat-losses-kwh%m-yr (document/steam-pressure doc) (/ dia-mm 1000.0))
         #?(:cljs
            (catch :default e))
         #?(:clj
            (catch Exception e))
         )))

(defn curves
  "Given a document, produce a blob of stuff that contains information
  for the functions below.

  They all have some common input so it is useful to have that available"
  [doc]

  (let [curve-data
        (case (document/medium doc)
          :hot-water
          (let [delta-t (document/delta-t doc)
                t-bar   (document/mean-temperature doc)
                delta-g (document/delta-ground doc)]
            (vec
             (for [[dia row] (pipe-rows doc)
                   :let [dia (/ dia 1000.0)
                         capacity-kw (or (:capacity-kw row)
                                         (hw/kw-per-m dia delta-t t-bar))
                         losses-kwh  (or (:losses-kwh row)
                                         (hw/heat-loss-w-per-m delta-g dia))]
                   :when (and capacity-kw losses-kwh)]
               (merge
                row
                {:diameter dia
                 :capacity-kw capacity-kw
                 :losses-kwh losses-kwh
                 }))))
          
          
          :saturated-steam
          (let [steam-pressure (document/steam-pressure doc)
                steam-velocity (document/steam-velocity doc)]
            (vec
             (for [[dia row] (pipe-rows doc)
                   :let [dia (/ dia 1000.0)
                         capacity-kw (or (:capacity-kw row)
                                         (try
                                           (steam/pipe-capacity-kw
                                            steam-pressure steam-velocity
                                            dia)
                                           (catch #?(:cljs :default :clj Exception) e))
                                         )
                         losses-kwh  (or (:losses-kwh row)
                                         (try
                                           (steam/heat-losses-kwh%m-yr steam-pressure dia)
                                           (catch #?(:cljs :default :clj Exception) e)))
                         ]
                   :when (and capacity-kw losses-kwh)]
               (merge
                row

                {:capacity-kw capacity-kw
                 
                 
                 :diameter dia

                 :losses-kwh losses-kwh
                 
                 })))))]
    
    {:default-civils
     (:default-civils (::document/pipe-costs doc))

     :dia->kw
     (->> curve-data
          (map (juxt :diameter :capacity-kw))
          (sort)
          (vec))

     :min-kw
     (reduce min
             #?(:clj Double/MAX_VALUE
                :cljs js/Number.MAX_VALUE)
             (map :capacity-kw curve-data))
     
     :max-kw
     (reduce max 0 (map :capacity-kw curve-data))
     
     :heat-loss-curve
     (->> curve-data
          (map (juxt :capacity-kw :losses-kwh))
          (sort)
          (vec))

     :data
     (-> curve-data
         (assoc-by :capacity-kw)
         (->> (into (sorted-map))))
     }
    
    
    ))

(defn dia->kw-range
  "Given curves from above, return [min kW, max kW], the range in kW for which we would use a pipe of this diameter. That is the size of the next pipe down + 1 kW upto this one.

  This function is not efficient, if you call it a lot you will want to come in and fix it up."
  [curves dia-mm]

  (let [dia (/ dia-mm 1000.0)
        dia->kw (into (sorted-map) (:dia->kw curves))
        next-up (rsubseq dia->kw <= dia)]
    (cond
      (or (empty? next-up))
      ;; diameter is below minimum, so we want 0 .. min
      [0 (second (first dia->kw))]

      (== (first (first next-up)) dia) ;; diameter is in the table
      (let [[[_ a] [_ b]] next-up] [(inc (or b -1)) a])

      :else ;; diameter is not in the table
      (let [diameter-up (first (first (subseq dia->kw > dia)))]
        (if (nil? diameter-up)
          (throw (ex-info "Diameter is larger than maximum known in table" {:dia dia :dia->kw dia->kw}))
          (dia->kw-range curves (* 1000.0 diameter-up)))))))


(comment
  ;; convert to test
  (dia->kw-range
   {:dia->kw [[0.01 50] [0.02 100] [0.03 200]]}
   30) ;; => [101 200]

  (dia->kw-range
   {:dia->kw [[0.01 50] [0.02 100] [0.03 200]]}
   29) ;; => [101 200]  

  (dia->kw-range
   {:dia->kw [[0.01 50] [0.02 100] [0.03 200]]}
   20) ;; => [51 100]

  (dia->kw-range
   {:dia->kw [[0.01 50] [0.02 100] [0.03 200]]}
   5) ;; => [0 50]
  )


(defn dia->kw
  "Given some curves from above, interpolate out the kw carried by a pipe of given dia.
  This is happy with diameters that are not in the table."
  [curves dia-mm]

  (linear-evaluate (:dia->kw curves) (/ dia-mm 1000.0)))

(defn max-kw
  "What's the max power on this set of curves?"
  [curves]

  (:max-kw curves 0))

(defn min-kw
  "What's the min power on this set of curves?"
  [curves]

  (:min-kw curves 0))

(defn heat-loss-curve
  "Return a vector like [[kwp, kwh-losses], ...]"
  [curves]

  (:heat-loss-curve curves))

(defn curve-rows
  "Return the rows information in curves. This is like the :rows in ::document/pipe-costs
  except that
  - the losses and capacity are filled in for rows where that data is missing.
  - it's a seq of maps, and the maps have :diameter, rather than being a map from diameter.
  "
  [curves]
  (sort-by :diameter (vals (:data curves))))

(defn linear-cost
  "Give the linearised approximate cost function for something with
  given civil cost and power range.

  - `curves` is the result of the function above
  - `length-by-civils` is a map from civil cost ID to length of the path that has that cost ID
  - `total-length` is used to normalize the cost that comes out
  - `kw-min` and `kw-max` decide which bit of the resulting cost curve we are approximating.

  Returns a tuple of [fixed cost, cost/kwp]
  "
  [curves length-by-civils total-length kw-min kw-max]

  (let [data (:data curves)
        default-cost (:default-civils curves)
        
        cost-curve
        (for [[capacity costs] data]
          [capacity
           (reduce-kv
            (fn [acc cost-id length]
              (+ acc
                 (* (/ length total-length)
                    (+ (get costs :pipe 0)
                       (get costs (or cost-id default-cost) 0)))))
            0
            length-by-civils)])]
    
    (let [curve-min (min-kw curves)
          curve-max (max-kw curves)
          
          kw-min (min curve-max (max kw-min curve-min))
          kw-max (max curve-min (min kw-max curve-max))

          terms (linear-approximate cost-curve kw-min kw-max)
          ;; [c m] terms
          ]
      terms
      )))

(defn solved-principal
  "Given a kwp, how much would a certain path cost per meter"
  [curves civil-id kwp]

  (let [default-cost (:default-civils curves)
        data    (:data curves)
        next-up (second (first (subseq data >= kwp)))]
    (+ (get next-up :pipe 0)
       (get next-up (or civil-id default-cost) 0))))

(defn solved-diameter [curves kwp]
  (let [data    (:data curves)
        next-up (second (first (subseq data >= kwp)))]
    (* 1000.0 (get next-up :diameter 0))))

