(ns thermos-ui.frontend.theme)

(defn create-stripes [fg bg on off]
  (let [canvas (js/document.createElement "canvas")
        context (.getContext canvas "2d")

        side (int (* (Math/sqrt 2) (+ on off)))
        
        line (fn [x y x1 y1]
               (.beginPath context)
               (.moveTo context x y)
               (.lineTo context x1 y1)
               (.stroke context))
        ]
    
    (set! (.. canvas -width) side)
    (set! (.. canvas -height) side)

    (set! (.. context -fillStyle) bg)

    (.beginPath context)
    (.rect context 0 0 side side)
    (.fill context)

    (set! (.. context -lineWidth) on)
    (set! (.. context -strokeStyle) fg)
    (set! (.. context -lineCap) "square")
    
    (line (- side) (- side)
          (* 2 side) (* 2 side))


    (line (- side) 0
          side (* 2 side))

    (line 0 (- side)
          (* 2 side) side)
    
    canvas))


(def blue "#00ACE6")
(def red "#ED3027")
(def white "#ffffff")
(def light-grey "#bbbbbb")
(def dark-grey "#555555")
(def green "#98fb98")

(def light-purple-stripes
  (create-stripes "#9400d3" "#bbbbbb" 4 8)
  )
(def dark-purple-stripes
  (create-stripes "#9400d3" "#555555" 4 8)
  )
