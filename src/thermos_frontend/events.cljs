(ns thermos-frontend.events
  (:require [thermos-frontend.events.core]
            ;; this is lame
            [thermos-frontend.events.pipe-events]))

(def handle thermos-frontend.events.core/handle)
