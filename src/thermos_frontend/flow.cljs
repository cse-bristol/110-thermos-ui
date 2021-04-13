;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.flow
  (:require [goog.async.nextTick]
            [reagent.ratom])
  ;; (:require-macros [thermos-frontend.flow-macro :refer [cache-view!]])
  )

(defprotocol IView
  (view
    [t f]
    [t f x]
    [t f x y]
    [t f x y z]
    "Return a view into this under f, which is a reagent reaction (i.e. you deref it.)
The result is IView so can itself be view-ed. Since the view is of (f this ...), the view
can itself depend on other views by asking for them")
  
  (view*
    [t f]
    [t f x]
    [t f x y]
    [t f x y z]
    "Like view, except the function is called with @this, so cannot ask for more views")
  )

(defprotocol IHandler
  "A reference type for doing a reactive type web thing"
  (fire! [this event])
  (flush! [this]))

(declare cache-view!)

(deftype View [reaction ^:mutable views]
  IView
  (view  [t f]          (cache-view! t false f))
  (view  [t f x]        (cache-view! t false f x))
  (view  [t f x y]      (cache-view! t false f x y))
  (view  [t f x y z]    (cache-view! t false f x y z))

  (view*  [t f]         (cache-view! t true f))
  (view*  [t f x]       (cache-view! t true f x))
  (view*  [t f x y]     (cache-view! t true f x y))
  (view*  [t f x y z]   (cache-view! t true f x y z))

  IDeref
  (-deref [_] (deref reaction))
  
  IWatchable
  (-add-watch [this key cb] (add-watch reaction key cb) this)
  (-remove-watch [this key] (remove-watch reaction key) this))

(defn- cache-view! [view deref? f & args]
  (let [key [deref f args]]
    (or (get (.-views view) key)

        (let [reaction (reagent.ratom/make-reaction
                        (let [[x y z] args]
                          (case (count args)
                            0
                            (if deref?
                              ;; TODO remove apply here
                              #(f (deref view))
                              #(f view))
                            1
                            (if deref?
                              ;; TODO remove apply here
                              #(f (deref view) x)
                              #(f view x))
                            2
                            (if deref?
                              ;; TODO remove apply here
                              #(f (deref view) x y)
                              #(f view x y))
                            3
                            (if deref?
                              ;; TODO remove apply here
                              #(f (deref view) x y z)
                              #(f view x y z))
                            
                            (if deref?
                              ;; TODO remove apply here
                              #(apply f (deref view) args)
                              #(apply f view args)))))
              subview (View. reaction {})
              ]
          (reagent.ratom/add-on-dispose!
           reaction
           (fn [_] (set! (.-views view)
                         (dissoc (.-views view) key))))
          
          (set! (.-views view) (assoc (.-views view) key subview))
          subview))))

(deftype Root [state handler v ^:mutable events]
  IView
  (view   [t f]          (view v f))
  (view   [t f x]        (view v f x))
  (view   [t f x y]      (view v f x y))
  (view   [t f x y z]    (view v f x y z))
  
  (view*  [t f]          (view* v f))
  (view*  [t f x]        (view* v f x))
  (view*  [t f x y]      (view* v f x y))
  (view*  [t f x y z]    (view* v f x y z))

  IDeref
  (-deref [_] (deref state))

  IWatchable
  (-add-watch [this key cb] (add-watch state key cb) this)
  (-remove-watch [this key] (remove-watch state key) this)
  
  IHandler
  (fire! [t event]
    (when (empty? events)
      (goog.async.nextTick #(flush! t)))
    (set! events (conj events event)))
  
  (flush! [t]
    (let [to-run events]
      (set! events #queue [])
      (let [[new-state fx]
            (loop [state @state
                   to-run to-run
                   fx nil]
              (if (empty? to-run)
                [state fx]
                (let [out (handler state (first to-run))]
                  (if-let [effects (::effects out)]
                    (recur
                     (::state out state)
                     (rest to-run)
                     (conj fx (::effects out)))
                    
                    (recur out (rest to-run) fx))
                  )))
            ]
        ;; trigger side-effects
        
        ;; update state (since we have no contention issues in js we
        ;; don't use swap!)
        (reset! state new-state)))))

(defn create-root [{:keys [state handler]}]
  (Root.
   state
   handler
   (View. state {})
   #queue []))

