;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.doc
  "Shorthand for components which read and change values in the document
  by their path, using the :doc/get subscription and the :doc/set and
  :doc/update events. Form inputs take a :path to do this themselves."
  (:require [re-frame.core :as rf]))

(defn value
  "The value at `path` in the document. Use while rendering."
  [path]
  @(rf/subscribe [:doc/get path]))

(defn set-value!
  "Set the value at `path` in the document"
  [path value]
  (rf/dispatch [:doc/set path value]))

(defn update!
  "Update the value at `path` in the document with `f` and `args`"
  [path f & args]
  (rf/dispatch (into [:doc/update path f] args)))
