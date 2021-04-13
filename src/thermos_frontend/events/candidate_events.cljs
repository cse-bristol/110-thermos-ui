;; This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
;; Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

(ns thermos-frontend.events.candidate-events
  (:require [thermos-frontend.events.core :refer [handle]]
            [thermos-specs.document :as document]
            [thermos-frontend.operations :as operations]
            [com.rpl.specter :as S]))

(defmethod handle :group-selection
  [state _]
  (let [selection (operations/selected-candidates-ids state)]
    (document/group-candidates state selection)))

(defmethod handle :ungroup-selection
  [state _]

  (let [selection (operations/selected-candidates-ids state)]
    (document/ungroup-candidates state selection)))

(defmethod handle :group-select-members
  [state]
  
  (document/select-group-members state))
