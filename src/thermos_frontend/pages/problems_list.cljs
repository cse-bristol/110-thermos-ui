(ns thermos-frontend.pages.problems-list
  (:require [thermos-frontend.pages.modal :as modal]
            [thermos-urls :as urls]
            [thermos-frontend.io :as io]
            ))

;; JS to be included in the problems list page (/:org-name)

;; Add a modal to confirm deletion of problem
(.forEach (js/document.querySelectorAll "button[data-action='delete-problem']")
          (fn [delete-button]
            (let [org (.getAttribute delete-button "data-org")
                  problem-name (.getAttribute delete-button "data-problem-name")
                  modal-element (js/document.querySelector (str ".modal[data-problem-name=\"" problem-name "\"]"))]
              (modal/bind-modal
               delete-button
               modal-element
               (fn []
                 (io/delete-problem
                  org
                  problem-name
                  (fn [e]
                    (let [success (= (.getStatus e.target) 204)]
                      (if success
                        ;; @TODO Make this better - put up a little thing saying whether successful or not
                        (js/console.log "Successfully deleted!")
                        (js/console.log "Unable to delete problem"))
                      (modal/hide-modal modal-element)
                      ;; Reload the page to update the list
                      (js/location.reload)
                      )))
                 )))))
