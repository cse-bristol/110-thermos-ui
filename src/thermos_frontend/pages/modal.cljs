(ns thermos-frontend.pages.modal)

(declare bind-modal show-modal hide-modal)

(defn bind-modal
  "Bind the modal to an element (usually a button) which will trigger its display when clicked."
  [source-element
   modal-element
   confirm-handler]
  ;; Add click listener to the element you want to trigger showing of the modal
  (.addEventListener
          source-element
          "click"
          (fn [e] (show-modal modal-element)))
  ;; Add click listeners to close the modal
  (.forEach
          (.querySelectorAll modal-element "[data-action=\"modal-close\"]")
          (fn [close-button]
            (.addEventListener
             close-button
             "click"
             (fn [e] (hide-modal modal-element)))))
  ;; Add confirm handler, for when you click "Confirm"
  (.addEventListener (.querySelector modal-element "[data-action=\"modal-confirm\"]")
                     "click"
                     confirm-handler)
  )

(defn show-modal
  [modal-element]
  (let [modal-mask (js/document.createElement "div")]
    (.appendChild (js/document.querySelector "body") modal-mask)
    (.. modal-mask -classList (add "modal__mask"))
    (.. modal-element -classList (add "modal--visible"))
    ))

(defn hide-modal
  [modal-element]
  (.removeChild (js/document.querySelector "body") (js/document.querySelector ".modal__mask"))
  (.. modal-element -classList (remove "modal--visible")))
