(ns thermos-backend.content-migrations.messages)

(def messages
  {:update-pipe-parameters
   [:div
    [:b "Pipe cost parameters changed"]

    [:p "Pipe costs have been converted from equations to a table, which you can see on the pipe costs page. "
     "Network solutions will now choose only pipes in the table."]
    [:p "This change will affect model results if you re-run the model."]]})
