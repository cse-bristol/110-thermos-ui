(ns thermos-frontend.pages.help
  (:require [reagent.core :as reagent]
            [goog.object :as o]
            ))

(enable-console-print!)

(declare menu-panel content-panel menu-item menu-subsection)

(defonce state
  (reagent/atom
   {:active-section-index 0
    :active-subsection-index nil}))

(defonce document
  [["Introduction"
    {:content
     [:div
      [:p
       "This document provides an overview of Version 43 of the THERMOS tool, which is being released in five development stages (Versions 1-5) over the course of the THERMOS project. It also acts as a manual1 for Version 43 which the THERMOS partners can use to note the changes from Version 3 and feedback comments and suggestions for improvements."]
      [:p "This version is intended to illustrate the proposed structure of the THERMOS tool, including the layout of the User Interface (UI) and its key features with regard to selecting objects from a map, filtering using a number of categories, the importing of data, results exporting functionalities and the design of the solver module. The output data is currently a summary table of the main features of the optimal network according to the selected building cluster and the optimisation criteria indicated by the user, which is also displayed as a map. However, current results are based on rough estimates or defaults and should not be used for further analysis."]
      [:p
       "This document also describes and provides examples of the main questions that the THERMOS tool will answer through specific use cases, which detail the objectives of the analysis and the steps to be followed for the tool to provide a solution. Further background on the tool design and methodology can be found in THERMOS Deliverables D1.1 and D3.1."]
      ]
     :subsections []}]
   ["Keyboard shortcuts"
    {:content [:div
               [:table.help__definition-table
                [:tbody
                 [:tr
                  [:td {:style {:width "20px"}} "c"]
                  [:td "Rotate the Constraint status of the selection"]]
                 [:tr
                  [:td "z"]
                  [:td "Zoom to fit the selection or document"]]
                 [:tr
                  [:td "a"]
                  [:td "Select everything in the document"]]
                 [:tr
                  [:td "e"]
                  [:td "Edit the selected objects"]]
                 [:tr
                  [:td "s"]
                  [:td "Change the supply parameters of a building"]]]]]
     :subsections []}]
   ["Structure of the tool"
    {:content
     [:div
      [:p
       "This section gives an overview of the key elements of the tool."]]
     :subsections
     [["Launch screen"
       {:content
        [:div
         [:p
          "For the purposes of this tool version, the link shown above takes the user to a launch screen which lists problems which have been previously saved. This displays the title of the problem, the date the most recent solution was run and when the problem configuration was last updated. The problem titles and dates of the most recent solution are both hyperlinked, allowing the user to re-load the selected building cluster map by clicking on the problem title, or the solution by clicking on the date. This table also allows the user to delete problems from the list. A new problem can be launched by clicking on ‘New +’ at the top right of the screen – see below."]
         [:figure
          {:id "new-problem-fig"}
          [:img {:src "/img/problems_screen.png"
                 :alt "Launching a new problem"}]
          [:figcaption "Launching a new problem"]
          ]]}]
      ["User interface"
       {:content
        [:div
         [:p
          "Once a new problem has been launched, the main user interface (UI) is shown. This is divided into three sections (Map, Options and Help), which can be accessed through the buttons at the top left corner of the screen. Each section is described in detail below."]
         [:h4 "Map"]
         [:p
          "There are three main elements to the map section of the user interface which are displayed simultaneously: an interactive map, a candidate selection form and a table of network candidates. In the bar at the top of the screen (where it says ‘untitled’), the user can type in their own title when beginning an analysis and save the configuration at any time using the ‘Save’ function in the top right corner."]
         [:figure
          {:id "map-fig"}
          [:img {:src "/img/map_ui.png"
                 :alt "User interface map section"}]
          [:figcaption "Elements of the user interface - map section"]]

         [:h4 "Interactive Map"]
         [:p
          "This version of the tool holds mapping data which covers each of the eight THERMOS cities, enabling interactive maps to be called up and viewed on the left side of the screen. Users can search for their city using the ‘Search’ function in the top right corner of the map area. The + and – buttons on the left of the map can then be used to zoom in to see more detail.1 The selection box in the top right corner allows the map view to be selected as follows:"]
         [:table.help__definition-table
          [:tbody
           [:tr
            [:td "None"]
            [:td "No map is displayed."]]
           [:tr
            [:td "Maps"]
            [:td "Displays the base map including buildings (unshaded or hatched polygons – depending on zoom level), roads and railways."]]
           [:tr
            [:td "Satellite"]
            [:td "Switches from base map view to satellite view."]]]]

         [:p "Additional layers can then be toggled on or off as follows:"]

         [:table.help__definition-table
          [:tbody
           [:tr
            [:td "Candidates"]
            [:td "Adds a layer which highlights buildings (shaded polygons), roads (single lines) and potential network connections linking the roads to the buildings (short single lines). These are all referred to as ‘candidates’ for the heat network."]]
           [:tr
            [:td "Heatmap"]
            [:td "Adds a heat map layer where spatial heat demand density is represented by colours."]]
           [:tr
            [:td "Labels"]
            [:td "Adds labels incl. names of roads and districts/neighbourhoods."]]]]

         [:p
          "The next step involves selecting candidates from the map by using one of the two selection buttons on the left. The user can choose to make selections by drawing a polygon or a rectangle. Alternatively, individual candidates can be selected by simply clicking on them (holding down the Ctrl key to select multiple items). Once candidates are selected, a summary of the selection is shown in the candidate selection form on the right hand side of the screen. The bottom selection button on the left of the map can then be used to ‘Zoom to selection’ if required."]
         [:figure
          {:id "map-controls-fig"}
          [:img
           {:src "/img/map_controls.png"
            :alt "THERMOS map controls"
            :style {:max-width "600px"}}]
          [:figcaption "THERMOS map controls"]]

         [:h4 "Candidate selection form"]
         [:p
          "The Candidate Selection Form is shown at the right of the screen and offers a way of viewing, filtering and quantifying candidates that have been selected on the interactive map. The form describes the selection on the map by specifying the total number of selected candidates and their categories or sub-categories (if any) as follows:"]

         [:table.help__definition-table
          [:tbody
           [:tr
            [:td {:style {:width :100px}} "Type"]
            [:td " Demand, supply or path. By default, all candidates which are buildings are deemed to be ‘demand’, although the user can switch this to ‘supply’ by right-clicking on the building in the map and selecting ‘supply point(s)’. This is denoted by white hatched lines within the building polygon. In a similar way, buildings classed as ‘supply’ can be switched back to ‘demand’. Note that the term ‘path’ usually refers to a residential street or connector i.e. connection routes between buildings."]]
           [:tr
            [:td "Classification"]
            [:td "Residential, retail, commercial offices, industrial, building connection + many others)."]]
           [:tr
            [:td "Constraint"]
            [:td
             [:p "This category can be user-defined by selecting candidates according to whether or not they will be included in the Table of Network Candidates (bottom of screen) and therefore included in the network optimisation analysis to follow. These can be categorised as:"]
             [:table.help__definition-table
              [:tr
               [:td "Forbidden"]
               [:td "The candidate will not be considered in the subsequent network analysis and will not appear in the ‘Table of network candidates’ at the bottom of the screen. Such candidates are coloured white on the map."]]
              [:tr
               [:td "Optional"]
               [:td "The candidate may potentially be included in the subsequent network analysis and will appear in the ‘Table of network candidates’ at the bottom of the screen. Such candidates are coloured blue on the map. "]]
              [:tr
               [:td "Required"]
               [:td "The candidate will certainly be included in the subsequent network analysis and will appear in the ‘Table of network candidates’ at the bottom of the screen. Such candidates are coloured red on the map."]]]
             [:p
              "By default, all candidates are deemed to be ‘Forbidden’. The user can therefore change the constraint of any candidate by right-clicking on it (or on multiple candidate selections) in the map and selecting ‘Set inclusion’. In this way, candidates can be added or removed from the ‘Table of Network Candidates’. Note that candidates will only appear in the Candidate Selection Form when they are actively selected on the map (indicated by thicker coloured lines and, additionally in the case of buildings, darker shading of the polygon area)."]
             [:figure
              {:id "selected-candidates-fig"}
              [:img
               {:src "/img/selected_candidates.png"
                :alt "THERMOS selected candidates"}]
              [:figcaption "Candidates' selection"]]
             [:p
              "The button on the left of the map ‘Hide unconstrained candidates’ will hide all candidates that have not been selected as ‘required’ or ‘optional’ i.e. this will display only the candidates included in the ‘Table of Network Candidates’. Clicking again on this button will do the reverse i.e. ‘Show unconstrained candidates’."]]]
           [:tr
            [:td "Name"]
            [:td "Name of each of the candidates."]]
           [:tr
            [:td "Cost"]
            [:td "(Only for roads and connections). "
             "Estimates the cost per meter of the connections within the candidates selected."]]
           [:tr
            [:td "Heat demand"]
            [:td "(Only for buildings). "
             "Estimates the total annual heat demand of the selected candidates. [Note that for Version 3 broad estimates only are included, future versions will implement a more robust heat demand estimation methodology to provide more accurate results – see THERMOS Deliverable D1.1]. "]]
           [:tr
            [:td "Peak demand"]
            [:td "(Only for buildings). "
             "Estimates the peak demand (Wp) for each candidate building."]]]]
         [:p
          "The selected candidates shown in the Candidate Selection Form can be further filtered by selecting or removing the individual sub-groups shown in the above list. For example, where multiple ‘Classifications‘ are shown (e.g. residential, industrial and retail), clicking on the word ‘residential’ will then select this candidate sub-group and remove all others. Alternatively, clicking on the cross in any candidate sub-group will remove it from the selection. "]

         [:h4 "Table of Network Candidates"]
         [:p
          "The Table of Network Candidates should be used to list all the candidates that the user eventually wants the THERMOS model to analyse. It will display all candidates that have been deemed ‘optional’ or ‘required’ i.e. those that are coloured blue or red on the map. These will all be considered in the network optimisation analysis to follow. The table contains columns that indicate the name and other selected properties of each candidate included."]
         [:p
          "In order to make further adjustments, individual candidates listed in the table can be actively selected manually using the tick box in the left-hand column, or as a whole using the tick box alongside the ‘Name’ column. These selected candidates will then be displayed in the Candidate Selection Form and highlighted in the map by use of thicker coloured lines, and additionally in the case of buildings, darker shading of the polygon area."]
         [:p
          "At any time, a file of the selected candidates can be saved using the ‘Save’ button at the top right of the screen. The file is then saved on the remote server hosting the tool application, and the file is displayed on the Launch Screen."]
         [:p
          "The ‘Optimise’ button at the top right of the screen will trigger a network optimisation analysis and a display of output results using the underlying THERMOS model. This takes the form of a new ‘Results’ section which appears in the UI (see "
          [:a.section-link
           {:href "#"
            :data-target-section "Structure of the tool"
            :data-target-subsection "Solver module"}
           ""]
          "). However, current results will be based on rough estimates and only constitute an example of the outputs that will be provided to the user, i.e. they should not be used for further analysis given that the model is currently under development."]

         [:h4 "Editing building and path characteristics"]
         [:p
          "Users can choose to add or change information relating to selected candidates by right-clicking on the item in the map section of the screen and then choosing the ‘Edit buildings’ (for buildings) or ‘Edit cost’ (for paths) option. Alternatively, clicking on the target object and using the ‘e’ key will also bring up the same pop-up box. Here, it is possible to replace modelled demand (annual and peak) figures with more accurate data, and to add a specific heat price or emissions levels (NOx, PM2.5 and CO2). Similarly, it is possible to add supplementary information to the selected supply point, including cost data, maximum capacity and emissions factors."]

         [:h4 "Options"]
         [:p
          "Once the candidates of the network have been selected using the ‘Map’ section of the UI, the user can insert and adjust some of the key parameters of the analysis in the ‘Options’ section, which currently includes the following categories:"]
         [:table.help__definition-table
          [:tbody
           [:tr
            [:td {:style {:width :150px}} "Costs and limits"]
            [:td "Allows the user to set period of operation (years) and discount rate (%), and to adjust the period (years) and interest rate (%) of the financing for both the plant and network. The user can also set a maximum cost per tonne for emissions (by type, i.e. NOx, PM2.5, CO2), and a pipe capacity limit (MWp)."]]
           [:tr
            [:td "Site defaults"]
            [:td "Allows the user to insert default figures for both the heat sale price and for emissions factors. These will be applied in all cases except where the user has added candidate-specific details in the previous tab."]]
           [:tr
            [:td "Optimisation parameters"]
            [:td "Indicates how close the solution should be to an ‘optimal’ one i.e. a measure of optimality. 10% is set by default. It is also possible to set a maximum runtime (the default is 3 hours)."]]]]
         [:figure
          {:id "thermos-options-fig"}
          [:img
           {:src "/img/thermos_options.png"
            :alt "THERMOS options page"}]
          [:figcaption "Elements of the user interface - options selection"]]

         [:p
          "Note that in this version, all costs within this section are expressed without a specific currency: the user is allowed to insert the data in the preferred currency, as long as there is a consistency within the values inserted."]
         [:p
          "Once adjustments are complete, the user can save the configuration and run the model using the buttons at the top right of the screen."]
         ]}]
      ["Solver module"
       {:content
        [:div
         [:p
          "The solver module (optimisation model) provides the user with the optimal solution according to the problem and constraints defined. "]
         [:p
          "Once the problem (i.e. identification of the candidates – supply and demand points and connections) has been defined in the ‘map’ section of the UI and the main parameters (e.g. costs, revenues, carbon emissions limit) have been determined in the ‘options’ section, the THERMOS model will deliver the optimal solution to the problem via the ‘Optimise’ button. Note that in Version 3 the solver incorporates a queuing system for problems that have been submitted concurrently and a display may appear informing the user of the current place in the queue.  "]
         [:p
          "Once the model has run, a new ‘Results’ section is created in the UI. This section provides the user with the key parameters of the optimised network if a solution has indeed been found. In Version 4 of the THERMOS tool, this section is divided into 4 separate tabs. The ‘Summary’ tab provides an overview of the financial modelling outputs, including figures for capital costs (both principal and through finance), annual operating costs, costs related to emissions (also annual), and revenue from heat sales. The ‘Demands’ tab lists all of the candidate buildings, along with associated heat demand (Wh/yr), connection size (W), heat price (c/kWh), and revenue (¤/yr). Similarly, the ‘Network’ tab includes a list of all pipework sections, plus details of length (m), principal costs (¤), capacity (W) and diversity. The ‘Supply’ tab provides headline plant information, including capacity (MWp), output (GWh/yr), diversity factor and principal cost."]
         [:figure
          {:id "results-section-fig"}
          [:img
           {:src "/img/results_section.png"
            :alt "THERMOS results section"}]
          [:figcaption "Results section"]]
         [:p
          "All elements which were ‘sent’ to the solver and included in the analysis (i.e. those listed in the Table of Network Candidates) are then shown on the map section, but those that have subsequently been included in the optimal solution are shown in blue (optional) and red (required) as before, while those excluded from the optimal solutionthat are shown in  green are optional candidates that have not been included in the optimal solution, and those shown in purple are candidates that could not be connected  (see figure below). Building on this information, the user can continue the simulations by excluding elements from the problem and/or incorporating new buildings and connections."]
         [:figure
          {:id "results-in-map-fig"}
          [:img
           {:src "/img/results_in_map.png"
            :alt "Results in map section"}]
          [:figcaption "Results in the map section"]]
         [:p
          "The elements included in the optimal solution are also now highlighted in the Table of Network Candidates by means of a tick in a new right-hand column:"]
         [:figure
          {:id "candidates-in-optimal-solution-fig"}
          [:img
           {:src "/img/candidates_in_solution.png"
            :alt "Candidates included in the optimal solution"}]
          [:figcaption "Candidates included in the optimal solution"]]
         ]}]
      ]}
    ]

   ["Use cases"
    {:content
     [:div
      [:p
       "This section describes the four main ‘use cases’ that the THERMOS tool will typically address according to the needs of the target users. These are:"]]
     :subsections
     [["Adding new sites and connections to an existing network"
       {:content
        [:div
         [:h4 "Objective"]
         [:p
          "The first THERMOS use case aims to let potential users (e.g. local governments, energy planners) assess possible expansions of an existing district heating and cooling network. The THERMOS tool makes it much easier to compare network options within the same analysis, therefore avoiding repeated individual analysis and the associated cost, both in terms of money and time."]
         [:p
          "Thus, under the first use case the user would specify the structure of the existing network, the options for extending it, and the criterion for selecting the best solution (e.g. maximum NPV, IRR, max. number of users connected). The tool would then return a description of the optimal solution for the criterion selected."]
         [:h4 "Procedure"]
         [:ol
          [:li "First, the user needs to define the existing heating network. In doing so, the user must select all the demand and supply points of the network and the existing connections. As outlined in Section 2.2.1, the user can either select the elements of the network individually or use the two selection buttons (draw a polygon or a rectangle) available on the left of the map. Where better information is available, the user can choose to manually override the default values for demand and supply. Once the network has been defined, the user needs to categorise it as ‘required’ (red, following the colour code), as shown in Figure 10 below."]
          [:li "The user then defines the potential candidates for expansion of the network, following the same process described in the first step. However, the potential candidates should be instead categorised as ‘optional’ (blue as set in the colour code), as shown in Figure 10. By doing this, the tool will only incorporate into the final solution those candidates which meet the requirements for optimisation (see step 3). Additionally, the user can exclude certain candidate buildings or connections from the final solution by selecting and categorising them as ‘forbidden’ (in white)."]
          [:li "The user then has to select what ‘best’ means in each particular analysis, i.e. what is the variable (annualised benefit, CO2 emissions, initial investment, etc.) to be optimised by the model. "]
          [:li "After pressing ‘Optimise’, if a solution is found, the tool will provide a description of the optimal solution based on the features of the existing network, the new potential candidates and the optimisation criteria selected."]]

         [:h4 "Results"]
         [:p
          "The THERMOS tool provides the user with the optimal solution, which may exclude some of the potential candidates for expansion identified (see Figure 11 below). In addition, the summary table including the main parameters of the optimal network is also provided (see Section 2.3)."]
         ]}]

      ["Planning a new network based on a given heat source"
       {:content
        [:div
         [:h4 "Objectives"]
         [:p
          "The second THERMOS use case allows the user to plan a new district heating network based on a given heat source. In this case, the user specifies the location of the heat source, the set of potential demands and connections, and the criterion for selecting the best solution. The tool returns a description of the optimal solution."]
         [:h4 "Procedure"]
         [:ol
          [:li "The user first defines the existing heat supply source in the map, which can be selected individually with default values overridden if necessary. Once the supply point has been identified, it must be categorised as a heat plant (hint - use the ‘t’ keyboard shortcut) and as a ‘required’ element (in red, according to the colour code), as shown in Figure 12."]
          [:li "The user then selects the set of candidate buildings and connections that could potentially comprise the new district heating network. The set of potential demands and connections can be selected either manually or using the two selection buttons on the left of the map but needs to be categorised as ‘optional’ (in blue). As in the first use case, the user can exclude buildings and connections from the analysis by categorising them as ‘forbidden’ (in white)."]
          [:li "The user finally selects the optimisation criterion and presses ‘Optimise’ to start the simulation."]
          [:li "If a solution is found, the tool will provide a description of the optimal solution based on the features of the existing supply source, the potential candidates and the optimisation criteria selected."]]
         [:h4 "Results"]
         [:p
          "The results section will include the summary table with the key parameters of the network (buildings to be included, heat plant to be incorporated and connections to be used) and economic, energy and environmental indicators, along with the tab containing information relating to each demand point included in the solution, the tab containing information about the supply point, and the tab containing the list of pipework sections."]
         [:p
          "In addition, a map with all the elements included in the optimal solution would be displayed, as shown in Figure 13 below:"]
         ]}]

      ["Designing a new network to supply a given set of buildings using one or more potential heat sources"
       {:content
        [:div
         [:h4 "Objectives"]
         [:p
          "The third THERMOS use case is similar to the second. The user specifies the set of buildings which must be served by the network, the location(s) of the potentially available heat source(s), and the criterion for selecting the best solution. The tool then returns a description of the optimal solution."]
         [:p
          "Through this kind of analysis, energy planners can easily identify the best potential routes and heat sources for an identified set of buildings whose owners may be willing to shift from individual heating solutions to a district heating network (e.g. buildings already contacted or new neighbourhoods)."]
         [:h4 "Procedure"]
         [:ol
          [:li "The user selects the set of buildings (demand points) to be incorporated into the new district heating network, either individually or through the two selection buttons available, and categorises them as ‘required’ (in red, according to the colour code), overriding the default heat demand values where appropriate."]
          [:li "The user then selects the potential heat sources, categorising them as supply points (hint – use the ‘t’ keyboard shortcut) and as ‘optional’ (in blue), and adjusting the default heat supply values if necessary."]
          [:li "After selecting the optimisation criterion to be followed by the solver, the user presses the ‘Optimise’ button."]
          [:li "If a solution is found, a description of the optimal solution (i.e. heat sources to be included in the potential heat network and routes to follow) is provided by the tool, in accordance with the optimisation criterion chosen."]]
         [:h4 "Results"]
         [:p
          "The THERMOS software provides the optimal district heating network, including only the potential heat sources, the buildings (demand points) and the connections (pipes) that fit the optimisation criteria indicated by the user. This solution would be accompanied by the summary table described in section 2.3."]]}]

      ["Assessing / comparing the performance of specific networks and/or non-networked solutions"
       {:content
        [:div
         [:h4 "Objectives"]
         [:p
          "The tool also allows for individual analysis of the performance of specific networks, thus allowing a comparison of options. Future versions may also incorporate a facility to compare with non-networked solutions. "]
         [:h4 "Procedure"]
         [:ol
          [:li "To perform this type of analysis, the problem structure process would be split into a set of questions. In each one, a network would be specified, with no option for expansion (i.e. categorising each network as ‘required’, in red). "]
          [:li "After pressing the ‘Optimise’ button, the tool would return a description of the main features of each network defined."]
          [:li "The descriptions would then be collated and compared by the user."]]
         [:h4 "Results"]
         [:p
          "The THERMOS tool provides the user with a description of the networks specified, including key performance parameters and energy and economic indicators. The user would then assess and compare the descriptions."]]}]
      ]}]])

;; This is so that if we want to we can reference figures from within the text.
(defn get-figures-list []
  (let [figures (atom [])
        get-figures (fn [content-tree]
                      (filter #(= (first %) :figure)
                              (tree-seq sequential? rest content-tree)))]

    (doseq [[title {content :content subsections :subsections}] document]
      (swap! figures #(vec (concat % (get-figures content))))
      (doseq [[title {content :content}] subsections]
        (swap! figures #(vec (concat % (get-figures content))))))
    @figures))

(defonce figures (get-figures-list))


(defn help-app
  []
  [:div.help__app-container
   [menu-panel]
   [:div.help__content-panel]
   ])

(defn menu-panel
  []
  [:div.help__menu-panel
   [:h2 "THERMOS Help"]
   [:input.help__search-input
    {:type :text
     :placeholder "Search help"}]
   [:ul.help__menu-items
    (doall
      (map-indexed
       (fn [index [title {content :content subsections :subsections}]]
         [menu-item index title content subsections])
       document))]])

(defn content-panel
  []
  (let [title (first (get document (:active-section-index @state)))
        content (:content (second (get document (:active-section-index @state))))
        subsections (:subsections (second (get document (:active-section-index @state))))
        active-section-index (:active-section-index @state)
        active-subsection-index (:active-subsection-index @state)

        go-to-previous (fn []
                         (cond
                           (pos? active-subsection-index)
                           (swap! state update :active-subsection-index dec)

                           (= active-subsection-index 0)
                           (swap! state assoc :active-subsection-index nil)

                           (pos? active-section-index)
                           (let [prev-section (get document (dec active-section-index))
                                 prev-subsections (:subsections (second prev-section))
                                 prev-subsection-last-ind (if (not-empty prev-subsections)
                                                            (dec (count prev-subsections))
                                                            nil)]
                             (swap! state update :active-section-index dec)
                             (swap! state assoc :active-subsection-index prev-subsection-last-ind))))

        go-to-next (fn []
                     (let [last-section-index (dec (count document))
                           last-subsection-index (dec (count subsections))]
                       (cond
                         (= active-subsection-index last-subsection-index)
                         (swap! state merge {:active-section-index (inc active-section-index)
                                             :active-subsection-index nil})

                         (and (nil? active-subsection-index)
                              (= active-section-index last-section-index)
                              (not-empty subsections))
                         (swap! state assoc :active-subsection-index 0)

                         (and (nil? active-subsection-index)
                              (< active-section-index last-section-index))
                         (if (not-empty subsections)
                           (swap! state assoc :active-subsection-index 0)
                           (swap! state update :active-section-index inc))

                         (< active-subsection-index last-subsection-index)
                         (swap! state update :active-subsection-index inc))))]

    [:div.help__content-panel
     {:ref (fn [node]
             (when node
               ;; For any figures in this section, add "Figure #:" to the start of the caption
               (doseq [fig (.from js/Array (.querySelectorAll node "figure"))]
                 (let [fig-id (o/get fig "id")
                       fig-index (first (first (filter (fn [[ind f]] (= (:id (second f)) fig-id))
                                                       (map-indexed vector figures))))
                       figcaption (.querySelector fig "figcaption")]

                   (o/set figcaption "innerText"
                          (str "Figure " (inc fig-index) ": " (o/get figcaption "innerText")))))

               ;; Make any section links work
               (doseq [link (.from js/Array (.querySelectorAll node "a.section-link"))]
                 (let [target-section (.getAttribute link "data-target-section")
                       target-subsection (.getAttribute link "data-target-subsection")
                       target-section-index (first (first (filter (fn [[ind [title stuff]]]
                                                                    (= title target-section))
                                                                  (map-indexed vector document))))

                       target-subsection-index (first (first (filter (fn [[ind [title stuff]]]
                                                                       (= title target-section))
                                                                     (map-indexed vector
                                                                                  (-> document
                                                                                      (get target-section-index)
                                                                                      second
                                                                                      :subsections)))))]

                   ;; @TODO Finish this off

                   ; (swap! state merge {:active-section-index target-section-index
                   ;                     :active-subsection-index target-subsection-index})

                   )
                 )
               ))}

     [:h2 (first (get document active-section-index))]

     ;; If we are in a subsection, render it
     (if (:active-subsection-index @state)
       (let [[title {content :content}] (get subsections active-subsection-index)]
         [:div
          [:h3 title]
          content])

       ;; Otherwise, display any top-level content and make a list of links to subsections, if they exist
       [:div
        (:content (second (get document active-section-index)))
        [:ul.help__content-panel-subsection-links
         (doall
           (map-indexed
            (fn [ind [title {content :content}]]
              [:li
               [:a
                {:key ind
                 :href (str "#" active-section-index "." ind)
                 :on-click (fn [e] (.preventDefault e)
                             (swap! state assoc :active-subsection-index ind))}
                "# " title]]
              )
            subsections))]])

     ;; Previous and Next buttons
     [:nav.help__nav
      (let [disabled? (and (= active-section-index 0)
                           (or (= active-subsection-index 0) (nil? active-subsection-index)))]
        (when-not disabled?
          [:button.help__nav-previous-button
           {:on-click #(go-to-previous)}
           "Previous"]))

      (let [disabled? (and (= active-section-index (dec (count document)))
                          (if (not-empty subsections)
                            (= active-subsection-index (dec (count subsections)))
                            (nil? active-subsection-index)))]
        (when-not disabled?
          [:button.help__nav-next-button
           {:on-click #(go-to-next)}
           "Next"]))]
     ]))

(defn menu-item
  [index title content subsections]

  [:li.help__menu-item
   {:key (str "section-" index)
    :class (str (if (= index (:active-section-index @state))
                  "help__menu-item--active" "")
                (if (not-empty subsections)
                  " help__menu-item--with-subsections" ""))}
   [:span.help__menu-item-title
    {:on-click (fn [] (swap! state assoc :active-section-index index)
                (swap! state assoc :active-subsection-index nil))}
    title]

   (when (and (not-empty subsections) (= index (:active-section-index @state)))
     [:ul.help__menu-subsections
      (doall
        (map-indexed
         (fn [idx [title {content :content}]] [menu-subsection index idx title content])
         subsections))]
     )])

(defn menu-subsection
  [parent-index index title content]
  [:li.help__menu-subsection
   {:key (str "subsection-" index)
    :class (when (= index (:active-subsection-index @state)) "help__menu-subsection--active")
    :on-click #(swap! state assoc :active-subsection-index index)}
   title])

(reagent/render
 [menu-panel]
 (js/document.getElementById "help-menu-panel"))

(reagent/render
 [content-panel]
 (js/document.getElementById "help-content-panel"))
