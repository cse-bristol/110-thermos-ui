// This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
// Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

// Leaflet maps for the server-rendered pages (map import, LIDAR).
//
// A map is a <div class="map-box" data-config="{...}">, where the config may have
//  - boundary:  geojson to draw and zoom to
//  - coverage:  LIDAR coverage geojson (features with source / filename)
//  - coverageUrl: URL to get coverage geojson from instead
//  - bounds:    [lat0, lat1, lon0, lon1] to zoom to (nominatim boundingbox order)
//  - draw:      true to allow drawing a box
//
// When drawing is allowed, the enclosing form should have hidden inputs called
// boundary, centroid, osm-id and map-position. Drawing a box or choosing a
// search result (from an <input class="map-search"> in the same form) fills
// them in and fires a "mapchange" event on the form, for htmx to save.

(function () {
    function featureStyle(feature) {
        return feature.properties && feature.properties.source === "system"
            ? { color: "#DD0077" }
            : { color: "#33ff88" };
    }

    function featureTooltip(feature) {
        return feature.properties && feature.properties.source === "system"
            ? "system LIDAR"
            : feature.properties && feature.properties.filename;
    }

    function fitTo(map, layer) {
        try {
            const bounds = layer.getBounds();
            if (bounds.isValid()) map.fitBounds(bounds);
        } catch (e) { }
    }

    function setInput(form, name, value) {
        const input = form && form.querySelector("input[name='" + name + "']");
        if (input) input.value = value == null ? "" : value;
    }

    function changed(form) {
        if (form) form.dispatchEvent(new Event("mapchange", { bubbles: true }));
    }

    function initMapBox(el) {
        if (el.dataset.bound) return;
        el.dataset.bound = "true";

        const config = JSON.parse(el.dataset.config || "{}");
        const form = el.closest("form");

        const map = L.map(el, {
            minZoom: 2, maxZoom: 20,
            zoom: 15, center: [51.553356, -0.109271]
        });

        map.addLayer(L.tileLayer(
            "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
            {
                minZoom: 0, maxZoom: 20,
                className: "leaflet-basemap",
                attribution: "Map tiles &copy; <a href=\"https://www.openstreetmap.org/copyright\">OpenStreetMap</a> contributors."
            }));

        const boundary = L.geoJSON(config.boundary || null).addTo(map);

        const coverage = L.geoJSON(null, {
            style: featureStyle,
            onEachFeature: function (feature, layer) {
                layer.bindTooltip(featureTooltip(feature), { direction: "center" });
            }
        }).addTo(map);

        function zoom() {
            const b = config.bounds;
            if (b && b[0] != null) {
                map.fitBounds(L.latLngBounds([+b[0], +b[2]], [+b[1], +b[3]]));
            } else if (config.boundary) {
                fitTo(map, boundary);
            } else {
                fitTo(map, coverage);
            }
        }

        if (config.coverage) coverage.addData(config.coverage);
        zoom();

        if (config.coverageUrl) {
            fetch(config.coverageUrl)
                .then(function (r) { return r.json(); })
                .then(function (data) { coverage.addData(data); zoom(); });
        }

        if (config.draw) {
            map.addControl(new L.Control.Draw({
                position: "topleft",
                draw: {
                    polyline: false, polygon: false, marker: false,
                    circle: false, circlemarker: false
                }
            }));

            map.on(L.Draw.Event.CREATED, function (e) {
                const b = e.layer.getBounds();
                const feature = e.layer.toGeoJSON();
                feature.properties = feature.properties || {};
                feature.properties.bounds = {
                    x1: b.getWest(), y1: b.getSouth(),
                    x2: b.getEast(), y2: b.getNorth()
                };
                const c = b.getCenter();

                boundary.clearLayers();
                boundary.addData(feature);

                setInput(form, "boundary", JSON.stringify(feature));
                setInput(form, "centroid", JSON.stringify({ lat: c.lat, lng: c.lng }));
                setInput(form, "osm-id", "");
                setInput(form, "map-position", "");
                changed(form);
            });
        }

        const search = form && form.querySelector("input.map-search");
        if (search) bindSearch(search, map, boundary, form);

        // the map may have been laid out before it was visible
        setTimeout(function () { map.invalidateSize(); zoom(); }, 0);
    }

    function bindSearch(input, map, boundary, form) {
        const results = input.parentElement.querySelector(".results");
        let timer = null;

        input.addEventListener("keydown", function (e) {
            if (e.key === "Enter") e.preventDefault();
        });

        input.addEventListener("input", function () {
            clearTimeout(timer);
            const query = input.value;
            if (!query.trim()) {
                results.replaceChildren();
                return;
            }
            timer = setTimeout(function () {
                const url = "https://nominatim.openstreetmap.org/search?" +
                    new URLSearchParams({
                        format: "json", q: query,
                        featuretype: "settlement", polygon_geojson: 1
                    });
                fetch(url)
                    .then(function (r) { return r.json(); })
                    .then(function (places) { showResults(places); });
            }, 500);
        });

        function showResults(places) {
            results.replaceChildren();
            places.forEach(function (place) {
                const div = document.createElement("div");
                div.className = "result";
                const b = document.createElement("b");
                b.textContent = place.display_name;
                const info = document.createElement("div");
                info.textContent = place.class + " - " + place.type + " - " + place.osm_type;
                div.append(b, info);
                div.addEventListener("click", function () { select(place); });
                results.append(div);
            });
        }

        function select(place) {
            results.replaceChildren();
            input.value = "";

            const bb = place.boundingbox;
            map.fitBounds(L.latLngBounds([+bb[0], +bb[2]], [+bb[1], +bb[3]]));
            setInput(form, "map-position", JSON.stringify(bb));

            if (place.osm_type === "way" || place.osm_type === "relation") {
                boundary.clearLayers();
                boundary.addData(place.geojson);
                setInput(form, "osm-id", place.osm_type + ":" + place.osm_id);
                setInput(form, "boundary", JSON.stringify(place.geojson));
                setInput(form, "centroid", JSON.stringify({
                    lat: (+bb[0] + +bb[1]) / 2,
                    lng: (+bb[2] + +bb[3]) / 2
                }));
            }
            changed(form);
        }
    }

    function init(root) {
        if (root.classList && root.classList.contains("map-box")) initMapBox(root);
        root.querySelectorAll(".map-box").forEach(initMapBox);
    }

    document.addEventListener("DOMContentLoaded", function () { init(document); });
    document.addEventListener("htmx:load", function (e) { init(e.detail.elt); });
})();
