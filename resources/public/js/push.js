// This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
// Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

// Push notifications for when jobs finish. This is used by the settings
// page, and by the editor (as window.thermosPush) to offer them when a
// run will take a while. /push-worker.js shows the notifications.

(function () {
    const supported = "serviceWorker" in navigator &&
          "PushManager" in window &&
          "Notification" in window;

    function keyBytes(s) {
        const b = atob(s.replace(/-/g, "+").replace(/_/g, "/"));
        return Uint8Array.from(b, function (c) { return c.charCodeAt(0); });
    }

    function sameKey(subscription, key) {
        const a = new Uint8Array(subscription.options.applicationServerKey || []);
        const b = keyBytes(key);
        return a.length === b.length && a.every(function (x, i) { return x === b[i]; });
    }

    async function currentSubscription() {
        const reg = await navigator.serviceWorker.getRegistration("/");
        return reg ? reg.pushManager.getSubscription() : null;
    }

    // One of "unsupported", "denied", "off" or "on", for this browser
    async function state() {
        if (!supported) return "unsupported";
        if (Notification.permission === "denied") return "denied";
        if (Notification.permission !== "granted") return "off";
        return (await currentSubscription()) ? "on" : "off";
    }

    // Subscribe this browser and tell the server about it. If the server's
    // key has changed since we subscribed, the old subscription is no good,
    // so we make a new one.
    async function subscribe() {
        await navigator.serviceWorker.register("/push-worker.js");
        const reg = await navigator.serviceWorker.ready;
        const key = (await (await fetch("/settings/push", {
            headers: { "Accept": "application/json" }
        })).json())["public-key"];

        let sub = await reg.pushManager.getSubscription();
        if (sub && !sameKey(sub, key)) {
            await sub.unsubscribe();
            sub = null;
        }
        if (!sub) {
            sub = await reg.pushManager.subscribe({
                userVisibleOnly: true,
                applicationServerKey: keyBytes(key)
            });
        }
        const json = sub.toJSON();
        const response = await fetch("/settings/push", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({
                endpoint: json.endpoint, p256dh: json.keys.p256dh, auth: json.keys.auth
            })
        });
        if (!response.ok) throw new Error("Server refused subscription");
        return "on";
    }

    // This must be called from something the user did, like a click, or
    // the browser won't ask them.
    async function enable() {
        if (!supported) return "unsupported";
        const permission = await Notification.requestPermission();
        if (permission !== "granted") return state();
        return subscribe();
    }

    async function disable() {
        const sub = supported && await currentSubscription();
        if (sub) {
            await fetch("/settings/push", {
                method: "DELETE",
                headers: { "Content-Type": "application/json" },
                body: JSON.stringify({ endpoint: sub.endpoint })
            });
            await sub.unsubscribe();
        }
        return state();
    }

    async function test() {
        return (await fetch("/settings/push/test", { method: "POST" })).json();
    }

    window.thermosPush = { state: state, enable: enable, disable: disable, test: test };

    // The settings page has an element for each state, which has buttons
    // with data-push-action to change it.
    function show(root, s) {
        root.querySelectorAll("[data-push-state]").forEach(function (e) {
            e.hidden = e.dataset.pushState !== s;
        });
    }

    function bindSettings(root) {
        state().then(function (s) {
            show(root, s);
            // tell the server about the subscription again, in case it has
            // forgotten it or changed its key
            if (s === "on") subscribe().catch(function (e) { console.error(e); });
        });

        root.addEventListener("click", function (e) {
            const button = e.target.closest("[data-push-action]");
            if (!button) return;
            e.preventDefault();
            const action = button.dataset.pushAction;
            const message = root.querySelector(".push-message");
            message.textContent = "";
            button.disabled = true;
            ({ enable: enable, disable: disable, test: test })[action]()
                .then(function (result) {
                    if (action === "test") {
                        message.textContent = result.sent ?
                            "Sent - it should appear shortly." :
                            "Nothing was sent; try turning notifications off and on again.";
                    } else {
                        show(root, result);
                    }
                })
                .catch(function (err) {
                    console.error(err);
                    message.textContent = "That didn't work: " + err.message;
                })
                .finally(function () { button.disabled = false; });
        });
    }

    document.addEventListener("DOMContentLoaded", function () {
        document.querySelectorAll(".push-settings").forEach(bindSettings);
    });
})();
