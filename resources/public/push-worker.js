// This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
// Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

// Service worker that shows push notifications sent by the server when
// jobs finish (see thermos-backend.notifications). It lives at the root
// so its scope covers every page, and clicking a notification can go to
// a tab that is already open.

self.addEventListener("install", function () { self.skipWaiting(); });
self.addEventListener("activate", function (e) { e.waitUntil(self.clients.claim()); });

self.addEventListener("push", function (e) {
    const data = e.data ? e.data.json() : {};
    e.waitUntil(self.registration.showNotification(data.title || "THERMOS", {
        body: data.body,
        tag: data.tag,
        icon: "/favicon-32x32.png",
        data: { url: data.url || "/" }
    }));
});

self.addEventListener("notificationclick", function (e) {
    e.notification.close();
    const url = new URL(e.notification.data.url, self.location.origin).href;
    e.waitUntil(
        self.clients.matchAll({ type: "window", includeUncontrolled: true })
            .then(function (windows) {
                for (const w of windows) {
                    if (w.url === url && "focus" in w) return w.focus();
                }
                return self.clients.openWindow(url);
            }));
});
