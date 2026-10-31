// This file is part of THERMOS, copyright © Centre for Sustainable Energy, 2017-2021
// Licensed under the Reciprocal Public License v1.5. See LICENSE for licensing details.

// Behaviour shared by the server-rendered pages. These use htmx; this
// file only has the few bits htmx doesn't do for us.

(function () {
    // Dialogs: anything loaded into #modal is shown as a modal dialog.
    // Elements inside it with data-close (or a form with method=dialog)
    // close it.
    document.addEventListener("htmx:afterSwap", function (e) {
        const modal = document.getElementById("modal");
        if (modal && e.detail.target === modal && !modal.open) {
            modal.showModal();
        }
    });

    document.addEventListener("click", function (e) {
        const closer = e.target.closest("#modal [data-close]");
        if (closer) {
            e.preventDefault();
            closer.closest("dialog").close();
        }
    });

    // A response can ask for the dialog to close with HX-Trigger: close-modal
    document.addEventListener("close-modal", function () {
        const modal = document.getElementById("modal");
        if (modal && modal.open) modal.close();
    });

    // Drop zones: a .drop-target containing an <input type=file>. Dropped
    // files are put into the input, which then fires change as if they had
    // been chosen.
    function bindDropTarget(target) {
        if (target.dataset.bound) return;
        target.dataset.bound = "true";
        const input = target.querySelector("input[type=file]");
        let depth = 0;

        target.addEventListener("dragenter", function (e) {
            e.preventDefault();
            depth++;
            target.classList.add("dragging");
        });
        target.addEventListener("dragleave", function (e) {
            e.preventDefault();
            depth--;
            if (depth <= 0) {
                depth = 0;
                target.classList.remove("dragging");
            }
        });
        target.addEventListener("dragover", function (e) { e.preventDefault(); });
        target.addEventListener("drop", function (e) {
            e.preventDefault();
            depth = 0;
            target.classList.remove("dragging");
            input.files = e.dataTransfer.files;
            input.dispatchEvent(new Event("change", { bubbles: true }));
        });
    }

    // Upload progress: an element with class upload-progress inside the
    // form doing the upload is shown as a progress bar.
    document.addEventListener("htmx:xhr:progress", function (e) {
        const form = e.target.closest("form");
        const bar = form && form.querySelector(".upload-progress");
        if (bar && e.detail.lengthComputable) {
            const pc = Math.round((100 * e.detail.loaded) / e.detail.total);
            bar.hidden = false;
            bar.style.background =
                "linear-gradient(90deg, #87ceeb " + pc + "%, #eee " + pc + "%)";
            bar.textContent = pc < 100 ? "Uploading... " + pc + "%" : "Processing...";
        }
    });

    document.addEventListener("htmx:afterRequest", function (e) {
        const form = e.target.closest && e.target.closest("form");
        const bar = form && form.querySelector(".upload-progress");
        if (bar) {
            bar.hidden = true;
            form.reset();
        }
    });

    function init(root) {
        root.querySelectorAll(".drop-target").forEach(bindDropTarget);
    }

    document.addEventListener("DOMContentLoaded", function () { init(document); });
    document.addEventListener("htmx:load", function (e) { init(e.detail.elt); });
})();
