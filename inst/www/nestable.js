/* nestable.js — loaded once per page via htmlDependency */

(function () {
  "use strict";

  /* Recursively hide all descendants of parentId without touching their
     aria-expanded state, so re-expanding restores the previous subtree. */
  function hideDescendants(parentId) {
    document.querySelectorAll("[data-parent='" + parentId + "']")
      .forEach(function (row) {
        row.style.display = "none";
        hideDescendants(row.id);
      });
  }

  /* Show direct children of parentId. For any child that was previously
     expanded, recursively restore its own children too. */
  function showChildren(parentId) {
    document.querySelectorAll("[data-parent='" + parentId + "']")
      .forEach(function (row) {
        row.style.display = "";
        var btn = row.querySelector("button.ntbl-toggle");
        if (btn && btn.getAttribute("aria-expanded") === "true") {
          showChildren(row.id);
        }
      });
  }

  /* Delegated click handler — attached once on document so it works for
     widgets injected dynamically (Shiny renderNestable, htmx, etc.). */
  function handleClick(event) {
    var btn = event.target.closest("button.ntbl-toggle");
    if (!btn) return;

    var targetId = btn.getAttribute("data-target");
    var expanded = btn.getAttribute("aria-expanded") === "true";

    if (expanded) {
      hideDescendants(targetId);
    } else {
      showChildren(targetId);
    }

    btn.setAttribute("aria-expanded", expanded ? "false" : "true");
  }

  /* Guard against double-registration (e.g. Shiny hot-reload). */
  if (!document._ntblListenerAttached) {
    document.addEventListener("click", handleClick);
    document._ntblListenerAttached = true;
  }

}());
