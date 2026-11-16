// Help: the project's help text (help.htm, copied into data/ at build time).

import { el } from "../ui.js";

export function create() {
  const body = el("div.prose", { style: { flex: "1 1 auto" } }, el("p", { text: "Loading…" }));
  const page = el("div.page", {}, body);
  return {
    id: "help", title: "Help", el: page,
    async init() {
      try {
        const res = await fetch("data/help.htm");
        if (!res.ok) throw new Error(res.status);
        // help.htm is project-authored static content shipped with the site.
        const doc = new DOMParser().parseFromString(await res.text(), "text/html");
        doc.querySelectorAll("script").forEach((s) => s.remove());
        body.replaceChildren(...doc.body.childNodes);
      } catch {
        body.replaceChildren(el("p", { text: "The help text could not be loaded." }));
      }
    },
  };
}
