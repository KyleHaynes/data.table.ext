/* duckdt ERD explorer: pick tables and columns, get a diagram and the code
   that selects them. The whole model is embedded in the page, so this file
   never talks to R (or anything else) once it has been written. */
(function () {
  "use strict";

  var model = JSON.parse(document.getElementById("duckdt-model").textContent);
  var tables = model.tables || [];
  var columns = model.columns || [];
  var references = model.references || [];
  var hasMermaid = typeof mermaid !== "undefined";

  var byTable = {};
  tables.forEach(function (t) { byTable[t.table] = t; });

  var colsOf = {};
  tables.forEach(function (t) { colsOf[t.table] = []; });
  columns.forEach(function (c) {
    if (colsOf[c.table]) colsOf[c.table].push(c);
  });

  // References collapsed to one entry per relationship (compound keys give
  // several rows sharing a ref_id).
  var links = [];
  var byId = {};
  references.forEach(function (r) {
    if (!byId[r.ref_id]) {
      byId[r.ref_id] = { from: r.table, to: r.ref, pairs: [] };
      links.push(byId[r.ref_id]);
    }
    byId[r.ref_id].pairs.push({ column: r.column, ref_col: r.ref_col });
  });

  var state = {
    selected: new Set(),
    cols: {},
    view: "all",
    search: "",
    onlySelected: false,
    zoom: 1,
    tab: "r"
  };

  tables.forEach(function (t) {
    state.cols[t.table] = new Set(colsOf[t.table].map(function (c) { return c.column; }));
  });

  var initial = model.selected && model.selected.length
    ? model.selected
    : (tables.length <= 12 ? tables.map(function (t) { return t.table; }) : []);
  initial.forEach(function (t) { if (byTable[t]) state.selected.add(t); });

  // ---- helpers -------------------------------------------------------------

  var el = function (id) { return document.getElementById(id); };

  function ident(x) { return String(x == null ? "unknown" : x).replace(/[^A-Za-z0-9_]+/g, "_"); }

  function quote(x) { return '"' + String(x).replace(/"/g, '""') + '"'; }

  function qualified(t) {
    var tab = byTable[t];
    return tab.schema ? quote(tab.schema) + "." + quote(tab.name) : quote(tab.name);
  }

  function selectedTables() {
    return tables.filter(function (t) { return state.selected.has(t.table); });
  }

  function visibleColumns(table) {
    var chosen = state.cols[table];
    return colsOf[table].filter(function (c) {
      if (!chosen.has(c.column)) return false;
      if (state.view === "keys_only") return c.key > 0 || c.ref;
      return true;
    });
  }

  function fmtRows(n) {
    if (n == null) return null;
    return String(Math.round(n)).replace(/\B(?=(\d{3})+(?!\d))/g, ",");
  }

  // ---- sidebar -------------------------------------------------------------

  function buildSidebar() {
    var list = el("table-list");
    tables.forEach(function (t) {
      var item = document.createElement("div");
      item.className = "table-item";
      item.dataset.table = t.table;

      var head = document.createElement("div");
      head.className = "table-head";

      var caret = document.createElement("span");
      caret.className = "caret";
      caret.textContent = "▶";

      var check = document.createElement("input");
      check.type = "checkbox";
      check.checked = state.selected.has(t.table);
      check.addEventListener("click", function (ev) { ev.stopPropagation(); });
      check.addEventListener("change", function () {
        if (check.checked) state.selected.add(t.table); else state.selected.delete(t.table);
        refresh();
      });

      var name = document.createElement("span");
      name.className = "table-name";
      if (t.schema && t.table.indexOf(".") >= 0) {
        name.innerHTML = '<span class="schema"></span>';
        name.firstChild.textContent = t.schema + ".";
        name.appendChild(document.createTextNode(t.name));
      } else {
        name.textContent = t.name;
      }

      var badge = document.createElement("span");
      badge.className = "badge" + (t.type === "VIEW" ? " view" : "");
      var rows = fmtRows(t.n_rows);
      badge.textContent = t.type === "VIEW"
        ? "view"
        : (rows === null ? colsOf[t.table].length + " cols" : rows + " rows");

      head.appendChild(caret);
      head.appendChild(check);
      head.appendChild(name);
      head.appendChild(badge);
      head.addEventListener("click", function () { item.classList.toggle("open"); });

      var cols = document.createElement("div");
      cols.className = "column-list";

      var tools = document.createElement("div");
      tools.className = "column-tools";
      [["All", true], ["None", false]].forEach(function (pair) {
        var b = document.createElement("button");
        b.className = "link-btn";
        b.textContent = pair[0];
        b.addEventListener("click", function () {
          state.cols[t.table] = new Set(
            pair[1] ? colsOf[t.table].map(function (c) { return c.column; }) : []
          );
          item.querySelectorAll(".column-row input").forEach(function (cb) {
            cb.checked = pair[1];
          });
          if (pair[1]) state.selected.add(t.table);
          check.checked = state.selected.has(t.table);
          refresh();
        });
        tools.appendChild(b);
      });
      cols.appendChild(tools);

      colsOf[t.table].forEach(function (c) {
        var row = document.createElement("div");
        row.className = "column-row";
        row.dataset.column = c.column.toLowerCase();

        var cb = document.createElement("input");
        cb.type = "checkbox";
        cb.checked = true;
        cb.addEventListener("change", function () {
          if (cb.checked) state.cols[t.table].add(c.column);
          else state.cols[t.table].delete(c.column);
          if (cb.checked && !state.selected.has(t.table)) {
            state.selected.add(t.table);
            check.checked = true;
          }
          refresh();
        });

        var cname = document.createElement("span");
        cname.className = "cname";
        cname.textContent = c.column;

        row.appendChild(cb);
        row.appendChild(cname);
        if (c.key > 0) {
          var pk = document.createElement("span");
          pk.className = "tag pk";
          pk.textContent = "PK";
          row.appendChild(pk);
        }
        if (c.ref) {
          var fk = document.createElement("span");
          fk.className = "tag fk";
          fk.textContent = "FK";
          fk.title = "references " + c.ref + "." + c.ref_col;
          row.appendChild(fk);
        }
        var type = document.createElement("span");
        type.className = "ctype";
        type.textContent = c.type || "";
        row.appendChild(type);

        cols.appendChild(row);
      });

      item.appendChild(head);
      item.appendChild(cols);
      list.appendChild(item);
    });
  }

  function applyFilter() {
    var q = state.search.trim().toLowerCase();
    document.querySelectorAll(".table-item").forEach(function (item) {
      var t = item.dataset.table;
      var selected = state.selected.has(t);
      var matchTable = !q || t.toLowerCase().indexOf(q) >= 0;
      var matchCols = 0;
      item.querySelectorAll(".column-row").forEach(function (row) {
        var hit = !q || matchTable || row.dataset.column.indexOf(q) >= 0;
        row.classList.toggle("hidden", !hit);
        if (hit && q && !matchTable) matchCols++;
      });
      var show = (matchTable || matchCols > 0) && (!state.onlySelected || selected);
      item.classList.toggle("hidden", !show);
      if (q && matchCols > 0 && !matchTable) item.classList.add("open");
    });
  }

  function syncChecks() {
    document.querySelectorAll(".table-item").forEach(function (item) {
      var box = item.querySelector(".table-head input");
      box.checked = state.selected.has(item.dataset.table);
    });
  }

  // ---- diagram -------------------------------------------------------------

  var renderCount = 0;

  function mermaidSource() {
    var sel = selectedTables();
    var lines = ["erDiagram"];
    sel.forEach(function (t) {
      lines.push("    " + t.entity + " {");
      if (state.view !== "title_only") {
        visibleColumns(t.table).forEach(function (c) {
          var tags = [];
          if (c.key > 0) tags.push("PK");
          if (c.ref) tags.push("FK");
          lines.push("        " + ident(c.type) + " " + ident(c.column) +
            (tags.length ? " " + tags.join(",") : ""));
        });
      }
      lines.push("    }");
    });
    links.forEach(function (link) {
      if (!state.selected.has(link.from) || !state.selected.has(link.to)) return;
      var label = link.pairs.map(function (p) { return p.column; }).join(", ");
      lines.push('    ' + byTable[link.to].entity + " ||--o{ " + byTable[link.from].entity +
        ' : "' + label + '"');
    });
    return lines.join("\n");
  }

  function drawDiagram() {
    var target = el("diagram");
    var empty = el("empty-hint");
    var sel = selectedTables();
    empty.classList.toggle("hidden", sel.length > 0);
    if (!sel.length) {
      target.innerHTML = "";
      return;
    }
    var src = mermaidSource();
    if (!hasMermaid) {
      target.innerHTML = "";
      var pre = document.createElement("pre");
      pre.className = "diagram-error";
      pre.textContent =
        "Mermaid could not be loaded (no internet connection?).\n" +
        "The diagram source is below -- everything else on this page still works.\n\n" + src;
      target.appendChild(pre);
      return;
    }
    renderCount++;
    mermaid.render("duckdt-erd-" + renderCount, src).then(function (res) {
      target.innerHTML = res.svg;
    }).catch(function (err) {
      target.innerHTML = "";
      var pre = document.createElement("pre");
      pre.className = "diagram-error";
      pre.textContent = "Mermaid could not draw this diagram:\n" + err + "\n\n" + src;
      target.appendChild(pre);
    });
  }

  function applyZoom() {
    el("diagram").style.transform = "scale(" + state.zoom + ")";
    el("zoom-reset").textContent = Math.round(state.zoom * 100) + "%";
  }

  // ---- generated code ------------------------------------------------------

  function aliases(sel) {
    var used = {};
    var out = {};
    sel.forEach(function (t) {
      var base = t.name.replace(/[^A-Za-z0-9]/g, "").toLowerCase().charAt(0) || "t";
      var alias = base;
      var n = 1;
      while (used[alias]) { n++; alias = base + n; }
      used[alias] = true;
      out[t.table] = alias;
    });
    return out;
  }

  // Order the selected tables so each (after the first) joins to one already
  // in the list; anything unreachable is reported rather than cross-joined.
  function joinPlan(sel) {
    var names = sel.map(function (t) { return t.table; });
    var inSel = {};
    names.forEach(function (n) { inSel[n] = true; });
    // A reference with no known target column can be drawn but not joined on.
    var usable = links.filter(function (l) {
      return inSel[l.from] && inSel[l.to] &&
        l.pairs.every(function (p) { return p.column && p.ref_col; });
    });

    var placed = [names[0]];
    var joins = [];
    var changed = true;
    while (changed) {
      changed = false;
      usable.forEach(function (l) {
        if (l.used) return;
        var fromIn = placed.indexOf(l.from) >= 0;
        var toIn = placed.indexOf(l.to) >= 0;
        if (fromIn === toIn) return;
        var next = fromIn ? l.to : l.from;
        placed.push(next);
        joins.push({ table: next, link: l });
        l.used = true;
        changed = true;
      });
    }
    usable.forEach(function (l) { delete l.used; });

    return {
      order: placed,
      joins: joins,
      orphans: names.filter(function (n) { return placed.indexOf(n) < 0; })
    };
  }

  function buildSql(sel) {
    if (!sel.length) return "-- Tick a table on the left.";
    var alias = aliases(sel);
    var plan = joinPlan(sel);

    var counts = {};
    sel.forEach(function (t) {
      visibleColumns(t.table).forEach(function (c) {
        counts[c.column] = (counts[c.column] || 0) + 1;
      });
    });

    var select = [];
    plan.order.concat(plan.orphans).forEach(function (t) {
      visibleColumns(t).forEach(function (c) {
        var expr = alias[t] + "." + quote(c.column);
        if (counts[c.column] > 1) {
          expr += " AS " + quote(byTable[t].name + "_" + c.column);
        }
        select.push("  " + expr);
      });
    });
    if (!select.length) select.push("  *");

    var sql = "SELECT\n" + select.join(",\n") + "\nFROM " +
      qualified(plan.order[0]) + " AS " + alias[plan.order[0]];

    plan.joins.forEach(function (j) {
      var l = j.link;
      var on = l.pairs.map(function (p) {
        return alias[l.from] + "." + quote(p.column) + " = " +
          alias[l.to] + "." + quote(p.ref_col);
      }).join("\n   AND ");
      sql += "\nLEFT JOIN " + qualified(j.table) + " AS " + alias[j.table] +
        "\n  ON " + on;
    });

    plan.orphans.forEach(function (t) {
      sql += "\n-- " + t + " has no known reference to the tables above." +
        "\n-- CROSS JOIN " + qualified(t) + " AS " + alias[t] + "  -- add a join condition";
    });

    return sql;
  }

  function buildR(sel) {
    if (!sel.length) {
      return "# Tick a table on the left to build a query.\n\nlibrary(data.table.ext)\n" +
        "con <- dbdt_connect(" + (model.dbdir ? JSON.stringify(model.dbdir) : "") + ")";
    }
    var head = "library(data.table.ext)\n\n# con <- dbdt_connect(" +
      (model.dbdir ? JSON.stringify(model.dbdir) : "") + ")\n\n";

    if (sel.length === 1) {
      var t = sel[0];
      var picked = visibleColumns(t.table).map(function (c) { return c.column; });
      var all = colsOf[t.table].length === picked.length;
      var plain = !t.schema || t.schema === "main" || t.schema === "dbo";
      if (plain) {
        var code = head + "d <- dbdt(con, " + JSON.stringify(t.name) + ")\n";
        if (all) {
          code += "d[]\n";
        } else if (picked.every(function (c) { return /^[A-Za-z.][A-Za-z0-9._]*$/.test(c); })) {
          code += "d[, .(" + picked.join(", ") + ")]\n";
        } else {
          code += "d[, .SD, .SDcols = c(" +
            picked.map(function (c) { return JSON.stringify(c); }).join(", ") + ")]\n";
        }
        code += "\n# add a filter and a group-by the data.table way:\n" +
          "# d[" + (picked[0] || "col") + " > 0, .N, by = " + (picked[0] || "col") + "]";
        return code;
      }
    }

    return head + "res <- DBI::dbGetQuery(con, '\n" + buildSql(sel) + "\n')\n" +
      "data.table::setDT(res)[]";
  }

  function updateCode() {
    var sel = selectedTables();
    var text = state.tab === "sql" ? buildSql(sel)
      : state.tab === "mermaid" ? (sel.length ? mermaidSource() : "erDiagram")
      : buildR(sel);
    el("code").textContent = text;
  }

  function copyCode() {
    var text = el("code").textContent;
    var done = function () {
      var status = el("copy-status");
      status.textContent = "Copied";
      setTimeout(function () { status.textContent = ""; }, 1500);
    };
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(text).then(done, fallback);
    } else {
      fallback();
    }
    function fallback() {
      var ta = document.createElement("textarea");
      ta.value = text;
      document.body.appendChild(ta);
      ta.select();
      try { document.execCommand("copy"); done(); } catch (e) { /* nothing else to try */ }
      document.body.removeChild(ta);
    }
  }

  // ---- wiring --------------------------------------------------------------

  function updateStats() {
    var sel = selectedTables();
    var nCols = sel.reduce(function (n, t) { return n + visibleColumns(t.table).length; }, 0);
    el("stats").innerHTML = "<b>" + sel.length + "</b> of " + tables.length +
      " tables &middot; <b>" + nCols + "</b> columns &middot; <b>" + links.length +
      "</b> reference" + (links.length === 1 ? "" : "s");
  }

  var pending = null;
  function refresh() {
    updateStats();
    applyFilter();
    updateCode();
    clearTimeout(pending);
    pending = setTimeout(drawDiagram, 60);
  }

  function init() {
    buildSidebar();
    syncChecks();

    el("search").addEventListener("input", function (e) {
      state.search = e.target.value;
      applyFilter();
    });
    el("only-selected").addEventListener("change", function (e) {
      state.onlySelected = e.target.checked;
      applyFilter();
    });
    el("view-type").addEventListener("change", function (e) {
      state.view = e.target.value;
      refresh();
    });
    document.querySelectorAll(".sidebar-buttons button").forEach(function (b) {
      b.addEventListener("click", function () {
        var action = b.dataset.action;
        if (action === "select-all") {
          tables.forEach(function (t) { state.selected.add(t.table); });
        } else if (action === "select-none") {
          state.selected.clear();
        } else if (action === "expand-all") {
          document.querySelectorAll(".table-item").forEach(function (i) { i.classList.add("open"); });
        } else if (action === "collapse-all") {
          document.querySelectorAll(".table-item").forEach(function (i) { i.classList.remove("open"); });
        }
        syncChecks();
        refresh();
      });
    });
    el("code-tabs").addEventListener("click", function (e) {
      if (!e.target.classList.contains("tab")) return;
      document.querySelectorAll(".tab").forEach(function (t) { t.classList.remove("active"); });
      e.target.classList.add("active");
      state.tab = e.target.dataset.tab;
      updateCode();
    });
    el("copy-btn").addEventListener("click", copyCode);
    el("zoom-in").addEventListener("click", function () {
      state.zoom = Math.min(3, state.zoom * 1.2); applyZoom();
    });
    el("zoom-out").addEventListener("click", function () {
      state.zoom = Math.max(0.2, state.zoom / 1.2); applyZoom();
    });
    el("zoom-reset").addEventListener("click", function () { state.zoom = 1; applyZoom(); });

    if (hasMermaid) {
      var dark = window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches;
      mermaid.initialize({
        startOnLoad: false,
        theme: dark ? "dark" : "default",
        securityLevel: "loose",
        er: { useMaxWidth: false }
      });
    }
    applyZoom();
    refresh();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
