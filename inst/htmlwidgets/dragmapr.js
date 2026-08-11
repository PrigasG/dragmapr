HTMLWidgets.widget({
  name: "dragmapr",
  type: "output",

  factory: function(el, width, height) {
    let state = null;

    function sendInput(name, value) {
      if (!window.Shiny || !HTMLWidgets.shinyMode) return;
      const id = el.id + "_" + name;
      window.Shiny.setInputValue(id, value, {priority: "event"});
    }

    function cleanOffset(row) {
      const dx = Number(row && row.dx_m);
      const dy = Number(row && row.dy_m);
      return {
        dx_m: Number.isFinite(dx) ? dx : 0,
        dy_m: Number.isFinite(dy) ? dy : 0
      };
    }

    function naturalCompare(a, b) {
      return String(a).localeCompare(String(b), undefined, {numeric: true, sensitivity: "base"});
    }

    function walkCoords(coords, fn) {
      if (!coords) return;
      if (typeof coords[0] === "number") {
        fn(coords);
        return;
      }
      coords.forEach(child => walkCoords(child, fn));
    }

    function coordBounds(collection) {
      let xmin = Infinity, ymin = Infinity, xmax = -Infinity, ymax = -Infinity;
      (collection.features || []).forEach(feature => {
        if (!feature.geometry) return;
        walkCoords(feature.geometry.coordinates, ([x, y]) => {
          xmin = Math.min(xmin, x);
          xmax = Math.max(xmax, x);
          ymin = Math.min(ymin, y);
          ymax = Math.max(ymax, y);
        });
      });
      if (!Number.isFinite(xmin)) return {xmin: 0, ymin: 0, xmax: 1, ymax: 1};
      if (xmin === xmax) xmax = xmin + 1;
      if (ymin === ymax) ymax = ymin + 1;
      return {xmin, ymin, xmax, ymax};
    }

    function backgroundClass(value) {
      const bg = String(value || "white").toLowerCase().replace("_", "-");
      return ["white", "transparent", "light-grid", "dark"].includes(bg) ? "bg-" + bg : "bg-white";
    }

    function colorFor(region) {
      const palette = ["#2166ac", "#d73027", "#1a9850", "#984ea3", "#ff7f00", "#a65628", "#f781bf", "#999999"];
      const style = styleFor(region);
      if (style && style.fill) return style.fill;
      const custom = state.display.regionPalette || {};
      if (custom[region]) return custom[region];
      return palette[state.regions.indexOf(region) % palette.length];
    }

    function styleFor(region) {
      return state.styles.get(String(region)) || null;
    }

    function styleRows() {
      return Array.from(state.styles.entries()).map(([region, style]) =>
        Object.assign({region: String(region)}, style)
      );
    }

    function compactDrag(eventType, item) {
      return Object.assign({
        widget_id: el.id,
        generation: state.generation,
        revision: state.revision,
        event: eventType
      }, item || {});
    }

    function emitLiveDrag(item) {
      const mode = state.interaction.stateEmit || "end";
      if (mode === "end") return;
      const now = window.performance ? window.performance.now() : Date.now();
      if (mode === "throttled" && now - state.lastDragEmit < state.interaction.throttleMs) return;
      state.lastDragEmit = now;
      sendInput("drag", compactDrag("drag", item));
    }

    function regionRows() {
      return state.regions.map(region => {
        const offset = state.regionOffsets.get(String(region)) || {dx_m: 0, dy_m: 0};
        return {
          region: String(region),
          dx_m: Math.round(offset.dx_m),
          dy_m: Math.round(offset.dy_m)
        };
      });
    }

    function labelRows() {
      return state.labels.map(label => {
        const offset = state.labelOffsets.get(String(label.label_id)) || {dx_m: 0, dy_m: 0};
        return {
          label_id: String(label.label_id),
          region: String(label.region),
          dx_m: Math.round(offset.dx_m),
          dy_m: Math.round(offset.dy_m)
        };
      });
    }

    function snapshot(eventType, extra) {
      state.revision += 1;
      const payload = Object.assign({
        widget_id: el.id,
        generation: state.generation,
        revision: state.revision,
        schema_version: state.schemaVersion,
        package_version: state.packageVersion,
        event: eventType || "state",
        level: state.stateLevel,
        region_col: state.regionCol,
        label_id_col: state.labelIdCol,
        binding: {
          region_col: state.regionCol,
          label_id_col: state.labelIdCol
        },
        crs: state.crs,
        geometry_id: state.geometryId,
        selected_feature: state.selectedFeature || "",
        region_offsets: regionRows(),
        label_offsets: labelRows(),
        expanded_groups: state.expandedGroups.slice(),
        styles: styleRows(),
        view: {
          scale: state.metricScale,
          width: state.geometry.width,
          height: state.geometry.height
        }
      }, extra || {});
      sendInput("state", payload);
      return payload;
    }

    function syncBackground() {
      state.root.classList.remove("bg-white", "bg-transparent", "bg-light-grid", "bg-dark");
      state.root.classList.add(backgroundClass(state.display.mapBackground));
    }

    function fit() {
      const bounds = state.bounds;
      const viewW = Math.max(300, state.scroll.clientWidth || width || 900);
      const viewH = Math.max(240, state.scroll.clientHeight || height || 650);
      const pad = 28;
      const sx = (viewW - pad * 2) / (bounds.xmax - bounds.xmin);
      const sy = (viewH - pad * 2) / (bounds.ymax - bounds.ymin);
      state.metricScale = Math.max(0.000001, Math.min(sx, sy));
      const cx = state.geometry.width / 2;
      const cy = state.geometry.height / 2;
      const tx = cx - (bounds.xmax + bounds.xmin) / 2 * state.metricScale;
      const ty = cy + (bounds.ymax + bounds.ymin) / 2 * state.metricScale;
      state.projection = d3.geoIdentity()
        .reflectY(true)
        .scale(state.metricScale)
        .translate([tx, ty]);
      state.path = d3.geoPath(state.projection);
    }

    function regionTransform(region) {
      const offset = state.regionOffsets.get(String(region)) || {dx_m: 0, dy_m: 0};
      return "translate(" + (offset.dx_m * state.metricScale) + "," + (-offset.dy_m * state.metricScale) + ")";
    }

    function labelPosition(label) {
      const p = state.projection([Number(label.x), Number(label.y)]);
      const ro = state.regionOffsets.get(String(label.region)) || {dx_m: 0, dy_m: 0};
      const lo = state.labelOffsets.get(String(label.label_id)) || {dx_m: 0, dy_m: 0};
      return [
        p[0] + (ro.dx_m + lo.dx_m) * state.metricScale,
        p[1] - (ro.dy_m + lo.dy_m) * state.metricScale
      ];
    }

    function labelTransform(label) {
      const p = labelPosition(label);
      return "translate(" + p[0] + "," + p[1] + ")";
    }

    function connectorPath(label) {
      const start = state.projection([Number(label.x), Number(label.y)]);
      const ro = state.regionOffsets.get(String(label.region)) || {dx_m: 0, dy_m: 0};
      const end = labelPosition(label);
      return "M" + (start[0] + ro.dx_m * state.metricScale) + "," +
        (start[1] - ro.dy_m * state.metricScale) + " L" + end[0] + "," + end[1];
    }

    function syncOriginOutlines() {
      const rows = state.display.showOriginOutlines ? state.regions : [];
      const outlines = state.originLayer.selectAll("g.dragmapr-origin-outline")
        .data(rows, d => String(d));
      outlines.exit().remove();
      const enter = outlines.enter().append("g").attr("class", "dragmapr-origin-outline");
      enter.selectAll("path")
        .data(region => state.grouped.get(String(region)) || [])
        .enter()
        .append("path")
        .attr("d", state.path);
      enter.merge(outlines)
        .style("display", region => {
          const o = state.regionOffsets.get(String(region)) || {dx_m: 0, dy_m: 0};
          return o.dx_m === 0 && o.dy_m === 0 ? "none" : null;
        });
    }

    function syncMovementConnectors() {
      const rows = state.display.showMovementConnectors ? state.regions : [];
      const lines = state.connectorLayer.selectAll("line.dragmapr-movement-connector")
        .data(rows, d => String(d));
      lines.exit().remove();
      lines.enter()
        .append("line")
        .attr("class", "dragmapr-movement-connector")
        .merge(lines)
        .attr("x1", region => state.centroids.get(String(region))[0])
        .attr("y1", region => state.centroids.get(String(region))[1])
        .attr("x2", region => {
          const p = state.centroids.get(String(region));
          const o = state.regionOffsets.get(String(region)) || {dx_m: 0, dy_m: 0};
          return p[0] + o.dx_m * state.metricScale;
        })
        .attr("y2", region => {
          const p = state.centroids.get(String(region));
          const o = state.regionOffsets.get(String(region)) || {dx_m: 0, dy_m: 0};
          return p[1] - o.dy_m * state.metricScale;
        })
        .style("display", region => {
          const o = state.regionOffsets.get(String(region)) || {dx_m: 0, dy_m: 0};
          return o.dx_m === 0 && o.dy_m === 0 ? "none" : null;
        });
    }

    function updateLayout(eventType) {
      state.regionLayer.selectAll("g.dragmapr-region")
        .attr("transform", regionTransform);
      state.labelLayer.selectAll("g.dragmapr-label")
        .attr("transform", labelTransform);
      state.labelConnectorLayer.selectAll("path.dragmapr-label-connector")
        .attr("d", connectorPath);
      syncOriginOutlines();
      syncMovementConnectors();
      if (eventType) snapshot(eventType);
    }

    function rebuildGeometryIndexes() {
      state.grouped = d3.group(state.geojson.features || [], f => String(f.properties.drag_region));
      state.regions = Array.from(state.grouped.keys()).sort(naturalCompare);
      state.bounds = coordBounds(state.geojson || {features: []});
      fit();
      const nextOffsets = new Map(state.regions.map(region => [
        String(region),
        state.regionOffsets.get(String(region)) || {dx_m: 0, dy_m: 0}
      ]));
      state.regionOffsets = nextOffsets;
      state.centroids = new Map();
      state.regions.forEach(region => {
        const fc = {type: "FeatureCollection", features: state.grouped.get(String(region)) || []};
        state.centroids.set(String(region), state.path.centroid(fc));
      });
    }

    function removeFeatures(featureIds, eventType) {
      const ids = Array.from(new Set((featureIds || []).map(String).filter(Boolean)));
      if (ids.length === 0) return [];
      const removeSet = new Set(ids);
      const existing = new Set(state.regions.map(String));
      const removed = ids.filter(id => existing.has(id));
      if (removed.length === 0) return [];
      state.geojson.features = (state.geojson.features || [])
        .filter(f => !removeSet.has(String(f.properties.drag_region)));
      state.labels = (state.labels || []).filter(label => !removeSet.has(String(label.region)));
      removed.forEach(id => {
        state.regionOffsets.delete(id);
        state.styles.delete(id);
        if (state.selectedFeature === id) state.selectedFeature = "";
      });
      for (const [labelId, offset] of Array.from(state.labelOffsets.entries())) {
        const label = (state.labels || []).find(d => String(d.label_id) === String(labelId));
        if (!label) state.labelOffsets.delete(labelId);
      }
      rebuildGeometryIndexes();
      renderRegions();
      renderLabels();
      syncSelection();
      applyDisplay(state.display);
      updateLayout();
      const payload = snapshot(eventType || "featuredelete", {removed_features: removed});
      sendInput("feature_delete", payload);
      return removed;
    }

    function renderRegions() {
      const groups = state.regionLayer.selectAll("g.dragmapr-region")
        .data(state.regions, d => String(d));
      groups.exit().remove();
      const enter = groups.enter()
        .append("g")
        .attr("class", "dragmapr-region");

      enter.each(function(region) {
        d3.select(this).selectAll("path")
          .data(state.grouped.get(String(region)) || [])
          .enter()
          .append("path")
          .attr("d", state.path);
      });

      const merged = enter.merge(groups)
        .attr("transform", regionTransform)
        .style("fill", colorFor);

      merged
        .classed("is-selected", region => String(region) === (state.selectedFeature || ""))
        .classed("is-highlighted", region => !!(styleFor(region) && styleFor(region).highlight));

      merged.selectAll("path")
        .attr("d", state.path)
        .attr("fill", function() { return colorFor(d3.select(this.parentNode).datum()); })
        .style("stroke", function() {
          const style = styleFor(d3.select(this.parentNode).datum());
          return style && style.stroke ? style.stroke : null;
        })
        .style("stroke-width", function() {
          const style = styleFor(d3.select(this.parentNode).datum());
          return style && style.stroke_width != null && Number.isFinite(Number(style.stroke_width)) ? Number(style.stroke_width) : null;
        })
        .style("opacity", function() {
          const style = styleFor(d3.select(this.parentNode).datum());
          return style && style.opacity != null && Number.isFinite(Number(style.opacity)) ? Number(style.opacity) : null;
        });

      if (state.interaction.draggableRegions) {
        const drag = d3.drag()
          .on("start", function(event, region) {
            state.dragMoved = 0;
            state.lastDragEmit = 0;
            d3.select(this).classed("is-active", true);
            sendInput("drag_start", compactDrag("dragstart", {region: String(region)}));
          })
          .on("drag", function(event, region) {
            const offset = state.regionOffsets.get(String(region));
            state.dragMoved += Math.hypot(event.dx, event.dy);
            offset.dx_m += event.dx / state.metricScale;
            offset.dy_m -= event.dy / state.metricScale;
            if (state.interaction.liveDrag) updateLayout();
            emitLiveDrag({
              region: String(region),
              dx_m: offset.dx_m,
              dy_m: offset.dy_m
            });
          })
          .on("end", function(event, region) {
            d3.select(this).classed("is-active", false);
            if (state.dragMoved < 5) {
              setSelection(region);
              sendInput("region_click", snapshot("region_click", {region: String(region)}));
            } else {
              if (!state.interaction.liveDrag) updateLayout();
              sendInput("drag_end", snapshot("dragend", {region: String(region)}));
            }
          });
        merged.call(drag);
      }
    }

    function syncSelection() {
      const selected = state.selectedFeature || "";
      state.regionLayer.selectAll("g.dragmapr-region")
        .classed("is-selected", region => String(region) === selected);
    }

    function setSelection(region) {
      const next = region == null ? "" : String(region);
      if (next === state.selectedFeature) return false;
      state.selectedFeature = next;
      syncSelection();
      return true;
    }

    function renderLabels() {
      const visibleLabels = (state.labels || []).filter(label => {
        const style = styleFor(label.region);
        return !style || style.label_visible !== false;
      });
      const connectors = state.labelConnectorLayer.selectAll("path.dragmapr-label-connector")
        .data(visibleLabels.filter(d => d.connector !== false), d => String(d.label_id));
      connectors.exit().remove();
      connectors.enter()
        .append("path")
        .attr("class", "dragmapr-label-connector")
        .merge(connectors)
        .attr("d", connectorPath);

      const labels = state.labelLayer.selectAll("g.dragmapr-label")
        .data(visibleLabels, d => String(d.label_id));
      labels.exit().remove();
      const enter = labels.enter()
        .append("g")
        .attr("class", "dragmapr-label");
      enter.append("rect")
        .attr("x", -32)
        .attr("y", -15)
        .attr("width", 64)
        .attr("height", 30)
        .attr("rx", 5);
      enter.append("text")
        .attr("text-anchor", "middle")
        .attr("dominant-baseline", "central");

      const merged = enter.merge(labels)
        .attr("transform", labelTransform);
      merged.select("text").text(d => d.label == null ? d.label_id : d.label);

      if (state.interaction.draggableLabels) {
        const drag = d3.drag()
          .on("drag", function(event, label) {
            const offset = state.labelOffsets.get(String(label.label_id));
            offset.dx_m += event.dx / state.metricScale;
            offset.dy_m -= event.dy / state.metricScale;
            if (state.interaction.liveDrag) updateLayout();
            emitLiveDrag({
              label_id: String(label.label_id),
              dx_m: offset.dx_m,
              dy_m: offset.dy_m
            });
          })
          .on("end", function(event, label) {
            if (!state.interaction.liveDrag) updateLayout();
            sendInput("drag_end", snapshot("labeldragend", {label_id: String(label.label_id)}));
          });
        merged.call(drag);
      }
    }

    function applyDisplay(display) {
      Object.assign(state.display, display || {});
      syncBackground();
      state.regionLayer.selectAll("g.dragmapr-region").style("fill", colorFor);
      state.regionLayer.selectAll("g.dragmapr-region").selectAll("path")
        .attr("fill", function() { return colorFor(d3.select(this.parentNode).datum()); })
        .style("stroke", function() {
          const style = styleFor(d3.select(this.parentNode).datum());
          return style && style.stroke ? style.stroke : null;
        })
        .style("stroke-width", function() {
          const style = styleFor(d3.select(this.parentNode).datum());
          return style && style.stroke_width != null && Number.isFinite(Number(style.stroke_width)) ? Number(style.stroke_width) : null;
        })
        .style("opacity", function() {
          const style = styleFor(d3.select(this.parentNode).datum());
          return style && style.opacity != null && Number.isFinite(Number(style.opacity)) ? Number(style.opacity) : null;
        });
      state.labelConnectorLayer.selectAll("path.dragmapr-label-connector")
        .style("stroke", state.display.connectorColor)
        .style("stroke-width", state.display.connectorLinewidth);
      syncOriginOutlines();
      syncMovementConnectors();
    }

    function installDom() {
      el.innerHTML = "";
      const root = document.createElement("div");
      root.className = "dragmapr-widget";
      const scroll = document.createElement("div");
      scroll.className = "dragmapr-scroll";
      const svgNode = document.createElementNS("http://www.w3.org/2000/svg", "svg");
      scroll.appendChild(svgNode);
      root.appendChild(scroll);
      el.appendChild(root);
      state.root = root;
      state.scroll = scroll;
      state.svg = d3.select(svgNode)
        .attr("width", state.geometry.width)
        .attr("height", state.geometry.height)
        .attr("viewBox", "0 0 " + state.geometry.width + " " + state.geometry.height);
      state.originLayer = state.svg.append("g").attr("class", "dragmapr-origin-layer");
      state.connectorLayer = state.svg.append("g").attr("class", "dragmapr-movement-layer");
      state.regionLayer = state.svg.append("g").attr("class", "dragmapr-region-layer");
      state.labelConnectorLayer = state.svg.append("g").attr("class", "dragmapr-label-connector-layer");
      state.labelLayer = state.svg.append("g").attr("class", "dragmapr-label-layer");
    }

    function buildState(x) {
      const features = (x.geojson && x.geojson.features) || [];
      const grouped = d3.group(features, f => String(f.properties.drag_region));
      const regions = Array.from(grouped.keys()).sort(naturalCompare);
      const labels = x.labels || [];
      const statePayload = x.state || {};
      const regionOffsets = new Map(regions.map(region => [String(region), {dx_m: 0, dy_m: 0}]));
      (statePayload.region_offsets || []).forEach(row => {
        const region = String(row.region);
        if (regionOffsets.has(region)) regionOffsets.set(region, cleanOffset(row));
      });
      const labelOffsets = new Map(labels.map(label => [String(label.label_id), {dx_m: 0, dy_m: 0}]));
      (statePayload.label_offsets || []).forEach(row => {
        const id = String(row.label_id);
        if (labelOffsets.has(id)) labelOffsets.set(id, cleanOffset(row));
      });

      const selected = (x.selectedFeature != null ? x.selectedFeature : statePayload.selected_feature) || "";
      const styles = new Map((statePayload.styles || []).map(row => {
        const copy = Object.assign({}, row);
        delete copy.region;
        return [String(row.region), copy];
      }));

      state = {
        generation: x.generation,
        revision: Number(x.revision) || 0,
        schemaVersion: statePayload.schema_version || "1.2.0",
        packageVersion: statePayload.package_version || "0.0.0",
        stateLevel: statePayload.level || "region",
        regionCol: statePayload.region_col || (statePayload.binding && statePayload.binding.region_col) || x.regionCol || statePayload.level || "region",
        labelIdCol: statePayload.label_id_col || (statePayload.binding && statePayload.binding.label_id_col) || x.labelIdCol || "label_id",
        crs: (x.crs != null ? x.crs : statePayload.crs) || null,
        geometryId: (x.geometryId != null ? x.geometryId : statePayload.geometry_id) || null,
        selectedFeature: String(selected),
        expandedGroups: (statePayload.expanded_groups || []).map(String),
        styles: styles,
        geojson: x.geojson || {type: "FeatureCollection", features: []},
        labels: labels,
        grouped: grouped,
        regions: regions,
        bounds: coordBounds(x.geojson || {features: []}),
        regionOffsets: regionOffsets,
        labelOffsets: labelOffsets,
        geometry: Object.assign({width: 7200, height: 4800}, x.geometry || {}),
        display: Object.assign({
          mapBackground: "white",
          connectorColor: "#334155",
          connectorLinewidth: 1.3,
          showOriginOutlines: false,
          showMovementConnectors: false,
          showDragTrail: false,
          regionPalette: null
        }, x.display || {}),
        interaction: Object.assign({
          draggableRegions: true,
          draggableLabels: true,
          stateEmit: "end",
          liveDrag: true,
          throttleMs: 75
        }, x.interaction || {}),
        centroids: new Map(),
        dragMoved: 0,
        lastDragEmit: 0
      };
    }

    function render(x) {
      buildState(x);
      installDom();
      fit();
      state.regions.forEach(region => {
        const fc = {type: "FeatureCollection", features: state.grouped.get(String(region)) || []};
        state.centroids.set(String(region), state.path.centroid(fc));
      });
      syncBackground();
      renderRegions();
      renderLabels();
      syncSelection();
      applyDisplay(state.display);
      requestAnimationFrame(() => {
        state.scroll.scrollLeft = state.geometry.width / 2 - state.scroll.clientWidth / 2;
        state.scroll.scrollTop = state.geometry.height / 2 - state.scroll.clientHeight / 2;
      });
      sendInput("ready", {
        widget_id: el.id,
        generation: state.generation,
        revision: state.revision,
        schema_version: state.schemaVersion,
        package_version: state.packageVersion,
        event: "ready",
        ready: true
      });
      sendInput("ack", {
        widget_id: el.id,
        generation: state.generation,
        revision: state.revision,
        event: "ready",
        accepted: true
      });
      snapshot("ready");
    }

    return {
      renderValue: render,
      resize: function() {
        if (!state) return;
        fit();
        renderRegions();
        renderLabels();
        updateLayout();
        sendInput("state", snapshot("viewchange"));
      },
      update: function(message) {
        if (!state) return;
        if (message && message.generation != null &&
            String(message.generation) !== String(state.generation)) {
          sendInput("ack", {
            widget_id: el.id,
            generation: state.generation,
            revision: state.revision,
            server_revision: message && message.serverRevision != null ? message.serverRevision : null,
            event: "update",
            accepted: false,
            reason: "stale-generation"
          });
          return;
        }
        applyDisplay((message && message.display) || {});
        let compositionChanged = false;
        if (message && Object.prototype.hasOwnProperty.call(message, "selectedFeature")) {
          if (setSelection(message.selectedFeature)) {
            sendInput("state", snapshot("selection", {selected_feature: state.selectedFeature || ""}));
          }
        }
        if (message && Array.isArray(message.expandedGroups)) {
          state.expandedGroups = Array.from(new Set(message.expandedGroups.map(String).filter(Boolean)));
          compositionChanged = true;
        }
        if (message && Array.isArray(message.styles)) {
          state.styles = new Map(message.styles.map(row => {
            const copy = Object.assign({}, row);
            delete copy.region;
            return [String(row.region), copy];
          }));
          renderRegions();
          renderLabels();
          applyDisplay(state.display);
          compositionChanged = true;
        }
        if (message && Array.isArray(message.removeFeatures)) {
          removeFeatures(message.removeFeatures, "featuredelete");
        }
        if (message && message.deleteSelected === true && state.selectedFeature) {
          removeFeatures([state.selectedFeature], "featuredelete");
        }
        if (compositionChanged) snapshot("composition");
        sendInput("ack", {
          widget_id: el.id,
          generation: state.generation,
          revision: state.revision,
          server_revision: message && message.serverRevision != null ? message.serverRevision : null,
          event: "update",
          accepted: true
        });
      }
    };
  }
});

if (HTMLWidgets.shinyMode && window.Shiny) {
  window.Shiny.addCustomMessageHandler("dragmapr-update", function(message) {
    const el = document.getElementById(message.id);
    if (!el || !window.HTMLWidgets || !window.HTMLWidgets.getInstance) return;
    const instance = window.HTMLWidgets.getInstance(el);
    if (instance.update) instance.update(message);
  });
}
