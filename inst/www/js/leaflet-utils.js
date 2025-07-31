window.leafletMaps = {};

function registerLeafletMap(leafletId, map) {
  window.leafletMaps[leafletId] = map;
}

function observeChart(containerId, leafletId, containerSizeId) {
  const container = document.getElementById(containerId);
  if (!container) {
    return;
  }
  const panelStyle = getComputedStyle(container);
  const resizeObserver = new ResizeObserver(entries => {
    for (let entry of entries) {
      const w = entry.contentRect.width;
      const h = entry.contentRect.height;
      if (w > 50 && h > 50) {
        const map = window.leafletMaps[leafletId];
        if (map) {
          map.invalidateSize();
          if (typeof Shiny != "undefined" && typeof Shiny.onInputChange !== 'undefined') {
            Shiny.onInputChange(containerSizeId, { width: w, height: h});
          }
        }
      }
    }
  });
  resizeObserver.observe(container);
}

Shiny.addCustomMessageHandler('updateLeafletContainerSize',
  function(data) {
    const container = document.getElementById(data.containerid);
    if (container) {
      container.style.width = data.width + 'px';
      container.style.height = data.height + 'px';
    }
    const map = window.leafletMaps[data.mapid];
    if (map) {
      map.invalidateSize();
    }
  }
);
