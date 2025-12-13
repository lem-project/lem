(in-package :lem-living-canvas/buffer)

;;; Canvas Buffer Class

(defclass canvas-buffer (lem:html-buffer)
  ((graph :initarg :graph
          :accessor canvas-buffer-graph
          :documentation "The call graph data")
   (source-buffer :initarg :source-buffer
                  :accessor canvas-buffer-source-buffer
                  :documentation "The source buffer being analyzed")
   (node-positions :initform (make-hash-table :test 'equal)
                   :accessor canvas-buffer-node-positions
                   :documentation "Cached node positions for persistence"))
  (:documentation "A buffer that displays a function call graph as an interactive canvas"))

;;; HTML Generation

(defun generate-canvas-html (graph-json)
  "Generate the HTML content for the canvas view"
  (format nil "<!DOCTYPE html>
<html>
<head>
  <meta charset='utf-8'>
  <meta name='viewport' content='width=device-width, initial-scale=1'>
  <script src='https://unpkg.com/cytoscape@3.28.1/dist/cytoscape.min.js'></script>
  <script src='https://unpkg.com/cytoscape-fcose@2.2.0/cytoscape-fcose.js'></script>
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body {
      background: #1e1e1e;
      overflow: hidden;
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    }
    #cy {
      width: 100vw;
      height: 100vh;
    }
    #info-panel {
      position: fixed;
      bottom: 10px;
      left: 10px;
      background: rgba(30, 30, 30, 0.95);
      border: 1px solid #3c3c3c;
      border-radius: 6px;
      padding: 12px 16px;
      color: #d4d4d4;
      font-size: 12px;
      max-width: 400px;
      display: none;
      z-index: 1000;
    }
    #info-panel.visible { display: block; }
    #info-panel h3 {
      color: #569cd6;
      margin-bottom: 8px;
      font-size: 14px;
    }
    #info-panel .type {
      color: #4ec9b0;
      font-size: 11px;
      margin-bottom: 6px;
    }
    #info-panel .docstring {
      color: #9cdcfe;
      font-style: italic;
      line-height: 1.4;
    }
    #controls {
      position: fixed;
      top: 10px;
      right: 10px;
      display: flex;
      gap: 8px;
      z-index: 1000;
    }
    .control-btn {
      background: #3c3c3c;
      border: 1px solid #5a5a5a;
      color: #d4d4d4;
      padding: 8px 12px;
      border-radius: 4px;
      cursor: pointer;
      font-size: 12px;
      transition: background 0.2s;
    }
    .control-btn:hover {
      background: #4a4a4a;
    }
    #search-box {
      position: fixed;
      top: 10px;
      left: 10px;
      z-index: 1000;
    }
    #search-input {
      background: #3c3c3c;
      border: 1px solid #5a5a5a;
      color: #d4d4d4;
      padding: 8px 12px;
      border-radius: 4px;
      font-size: 12px;
      width: 200px;
    }
    #search-input::placeholder {
      color: #808080;
    }
  </style>
</head>
<body>
  <div id='search-box'>
    <input type='text' id='search-input' placeholder='Search functions...'>
  </div>
  <div id='controls'>
    <button class='control-btn' onclick='fitView()'>Fit View</button>
    <button class='control-btn' onclick='runLayout()'>Re-layout</button>
  </div>
  <div id='cy'></div>
  <div id='info-panel'>
    <h3 id='info-name'></h3>
    <div class='type' id='info-type'></div>
    <div class='docstring' id='info-doc'></div>
  </div>

  <script>
    const graphData = ~A;

    const cy = cytoscape({
      container: document.getElementById('cy'),
      elements: graphData.elements,
      style: [
        // File container nodes (parents)
        {
          selector: 'node[type=\"file\"]',
          style: {
            'background-color': '#252526',
            'background-opacity': 0.8,
            'border-color': '#3c3c3c',
            'border-width': 2,
            'label': 'data(name)',
            'color': '#808080',
            'text-valign': 'top',
            'text-halign': 'center',
            'font-size': '12px',
            'font-weight': 'bold',
            'font-family': 'Consolas, Monaco, monospace',
            'padding': '20px',
            'shape': 'roundrectangle',
            'text-margin-y': -8
          }
        },
        // Function nodes
        {
          selector: 'node[type=\"function\"]',
          style: {
            'background-color': '#2d2d2d',
            'border-color': '#569cd6',
            'border-width': 2,
            'label': 'data(name)',
            'color': '#d4d4d4',
            'text-valign': 'center',
            'text-halign': 'center',
            'font-size': '10px',
            'font-family': 'Consolas, Monaco, monospace',
            'width': 'label',
            'height': 24,
            'padding': '8px',
            'shape': 'roundrectangle',
            'text-wrap': 'none'
          }
        },
        {
          selector: 'node[type=\"macro\"]',
          style: {
            'background-color': '#3d2d3d',
            'border-color': '#c586c0',
            'border-width': 2,
            'label': 'data(name)',
            'color': '#d4d4d4',
            'text-valign': 'center',
            'text-halign': 'center',
            'font-size': '10px',
            'font-family': 'Consolas, Monaco, monospace',
            'width': 'label',
            'height': 24,
            'padding': '8px',
            'shape': 'roundrectangle'
          }
        },
        {
          selector: 'node[type=\"generic-function\"]',
          style: {
            'background-color': '#2d3d3d',
            'border-color': '#4ec9b0',
            'border-width': 2,
            'label': 'data(name)',
            'color': '#d4d4d4',
            'text-valign': 'center',
            'text-halign': 'center',
            'font-size': '10px',
            'font-family': 'Consolas, Monaco, monospace',
            'width': 'label',
            'height': 24,
            'padding': '8px',
            'shape': 'roundrectangle'
          }
        },
        {
          selector: 'edge',
          style: {
            'width': 1,
            'line-color': '#404040',
            'target-arrow-color': '#404040',
            'target-arrow-shape': 'triangle',
            'curve-style': 'bezier',
            'arrow-scale': 0.6
          }
        },
        {
          selector: 'node:selected',
          style: {
            'border-color': '#007acc',
            'border-width': 3,
            'background-color': '#264f78'
          }
        },
        {
          selector: 'node.highlighted',
          style: {
            'border-color': '#dcdcaa',
            'border-width': 3
          }
        },
        {
          selector: 'edge.highlighted',
          style: {
            'line-color': '#569cd6',
            'target-arrow-color': '#569cd6',
            'width': 2
          }
        },
        {
          selector: '.faded',
          style: {
            'opacity': 0.25
          }
        },
        {
          selector: 'node[type=\"file\"]:selected',
          style: {
            'border-color': '#007acc',
            'background-color': '#1e3a5f'
          }
        }
      ],
      layout: {
        name: 'fcose',
        quality: 'proof',
        randomize: false,
        animate: false,
        fit: true,
        padding: 50,
        nodeDimensionsIncludeLabels: true,
        packComponents: true,
        nodeRepulsion: 8000,
        idealEdgeLength: 80,
        edgeElasticity: 0.45,
        nestingFactor: 0.1,
        gravity: 0.25,
        gravityRange: 3.8,
        gravityCompound: 1.0,
        gravityRangeCompound: 1.5,
        numIter: 2500,
        tile: true,
        tilingPaddingVertical: 20,
        tilingPaddingHorizontal: 20
      },
      minZoom: 0.1,
      maxZoom: 3,
      wheelSensitivity: 0.3
    });

    // Info panel
    const infoPanel = document.getElementById('info-panel');
    const infoName = document.getElementById('info-name');
    const infoType = document.getElementById('info-type');
    const infoDoc = document.getElementById('info-doc');

    function showInfo(node) {
      const data = node.data();
      if (data.type === 'file') {
        infoName.textContent = data.name;
        infoType.textContent = 'FILE';
        infoDoc.textContent = data.filepath || '';
      } else {
        infoName.textContent = data.name;
        infoType.textContent = data.type.toUpperCase() + ' in ' + (data.package || '');
        infoDoc.textContent = data.docstring || '(no documentation)';
      }
      infoPanel.classList.add('visible');
    }

    function hideInfo() {
      infoPanel.classList.remove('visible');
    }

    // Event handlers
    cy.on('tap', 'node', function(e) {
      const node = e.target;
      showInfo(node);

      // Highlight connected edges (only for non-file nodes)
      cy.elements().removeClass('highlighted faded');
      if (node.data('type') !== 'file') {
        const connected = node.connectedEdges().connectedNodes();
        const neighborhood = node.connectedEdges().add(connected).add(node);
        cy.elements().not(neighborhood).addClass('faded');
        neighborhood.addClass('highlighted');
      }

      // Notify Lem
      if (window.invokeLem) {
        invokeLem('canvas:node-selected', { nodeId: node.id() });
      }
    });

    cy.on('dbltap', 'node', function(e) {
      const node = e.target;
      // Only jump to source for function nodes, not file nodes
      if (node.data('type') !== 'file' && window.invokeLem) {
        invokeLem('canvas:open-source', { nodeId: node.id() });
      }
    });

    cy.on('tap', function(e) {
      if (e.target === cy) {
        hideInfo();
        cy.elements().removeClass('highlighted faded');
      }
    });

    cy.on('dragfree', 'node', function(e) {
      const node = e.target;
      const pos = node.position();
      if (window.invokeLem) {
        invokeLem('canvas:node-moved', {
          nodeId: node.id(),
          x: Math.round(pos.x),
          y: Math.round(pos.y)
        });
      }
    });

    // Search functionality
    const searchInput = document.getElementById('search-input');
    searchInput.addEventListener('input', function(e) {
      const query = e.target.value.toLowerCase();
      if (query === '') {
        cy.elements().removeClass('faded highlighted');
        return;
      }
      cy.elements().addClass('faded');
      cy.nodes().forEach(function(node) {
        if (node.data('name').toLowerCase().includes(query)) {
          node.removeClass('faded').addClass('highlighted');
          // Also highlight parent file node
          const parent = node.parent();
          if (parent.length > 0) {
            parent.removeClass('faded');
          }
        }
      });
    });

    // Control functions
    function fitView() {
      cy.fit(null, 50);
    }

    function runLayout() {
      cy.layout({
        name: 'fcose',
        quality: 'proof',
        randomize: false,
        animate: true,
        animationDuration: 500,
        fit: true,
        padding: 50,
        nodeDimensionsIncludeLabels: true,
        packComponents: true,
        nodeRepulsion: 8000,
        idealEdgeLength: 80,
        nestingFactor: 0.1
      }).run();
    }

    // API for Lem
    window.updateGraph = function(data) {
      cy.json({ elements: data.elements });
      runLayout();
    };

    window.highlightNode = function(nodeId) {
      cy.elements().removeClass('executing');
      const node = cy.$('#' + CSS.escape(nodeId));
      if (node.length > 0) {
        node.addClass('executing');
        cy.animate({
          center: { eles: node },
          duration: 300
        });
      }
    };

    window.clearHighlight = function() {
      cy.elements().removeClass('executing');
    };

    window.fitView = fitView;

    // Initial fit
    cy.ready(function() {
      setTimeout(fitView, 100);
    });
  </script>
</body>
</html>" graph-json))

;;; Buffer Creation

(defun make-canvas-buffer (name source-buffer graph)
  "Create a new canvas buffer displaying the given call graph"
  (let* ((graph-json (graph-to-cytoscape-json graph))
         (html (generate-canvas-html graph-json))
         (buffer (lem:make-buffer name)))
    (change-class buffer 'canvas-buffer
                  :graph graph
                  :source-buffer source-buffer
                  :html html)
    buffer))

;;; Buffer Update

(defun update-canvas-buffer (buffer)
  "Update the canvas buffer with fresh graph data"
  (let* ((source-buffer (canvas-buffer-source-buffer buffer))
         (graph (analyze-buffer source-buffer))
         (graph-json (graph-to-cytoscape-json graph)))
    (setf (canvas-buffer-graph buffer) graph)
    (setf (lem:html-buffer-html buffer)
          (generate-canvas-html graph-json))
    buffer))
