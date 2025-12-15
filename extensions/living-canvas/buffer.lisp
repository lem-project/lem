(defpackage :lem-living-canvas/buffer
  (:use :cl :lem)
  (:import-from :lem-living-canvas/call-graph
                #:graph-to-json
                #:json-to-graph
                #:analyze-buffer
                #:save-graph)
  (:import-from :lem-living-canvas/graph-format
                #:convert-to-cytoscape)
  (:export #:canvas-buffer
           #:make-canvas-buffer
           #:make-canvas-buffer-from-json
           #:make-canvas-buffer-from-file
           #:save-canvas-to-file
           #:canvas-buffer-graph
           #:canvas-buffer-source-buffer
           #:canvas-buffer-node-positions
           #:canvas-buffer-json
           #:update-canvas-buffer))
(in-package :lem-living-canvas/buffer)

;;; Canvas Buffer Class

(defclass canvas-buffer (lem:html-buffer)
  ((graph :initarg :graph
          :initform nil
          :accessor canvas-buffer-graph
          :documentation "The call graph data")
   (source-buffer :initarg :source-buffer
                  :initform nil
                  :accessor canvas-buffer-source-buffer
                  :documentation "The source buffer being analyzed")
   (node-positions :initform (make-hash-table :test 'equal)
                   :accessor canvas-buffer-node-positions
                   :documentation "Cached node positions for persistence")
   (json :initarg :json
         :initform nil
         :accessor canvas-buffer-json
         :documentation "The Living Canvas JSON data (portable format)"))
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
  <script src='https://unpkg.com/dagre@0.8.5/dist/dagre.min.js'></script>
  <script src='https://unpkg.com/cytoscape-dagre@2.5.0/cytoscape-dagre.js'></script>
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
      max-width: 450px;
      min-width: 280px;
      display: none;
      z-index: 1000;
    }
    #info-panel.visible { display: block; }
    #info-panel h3 {
      color: #569cd6;
      margin-bottom: 4px;
      font-size: 14px;
      font-family: Consolas, Monaco, monospace;
    }
    #info-panel .type-badge {
      display: inline-block;
      background: #3c3c3c;
      color: #4ec9b0;
      font-size: 10px;
      padding: 2px 6px;
      border-radius: 3px;
      margin-bottom: 8px;
      text-transform: uppercase;
    }
    #info-panel .type-badge.macro { background: #3d2d3d; color: #c586c0; }
    #info-panel .type-badge.generic-function { background: #2d3d3d; color: #4ec9b0; }
    #info-panel .type-badge.command { background: #1e3a5f; color: #1e88e5; }
    #info-panel .type-badge.major-mode { background: #1e3f1e; color: #66bb6a; }
    #info-panel .type-badge.minor-mode { background: #2d3f2d; color: #a5d6a7; }
    #info-panel .type-badge.mode { background: #1e3f2d; color: #4db6ac; }
    #info-panel .info-section {
      border-top: 1px solid #3c3c3c;
      padding-top: 8px;
      margin-top: 8px;
    }
    #info-panel .info-row {
      display: flex;
      margin-bottom: 4px;
    }
    #info-panel .info-label {
      color: #808080;
      min-width: 70px;
      font-size: 11px;
    }
    #info-panel .info-value {
      color: #d4d4d4;
      font-family: Consolas, Monaco, monospace;
      font-size: 11px;
      word-break: break-all;
    }
    #info-panel .info-value.arglist {
      color: #dcdcaa;
    }
    #info-panel .info-value.source {
      color: #ce9178;
    }
    #info-panel .connection-stats {
      display: flex;
      gap: 16px;
      margin-top: 8px;
    }
    #info-panel .stat-item {
      display: flex;
      align-items: center;
      gap: 4px;
    }
    #info-panel .stat-count {
      font-weight: bold;
      color: #569cd6;
    }
    #info-panel .stat-label {
      color: #808080;
      font-size: 11px;
    }
    #info-panel .docstring {
      color: #9cdcfe;
      font-style: italic;
      line-height: 1.4;
      margin-top: 8px;
      padding-top: 8px;
      border-top: 1px solid #3c3c3c;
      max-height: 80px;
      overflow-y: auto;
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
    #debug-info {
      position: fixed;
      bottom: 10px;
      right: 10px;
      background: rgba(30, 30, 30, 0.9);
      border: 1px solid #3c3c3c;
      border-radius: 4px;
      padding: 6px 10px;
      color: #808080;
      font-size: 11px;
      font-family: monospace;
      z-index: 1000;
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
    <span class='type-badge' id='info-type-badge'></span>
    <div class='info-section'>
      <div class='info-row'>
        <span class='info-label'>Args:</span>
        <span class='info-value arglist' id='info-arglist'></span>
      </div>
      <div class='info-row'>
        <span class='info-label'>Source:</span>
        <span class='info-value source' id='info-source'></span>
      </div>
    </div>
    <div class='connection-stats' id='info-connections'>
      <div class='stat-item'>
        <span class='stat-count' id='info-callers'>0</span>
        <span class='stat-label'>callers</span>
      </div>
      <div class='stat-item'>
        <span class='stat-count' id='info-callees'>0</span>
        <span class='stat-label'>callees</span>
      </div>
    </div>
    <div class='docstring' id='info-doc'></div>
  </div>
  <div id='debug-info'>Loading...</div>

  <script>
    const graphData = ~A;

    // Debug logging
    console.log('Graph data:', graphData);
    console.log('Total elements:', graphData.elements ? graphData.elements.length : 0);

    const nodes = graphData.elements ? graphData.elements.filter(e => e.group === 'nodes') : [];
    const edges = graphData.elements ? graphData.elements.filter(e => e.group === 'edges') : [];
    console.log('Nodes:', nodes.length, 'Edges:', edges.length);

    // Check for file nodes vs function nodes
    const fileNodes = nodes.filter(n => n.data && n.data.type === 'file');
    const funcNodes = nodes.filter(n => n.data && n.data.type !== 'file');
    console.log('File nodes:', fileNodes.length, 'Function nodes:', funcNodes.length);

    // Update debug info
    const debugEl = document.getElementById('debug-info');
    debugEl.textContent = 'Data: ' + nodes.length + ' nodes, ' + edges.length + ' edges (' + fileNodes.length + ' files)';

    // If no nodes, show message
    if (nodes.length === 0) {
      document.getElementById('cy').innerHTML = '<div style=\"color: #d4d4d4; text-align: center; padding-top: 40vh; font-family: sans-serif;\"><h2>No functions found</h2><p>The package may need to be loaded first, or it contains no accessible function definitions.</p></div>';
      debugEl.textContent = 'No data to display';
    }

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
        // Lem-specific node types
        {
          selector: 'node[type=\"command\"]',
          style: {
            'background-color': '#1e3a5f',
            'border-color': '#1e88e5',
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
          selector: 'node[type=\"major-mode\"]',
          style: {
            'background-color': '#1e3f1e',
            'border-color': '#66bb6a',
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
            'shape': 'diamond'
          }
        },
        {
          selector: 'node[type=\"minor-mode\"]',
          style: {
            'background-color': '#2d3f2d',
            'border-color': '#a5d6a7',
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
            'shape': 'diamond'
          }
        },
        {
          selector: 'node[type=\"mode\"]',
          style: {
            'background-color': '#1e3f2d',
            'border-color': '#4db6ac',
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
            'shape': 'diamond'
          }
        },
        {
          selector: 'edge',
          style: {
            'width': 1.5,
            'line-color': '#606060',
            'target-arrow-color': '#606060',
            'target-arrow-shape': 'triangle',
            'curve-style': 'bezier',
            'arrow-scale': 0.8,
            'opacity': 0.7
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
            'border-color': '#ffcc00',
            'border-width': 3,
            'background-color': '#3d3d1d'
          }
        },
        {
          selector: 'edge.highlighted',
          style: {
            'line-color': '#4fc3f7',
            'target-arrow-color': '#4fc3f7',
            'width': 3,
            'opacity': 1
          }
        },
        {
          selector: '.faded',
          style: {
            'opacity': 0.6
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
        name: 'preset'  // Start with preset, run layout after
      },
      minZoom: 0.1,
      maxZoom: 3,
      wheelSensitivity: 0.3
    });

    // Info panel elements
    const infoPanel = document.getElementById('info-panel');
    const infoName = document.getElementById('info-name');
    const infoTypeBadge = document.getElementById('info-type-badge');
    const infoArglist = document.getElementById('info-arglist');
    const infoSource = document.getElementById('info-source');
    const infoCallers = document.getElementById('info-callers');
    const infoCallees = document.getElementById('info-callees');
    const infoConnections = document.getElementById('info-connections');
    const infoDoc = document.getElementById('info-doc');
    const infoSection = document.querySelector('#info-panel .info-section');

    function showInfo(node) {
      const data = node.data();
      if (data.type === 'file') {
        // File node - simplified view
        infoName.textContent = data.name;
        infoTypeBadge.textContent = 'FILE';
        infoTypeBadge.className = 'type-badge';
        infoSection.style.display = 'none';
        infoConnections.style.display = 'none';
        infoDoc.textContent = data.filepath || '';
        infoDoc.style.display = data.filepath ? 'block' : 'none';
      } else {
        // Function node - full details
        infoName.textContent = data.name;

        // Type badge with color
        infoTypeBadge.textContent = data.type.toUpperCase();
        infoTypeBadge.className = 'type-badge ' + data.type;

        // Arglist
        const arglist = data.arglist || '()';
        infoArglist.textContent = arglist;

        // Source location
        const sourceFile = data.sourceFile || '';
        const lineNumber = data.lineNumber || 0;
        if (sourceFile && lineNumber > 0) {
          infoSource.textContent = sourceFile + ':' + lineNumber;
        } else if (sourceFile) {
          infoSource.textContent = sourceFile;
        } else {
          infoSource.textContent = '(unknown)';
        }

        // Connection counts (callers = incoming, callees = outgoing)
        const incomingEdges = node.incomers('edge').length;
        const outgoingEdges = node.outgoers('edge').length;
        infoCallers.textContent = incomingEdges;
        infoCallees.textContent = outgoingEdges;

        // Docstring
        const docstring = data.docstring;
        if (docstring && docstring.trim()) {
          infoDoc.textContent = docstring;
          infoDoc.style.display = 'block';
        } else {
          infoDoc.style.display = 'none';
        }

        // Show sections
        infoSection.style.display = 'block';
        infoConnections.style.display = 'flex';
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

      // Clear previous state
      cy.elements().removeClass('highlighted faded');

      // Highlight connected edges (only for non-file nodes)
      if (node.data('type') !== 'file') {
        const connectedEdges = node.connectedEdges();
        const connectedNodes = connectedEdges.connectedNodes();
        const neighborhood = connectedEdges.add(connectedNodes).add(node);

        // Show and highlight connected edges
        connectedEdges.addClass('highlighted');
        connectedNodes.addClass('highlighted');
        node.addClass('highlighted');

        // Fade non-connected elements
        cy.nodes().not(neighborhood).addClass('faded');
      }

      // Notify Lem (only for function nodes, not file containers)
      if (node.data('type') !== 'file') {
        console.log('Node clicked:', node.id(), 'invokeLem available:', !!window.invokeLem);
        if (window.invokeLem) {
          console.log('Calling invokeLem canvas:node-selected');
          invokeLem('canvas:node-selected', { nodeId: node.id() });
        } else {
          console.warn('invokeLem not available');
        }
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
      const nodeCount = cy.nodes().length;
      const edgeCount = cy.edges().length;
      console.log('Running dagre layout for', nodeCount, 'nodes,', edgeCount, 'edges');

      // Use dagre layout for directed graph with edge routing
      // dagre minimizes edge crossings and handles hierarchical layouts well
      const layout = cy.layout({
        name: 'dagre',
        rankDir: 'TB',           // Top to Bottom (caller -> callee)
        nodeSep: 60,             // Horizontal spacing between nodes
        rankSep: 80,             // Vertical spacing between ranks
        edgeSep: 20,             // Minimum edge separation
        ranker: 'network-simplex', // Algorithm for ranking nodes
        fit: true,
        padding: 50,
        animate: false,
        // Handle compound nodes (file containers)
        nodeDimensionsIncludeLabels: true
      });

      layout.run();
      console.log('Dagre layout complete');
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

    // Initial layout and fit
    cy.ready(function() {
      console.log('Cytoscape ready');
      const cyNodes = cy.nodes().length;
      const cyEdges = cy.edges().length;
      const fileCount = cy.nodes('[type = \"file\"]').length;
      debugEl.textContent = 'Loaded: ' + cyNodes + ' nodes, ' + cyEdges + ' edges';
      console.log('Cytoscape nodes:', cyNodes, 'edges:', cyEdges, 'files:', fileCount);

      if (cyNodes > 0) {
        try {
          console.log('Running dagre layout...');
          runLayout();
          debugEl.textContent = cyNodes + ' nodes, ' + cyEdges + ' edges';
          setTimeout(fitView, 100);
        } catch (e) {
          console.error('Dagre layout failed:', e);
          debugEl.textContent = 'Error: ' + e.message;
          // Fallback to grid
          cy.layout({ name: 'grid', fit: true, padding: 30 }).run();
        }
      } else {
        debugEl.textContent = 'No nodes loaded!';
      }
    });
  </script>
</body>
</html>" graph-json))

;;; Buffer Creation

(defun make-canvas-buffer (name source-buffer graph)
  "Create a new canvas buffer displaying the given call graph.
Uses the new data layer: graph -> Living Canvas JSON -> Cytoscape JSON -> HTML"
  (let* ((living-canvas-json (graph-to-json graph))
         (cytoscape-json (convert-to-cytoscape living-canvas-json))
         (html (generate-canvas-html cytoscape-json))
         (buffer (lem:make-buffer name)))
    (change-class buffer 'canvas-buffer
                  :graph graph
                  :source-buffer source-buffer
                  :json living-canvas-json
                  :html html)
    buffer))

(defun make-canvas-buffer-from-json (name json-string &optional source-buffer)
  "Create a new canvas buffer from Living Canvas JSON string.
This allows displaying graphs loaded from files or external sources."
  (let* ((cytoscape-json (convert-to-cytoscape json-string))
         (html (generate-canvas-html cytoscape-json))
         (graph (json-to-graph json-string))
         (buffer (lem:make-buffer name)))
    (change-class buffer 'canvas-buffer
                  :graph graph
                  :source-buffer source-buffer
                  :json json-string
                  :html html)
    buffer))

(defun make-canvas-buffer-from-file (name pathname &optional source-buffer)
  "Create a new canvas buffer from a saved JSON file."
  (unless (probe-file pathname)
    (lem:editor-error "File not found: ~A" pathname))
  (let ((json-string (with-open-file (stream pathname :direction :input)
                       (let ((contents (make-string (file-length stream))))
                         (read-sequence contents stream)
                         contents))))
    (make-canvas-buffer-from-json name json-string source-buffer)))

(defun save-canvas-to-file (buffer pathname)
  "Save the current canvas state (including positions) to a JSON file.
Returns the pathname on success."
  (let ((graph (canvas-buffer-graph buffer)))
    (unless graph
      (lem:editor-error "Buffer has no graph data"))
    ;; Update positions from cached node-positions
    (let ((positions (canvas-buffer-node-positions buffer)))
      (when positions
        (maphash (lambda (node-id pos)
                   (let ((node (gethash node-id
                                        (lem-living-canvas/call-graph:call-graph-nodes graph))))
                     (when node
                       (setf (lem-living-canvas/call-graph:graph-node-position node) pos))))
                 positions)))
    ;; Save with layout
    (save-graph graph pathname :include-layout t)
    ;; Update buffer's json cache
    (setf (canvas-buffer-json buffer)
          (graph-to-json graph :include-layout t))
    pathname))

;;; Buffer Update

(defun update-canvas-buffer (buffer)
  "Update the canvas buffer with fresh graph data.
Uses the new data layer for conversion."
  (let* ((source-buffer (canvas-buffer-source-buffer buffer))
         (graph (analyze-buffer source-buffer))
         (living-canvas-json (graph-to-json graph))
         (cytoscape-json (convert-to-cytoscape living-canvas-json)))
    (setf (canvas-buffer-graph buffer) graph)
    (setf (canvas-buffer-json buffer) living-canvas-json)
    (setf (lem:html-buffer-html buffer)
          (generate-canvas-html cytoscape-json))
    buffer))
