"use strict";

const rpc = require("vscode-jsonrpc");
const cp = require("child_process");
const utf8 = require("utf-8");

const ipcRenderer = require("electron").ipcRenderer;
const remote = require("@electron/remote");

const { JSONRPC } = require("./jsonrpc");
const keyevent = require("./keyevent");
const option = require("./option").option;

class FontAttribute {
  constructor(name, size) {
    const font = `${size}px ${name}`;
    const canvas = document.createElement("canvas");
    const ctx = canvas.getContext("2d", { alpha: false });
    ctx.font = font;
    const width = ctx.measureText("W").width;
    this.update(font, name, size, width, size);
  }
  update(font, name, pixel, width, height) {
    this.font = font;
    this.name = name;
    this.pixel = pixel;
    this.width = width;
    this.height = height + 2;
  }
}

let fontAttribute = new FontAttribute(option.fontName, option.fontSize);

const inputKinds = {
  abort: 0,
  keyEvent: 1,
  resize: 2,
  command: 3,
  method: 4,
};

function calcDisplayCols(width) {
  return Math.floor(width / fontAttribute.width);
}

function calcDisplayRows(height) {
  return Math.floor(height / fontAttribute.height);
}

class LemEditorPane extends HTMLElement {
  constructor() {
    super();
  }
}

function doLog(message, params) {
  //console.log(message, params);
}

class WebSocketRPCDelegator {
  constructor() {
    const webSocket = new WebSocket("ws://127.0.0.1:50000");
    this.jsonrpc = new JSONRPC(webSocket);
  }

  listen() {}

  on(method, handler) {
    this.jsonrpc.on(method, handler);
  }

  request(method, arg, callback) {
    this.jsonrpc.request(method, arg, callback);
  }

  notify(method, arg) {
    this.jsonrpc.notify(method, arg);
  }
}

class VSCodeJSONRPCDelegator {
  constructor() {
    const childProcess = cp.spawn("lem-jsonrpc", ["--mode", "stdio"]);
    this.rpcConnection = rpc.createMessageConnection(
      new rpc.StreamMessageReader(childProcess.stdout),
      new rpc.StreamMessageWriter(childProcess.stdin),
    );
  }

  listen() {
    this.rpcConnection.listen();
  }

  on(method, handler) {
    this.rpcConnection.onNotification(method, handler);
  }

  request(method, arg, callback) {
    this.rpcConnection.sendRequest(method, arg, callback);
  }

  notify(method, arg) {
    this.rpcConnection.sendNotification(method, arg);
  }
}

class LemEditor extends HTMLElement {
  constructor() {
    super();

    this.viewTable = {};

    this.rpcDelegator = new WebSocketRPCDelegator();
    // this.rpcDelegator = new VSCodeJSONRPCDelegator();

    this.rpcDelegator.on("update-foreground", this.updateForeground.bind(this));
    this.rpcDelegator.on("update-background", this.updateBackground.bind(this));
    this.rpcDelegator.on("make-view", this.makeView.bind(this));
    this.rpcDelegator.on("delete-view", this.deleteView.bind(this));
    this.rpcDelegator.on("resize-view", this.resizeView.bind(this));
    this.rpcDelegator.on("move-view", this.moveView.bind(this));
    this.rpcDelegator.on("clear", this.clear.bind(this));
    this.rpcDelegator.on("clear-eol", this.clearEol.bind(this));
    this.rpcDelegator.on("clear-eob", this.clearEob.bind(this));
    this.rpcDelegator.on("put", this.put.bind(this));
    this.rpcDelegator.on("modeline-put", this.modelinePut.bind(this));
    this.rpcDelegator.on("touch", this.touch.bind(this));
    this.rpcDelegator.on("move-cursor", this.moveCursor.bind(this));
    this.rpcDelegator.on("scroll", this.scroll.bind(this));
    this.rpcDelegator.on("update-display", this.updateDisplay.bind(this));
    this.rpcDelegator.on("set-font", this.setFont.bind(this));
    this.rpcDelegator.on("exit", this.exit.bind(this));

    this.rpcDelegator.listen();

    this.lemEditorPane = document.createElement("lem-editor-pane");
    this.lemEditorPane.style.float = "left";
    this.appendChild(this.lemEditorPane);

    const mainWindow = remote.getCurrentWindow();
    const contentBounds = mainWindow.getContentBounds();
    this.width = contentBounds.width;
    this.height = contentBounds.height;

    this.userId = null;

    this.rpcDelegator.request("login", {
      width: calcDisplayCols(this.width),
      height: calcDisplayRows(this.height),
      foreground: option.foreground,
      background: option.background,
    }, (response) => {
      this.userId = response.userId;
    });

    // will updated by setFont()
    this.fontWidth = fontAttribute.width;
    this.fontHeight = fontAttribute.height;

    const charUnitWindowRect = (desiredBounds) => {
      const { x, y, width, height } = mainWindow.getBounds();
      const cb = mainWindow.getContentBounds();
      const dw = width - cb.width;
      const dh = height - cb.height;
      const ncw =
        this.fontWidth *
        Math.round((desiredBounds.width - dw) / this.fontWidth);
      const nch =
        this.fontHeight *
        Math.round((desiredBounds.height - dh) / this.fontHeight);
      const nw = Math.ceil(ncw + dw);
      const nh = Math.ceil(nch + dh);
      const nx = desiredBounds.x === x ? x : x - (nw - width);
      const ny = desiredBounds.y === y ? y : y - (nh - height);
      return {
        contentWidth: ncw,
        contentHeight: nch,
        windowRect: { x: nx, y: ny, width: nw, height: nh },
      };
    };

    // 'will-resize' event handling.
    // Linux: Not supported
    // MacOS: Does not work properly e.g. https://github.com/electron/electron/issues/21777
    if (process.platform === "win32") {
      mainWindow.on("will-resize", (_, newBounds) => {
        const { contentWidth, contentHeight, windowRect } =
          charUnitWindowRect(newBounds);
        mainWindow.setBounds(windowRect);
      });
    }

    let timeoutId = null;
    const resizeHandler = () => {
      if (process.platform === "win32" || process.platform === "linux") {
        const { width, height } = mainWindow.getContentBounds();
        this.resize(width, height);
      } else {
        const r = mainWindow.getBounds();
        const { contentWidth, contentHeight, windowRect } =
          charUnitWindowRect(r);
        mainWindow.setBounds(windowRect);
        this.resize(contentWidth, contentHeight);
      }
    };
    mainWindow.on("resize", () => {
      if (timeoutId) {
        clearTimeout(timeoutId);
      }
      timeoutId = setTimeout(resizeHandler, 200);
    });

    ipcRenderer.on("command", (event, message) => {
      this.emitInput(inputKinds.command, message);
    });

    // create input text box;
    this.picker = new Picker(this);

    // resize to initial size
    this.resizeTo(option.cols, option.rows);
  }

  resizeTo(col, row) {
    const mainWindow = remote.getCurrentWindow();
    const { x, y, width, height } = mainWindow.getBounds();
    const cb = mainWindow.getContentBounds();
    const dw = width - cb.width;
    const dh = height - cb.height;
    const nw = Math.ceil(this.fontWidth * col + dw);
    const nh = Math.ceil(this.fontHeight * row + dh);
    mainWindow.setBounds({ x: x, y: y, width: nw, height: nh });
  }

  setFont(params) {
    fontAttribute = new FontAttribute(params.name, params.size);
    this.fontWidth = fontAttribute.width;
    this.fontHeight = fontAttribute.height;
  }

  exit(params) {
    ipcRenderer.send("exit");
  }

  sendNotification(method, params) {
    this.emitInput(inputKinds.method, { method: method, params: params });
  }

  emitInput(kind, value) {
    this.rpcDelegator.notify("input", {
      userId: this.userId,
      kind: kind,
      value: value,
    });
  }

  resize(width, height) {
    this.width = width;
    this.height = height;
    this.emitInput(inputKinds.resize, {
      width: calcDisplayCols(width),
      height: calcDisplayRows(height),
    });
    this.lemEditorPane.style.width = width;
    this.lemEditorPane.style.height = height;
  }

  updateForeground(params) {
    doLog("update-foreground", params);
    option.foreground = params;
    this.picker.updateForeground(params);
  }

  updateBackground(params) {
    doLog("update-background", params);
    option.background = params;
    this.picker.updateBackground(params);
    this.lemEditorPane.style["background-color"] = option.background;
  }

  makeView(params) {
    doLog("make-view", params);
    const { id, x, y, width, height, use_modeline, kind } = params;
    const view = new View(id, x, y, width, height, use_modeline, kind);
    view.allTags().forEach((child) => {
      this.lemEditorPane.appendChild(child);
    });
    this.viewTable[id] = view;
  }

  deleteView(params) {
    doLog("delete-view", params);
    const { id } = params.viewInfo;
    const view = this.viewTable[id];
    view.delete();
    delete this.viewTable[id];
  }

  resizeView(params) {
    doLog("resize-view", params);
    const { viewInfo, width, height } = params;
    const view = this.viewTable[viewInfo.id];
    view.resize(width, height);
  }

  moveView(params) {
    doLog("move-view", params);
    const { x, y, viewInfo } = params;
    const view = this.viewTable[viewInfo.id];
    view.move(x, y);
  }

  clear(params) {
    doLog("clear", params);
    const view = this.viewTable[params.viewInfo.id];
    view.clear();
  }

  clearEol(params) {
    doLog("clear-eol", params);
    const { viewInfo, x, y } = params;
    const view = this.viewTable[viewInfo.id];
    view.clearEol(x, y);
  }

  clearEob(params) {
    doLog("clear-eob", params);
    const { viewInfo, x, y } = params;
    const view = this.viewTable[viewInfo.id];
    view.clearEob(x, y);
  }

  put(params) {
    doLog("put", params);
    const { viewInfo, x, y, text, textWidth, attribute } = params;
    const view = this.viewTable[viewInfo.id];
    view.put(x, y, text, textWidth, attribute);
  }

  modelinePut(params) {
    doLog("modeline-put", params);
    const { viewInfo, x, y, text, textWidth, attribute } = params;
    const view = this.viewTable[viewInfo.id];
    view.modelinePut(x, text, textWidth, attribute);
  }

  touch(params) {
    doLog("touch", params);
    const { viewInfo } = params;
    const view = this.viewTable[viewInfo.id];
    view.touch();
  }

  moveCursor(params) {
    doLog("move-cursor", params);
    const { viewInfo, x, y } = params;
    const view = this.viewTable[viewInfo.id];
    view.setCursor(x, y);
    const left =
      view.editSurface.canvas.offsetLeft + x * fontAttribute.width + 3;
    const top =
      view.editSurface.canvas.offsetTop + y * fontAttribute.height + 3;
    this.picker.movePicker(left, top);
  }

  scroll(params) {
    doLog("scroll", params);
    const { viewInfo, n } = params;
    const view = this.viewTable[viewInfo.id];
    view.scroll(n);
  }

  updateDisplay(params) {}
}

class Picker {
  constructor(editor) {
    this.__composition = false;

    this.editor = editor;

    this.measure = document.createElement("span");
    this.picker = document.createElement("input");
    this.picker.style.backgroundColor = "transparent";
    this.picker.style.color = "transparent";
    this.picker.style.width = "0";
    this.picker.style.padding = "0";
    this.picker.style.margin = "0";
    this.picker.style.border = "none";
    this.picker.style.position = "absolute";
    this.picker.style.zIndex = "-10";

    this.measure.style.color = option.foreground;
    this.measure.style.backgroundColor = option.background;
    this.measure.style.position = "absolute";
    this.measure.style.zIndex = "";

    this.picker.style.top = "0";
    this.picker.style.left = "0";
    this.picker.style.font = fontAttribute.font;
    this.measure.style.top = "0";
    this.measure.style.left = "0";
    this.measure.style.font = fontAttribute.font;

    this.picker.addEventListener("blur", () => {
      this.picker.focus();
    });
    this.picker.addEventListener("keydown", (event) => {
      event.preventDefault();
      if (event.isComposing !== true && event.code !== "") {
        const k = keyevent.convertKeyEvent(event);
        this.editor.emitInput(inputKinds.keyEvent, k);
        this.picker.value = "";
        return false;
      }
    });

    this.picker.addEventListener("input", (event) => {
      if (this.__composition === false) {
        this.picker.value = "";
        this.measure.innerHTML = this.picker.value;
        this.picker.style.width = "0";
      }
    });
    this.picker.addEventListener("compositionstart", (event) => {
      this.__composition = true;
      console.log(event);
      this.measure.innerHTML = this.picker.value;
      this.picker.style.width = this.measure.offsetWidth + "px";
    });
    this.picker.addEventListener("compositionupdate", (event) => {
      this.measure.innerHTML = event.data;
      this.picker.style.width = this.measure.offsetWidth + "px";
    });
    this.picker.addEventListener("compositionend", (event) => {
      this.__composition = false;
      console.log(this.picker.value); // TODO
      let chars = this.picker.value
        .split("")
        .map((char) => utf8.setBytesFromString(char));
      this.editor.emitInput(inputKinds.command, ["input-string", chars]);
      this.picker.value = "";
      this.measure.innerHTML = this.picker.value;
      this.picker.style.width = "0";
    });
    document.body.appendChild(this.picker);
    document.body.appendChild(this.measure);
    this.picker.focus();
  }

  movePicker(left, top) {
    this.measure.style.top = top + "px";
    this.measure.style.left = left + "px";
    // picker follow measure
    this.picker.style.top = this.measure.offsetTop + "px";
    this.picker.style.left = this.measure.offsetLeft + "px";
  }

  updateForeground(color) {
    this.measure.style.color = color;
  }

  updateBackground(color) {
    this.measure.style.backgroundColor = color;
  }
}

class VerticalBorder {
  constructor(x, y, height) {
    this.border = document.createElement("div");
    this.border.style.backgroundColor = "#ccc";
    this.border.style.width = "2px";
    this.border.style.height = height * fontAttribute.height;
    this.border.style.position = "absolute";
    this.move(x, y);
  }

  move(x, y) {
    this.border.style.left = Math.floor(
      x * fontAttribute.width - fontAttribute.width / 2,
    );
    this.border.style.top = y * fontAttribute.height;
  }

  resize(height) {
    this.border.style.height = height * fontAttribute.height;
  }
}

const viewStyles = {
  tile: {},
  floating: {
    zIndex: 5,
    "border-radius": "10px",
    border: "2px solid #FFF",
    padding: "10px",
    background: "#333", // option.background
  },
};

function getViewStyle(kind) {
  return viewStyles[kind] || {};
}

class View {
  constructor(id, x, y, width, height, use_modeline, kind) {
    this.id = id;
    this.width = width;
    this.height = height;
    this.use_modeline = use_modeline;
    this.kind = kind;
    this.editSurface = new Surface(x, y, width, height, getViewStyle(kind));

    if (use_modeline) {
      this.modelineSurface = new Surface(x, y + height, width, 1, {
        zIndex: 1,
      });
    } else {
      this.modelineSurface = null;
    }

    if (x > 0 && kind === "tile") {
      this.leftSideBorder = new VerticalBorder(
        x - 2,
        y,
        height + (this.modelineSurface === null ? 0 : 1),
      );
    } else {
      this.leftSideBorder = null;
    }

    this.move(x, y);
    this.resize(width, height);

    this.cursor = { x: null, y: null, color: option.foreground };
  }

  updateEditSurfaceStyle() {
    this.editSurface.updateStyle(getViewStyle(this.kind));
  }

  allTags() {
    let tags = [this.editSurface.canvas];

    if (this.modelineSurface !== null) {
      tags.push(this.modelineSurface.canvas);
    }
    if (this.leftSideBorder !== null) {
      tags.push(this.leftSideBorder.border);
    }

    return tags;
  }

  delete() {
    this.editSurface.canvas.parentNode.removeChild(this.editSurface.canvas);
    if (this.modelineSurface !== null) {
      this.modelineSurface.canvas.parentNode.removeChild(
        this.modelineSurface.canvas,
      );
    }
    if (this.leftSideBorder !== null) {
      this.leftSideBorder.border.parentNode.removeChild(
        this.leftSideBorder.border,
      );
    }
  }

  move(x, y) {
    this.editSurface.move(x, y);
    if (this.modelineSurface !== null) {
      this.modelineSurface.move(x, y + this.height);
    }
    if (this.leftSideBorder !== null) {
      this.leftSideBorder.move(x, y);
    }
  }

  resize(width, height) {
    this.width = width;
    this.height = height;
    this.editSurface.resize(width, height);
    if (this.modelineSurface !== null) {
      this.modelineSurface.move(
        this.x,
        this.editSurface.y + this.editSurface.height,
      );
      this.modelineSurface.resize(width, 1);
    }
    if (this.leftSideBorder !== null) {
      this.leftSideBorder.move(this.x, this.editSurface.y - 2);
      this.leftSideBorder.resize(
        height + (this.modelineSurface === null ? 0 : 1),
      );
    }
  }

  clear() {
    this.editSurface.drawBlock(0, 0, this.width, this.height);
  }

  clearEol(x, y) {
    this.editSurface.drawBlock(x, y, this.width - x, 1);
  }

  clearEob(x, y) {
    this.clearEol(x, y);
    this.editSurface.drawBlock(x, y + 1, this.width, this.height - y - 1);
  }

  put(x, y, text, textWidth, attribute) {
    this.editSurface.put(x, y, text, textWidth, attribute);
  }

  modelinePut(x, text, textWidth, attribute) {
    if (this.modelineSurface !== null) {
      this.modelineSurface.put(x, 0, text, textWidth, attribute);
    }
  }

  touch() {
    this.editSurface.touch();
    if (this.modelineSurface !== null) {
      this.modelineSurface.touch();
    }
  }

  setCursor(x, y) {
    this.cursor.x = x;
    this.cursor.y = y;
  }

  scroll(n) {
    this.editSurface.scroll(n);
  }
}

class DrawingEvent {}

class DrawBlock extends DrawingEvent {
  constructor({ style, x, y, w, h }) {
    super();
    this.style = style;
    this.x = x;
    this.y = y;
    this.w = w;
    this.h = h;
  }

  run(ctx) {
    ctx.fillStyle = this.style;
    ctx.fillRect(this.x, this.y, this.w, this.h);
  }
}

class DrawText extends DrawingEvent {
  constructor({ style, font, x, y, text }) {
    super();
    this.style = style;
    this.font = font;
    this.x = x;
    this.y = y;
    this.text = text;
  }

  run(ctx) {
    ctx.fillStyle = this.style;
    ctx.font = this.font;
    ctx.textBaseline = "top";
    ctx.fillText(this.text, this.x, this.y);
  }
}

class DrawUnderline extends DrawingEvent {
  constructor({ style, x, y, width }) {
    super();
    this.style = style;
    this.x = x;
    this.y = y;
    this.width = width;
  }

  run(ctx) {
    ctx.strokeStyle = this.style;
    ctx.lineWidth = 1;
    ctx.setLineDash([]);
    ctx.beginPath();
    ctx.moveTo(this.x, this.y);
    ctx.lineTo(this.x + this.width, this.y);
    ctx.stroke();
  }
}

class Surface {
  constructor(x, y, width, height, styles) {
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
    this.canvas = document.createElement("canvas");
    this.canvas.style.position = "absolute";
    this.updateStyle(styles);
    this.ctx = this.canvas.getContext("2d", { alpha: false });
    this.ctx.textBaseline = "top";
    this.ctx.font = fontAttribute.font;
    this.event_queue = [];
  }

  updateStyle(styles) {
    for (let key in styles) {
      this.canvas.style[key] = styles[key];
    }
  }

  move(x, y) {
    this.x = x;
    this.y = y;
    this.canvas.style.left = Math.floor(x * fontAttribute.width);
    this.canvas.style.top = Math.floor(y * fontAttribute.height);
  }

  resize(width, height) {
    const ratio = window.devicePixelRatio || 1;
    this.width = width;
    this.height = height;
    this.canvas.width = width * fontAttribute.width * ratio;
    this.canvas.height = height * fontAttribute.height * ratio;
    this.canvas.style.width = width * fontAttribute.width + "px";
    this.canvas.style.height = height * fontAttribute.height + "px";
    this.ctx.scale(ratio, ratio);
  }

  drawBlock(x, y, w, h, color) {
    this.event_queue.push(
      new DrawBlock({
        style: color || option.background,
        x: x * fontAttribute.width,
        y: y * fontAttribute.height,
        w: w * fontAttribute.width + 1,
        h: h * fontAttribute.height,
      }),
    );
  }

  drawText(x, y, text, font, color) {
    this.event_queue.push(
      new DrawText({
        style: color,
        font: font,
        x: x * fontAttribute.width,
        y: y * fontAttribute.height,
        text: text,
      }),
    );
  }

  drawUnderline(x, y, length, color) {
    this.event_queue.push(
      new DrawUnderline({
        style: color,
        x: x * fontAttribute.width,
        y: (y + 1) * fontAttribute.height - 3,
        width: fontAttribute.width * length,
      }),
    );
  }

  put(x, y, text, textWidth, attribute) {
    if (attribute === null) {
      this.drawBlock(x, y, textWidth, 1, option.background);
      this.drawText(x, y, text, fontAttribute.font, option.foreground);
    } else {
      let font = fontAttribute.font;
      let foreground = attribute.foreground || option.foreground;
      let background = attribute.background || option.background;
      const { bold, reverse, underline } = attribute;
      if (reverse) {
        const tmp = foreground;
        foreground = background;
        background = tmp;
      }
      if (bold) {
        font = "bold " + font;
      }
      this.drawBlock(x, y, textWidth, 1, background);
      this.drawText(x, y, text, font, foreground);
      if (underline) {
        this.drawUnderline(x, y, text.length, foreground);
      }
    }
  }

  touch() {
    for (let event of this.event_queue) {
      event.run(this.ctx);
    }
    this.event_queue = [];
  }

  scroll(n) {
    if (n > 0) {
      const image = this.ctx.getImageData(
        0,
        n * fontAttribute.height,
        this.width * fontAttribute.width,
        (this.height - n) * fontAttribute.height,
      );
      this.ctx.putImageData(image, 0, 0);
    } else {
      n = -n;
      const image = this.ctx.getImageData(
        0,
        0,
        this.width * fontAttribute.width,
        (this.height - n) * fontAttribute.height,
      );
      this.ctx.putImageData(
        image,
        x * fontAttribute.width,
        (y + n) * fontAttribute.height,
      );
    }
  }
}

customElements.define("lem-editor", LemEditor);
customElements.define("lem-editor-pane", LemEditorPane);
