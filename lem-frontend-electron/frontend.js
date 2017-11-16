'use strict';

const rpc = require('vscode-jsonrpc');
const cp = require('child_process');
const utf8 = require('utf-8')
const ipcRenderer = require('electron').ipcRenderer;
const getCurrentWindow = require('electron').remote.getCurrentWindow;
const keyevent = require('./keyevent');
const { option } = require('./option');

class FontAttribute {
    constructor(name, size) {
        const font = `${size}px ${name}`;
        const canvas = document.createElement('canvas');
        const ctx = canvas.getContext('2d', { alpha: false });
        ctx.font = font;
        const width = ctx.measureText('a').width;
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

let fontAttribute = new FontAttribute(option.fontName, option.fontPixel);

const kindAbort = 0;
const kindKeyEvent = 1;
const kindResize = 2;
const kindCommand = 3;

const viewTable = {};

function calcDisplayCols(width) {
    return Math.floor(width / fontAttribute.width);
}

function calcDisplayRows(height) {
    return Math.floor(height / fontAttribute.height) - 2;
}

function getCurrentWindowSize() {
    return getCurrentWindow().getSize();
}

class LemModule extends HTMLElement {
    constructor() {
        super();
    }
}

class LemEditor extends HTMLElement {
    constructor() {
        super();

        const childProcess = cp.spawn("lem-rpc");
        this.rpcConnection = rpc.createMessageConnection(
            new rpc.StreamMessageReader(childProcess.stdout),
            new rpc.StreamMessageWriter(childProcess.stdin)
        );

        this.on('update-foreground', this.updateForeground.bind(this));
        this.on('update-background', this.updateBackground.bind(this));
        this.on('make-view', this.makeView.bind(this));
        this.on('delete-view', this.deleteView.bind(this));
        this.on('resize-view', this.resizeView.bind(this));
        this.on('move-view', this.moveView.bind(this));
        this.on('clear', this.clear.bind(this));
        this.on('clear-eol', this.clearEol.bind(this));
        this.on('clear-eob', this.clearEob.bind(this));
        this.on('put', this.put.bind(this));
        this.on('modeline-put', this.modelinePut.bind(this));
        this.on('touch', this.touch.bind(this));
        this.on('move-cursor', this.moveCursor.bind(this));
        this.on('scroll', this.scroll.bind(this));
        this.on('update-display', this.updateDisplay.bind(this));
        this.on('delete-pane', this.deletePane.bind(this));
        this.on('import', this.importModule.bind(this));
        this.on('exit', this.exit.bind(this));

        this.rpcConnection.listen();

        this.lemModule = document.createElement('lem-module');
        this.lemModule.style.float = 'left';
        this.appendChild(this.lemModule);
        this.sideElement = null;

        const [width, height] = getCurrentWindowSize();
        this.width = width;
        this.height = height;

        this.rpcConnection.sendRequest('ready', {
            "width": calcDisplayCols(this.width),
            "height": calcDisplayRows(this.height),
            "foreground": option.foreground,
            "background": option.background,
        });

        window.onkeydown = (e) => {
            const k = keyevent.convertKeyEvent(e);
            this.emitInput(kindKeyEvent, k);
        };

        ipcRenderer.on('resize', (event, message) => {
            this.resize(message.width, message.height);
        });

        ipcRenderer.on('command', (event, message) => {
            console.log(message);
            this.emitInput(kindCommand, message);
        })
    }

    on(method, handler) {
        this.rpcConnection.onNotification(method, handler);
    }

    setPane(e) {
        if (this.sideElement !== null) this.deletePane();
        this.resize(this.width / 2, this.height);
        this.appendChild(e);
        this.sideElement = e;
    }

    deletePane() {
        if (this.sideElement) {
            this.removeChild(this.sideElement);
            this.sideElement = null;
            this.resize(...getCurrentWindowSize());
        }
    }

    importModule(params) {
        try {
            require(params.name);
        } catch (e) { console.log(e); }
    }

    exit(params) {
        try {
            ipcRenderer.send('exit');
        } catch (e) { console.log(e); }
    }

    emitInput(kind, value) {
        this.rpcConnection.sendNotification('input', {
            "kind": kind,
            "value": value
        });
    }

    resize(width, height) {
        if (this.sideElement !== null) width /= 2;
        this.width = width;
        this.height = height;
        this.emitInput(kindResize, {
            "width": calcDisplayCols(width),
            "height": calcDisplayRows(height)
        });
        this.lemModule.style.width = width;
        this.lemModule.style.height = height;
    }

    updateForeground(params) {
        option.foreground = params;
    }

    updateBackground(params) {
        option.background = params;
    }

    makeView(params) {
        try {
            const { id, x, y, width, height, use_modeline, kind } = params;
            const view = new View(id, x, y, width, height, use_modeline, kind);
            view.allTags().forEach((child) => { this.lemModule.appendChild(child); });
            viewTable[id] = view;
        } catch (e) { console.log(e); }
    }

    deleteView(params) {
        try {
            const { id } = params.viewInfo;
            const view = viewTable[id];
            view.delete();
            delete viewTable[id];
        } catch (e) { console.log(e); }
    }

    resizeView(params) {
        try {
            const { viewInfo, width, height } = params;
            const view = viewTable[viewInfo.id];
            view.resize(width, height);
        } catch (e) { console.log(e); }
    }

    moveView(params) {
        try {
            const { x, y, viewInfo } = params;
            const view = viewTable[viewInfo.id];
            view.move(x, y);
        } catch (e) { console.log(e); }
    }

    clear(params) {
        try {
            const view = viewTable[params.viewInfo.id];
            view.clear();
        } catch (e) { console.log(e); }
    }

    clearEol(params) {
        try {
            const { viewInfo, x, y } = params;
            const view = viewTable[viewInfo.id];
            view.clearEol(x, y);
        } catch (e) { console.log(e); }
    }

    clearEob(params) {
        try {
            const { viewInfo, x, y } = params;
            const view = viewTable[viewInfo.id];
            view.clearEob(x, y);
        } catch (e) { console.log(e); }
    }

    put(params) {
        try {
            const { viewInfo, x, y, chars, attribute } = params;
            const view = viewTable[viewInfo.id];
            view.put(x, y, chars, attribute);
        } catch (e) { console.log(e); }
    }

    modelinePut(params) {
        try {
            const { viewInfo, x, y, chars, attribute } = params;
            const view = viewTable[viewInfo.id];
            view.modelinePut(x, chars, attribute);
        } catch (e) { console.log(e); }
    }

    touch(params) {
        try {
            const { viewInfo } = params;
            const view = viewTable[viewInfo.id];
            view.touch();
        } catch (e) { console.log(e); }
    }

    moveCursor(params) {
        try {
            const { viewInfo, x, y } = params;
            const view = viewTable[viewInfo.id];
            view.setCursor(x, y);
        } catch (e) { console.log(e); }
    }

    scroll(params) {
        try {
            const { viewInfo, n } = params;
            const view = viewTable[viewInfo.id];
            view.scroll(n);
        } catch (e) { console.log(e); }
    }

    updateDisplay(params) {
        try {
        } catch (e) { console.log(e); }
    }
}

class Surface {
    constructor(x, y, width, height, styles) {
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;
        this.canvas = document.createElement('canvas');
        this.canvas.style.position = 'absolute';
        for (let key in styles) {
            this.canvas.style[key] = styles[key];
        }
        this.ctx = this.canvas.getContext('2d', { alpha: false });
        this.ctx.textBaseline = 'top';
        this.ctx.font = fontAttribute.font;
        this.canvas2 = document.createElement('canvas');
        this.ctx2 = this.canvas2.getContext('2d', { alpha: false });
        this.ctx2.textBaseline = 'top';
        this.ctx2.font = fontAttribute.font;
        this.dirtyRectangles = [];
    }

    move(x, y) {
        this.x = x;
        this.y = y;
        this.canvas.style.left = x * fontAttribute.width;
        this.canvas.style.top = y * fontAttribute.height;
    }

    resize(width, height) {
        this.width = width;
        this.height = height;
        this.canvas.width = width * fontAttribute.width;
        this.canvas.height = height * fontAttribute.height;
        this.canvas2.width = width * fontAttribute.width;
        this.canvas2.height = height * fontAttribute.height;
    }

    drawBlock(x, y, w, h, color) {
        this.dirtyRectangles.push([x, y, w, h]);
        this.ctx2.fillStyle = color || option.background;
        this.ctx2.fillRect(
            x * fontAttribute.width,
            y * fontAttribute.height,
            w * fontAttribute.width,
            h * fontAttribute.height,
        );
    }

    drawChars(x, y, chars, font, color) {
        this.ctx2.fillStyle = color;
        this.ctx2.font = font;
        this.ctx2.textBaseline = 'top';
        x *= fontAttribute.width;
        y *= fontAttribute.height;
        for (let bytes of chars) {
            const str = utf8.getStringFromBytes(bytes, 1);
            this.ctx2.fillText(str, x, y);
            x += fontAttribute.width * bytes[0];
        }
    }

    drawUnderline(x, y, length, color) {
        this.ctx2.strokeStyle = color;
        this.ctx2.lineWidth = 1;
        this.ctx2.setLineDash([]);
        this.ctx2.beginPath();
        x *= fontAttribute.width;
        y = (y + 1) * fontAttribute.height - 3;
        this.ctx2.moveTo(x, y);
        this.ctx2.lineTo(x + fontAttribute.width * length, y);
        this.ctx2.stroke();
    }

    static calcCharsWidth(chars) {
        return chars.reduce((w, bytes) => { return w + bytes[0] }, 0);
    }

    put(x, y, chars, attribute) {
        const charsWidth = Surface.calcCharsWidth(chars);
        if (attribute === null) {
            this.drawBlock(x, y, charsWidth, 1, option.background);
            this.drawChars(x, y, chars, fontAttribute.font, option.foreground);
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
                font = 'bold ' + font;
            }
            this.drawBlock(x, y, charsWidth, 1, background);
            this.drawChars(x, y, chars, font, foreground);
            if (underline) {
                this.drawUnderline(x, y, chars.length, foreground);
            }
        }
    }

    touch() {
        for (let rect of this.dirtyRectangles) {
            const [x, y, w, h] = rect;
            if (w > 0 && h > 0) {
                const x1 = Math.ceil(x * fontAttribute.width);
                const y1 = y * fontAttribute.height;
                const w1 = Math.ceil(w * fontAttribute.width);
                const h1 = h * fontAttribute.height;
                const image = this.ctx2.getImageData(x1, y1, w1, h1);
                this.ctx.putImageData(image, x1, y1);
            }
        }
        this.dirtyRectangles = [];
    }

    scroll(n) {
        if (n > 0) {
            const image = this.ctx2.getImageData(
                0,
                n * fontAttribute.height,
                this.width * fontAttribute.width,
                (this.height - n) * fontAttribute.height,
            );
            this.ctx2.putImageData(image, 0, 0);
        } else {
            n = -n;
            const image = this.ctx2.getImageData(0,
                0,
                this.width * fontAttribute.width,
                (this.height - n) * fontAttribute.height
            );
            this.ctx2.putImageData(
                image,
                x * fontAttribute.width,
                (y + n) * fontAttribute.height
            );
        }
    }
}

const viewStyleTable = {
    "minibuffer": {},
    "popup": { "zIndex": 2 },
};

class View {
    constructor(id, x, y, width, height, use_modeline, kind) {
        this.id = id;
        this.width = width;
        this.height = height;
        this.use_modeline = use_modeline;
        this.editSurface = new Surface(x, y, width, height, viewStyleTable[kind] || {});
        if (use_modeline) {
            this.modelineSurface = new Surface(x, y + height, width, 1, { "zIndex": 1 });
        } else {
            this.modelineSurface = null;
        }
        this.move(x, y);
        this.resize(width, height);
        this.cursor = { x: null, y: null, color: option.foreground };
    }

    allTags() {
        if (this.modelineSurface !== null) {
            return [this.editSurface.canvas, this.modelineSurface.canvas];
        } else {
            return [this.editSurface.canvas];
        }
    }

    delete() {
        this.editSurface.canvas.parentNode.removeChild(this.editSurface.canvas);
        if (this.modelineSurface !== null) {
            this.modelineSurface.canvas.parentNode.removeChild(this.modelineSurface.canvas);
        }
    }

    move(x, y) {
        this.editSurface.move(x, y);
        if (this.modelineSurface !== null) {
            this.modelineSurface.move(x, y + this.height);
        }
    }

    resize(width, height) {
        this.width = width;
        this.height = height;
        this.editSurface.resize(width, height);
        if (this.modelineSurface !== null) {
            this.modelineSurface.move(this.x, this.editSurface.y + this.editSurface.height);
            this.modelineSurface.resize(width, 1);
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

    put(x, y, chars, attribute) {
        this.editSurface.put(x, y, chars, attribute);
    }

    modelinePut(x, chars, attribute) {
        if (this.modelineSurface !== null) {
            this.modelineSurface.put(x, 0, chars, attribute);
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

customElements.define('lem-module', LemModule);
customElements.define('lem-editor', LemEditor);