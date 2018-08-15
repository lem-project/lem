'use strict';
const MODIFIERS = ["Shift", "Control", "Alt", "Meta"];
const CONVERT_TABLE = {
    "Enter": "Return",
    "ArrowRight": "Right",
    "ArrowLeft": "Left",
    "ArrowUp": "Up",
    "ArrowDown": "Down"
};

exports.convertKeyEvent = function (e) {
    let key = e.key;
    if (MODIFIERS.indexOf(key) !== -1) {
        return null;
    }
    key = CONVERT_TABLE[key] || key;
    return {
        "key": key,
        "ctrl": e.ctrlKey,
        "meta": e.altKey,
        "super": e.metaKey,
        "shift": e.shiftKey,
    }
}
