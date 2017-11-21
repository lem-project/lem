'use strict';

const MODIFIERS = ["Shift", "Control", "Alt", "Meta"];

exports.convertKeyEvent = function (e) {
    let key = e.key;
    if (MODIFIERS.indexOf(key) !== -1) {
        return null;
    }
    return {
        "key": key,
        "ctrl": e.ctrlKey,
        "meta": e.altKey,
        "super": e.metaKey
    }
}