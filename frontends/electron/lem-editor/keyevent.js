"use strict";
const keymap = process.platform === "darwin" ? require("native-keymap") : null;

const MODIFIERS = ["Shift", "Control", "Alt", "Meta"];
const CONVERT_TABLE = {
  Enter: "Return",
  ArrowRight: "Right",
  ArrowLeft: "Left",
  ArrowUp: "Up",
  ArrowDown: "Down",
};
// bypass layout and locale settings of system. use Standard 101 layout and en_US locale.
// https://www.w3.org/TR/uievents-code/#code-value-tables
const CODE_VALUE_TABLE = {
  Backquote: ["`", "~"],
  Backslash: ["\\", "|"],
  BracketLeft: ["[", "{"],
  BracketRight: ["]", "}"],
  Comma: [",", "<"],
  Digit0: ["0", ")"],
  Digit1: ["1", "!"],
  Digit2: ["2", "@"],
  Digit3: ["3", "#"],
  Digit4: ["4", "$"],
  Digit5: ["5", "%"],
  Digit6: ["6", "^"],
  Digit7: ["7", "&"],
  Digit8: ["8", "*"],
  Digit9: ["9", "("],
  Equal: ["=", "+"],
  KeyA: ["a", "A"],
  KeyB: ["b", "B"],
  KeyC: ["c", "C"],
  KeyD: ["d", "D"],
  KeyE: ["e", "E"],
  KeyF: ["f", "F"],
  KeyG: ["g", "G"],
  KeyH: ["h", "H"],
  KeyI: ["i", "I"],
  KeyJ: ["j", "J"],
  KeyK: ["k", "K"],
  KeyL: ["l", "L"],
  KeyM: ["m", "M"],
  KeyN: ["n", "N"],
  KeyO: ["o", "O"],
  KeyP: ["p", "P"],
  KeyQ: ["q", "Q"],
  KeyR: ["r", "R"],
  KeyS: ["s", "S"],
  KeyT: ["t", "T"],
  KeyU: ["u", "U"],
  KeyV: ["v", "V"],
  KeyW: ["w", "W"],
  KeyX: ["x", "X"],
  KeyY: ["y", "Y"],
  KeyZ: ["z", "Z"],
  Minus: ["-", "_"],
  Period: [".", ">"],
  Quote: ["'", '"'],
  Semicolon: [";", ":"],
  Slash: ["/", "?"],
};

exports.convertKeyEvent = function (e) {
  let key = e.key;
  if (MODIFIERS.indexOf(key) !== -1) {
    return null;
  }
  key = CONVERT_TABLE[key] || key;
  if (keymap !== null && e.altKey && e.code != "Backspace") {
    key = keymap.getKeyMap()[e.code][e.shiftKey ? "withShift" : "value"];
  }
  return {
    key: key,
    ctrl: e.ctrlKey,
    meta: e.altKey,
    super: e.metaKey,
    shift: e.shiftKey,
  };
};
