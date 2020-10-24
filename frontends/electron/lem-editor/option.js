"use strict";

if (process.platform === "darwin") {
  var option = {
    fontName: "Menlo",
    fontSize: 15,
    background: "#333",
    foreground: "#ccc",
    cols: 80,
    rows: 24,
  };
} else {
  var option = {
    fontName: "monospace",
    fontSize: 14,
    background: "#333",
    foreground: "#ccc",
    cols: 80,
    rows: 24,
  };
}

exports.option = option;
