"use strict";

const fs = require("fs");
const path = require("path");
const yaml = require("js-yaml");

var defaultOption = {
  fontName: "monospace",
  fontSize: 18,
  background: "#333",
  foreground: "#ccc",
  cols: 80,
  rows: 24,
};

// load option file
const OPTION_FILE_NAME = "electron.yml";
const homedir =
  process.env[process.platform === "win32" ? "USERPROFILE" : "HOME"];
const lemHome = process.env["LEM_HOME"] || path.join(homedir, ".lem");
const optionFilePath = path.join(lemHome, OPTION_FILE_NAME);
function loadOption(path) {
  try {
    return yaml.safeLoad(fs.readFileSync(path, "utf-8"));
  } catch (e) {
    return {};
  }
}
const option = Object.assign(defaultOption, loadOption(optionFilePath));

exports.option = option;
