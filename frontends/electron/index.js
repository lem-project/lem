"use strict";

const electron = require("electron");
const menu = require("./menu");

require("@electron/remote/main").initialize();

let mainWindow;

electron.app.on("ready", function () {
  mainWindow = new electron.BrowserWindow({
    show: false,
    webPreferences: {
      nodeIntegration: true,
      contextIsolation: false,
      devTools: true,
    },
  });

  require("@electron/remote/main").enable(mainWindow.webContents);

  mainWindow.once("ready-to-show", () => {
    mainWindow.show();
    //mainWindow.webContents.openDevTools();
  });

  mainWindow.setMenu(null);
  menu.setMenu(mainWindow);
  mainWindow.loadURL(`file://${__dirname}/index.html`);
  mainWindow.on("closed", function () {
    mainWindow = null;
  });

  // 'will-resize' event handling.
  // Linux: Not supported
  // MacOS: Does not work properly e.g. https://github.com/electron/electron/issues/21777
  if (process.platform === "win32") {
    mainWindow.on("will-resize", function (event) {
      event.preventDefault();
    });
  }

  electron.ipcMain.on("exit", function () {
    electron.app.quit();
  });
});

electron.app.on("window-all-closed", function () {
  electron.app.quit();
});

electron.app.on("activate", function () {});
