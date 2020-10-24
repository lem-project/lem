"use strict";

const { Menu, dialog } = require("electron");

function setMenu(win) {
  const template = [
    {
      label: "File",
      submenu: [
        {
          label: "Open File",
          click: () => {
            dialog.showOpenDialog(null, {}).then((params) => {
              const file_path = params.filePaths[0];
              if (file_path)
                win.webContents.send("command", ["find-file", file_path]);
            });
          },
        },
        {
          label: "Open Directory",
          click: () => {
            dialog
              .showOpenDialog({ properties: ["openFile", "openDirectory"] })
              .then((params) => {
                const directory_path = params.filePaths[0];
                if (directory_path)
                  win.webContents.send("command", [
                    "find-file",
                    directory_path,
                  ]);
              });
          },
        },
        {
          type: "separator",
        },
        {
          label: "Save File",
          click: () => {
            win.webContents.send("command", ["save-buffer"]);
          },
        },
        {
          label: "Save As...",
          click: () => {
            dialog
              .showOpenDialog({ properties: ["openFile", "promptToCreate"] })
              .then((params) => {
                const file_path = params.filePaths[0];
                if (file_path)
                  win.webContents.send("command", ["write-file", file_path]);
              });
          },
        },
        {
          type: "separator",
        },
        {
          label: "openDevTools",
          click: () => {
            win.webContents.openDevTools();
          },
        },
        {
          type: "separator",
        },
        {
          label: "Quit",
          click: () => {
            win.webContents.send("command", ["exit-lem"]);
          },
        },
      ],
    },
  ];

  const menu = Menu.buildFromTemplate(template);
  Menu.setApplicationMenu(menu);
}

exports.setMenu = setMenu;
