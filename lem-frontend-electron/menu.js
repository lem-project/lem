'use strict';

const { Menu, dialog } = require('electron');

function setMenu(win) {
    const template = [
        {
            label: 'File',
            submenu: [
                {
                    label: 'Open File',
                    click: () => {
                        dialog.showOpenDialog(null,
                            (files) => {
                                if (files)
                                    win.webContents.send('command', ['find-file', files[0]]);
                            });
                    }
                },
                {
                    label: 'Open Directory',
                    click: () => {
                        dialog.showOpenDialog({ properties: ['openFile', 'openDirectory'] },
                            (files) => {
                                if (files)
                                    win.webContents.send('command', ['find-file', files[0]]);
                            });
                    }
                },
                {
                    type: 'separator'
                },
                {
                    label: 'Save File',
                    click: () => {
                        win.webContents.send('command', ['save-buffer']);
                    }
                },
                {
                    label: 'Save As...',
                    click: () => {
                        dialog.showOpenDialog({ properties: ['openFile', 'promptToCreate'] },
                            (files) => {
                                if (files)
                                    win.webContents.send('command', ['write-file', files[0]]);
                            });
                    }
                },
                {
                    type: 'separator'
                },
                {
                    label: 'openDevTools',
                    click: () => {
                        win.webContents.openDevTools();
                    }
                },
                {
                    type: 'separator'
                },
                {
                    label: 'Quit',
                    click: () => {
                        win.webContents.send('command', ['exit-lem']);
                    }
                }
            ]
        }
    ]

    const menu = Menu.buildFromTemplate(template);
    Menu.setApplicationMenu(menu);
}

exports.setMenu = setMenu;