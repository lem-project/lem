'use strict';

const lemEditor = document.getElementById('lem-editor');
const view = document.createElement('webview');
view.autosize = 'on';
view.style.height = '100%';

lemEditor.on('webview-open', function (params) {
    view.src = params.url;
});

lemEditor.setPane(view);
