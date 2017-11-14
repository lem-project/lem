'use strict';

const marked = require('marked');

class Markdown extends HTMLElement {
    constructor() {
        super();
    }

    update(text) {
        try {
            const md = marked(text);
            this.innerHTML = md;
        } catch (e) {
            console.error(e);
        }
    }
}

customElements.define('lem-markdown', Markdown);

const lemEditor = document.getElementById('lem-editor');
const markdown = document.createElement('lem-markdown');
lemEditor.on('markdown-update', function (params) {
    markdown.update(params.text);
});
lemEditor.setPane(markdown);