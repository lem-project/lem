'use strict';

const marked = require('marked');
const utf8 = require('utf-8');

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
