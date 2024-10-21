import { Editor } from './editor.js';

const canvas = document.querySelector('#editor');

function main() {
  document.fonts.ready.then(() => {
    const editor = new Editor({
      canvas: canvas,
      fontName: 'Monospace',
      fontSize: 19,
      onLoaded: null,
      url: `ws://${window.location.hostname}:50000`,
      onExit: null,
      onClosed: null,
      onRestart: null,
      onUserInput: null,
    });

    editor.init();
  });
}

main();
