import { Editor } from './editor.js';

const canvas = document.querySelector('#editor');

async function main() {
  await Promise.all([
    document.fonts.load('19px file-icons'),
    document.fonts.load('19px AllTheIcons'),
    document.fonts.load('19px fontawesome'),
    document.fonts.load('19px material-design-icons'),
    document.fonts.load('19px octicons')
  ]);
  await document.fonts.ready;
  const protocol = window.location.protocol === 'https:' ? 'wss' : 'ws';
  const editor = new Editor({
    canvas: canvas,
    fontName: 'Monospace',
    fontSize: 19,
    url: `${protocol}://${window.location.hostname}:${window.location.port}`,
    onExit: null,
    onClosed: null,
  });

  window.addEventListener('message', (event) => {
    if (event.data.type === 'invoke-lem') {
      editor.notifyToServer(event.data.method, event.data.args);
    }
  });

  editor.init();
}

main();
