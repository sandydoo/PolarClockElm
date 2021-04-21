import { Elm } from '../src/Main.elm'

let width = window.innerWidth,
    height = window.innerHeight;

let supportsP3Color = CSS.supports('color: color(display-p3 1 1 1)');

let app = Elm.Main.init({
  node: document.getElementById('polar-clock'),
  flags: {
    currentTime: Date.now(),
    dimensions: { width, height },
    supportsP3Color,
  }
});
