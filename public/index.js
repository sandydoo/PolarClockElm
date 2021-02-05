import { Elm } from '../src/Main.elm'

let width = window.innerWidth,
    height = window.innerHeight;

let app = Elm.Main.init({
  node: document.getElementById('polar-clock'),
  flags: {
    currentTime: Date.now(),
    dimensions: { width, height }
  }
});
