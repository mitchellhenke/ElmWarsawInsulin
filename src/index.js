import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var storedState = localStorage.getItem('carbRatioCache');
var startingState = storedState ? storedState : "10";

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: startingState
});

app.ports.carbRatioCache.subscribe(function(string) {
  localStorage.setItem('carbRatioCache', string);
});

registerServiceWorker();
